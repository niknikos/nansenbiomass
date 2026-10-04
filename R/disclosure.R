# disclosure: check candidate exports and stage them for release
# Data class touched: C1 to C2; Claude may run it on synthetic data only (docs/spec.md, Section 4).
#
# Implements the airlock (docs/spec.md, Section 3.3, layer 5) under D-03: a field
# whitelist limited to the Section 9 fields, a minimum number of stations and of
# stations with a positive catch behind every released cell, and no release that
# lets a withheld stratum be recovered by subtraction from the total.

# Rules version, recorded in the `disclosure` field of every staged export.
disclosure_rules_version <- "rules-v1"

# D-03 floors: callers and configurations may raise these, never lower them.
min_stations_floor <- 5L
min_positive_floor <- 3L

allowed_values <- list(
  method = c("stox_sweptarea", "sdmtmb", "stox_acoustic"),
  quantity = c("biomass", "abundance"),
  unit = c("tonnes", "kg", "millions", "thousands"),
  ci_type = c("bootstrap_percentile_95", "model_se_95")
)

#' The released estimate schema
#'
#' The long-format table that every method writes, and the only estimate format
#' permitted to reach `outbox/` (docs/spec.md, Section 9). Its fields are the
#' disclosure whitelist.
#'
#' @return A tibble with one row per field and the columns `field`, `type`,
#'   `required` (must not be missing) and `description`.
#' @export
#' @examples
#' estimate_schema()
estimate_schema <- function() {
  tibble::tribble(
    ~field,         ~type,       ~required, ~description,
    "survey_id",    "character", TRUE,      "Programme survey identifier",
    "year",         "integer",   TRUE,      "Survey year",
    "species_code", "character", TRUE,      "Taxon code from the Programme taxonomy reference",
    "stratum",      "character", TRUE,      "Stratum name, or total",
    "method",       "character", TRUE,      "stox_sweptarea, sdmtmb or stox_acoustic",
    "quantity",     "character", TRUE,      "biomass or abundance",
    "value",        "double",    TRUE,      "Estimate",
    "unit",         "character", TRUE,      "For example tonnes or millions",
    "cv",           "double",    FALSE,     "Coefficient of variation",
    "ci_lower",     "double",    FALSE,     "Lower interval bound",
    "ci_upper",     "double",    FALSE,     "Upper interval bound",
    "ci_type",      "character", FALSE,     "For example bootstrap_percentile_95 or model_se_95",
    "config_hash",  "character", TRUE,      "Hash of the configuration file used",
    "code_version", "character", TRUE,      "Package version and Git commit",
    "run_time",     "datetime",  TRUE,      "UTC timestamp of the run",
    "disclosure",   "character", FALSE,     "Outcome of the disclosure check"
  )
}

# Field names that identify sampling units or positions: NMDBiotic keys and
# position fields (BAIT's field glossary), plus common generic names.
detail_field_pattern <- paste0(
  "^(missiontype|platform|missionnumber|callsignal|cruise|serialnumber|station|",
  "catchsampleid|specimenid|ageid|preysampleid|latitude.*|longitude.*|lat|lon|long|",
  "x|y|geometry|geom|haul.*|station.*|psu|edsu|ssu|individual.*|sample.*|position.*)$"
)

# Text resembling a coordinate: decimal degrees with three or more decimals, or
# degree signs. A heuristic, as defence in depth behind the whitelist.
coordinate_text_pattern <- "((?<![\\d.,])-?\\d{1,3}[.,]\\d{3,}(?!\\d))|(\\d{1,3}\\s*\u00b0)"

key_fields <- c("survey_id", "year", "species_code", "stratum", "method", "quantity")
cell_fields <- c("survey_id", "year", "species_code", "stratum")

dc_row <- function(check_id, n_rows, fields = NA_character_, description, status = NULL) {
  tibble::tibble(
    check_id = check_id,
    status = if (is.null(status)) ifelse(n_rows > 0, "fail", "pass") else status,
    n_rows = as.integer(n_rows),
    fields = if (length(fields) == 0L || all(is.na(fields))) NA_character_ else
      paste(unique(safe_field_names(fields)), collapse = ", "),
    description = description
  )
}

is_whole <- function(x) is.numeric(x) && all(is.na(x) | x == round(x))

type_ok <- function(x, type) {
  switch(type,
    character = is.character(x),
    integer = is_whole(x),
    double = is.numeric(x),
    datetime = inherits(x, "POSIXct") ||
      (is.character(x) && all(is.na(x) | grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$", x)))
  )
}

check_floor <- function(value, floor, code, name) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) || value < floor) {
    nb_abort(code, paste0("`", name, "` must be a single number of at least ", floor,
                          " (D-03); it may be raised, never lowered."))
  }
  as.integer(value)
}

check_support <- function(support, strata) {
  need <- c(cell_fields, "n_stations", "n_positive")
  if (!is.data.frame(support) || !all(need %in% names(support))) {
    nb_abort("DC-ARG-03", paste0("`support` must be a data frame with the columns ",
                                 paste(need, collapse = ", "), "."))
  }
  if (any(support$stratum == "total", na.rm = TRUE)) {
    nb_abort("DC-ARG-04", "`support` holds strata only; totals are summed from them.")
  }
  if (any(!support$stratum %in% strata)) {
    nb_abort("DC-ARG-05", "`support` names strata that are not in `strata`.")
  }
  if (any(support$n_positive > support$n_stations, na.rm = TRUE)) {
    nb_abort("DC-ARG-06", "`support` has more positive stations than stations in some rows.")
  }
  support <- tibble::as_tibble(support[need])
  support$year <- as.integer(support$year)
  totals <- support |>
    dplyr::summarise(
      n_stations = sum(.data$n_stations), n_positive = sum(.data$n_positive),
      .by = c("survey_id", "year", "species_code")
    ) |>
    dplyr::mutate(stratum = "total")
  dplyr::bind_rows(support, totals)
}

#' Check a candidate export for disclosure
#'
#' Validates a candidate export against the airlock rules (D-03) before a
#' person decides whether to release it. The export must use the Section 9
#' schema ([estimate_schema()]); the station counts behind each cell come from
#' a separate `support` table that stays in the data zone and is never staged.
#'
#' The checks, each reported as `pass` or `fail`:
#'
#' * `DC-FLD-01` only whitelisted (Section 9) fields;
#'   `DC-FLD-02` all Section 9 fields present;
#'   `DC-FLD-03` no spatial objects or list columns;
#'   `DC-FLD-04` no field named like a station, haul or individual identifier, or a
#'   position.
#' * `DC-TYP-01` field types match the schema; `DC-TYP-02` required fields filled.
#' * `DC-VAL-01` allowed values for `method`, `quantity`, `unit`, `ci_type` and
#'   `stratum` (the survey's strata or `total`); `DC-VAL-02` no text that resembles a
#'   coordinate.
#' * `DC-AGG-01` no duplicate cells, which suggest record-level rows.
#' * `DC-AGG-02` every cell has support from at least `min_stations` stations.
#' * `DC-AGG-03` differencing: where the total is released, no group has exactly
#'   one stratum withheld, since the total minus the released strata would reveal it.
#' * `DC-AGG-04` positive stations: no cell rests on fewer than `min_positive`
#'   stations with a positive catch, unless none was positive (the estimate is
#'   then zero and reveals no catch).
#'
#' The report names checks, fields and row counts only, never values.
#'
#' @param export The candidate export: a data frame with the Section 9 fields.
#' @param support A data frame with one row per survey, year, species and
#'   stratum (no totals): `survey_id`, `year`, `species_code`, `stratum`,
#'   `n_stations` and `n_positive` (stations with a positive catch). The same
#'   counts apply to every method, so that both frameworks withhold the same cells.
#' @param strata Character vector of the survey's stratum names.
#' @param min_stations Minimum number of stations behind a released cell; at
#'   least 5 (D-03).
#' @param min_positive Minimum number of stations with a positive catch behind a
#'   released cell that is not zero; at least 3 (D-03).
#' @return An `nb_disclosure_report` tibble with the columns `check_id`,
#'   `status`, `n_rows`, `fields` and `description`, and the attributes
#'   `outcome` (`"pass"` or `"fail"`), `rules`, `min_stations` and `min_positive`.
#' @export
#' @examples
#' sv <- synth_survey(seed = 1)
#' cand <- synth_export(sv)
#' check_disclosure(cand$export, cand$support, strata = sv$design$strata$stratum)
check_disclosure <- function(export, support, strata, min_stations = min_stations_floor,
                             min_positive = min_positive_floor) {
  min_stations <- check_floor(min_stations, min_stations_floor, "DC-ARG-01", "min_stations")
  min_positive <- check_floor(min_positive, min_positive_floor, "DC-ARG-02", "min_positive")
  if (!is.character(strata) || length(strata) == 0L || any(strata == "total")) {
    nb_abort("DC-ARG-07", "`strata` must name the survey's strata (not `total`).")
  }
  if (!is.data.frame(export)) {
    nb_abort("DC-ARG-08", "`export` must be a data frame.")
  }
  support <- check_support(support, strata)
  schema <- estimate_schema()
  out <- list()
  add <- function(...) out[[length(out) + 1L]] <<- dc_row(...)
  n <- nrow(export)

  # Fields
  extra <- setdiff(names(export), schema$field)
  add("DC-FLD-01", length(extra), extra, "Only Section 9 fields")
  missing <- setdiff(schema$field, names(export))
  add("DC-FLD-02", length(missing), missing, "All Section 9 fields present")
  spatial <- inherits(export, c("sf", "sfc", "SpatVector", "Spatial"))
  list_cols <- names(export)[vapply(export, function(col) is.list(col) || inherits(col, "sfc"),
                                    logical(1))]
  add("DC-FLD-03", as.integer(spatial) + length(list_cols), list_cols,
      "No spatial objects or list columns")
  detail <- names(export)[grepl(detail_field_pattern, names(export), ignore.case = TRUE)]
  add("DC-FLD-04", length(detail), detail,
      "No station, haul, individual identifier or position fields")

  present <- intersect(schema$field, names(export))
  df <- as.data.frame(export)[present]
  if (spatial) df <- sf::st_drop_geometry(export)[present]

  # Types and required values
  wrong <- present[!mapply(type_ok, df[present], schema$type[match(present, schema$field)])]
  add("DC-TYP-01", length(wrong), wrong, "Field types match the Section 9 schema")
  req <- intersect(schema$field[schema$required], present)
  n_missing <- if (length(req)) sum(!stats::complete.cases(df[req])) else 0L
  add("DC-TYP-02", n_missing, req, "Required fields filled")

  # Values
  bad_rows <- rep(FALSE, n)
  bad_fields <- character(0)
  for (f in intersect(names(allowed_values), present)) {
    v <- df[[f]]
    bad <- !is.na(v) & !v %in% allowed_values[[f]]
    if (f != "ci_type") bad <- bad | is.na(v)
    if (any(bad)) bad_fields <- c(bad_fields, f)
    bad_rows <- bad_rows | bad
  }
  if ("stratum" %in% present) {
    bad <- is.na(df$stratum) | !df$stratum %in% c(strata, "total")
    if (any(bad)) bad_fields <- c(bad_fields, "stratum")
    bad_rows <- bad_rows | bad
  }
  add("DC-VAL-01", sum(bad_rows), bad_fields, "Allowed values and known strata")
  text_cols <- present[vapply(df[present], is.character, logical(1))]
  text_cols <- setdiff(text_cols, c("run_time", "config_hash", "code_version"))
  coord_rows <- rep(FALSE, n)
  coord_fields <- character(0)
  for (f in text_cols) {
    hit <- !is.na(df[[f]]) & grepl(coordinate_text_pattern, df[[f]], perl = TRUE)
    if (any(hit)) coord_fields <- c(coord_fields, f)
    coord_rows <- coord_rows | hit
  }
  add("DC-VAL-02", sum(coord_rows), coord_fields, "No text resembling coordinates")

  # Aggregation
  keys_present <- all(key_fields %in% present)
  if (keys_present) {
    dup <- sum(duplicated(df[key_fields]))
    add("DC-AGG-01", dup, key_fields, "One row per survey, year, species, stratum, method and quantity")
    cells <- tibble::as_tibble(df[key_fields]) |>
      dplyr::mutate(year = as.integer(.data$year)) |>
      dplyr::left_join(support, by = cell_fields)
    low <- is.na(cells$n_stations) | cells$n_stations < min_stations
    add("DC-AGG-02", sum(low), "stratum",
        paste0("Every cell has support from at least ", min_stations, " stations"))
    released <- tibble::as_tibble(df[key_fields])
    groups <- released |>
      dplyr::summarise(
        has_total = any(.data$stratum == "total"),
        n_withheld = sum(!strata %in% .data$stratum),
        .by = c("survey_id", "year", "species_code", "method", "quantity")
      )
    diff_groups <- groups$has_total & groups$n_withheld == 1L
    add("DC-AGG-03", sum(diff_groups), "stratum",
        "Total not released alongside all strata but one (differencing)")
    few_pos <- !is.na(cells$n_positive) & cells$n_positive > 0L & cells$n_positive < min_positive
    add("DC-AGG-04", sum(few_pos), "stratum",
        paste0("No non-zero cell rests on fewer than ", min_positive, " positive stations"))
  } else {
    for (id in c("DC-AGG-01", "DC-AGG-02", "DC-AGG-03", "DC-AGG-04")) {
      add(id, n, setdiff(key_fields, present), "Not run: key fields missing", status = "fail")
    }
  }

  report <- dplyr::bind_rows(out)
  class(report) <- c("nb_disclosure_report", class(report))
  attr(report, "outcome") <- if (all(report$status == "pass")) "pass" else "fail"
  attr(report, "rules") <- disclosure_rules_version
  attr(report, "min_stations") <- min_stations
  attr(report, "min_positive") <- min_positive
  report
}

#' @export
print.nb_disclosure_report <- function(x, ...) {
  cat(sprintf(
    "<nb_disclosure_report> %s: %d of %d checks failed (%s, min_stations = %d, min_positive = %d)\n",
    toupper(attr(x, "outcome")), sum(x$status == "fail"), nrow(x), attr(x, "rules"),
    attr(x, "min_stations"), attr(x, "min_positive")
  ))
  y <- x
  class(y) <- setdiff(class(y), "nb_disclosure_report")
  print(tibble::as_tibble(y)[c("check_id", "status", "n_rows", "fields")], n = Inf)
  invisible(x)
}

#' Stage a candidate export for release
#'
#' Runs [check_disclosure()] and writes the result to a staging folder in the
#' data zone. The check report is always written; the export is written only
#' when every check passes, with the `disclosure` field filled. Staging never
#' writes to `outbox/`: a person reviews the report and copies passing files
#' there (docs/spec.md, Section 3.3, layer 5). A staging path that contains an
#' `outbox` folder is refused, and in a cloud session only a folder inside
#' `tempdir()` is accepted.
#'
#' @inheritParams check_disclosure
#' @param run_label Name of the run's staging subfolder: letters, digits, `.`,
#'   `_` and `-` only.
#' @param staging_dir The staging folder. Defaults to `staging` under
#'   [data_root()].
#' @return Invisibly, a list with `outcome`, `report` and `files` (the paths
#'   written).
#' @export
#' @examples
#' sv <- synth_survey(seed = 1)
#' cand <- synth_export(sv)
#' staged <- stage_export(cand$export, cand$support, sv$design$strata$stratum,
#'                        run_label = "synthetic-example",
#'                        staging_dir = file.path(tempdir(), "staging"))
#' staged$outcome
stage_export <- function(export, support, strata, run_label,
                         min_stations = min_stations_floor,
                         min_positive = min_positive_floor,
                         staging_dir = file.path(data_root(), "staging")) {
  if (!is.character(run_label) || length(run_label) != 1L ||
      !grepl("^[A-Za-z0-9._-]+$", run_label) || run_label %in% c(".", "..")) {
    nb_abort("DC-STAGE-02", "`run_label` may contain only letters, digits, '.', '_' and '-'.")
  }
  parts <- strsplit(staging_dir, "[/\\\\]")[[1]]
  if (any(tolower(parts) == "outbox")) {
    nb_abort("DC-STAGE-01", "Staging into an outbox folder is refused; a person releases files.")
  }
  check_cloud_root(staging_dir)
  if (any(tolower(strsplit(normalize_existing(staging_dir), "/")[[1]]) == "outbox")) {
    nb_abort("DC-STAGE-01", "Staging into an outbox folder is refused; a person releases files.")
  }
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
  report <- check_disclosure(export, support, strata, min_stations, min_positive)
  out_dir <- file.path(staging_dir, run_label)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  report_csv <- file.path(out_dir, "disclosure-report.csv")
  report_txt <- file.path(out_dir, "disclosure-report.txt")
  estimates_csv <- file.path(out_dir, "estimates.csv")
  utils::write.csv(as.data.frame(report), report_csv, row.names = FALSE, na = "")
  writeLines(utils::capture.output(print(report)), report_txt)
  files <- c(report_csv, report_txt)
  if (attr(report, "outcome") == "pass") {
    schema <- estimate_schema()
    out <- as.data.frame(export)[schema$field]
    if (inherits(out$run_time, "POSIXct")) {
      out$run_time <- format(out$run_time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    }
    out$disclosure <- sprintf("pass:%s:min%d:pos%d", attr(report, "rules"),
                              attr(report, "min_stations"), attr(report, "min_positive"))
    utils::write.csv(out, estimates_csv, row.names = FALSE, na = "")
    files <- c(files, estimates_csv)
  } else if (file.exists(estimates_csv)) {
    # A failing rerun must not leave an earlier passing export behind.
    file.remove(estimates_csv)
  }
  invisible(list(outcome = attr(report, "outcome"), report = report, files = files))
}

#' A synthetic candidate export
#'
#' Builds a candidate export in the Section 9 schema from a synthetic survey's
#' true values, with the matching `support` table, for testing the airlock and
#' for examples. The values are the known truth, not an estimate; `method` is
#' labelled `stox_sweptarea` only so that the schema is complete.
#'
#' @param sv An `nb_synth` object from [synth_survey()].
#' @param quantity `"biomass"`, `"abundance"` or both.
#' @return A list with `export` (a tibble in the Section 9 schema) and `support`
#'   (station and positive-station counts by species and stratum).
#' @export
#' @examples
#' cand <- synth_export(synth_survey(seed = 1))
#' cand$support
synth_export <- function(sv, quantity = "biomass") {
  if (!inherits(sv, "nb_synth")) {
    nb_abort("SY-ARG-03", "`sv` must come from synth_survey().")
  }
  truth <- sv$truth[sv$truth$quantity %in% quantity, ]
  export <- truth |>
    dplyr::mutate(
      method = "stox_sweptarea",
      cv = 0.2,
      ci_lower = .data$value * 0.6,
      ci_upper = .data$value * 1.4,
      ci_type = "bootstrap_percentile_95",
      config_hash = "synthetic",
      code_version = paste0("nansenbiomass ", utils::packageVersion("nansenbiomass")),
      run_time = as.POSIXct("2000-01-01 00:00:00", tz = "UTC"),
      disclosure = NA_character_
    ) |>
    dplyr::select(dplyr::all_of(estimate_schema()$field))
  stations <- sv$stations[sv$stations$design_station, ]
  catch <- sv$survey$catch
  species <- sv$design$species$species_code
  support <- dplyr::bind_rows(lapply(species, function(sp) {
    pos <- unique(catch$serialnumber[catch$catchcategory == sp & catch$catchweight > 0])
    stations |>
      dplyr::summarise(
        n_stations = dplyr::n(),
        n_positive = sum(.data$serialnumber %in% pos),
        .by = "stratum"
      ) |>
      dplyr::mutate(species_code = sp)
  })) |>
    dplyr::mutate(survey_id = sv$design$mission$cruise, year = sv$design$year) |>
    dplyr::select("survey_id", "year", "species_code", "stratum", "n_stations", "n_positive")
  list(export = export, support = support)
}

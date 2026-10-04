# stox_official: helpers for working with an existing, official StoX project
# Data class touched: C0/C1 (official projects hold station keys and file
# locations; the helpers run on the laptop and print counts only).

# Station keys listed in a filter expression, or NULL if it has no clause on the
# field `Station`. Only plain exclusions are understood: `Station %notin% c(...)`,
# `!Station %in% c(...)` and `Station != '...'`; any other clause on the field is
# refused, without echoing it.
parse_station_exclusions <- function(expr) {
  parts <- split_expression(expr)
  if (is.null(parts)) nb_abort("SX-EXC-04", "A station filter in the project could not be parsed.")
  quoted <- function(x) {
    lit <- regmatches(x, gregexpr("'[^']*'|\"[^\"]*\"", x))[[1]]
    rest <- gsub("'[^']*'|\"[^\"]*\"", "", x)
    if (length(lit) == 0L || !grepl("^[\\s,)\\]]*$", rest, perl = TRUE)) return(NULL)
    substr(lit, 2L, nchar(lit) - 1L)
  }
  list_open <- "(?:c\\(|\\[|\\()"
  patterns <- c(
    paste0("^\\s*\\(*\\s*Station\\s*%notin%\\s*", list_open, "(.*)(?:\\)|\\])\\s*\\)*\\s*$"),
    paste0("^\\s*\\(*\\s*!\\s*\\(?\\s*Station\\s*%in%\\s*", list_open, "(.*)(?:\\)|\\])\\s*\\)*\\s*$"),
    "^\\s*\\(*\\s*Station\\s*!=\\s*(.*?)\\s*\\)*\\s*$"
  )
  keys <- character(0)
  found <- FALSE
  for (cl in parts$clauses) {
    if (!grepl("(^|[^A-Za-z0-9_.])Station($|[^A-Za-z0-9_])", cl)) next
    found <- TRUE
    ok <- FALSE
    for (pat in patterns) {
      m <- regmatches(cl, regexec(pat, cl, perl = TRUE))[[1]]
      if (length(m) == 2L) {
        k <- quoted(m[2])
        if (!is.null(k)) { keys <- c(keys, k); ok <- TRUE; break }
      }
    }
    if (!ok) nb_abort("SX-EXC-04", "A station filter in the project is not a plain exclusion list.")
  }
  if (found) unique(keys) else NULL
}

#' Build a station exclusion file from an official StoX project
#'
#' Official projects often leave out individual stations with a filter such as
#' `Station %notin% c(...)`, which lists StoX station keys. This reads that list
#' from the project's `project.json` (StoX 3 or later), maps the keys to serial
#' numbers by building StoxBiotic from the project's own copy of the biotic file,
#' and writes the serial numbers to a text file in the data zone, one per line,
#' for `inclusion.exclude_stations_file` in a survey configuration. Station keys
#' and serial numbers are station-level (class C1): the file stays in the data
#' zone, and the function prints counts only. Only plain exclusions are
#' understood; any other filter on the station key is refused. Keys that match no
#' station in the file are counted and reported with a warning (for example if the
#' StoX version that made the project builds keys differently).
#'
#' @param project Path, relative to `root`, of the official project folder (or of
#'   its `project.json`).
#' @param biotic Path, relative to `root`, of the biotic file to map the keys on:
#'   the project's own copy of it.
#' @param out Path, relative to `root`, of the exclusion file to write.
#' @param overwrite Replace an existing exclusion file.
#' @inheritParams read_survey
#' @return Invisibly, an `nb_exclusions` list with the counts `n_listed`,
#'   `n_matched`, `n_unmatched` and `n_serials`.
#' @export
#' @examples
#' if (requireNamespace("RstoxFramework", quietly = TRUE)) {
#'   root <- tempfile("nansen-root-")
#'   dir.create(file.path(root, "surveys"), recursive = TRUE)
#'   dir.create(file.path(root, "official", "process"), recursive = TRUE)
#'   sv <- synth_survey(seed = 1)
#'   write_biotic(sv, file.path(root, "surveys", "synthetic-seed1.xml"))
#'   sb <- suppressWarnings(RstoxData::StoxBiotic(RstoxData::ReadBiotic(
#'     file.path(root, "surveys", "synthetic-seed1.xml"))))
#'   keys <- sb$Station$Station[1:2]
#'   jsonlite::write_json(
#'     list(project = list(models = list(baseline = list(list(
#'       processName = "FilterStoxBiotic", functionName = "RstoxData::FilterStoxBiotic",
#'       functionParameters = list(FilterExpression = list(
#'         Station = paste0("Station %notin% c('", keys[1], "', '", keys[2], "')")))
#'     ))))), file.path(root, "official", "process", "project.json"), auto_unbox = TRUE)
#'   stox_station_exclusions("official", "surveys/synthetic-seed1.xml",
#'                           "exclusions/synthetic.txt", root = root)
#' }
stox_station_exclusions <- function(project, biotic, out, root = data_root(), overwrite = FALSE) {
  target <- resolve_data_path(project, root)
  biotic_file <- resolve_data_path(biotic, root)
  out_file <- resolve_data_path(out, root, must_exist = FALSE)
  if (file.exists(out_file) && !isTRUE(overwrite)) {
    nb_abort("SX-EXC-05", "The exclusion file exists; use overwrite = TRUE to replace it.")
  }
  with_sanitised_errors(
    {
      check_stox_ready(list(stox = list(version = stox_pinned_version)))
      file <- locate_stox_project_file(target)
      if (!grepl("\\.json$", file, ignore.case = TRUE)) {
        nb_abort("SX-EXC-06", "Only StoX 3 or later projects (project.json) are read.")
      }
      j <- jsonlite::read_json(file, simplifyVector = FALSE)$project
      keys <- NULL
      for (model in j$models) {
        for (p in model) {
          if (!grepl("Filter", if (is.null(p$functionName)) "" else p$functionName)) next
          for (e in p$functionParameters$FilterExpression) {
            if (is.character(e)) keys <- c(keys, parse_station_exclusions(e))
          }
        }
      }
      if (is.null(keys)) nb_abort("SX-EXC-07", "The project has no station exclusion list in its filters.")
      keys <- unique(keys)
      sb <- RstoxData::StoxBiotic(RstoxData::ReadBiotic(biotic_file))
      map <- merge(as.data.frame(sb$Station)[c("CruiseKey", "StationKey", "Station")],
                   as.data.frame(sb$Haul)[c("CruiseKey", "StationKey", "HaulKey")],
                   by = c("CruiseKey", "StationKey"))
      serials <- unique(as.character(map$HaulKey[map$Station %in% keys]))
      n_unmatched <- sum(!keys %in% map$Station)
      if (n_unmatched > 0L) {
        nb_warn("SX-EXC-03", paste0(n_unmatched, " listed stations match no station in the biotic file."))
      }
      dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
      writeLines(c("# serial numbers of stations to exclude; station-level (class C1): keep in the data zone",
                   serials), out_file)
      structure(list(n_listed = length(keys), n_matched = length(keys) - n_unmatched,
                     n_unmatched = n_unmatched, n_serials = length(serials)),
                class = "nb_exclusions")
    },
    log_dir = file.path(root, "logs"),
    code = "SX-EXC-01",
    message = "The exclusion file could not be built."
  )
}

#' @export
print.nb_exclusions <- function(x, ...) {
  cat("<nb_exclusions> counts only; the file is in the data zone\n")
  cat("  stations listed in the project's filter:", x$n_listed, "\n")
  cat("  matched to stations in the biotic file: ", x$n_matched, "\n")
  cat("  not matched:                            ", x$n_unmatched, "\n")
  cat("  serial numbers written:                 ", x$n_serials, "\n")
  invisible(x)
}

# ---- Running a copy of an official project, and comparing ----------------------------

#' Run a copy of an official StoX project under the installed StoX
#'
#' Copies an official StoX project folder (the original is never touched) to a
#' fresh folder in the data zone, opens and runs it under the installed
#' RstoxFramework (4.2.1), and returns its reports. RstoxFramework 4.2.1 converts
#' projects saved by older StoX versions when it opens them, so this shows what
#' the installed version gives for the same project and the same input, which is
#' the reference for judging our own template (same engine, same data). If the
#' copy still holds the outputs of the original run, they are read before the
#' rerun and returned as `reference`. The reports are saved in the data zone and
#' the function prints names and row counts only.
#'
#' @param project Path, relative to `root`, of the official project folder.
#' @param replicates Optional number of bootstrap replicates for the copy, to
#'   make a check quicker than the official run; `NULL` keeps the project's own.
#' @param cores Optional number of cores for the bootstrap of the copy.
#' @inheritParams read_survey
#' @return Invisibly, an `nb_official_run` list with `reports` (the report
#'   outputs of the rerun), `reference` (the outputs saved with the project, or
#'   `NULL`) and `folder` (the copy, relative to `root`).
#' @export
#' @examples
#' if (requireNamespace("RstoxFramework", quietly = TRUE)) {
#'   root <- tempfile("nansen-root-")
#'   dir.create(file.path(root, "surveys"), recursive = TRUE)
#'   dir.create(file.path(root, "strata"))
#'   sv <- synth_survey(seed = 1)
#'   write_biotic(sv, file.path(root, "surveys", "synthetic-seed1.xml"))
#'   strata <- sv$strata
#'   names(strata)[names(strata) == "stratum"] <- "StratumName"
#'   sf::st_write(strata, file.path(root, "strata", "synthetic-strata.geojson"), quiet = TRUE)
#'   cfg <- read_config(system.file("configs", "synthetic-example.yml", package = "nansenbiomass"))
#'   cfg$bootstrap$replicates <- 3L
#'   res <- suppressWarnings(run_estimate(cfg, root = root))
#'   # a project generated by run_estimate() stands in for an official project here
#'   rel <- sub(paste0("^", normalizePath(root, winslash = "/"), "/"), "",
#'              normalizePath(res$project_path, winslash = "/"))
#'   run <- suppressWarnings(stox_official_copy_run(rel, root = root, replicates = 2L))
#'   run
#' }
stox_official_copy_run <- function(project, root = data_root(), replicates = NULL, cores = NULL) {
  src <- resolve_data_path(project, root)
  if (!dir.exists(src)) nb_abort("SX-OFF-02", "`project` must be a StoX project folder.")
  if (!is.null(replicates) && (!whole_number(replicates) || replicates < 1)) {
    nb_abort("SX-OFF-03", "`replicates` must be a positive whole number.")
  }
  stamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  parent <- file.path(root, "stox_official_check")
  dest <- file.path(parent, paste0("copy-", stamp))
  run_log <- file.path(root, "logs", paste0("official_copy_run-", stamp, ".log"))
  with_sanitised_errors(
    {
      check_stox_ready(list(stox = list(version = stox_pinned_version)))
      locate_stox_project_file(src)
      dir.create(parent, recursive = TRUE, showWarnings = FALSE)
      if (!file.copy(src, parent, recursive = TRUE)) nb_abort("SX-OFF-04", "The project could not be copied.")
      copied <- file.path(parent, basename(src))
      if (!file.rename(copied, dest)) nb_abort("SX-OFF-04", "The project copy could not be renamed.")
      log_stox_messages(RstoxFramework::openProject(dest, showWarnings = FALSE), run_log)
      report_names <- RstoxFramework::getProcessTable(dest, "report")$processName
      reference <- tryCatch(
        log_stox_messages(RstoxFramework::getModelData(dest, "report"), run_log),
        error = function(e) NULL
      )
      if (length(reference) == 0L) reference <- NULL
      table <- RstoxFramework::getProcessTable(dest, "analysis")
      if (!is.null(replicates) && "Bootstrap" %in% table$processName) {
        RstoxFramework::modifyProcess(dest, "analysis", "Bootstrap", newValues = list(
          functionParameters = list(NumberOfBootstraps = as.integer(replicates))))
      }
      if (!is.null(cores) && "Bootstrap" %in% table$processName) {
        RstoxFramework::modifyProcess(dest, "analysis", "Bootstrap", newValues = list(
          functionParameters = list(NumberOfCores = as.integer(cores))))
      }
      r <- log_stox_messages(
        RstoxFramework::runProject(dest, modelNames = c("baseline", "analysis", "report"),
                                   msg = FALSE, try = FALSE),
        run_log
      )
      reports <- r[intersect(report_names, names(r))]
      saveRDS(list(reports = reports, reference = reference),
              file.path(parent, paste0("copy-", stamp, "-reports.rds")))
      structure(list(reports = reports, reference = reference,
                     folder = file.path("stox_official_check", paste0("copy-", stamp))),
                class = "nb_official_run")
    },
    log_dir = file.path(root, "logs"),
    code = "SX-OFF-01",
    message = "The copy of the official project could not be run."
  )
}

#' @export
print.nb_official_run <- function(x, ...) {
  cat("<nb_official_run> names and row counts only; the reports are in the data zone\n")
  cat("Reports of the rerun:\n")
  for (n in names(x$reports)) {
    cat(sprintf("  %-40s %d rows\n", safe_field_names(n),
                if (is.data.frame(x$reports[[n]])) nrow(x$reports[[n]]) else NA_integer_))
  }
  cat("Reference outputs saved with the project:",
      if (is.null(x$reference)) "none readable" else paste(length(x$reference), "reports"), "\n")
  invisible(x)
}

#' Convert StoX report tables to a comparable table
#'
#' Turns the report outputs of a StoX project (for example from
#' [stox_official_copy_run()]) into a table with one row per quantity, stratum
#' (or `total`) and species category, with the baseline value or the bootstrap
#' mean, SD, CV and percentile limits where the report has them. Only reports
#' grouped by stratum, species category and survey are used; reports by length
#' group are skipped. Values are multiplied by `scale`; StoX reports
#' super-individual biomass in grams and abundance in numbers, so the default
#' converts to tonnes and millions. Check the units of your project: a ratio far
#' from 1 in [compare_estimates()] is the sign of a wrong scale.
#'
#' @param reports A list of report tables (`data.frame`s).
#' @param scale Named multipliers for `biomass` and `abundance`.
#' @return A tibble with `process`, `kind` (`baseline` or `bootstrap`),
#'   `quantity`, `stratum`, `species_category` and the value columns `baseline`,
#'   `mean`, `sd`, `cv`, `lower` and `upper` that are present.
#' @export
#' @examples
#' reports <- list(R = data.frame(Stratum = c("A", "B"), Biomass_sum = c(2e6, 3e6)))
#' official_reports_to_table(reports)
official_reports_to_table <- function(reports, scale = c(biomass = 1e-6, abundance = 1e-6)) {
  stats <- c(baseline = "_sum$", mean = "_mean$", sd = "_sd$", cv = "_cv$",
             lower = "_2\\.5%$", upper = "_97\\.5%$")
  rows <- lapply(names(reports), function(name) {
    df <- reports[[name]]
    if (!is.data.frame(df) || nrow(df) == 0L) return(NULL)
    df <- as.data.frame(df)
    value_cols <- grep("^(Abundance|Biomass)_", names(df), value = TRUE)
    groups <- setdiff(names(df), value_cols)
    if (length(value_cols) == 0L || !all(groups %in% c("Stratum", "SpeciesCategory", "Survey"))) {
      return(NULL)
    }
    quantity <- tolower(sub("_.*$", "", value_cols[1]))
    if ("Survey" %in% names(df)) df <- df[!is.na(df$Survey), , drop = FALSE]
    if (nrow(df) == 0L) return(NULL)
    out <- data.frame(
      process = name,
      kind = if (any(grepl(stats[["mean"]], value_cols))) "bootstrap" else "baseline",
      quantity = quantity,
      stratum = if ("Stratum" %in% names(df)) as.character(df$Stratum) else "total",
      species_category = if ("SpeciesCategory" %in% names(df)) as.character(df$SpeciesCategory) else NA_character_,
      stringsAsFactors = FALSE
    )
    for (st in names(stats)) {
      hit <- grep(stats[[st]], value_cols, value = TRUE)
      if (length(hit) == 1L) {
        out[[st]] <- as.numeric(df[[hit]]) * (if (st == "cv") 1 else scale[[quantity]])
      }
    }
    # a stratum where the species was not caught comes with an empty row: no information
    has_value <- rowSums(!is.na(out[intersect(names(stats), names(out))])) > 0L
    out[has_value, , drop = FALSE]
  })
  tibble::as_tibble(dplyr::bind_rows(rows))
}

#' Compare our estimates with another set
#'
#' Compares a Section 9 table (from [run_estimate()]) with a table of reference
#' values (from [official_reports_to_table()]) by quantity, stratum and, where the
#' reference has species categories, species. It reports ratios, not values: the
#' ratio of our estimate to the reference, its difference from 1, whether it is
#' within `tolerance`, and the ratio of the CVs where both exist. What is
#' released is the person's decision (D-03 applies to anything released).
#'
#' @param x Our estimates (a Section 9 table).
#' @param reference A table from [official_reports_to_table()].
#' @param reference_value `baseline` or `mean` (the bootstrap mean): which
#'   reference value to compare our `value` with.
#' @param tolerance Relative tolerance for the flag `within`.
#' @return An `nb_comparison` tibble with `quantity`, `stratum`, `species_code`,
#'   `ratio`, `rel_diff`, `within` and `cv_ratio`.
#' @export
#' @examples
#' ours <- tibble::tibble(species_code = "SP1", stratum = c("A", "total"), quantity = "biomass",
#'                        value = c(2.01, 5.0), cv = c(0.2, 0.1))
#' reference <- official_reports_to_table(list(R = data.frame(
#'   Stratum = "A", SpeciesCategory = "n/SP1/NA/s", Biomass_sum = 2e6)))
#' compare_estimates(ours, reference)
compare_estimates <- function(x, reference, reference_value = c("baseline", "mean"), tolerance = 0.01) {
  reference_value <- match.arg(reference_value)
  kind <- if (reference_value == "mean") "bootstrap" else "baseline"
  if (!reference_value %in% names(reference)) {
    nb_abort("SX-CMP-01", "The reference has no values of the requested kind.")
  }
  y <- reference[reference$kind == kind & !is.na(reference[[reference_value]]), , drop = FALSE]
  if (nrow(y) == 0L) nb_abort("SX-CMP-01", "The reference has no values of the requested kind.")
  y$species_code <- if (all(is.na(y$species_category))) NA_character_ else
    species_from_category(y$species_category, unique(x$species_code))
  y$reference <- y[[reference_value]]
  y$reference_cv <- if ("cv" %in% names(y)) y$cv else NA_real_
  by <- c("quantity", "stratum", if (!all(is.na(y$species_code))) "species_code")
  m <- merge(as.data.frame(x)[c("species_code", "stratum", "quantity", "value", "cv")],
             as.data.frame(y)[c(by, "reference", "reference_cv")], by = by)
  if (nrow(m) == 0L) nb_abort("SX-CMP-02", "No quantity and stratum is in both tables.")
  m$ratio <- m$value / m$reference
  structure(
    tibble::tibble(quantity = m$quantity, stratum = m$stratum, species_code = m$species_code,
                   ratio = m$ratio, rel_diff = m$ratio - 1, within = abs(m$ratio - 1) <= tolerance,
                   cv_ratio = m$cv / m$reference_cv),
    tolerance = tolerance, class = c("nb_comparison", "tbl_df", "tbl", "data.frame")
  )
}

#' @export
print.nb_comparison <- function(x, ...) {
  cat(sprintf("<nb_comparison> ratios of our estimate to the reference; tolerance %.3g\n",
              attr(x, "tolerance")))
  cat(sprintf("%d compared, %d within tolerance; ratio min %.4g, median %.4g, max %.4g\n",
              nrow(x), sum(x$within), min(x$ratio), stats::median(x$ratio), max(x$ratio)))
  y <- x
  class(y) <- setdiff(class(y), "nb_comparison")
  print(tibble::as_tibble(y), n = Inf)
  invisible(x)
}

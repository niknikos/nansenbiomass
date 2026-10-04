# stox_sweptarea: generate StoX projects from template and configuration; run them headless via RstoxFramework
# Data class touched: C0; Claude may run it on synthetic data only (docs/spec.md, Section 4).

# ---- Structure of an existing StoX project -----------------------------------
#
# Official estimates were produced with StoX projects that live in the data zone.
# A StoX 2.7 project.xml holds, besides its settings, process data such as
# station-to-PSU assignments (station identifiers) and stratum polygons. The
# functions below report the structure of a project, its models, processes,
# functions and settings, so that the pipeline can replicate the official rules
# (D-09, D-10) without any of that material leaving the laptop.

# Field names referenced in a filter expression: identifiers left after removing
# quoted literals and numbers. Literal values are never returned.
expression_fields <- function(x) {
  x <- gsub("'[^']*'|\"[^\"]*\"", " ", x)
  tokens <- regmatches(x, gregexpr("[A-Za-z_][A-Za-z0-9_.]*", x))[[1]]
  noise <- c("in", "not", "and", "or", "TRUE", "FALSE", "true", "false", "NA", "NULL",
             "null", "c", "is.na", "nchar", "as.numeric", "as.character", "as.integer")
  tokens <- setdiff(unique(tokens), noise)
  tokens <- safe_field_names(tokens)
  unique(tokens[tokens != "<name withheld>"])
}

# Classifies one leaf value of a StoX setting. Returns the value to show (or a
# withheld marker), its status and, for expressions, the fields it references.
classify_stox_value <- function(name, value) {
  shown <- function(v) list(value = v, status = "shown", fields = NA_character_)
  if (is.null(value) || length(value) == 0L) return(shown(""))
  if (is.logical(value)) return(shown(as.character(value)))
  if (is.numeric(value)) return(shown(format(value, scientific = FALSE, trim = TRUE)))
  value <- as.character(value)
  if (is.na(value) || !nzchar(trimws(value))) return(shown(""))
  v <- trimws(value)
  if (grepl("^-?[0-9]+(\\.[0-9]+)?$", v)) return(shown(v))
  is_expression <- grepl("expr|filter|condition", name, ignore.case = TRUE) ||
    grepl("==|!=|>=|<=|%in%|%notin%|[<>&|]| in \\[| not in ", v)
  if (is_expression) {
    fields <- expression_fields(v)
    return(list(value = "<withheld: expression>", status = "withheld",
                fields = if (length(fields)) paste(fields, collapse = ", ") else NA_character_))
  }
  if (grepl("[/\\\\]", v) ||
      grepl("\\.(xml|json|txt|csv|tsv|geojson|shp|wkt|rds|zip)$", v, ignore.case = TRUE)) {
    return(list(value = "<withheld: path>", status = "withheld", fields = NA_character_))
  }
  if (grepl("^[A-Za-z][A-Za-z0-9_.:-]{0,60}$", v) && !grepl("[0-9]{3,}", v)) {
    return(shown(v))
  }
  list(value = "<withheld: text>", status = "withheld", fields = NA_character_)
}

# Flattens a (possibly nested) JSON parameter into leaves named by their path.
flatten_parameter <- function(x, name, max_leaves = 50L) {
  leaves <- list()
  walk <- function(v, path) {
    if (is.list(v) && length(v) > 0L) {
      nm <- names(v)
      for (i in seq_along(v)) {
        key <- if (!is.null(nm) && nzchar(nm[i])) paste0(path, "$", nm[i]) else
          sprintf("%s[%d]", path, i)
        walk(v[[i]], key)
      }
    } else {
      leaves[[length(leaves) + 1L]] <<- list(path = path, value = if (is.list(v)) NULL else v)
    }
  }
  walk(x, name)
  if (length(leaves) > max_leaves) {
    return(list(list(path = name, value = NULL, long = length(leaves))))
  }
  leaves
}

stox_rows <- function(model, step, process, fun, parameter, cls) {
  tibble::tibble(
    model = model, step = as.integer(step),
    process = safe_field_names(process), "function" = fun, parameter = parameter,
    value = cls$value, status = cls$status, fields = cls$fields
  )
}

describe_stox_json <- function(file) {
  j <- jsonlite::read_json(file, simplifyVector = FALSE)$project
  if (is.null(j) || is.null(j$models)) {
    nb_abort("SX-READ-03", "The file is not a StoX project.json.")
  }
  versions <- unlist(j$RstoxPackageVersion)
  versions <- if (length(versions)) versions else NA_character_
  rows <- list()
  data_rows <- list()
  for (model in names(j$models)) {
    procs <- j$models[[model]]
    for (i in seq_along(procs)) {
      p <- procs[[i]]
      pname <- if (is.null(p$processName)) NA_character_ else p$processName
      fun <- classify_stox_value("function", p$functionName)$value
      params <- c(p$functionParameters, p$processParameters)
      if (length(params) == 0L) {
        rows[[length(rows) + 1L]] <- stox_rows(model, i, pname, fun, NA_character_,
                                               list(value = "", status = "shown",
                                                    fields = NA_character_))
      }
      for (par in names(params)) {
        for (leaf in flatten_parameter(params[[par]], par)) {
          cls <- if (!is.null(leaf$long)) {
            list(value = sprintf("<withheld: long list, %d items>", leaf$long),
                 status = "withheld", fields = NA_character_)
          } else {
            classify_stox_value(leaf$path, leaf$value)
          }
          rows[[length(rows) + 1L]] <- stox_rows(model, i, pname, fun, leaf$path, cls)
        }
      }
      for (inp in names(p$functionInputs)) {
        rows[[length(rows) + 1L]] <- stox_rows(
          model, i, pname, fun, paste0("input:", inp),
          classify_stox_value(inp, unlist(p$functionInputs[[inp]])[1])
        )
      }
      # Rows of the process-data tables (a table is a list of records).
      n_data <- sum(vapply(p$processData, function(t) if (is.list(t)) length(t) else 1L,
                           integer(1)))
      if (n_data > 0L) {
        data_rows[[length(data_rows) + 1L]] <- tibble::tibble(
          section = model, element = safe_field_names(pname), n_entries = n_data
        )
      }
    }
  }
  new_stox_description("StoX >= 3 (project.json)", versions, rows, data_rows)
}

describe_stox_xml <- function(file) {
  doc <- xml2::read_xml(file)
  root <- xml2::xml_root(doc)
  if (xml2::xml_name(root) != "project") {
    nb_abort("SX-READ-03", "The file is not a StoX 2.7 project.xml.")
  }
  attrs <- xml2::xml_attrs(root)
  versions <- if (length(attrs)) {
    vapply(names(attrs), function(a) {
      paste0(a, "=", classify_stox_value(a, attrs[[a]])$value)
    }, character(1), USE.NAMES = FALSE)
  } else {
    NA_character_
  }
  rows <- list()
  data_rows <- list()
  for (section in xml2::xml_children(root)) {
    sname <- xml2::xml_name(section)
    if (sname == "processdata") {
      for (el in xml2::xml_children(section)) {
        data_rows[[length(data_rows) + 1L]] <- tibble::tibble(
          section = "processdata", element = safe_field_names(xml2::xml_name(el)),
          n_entries = xml2::xml_length(el)
        )
      }
      next
    }
    if (sname != "model") next
    model <- xml2::xml_attr(section, "name")
    procs <- xml2::xml_children(section)
    procs <- procs[xml2::xml_name(procs) == "process"]
    for (i in seq_along(procs)) {
      p <- procs[[i]]
      pname <- xml2::xml_attr(p, "name")
      kids <- xml2::xml_children(p)
      knames <- xml2::xml_name(kids)
      fun <- classify_stox_value("function", xml2::xml_text(kids[knames == "function"])[1])$value
      for (k in which(knames != "function")) {
        par <- if (knames[k] == "parameter") xml2::xml_attr(kids[[k]], "name") else knames[k]
        cls <- if (xml2::xml_length(kids[[k]]) > 0L) {
          list(value = "<withheld: data>", status = "withheld", fields = NA_character_)
        } else {
          classify_stox_value(par, xml2::xml_text(kids[[k]]))
        }
        rows[[length(rows) + 1L]] <- stox_rows(model, i, pname, fun, par, cls)
      }
    }
  }
  new_stox_description("StoX 2.7 (project.xml)", versions, rows, data_rows)
}

new_stox_description <- function(format, versions, rows, data_rows) {
  processes <- dplyr::bind_rows(rows)
  if (nrow(processes) == 0L) {
    processes <- tibble::tibble(model = character(), step = integer(), process = character(),
                                "function" = character(), parameter = character(),
                                value = character(), status = character(),
                                fields = character())
  }
  process_data <- dplyr::bind_rows(data_rows)
  if (nrow(process_data) == 0L) {
    process_data <- tibble::tibble(section = character(), element = character(),
                                   n_entries = integer())
  }
  structure(
    list(format = format, versions = versions, processes = processes,
         process_data = process_data),
    class = "nb_stox_description"
  )
}

# A project folder holds its file in process/ (StoX's own layout) or, when only
# the file was copied, directly in the folder.
locate_stox_project_file <- function(target) {
  if (dir.exists(target)) {
    candidates <- c(file.path(target, "process", c("project.json", "project.xml")),
                    file.path(target, c("project.json", "project.xml")))
    hit <- candidates[file.exists(candidates)]
    if (length(hit) == 0L) {
      nb_abort("SX-READ-02",
               "No project.json or project.xml in the folder or in its process/ folder.")
    }
    return(hit[1])
  }
  target
}

#' Describe the structure of a StoX project
#'
#' Reports how an existing StoX project is set up, without any of its data: the
#' models and processes in order, each process's function, its parameter names
#' and the values that are plain settings (methods, options, numbers such as
#' bootstrap iterations and seeds). For filter expressions it reports only the
#' field names they reference. It withholds file paths, literal values in
#' expressions, free text, and the whole process-data section (station-to-PSU
#' assignments, stratum polygons), of which it reports only entry counts. It
#' reads StoX 2.7 projects (`process/project.xml`) and StoX 3 or later projects
#' (`process/project.json`).
#'
#' Use it on the laptop to learn the rules behind official estimates (D-09,
#' D-10); review the output before sharing it. Errors are handled as in
#' [read_survey()].
#'
#' @param path Path, relative to `root`, of a StoX project folder (with its
#'   project file in `process/` or directly in the folder) or of the
#'   `project.xml` or `project.json` file itself.
#' @inheritParams read_survey
#' @return An `nb_stox_description`: a list with `format`, `versions` (StoX or
#'   Rstox versions recorded in the project), `processes` (a tibble of model,
#'   step, process, function, parameter, value, status and fields) and
#'   `process_data` (a tibble of section, element and number of entries).
#' @export
#' @examples
#' root <- tempfile("nansen-root-")
#' dir.create(file.path(root, "synthetic", "process"), recursive = TRUE)
#' jsonlite::write_json(
#'   list(project = list(
#'     RstoxPackageVersion = list("RstoxFramework_4.2.1"),
#'     models = list(baseline = list(list(
#'       processName = "FilterStoxBiotic",
#'       functionName = "RstoxData::FilterStoxBiotic",
#'       functionParameters = list(FilterExpression = list(Haul = "HaulQuality == 12"))
#'     )))
#'   )),
#'   file.path(root, "synthetic", "process", "project.json"), auto_unbox = TRUE
#' )
#' describe_stox_project("synthetic", root = root)
describe_stox_project <- function(path, root = data_root()) {
  target <- resolve_data_path(path, root)
  with_sanitised_errors(
    {
      file <- locate_stox_project_file(target)
      if (grepl("\\.json$", file, ignore.case = TRUE)) {
        describe_stox_json(file)
      } else if (grepl("\\.xml$", file, ignore.case = TRUE)) {
        describe_stox_xml(file)
      } else {
        nb_abort("SX-READ-03", "The file is neither a project.xml nor a project.json.")
      }
    },
    log_dir = file.path(root, "logs"),
    code = "SX-READ-01",
    message = "The StoX project could not be read."
  )
}

#' @export
print.nb_stox_description <- function(x, ...) {
  cat("<nb_stox_description> structure only; paths, expressions and process data withheld\n")
  cat("Format:", x$format, "\n")
  cat("Versions:", paste(x$versions, collapse = "; "), "\n\n")
  cat("Processes and settings:\n")
  print(x$processes, n = Inf, width = Inf)
  cat("\nProcess data (entry counts only):\n")
  print(x$process_data, n = Inf)
  invisible(x)
}

# ---- Pinned StoX release ------------------------------------------------------

# The RstoxFramework version the pipeline is built and tested against (StoX 4.2;
# with RstoxBase and RstoxData 2.2.1). Change together with cloud/setup.sh,
# DESCRIPTION and docs/m2-plan.md.
stox_pinned_version <- "4.2.1"

# ---- Inclusion rules ----------------------------------------------------------

# Stratum of each station from its start position; NA outside every polygon.
station_strata <- function(station, polygons, label) {
  if (!label %in% names(polygons)) {
    nb_abort("SX-STRATA-02", "The stratum polygons have no attribute named by data.stratum_label.")
  }
  ok <- !is.na(station$longitudestart) & !is.na(station$latitudestart)
  out <- rep(NA_character_, nrow(station))
  if (any(ok)) {
    pts <- sf::st_as_sf(data.frame(lon = station$longitudestart[ok],
                                   lat = station$latitudestart[ok]),
                        coords = c("lon", "lat"), crs = 4326)
    polygons <- sf::st_transform(polygons, 4326)
    hit <- sf::st_intersects(pts, polygons)
    first <- vapply(hit, function(h) if (length(h)) h[[1]] else NA_integer_, integer(1))
    out[ok] <- as.character(polygons[[label]])[first]
  }
  out
}

# Stratum polygons from a polygon file (any format sf reads) or from the process
# data of a StoX 2.7 project.xml (through RstoxBase, as StoX itself does). For a
# project.xml, each stratum's `includeintotal` flag is kept.
read_strata_polygons <- function(file, label) {
  if (grepl("\\.xml$", file, ignore.case = TRUE)) {
    if (!requireNamespace("RstoxBase", quietly = TRUE)) {
      nb_abort("SX-STOX-01", "RstoxBase is needed to read strata from a StoX project.xml.")
    }
    dt <- RstoxBase::readStratumPolygonFrom2.7(file, remove_includeintotal = FALSE,
                                               StratumNameLabel = label)
    df <- as.data.frame(dt)
    df$includeintotal <- as.logical(df$includeintotal)
    return(sf::st_as_sf(df, wkt = "geometry", crs = 4326))
  }
  sf::st_read(file, quiet = TRUE)
}

# Great-circle distance in nautical miles between start and end positions.
position_distance_nmi <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180
  a <- sin((lat2 - lat1) * rad / 2)^2 +
    cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  2 * 3440.065 * asin(pmin(1, sqrt(a)))
}

# Towed distance that can be recovered where the recorded one is zero or missing:
# from the log (stop minus start), else from the start and end positions.
recoverable_distance <- function(st) {
  from_log <- st$logstop - st$logstart
  from_log[!is.finite(from_log) | from_log <= 0] <- NA_real_
  from_pos <- position_distance_nmi(st$latitudestart, st$longitudestart,
                                    st$latitudeend, st$longitudeend)
  from_pos[!is.finite(from_pos) | from_pos <= 0] <- NA_real_
  list(log = from_log, positions = from_pos)
}

#' Station counts kept and excluded by a survey's inclusion rules
#'
#' Applies a configuration's inclusion rules (D-09) to its biotic files, in
#' order: allowed `stationtype`, `samplequality` and `gearcondition` codes, a
#' positive towed distance, and a start position inside the strata. A station
#' with a missing code is excluded by a rule that lists allowed codes. The
#' result holds counts only, to compare with the station numbers in a survey
#' report before any estimate is made.
#'
#' Stations with a zero or missing distance are always flagged, with the number
#' whose distance could be recovered from the log (stop minus start) or, failing
#' that, from the start and end positions. Whether recovered distances are used
#' is set by `inclusion.distance_recovery` (`none`, `log` or `log_or_positions`).
#' Strata can come from a polygon file or from a StoX 2.7 `project.xml` (read
#' with RstoxBase); for the latter, each stratum's `includeintotal` flag is
#' reported.
#'
#' @param config A configuration file path, or an `nb_config` from
#'   [read_config()].
#' @inheritParams read_survey
#' @return An `nb_inclusion` list: `rules` (a tibble of step, rule, stations
#'   before, excluded and after), `distance` (counts of zero or missing
#'   distances, recoverable and recovered) and `by_stratum` (stations kept per
#'   stratum, including strata with none, and `include_in_total` where known).
#' @export
#' @examples
#' root <- tempfile("nansen-root-")
#' dir.create(file.path(root, "surveys"), recursive = TRUE)
#' dir.create(file.path(root, "strata"))
#' sv <- synth_survey(synth_design(excluded = c(pelagic = 3, aborted = 2)), seed = 1)
#' write_biotic(sv, file.path(root, "surveys", "synthetic-seed1.xml"))
#' strata <- sv$strata
#' names(strata)[names(strata) == "stratum"] <- "StratumName"
#' sf::st_write(strata, file.path(root, "strata", "synthetic-strata.geojson"), quiet = TRUE)
#' inclusion_summary(system.file("configs", "synthetic-example.yml",
#'                               package = "nansenbiomass"), root = root)
inclusion_summary <- function(config, root = data_root()) {
  cfg <- if (inherits(config, "nb_config")) config else read_config(config)
  files <- vapply(cfg$data$biotic, resolve_data_path, character(1), root = root,
                  USE.NAMES = FALSE)
  strata_file <- resolve_data_path(cfg$data$strata, root)
  with_sanitised_errors(
    {
      survey <- read_biotic(files)
      polygons <- read_strata_polygons(strata_file, cfg$data$stratum_label)
      inclusion_counts(survey$station, cfg, polygons)
    },
    log_dir = file.path(root, "logs"),
    code = "SX-INC-01",
    message = "The inclusion rules could not be applied."
  )
}

# Applies the inclusion rules (D-09) to the station table. The single source of
# truth for which stations are kept: inclusion_summary() reports counts from it
# and the StoX project is built from the same result. The returned `keep`,
# `stratum` and `distance_used` are station-level (C1) and never leave the
# data zone.
apply_inclusion <- function(st, cfg, polygons) {
  keep <- rep(TRUE, nrow(st))
  rows <- list()
  step <- function(rule, bad) {
    before <- sum(keep)
    excluded <- sum(keep & bad)
    keep <<- keep & !bad
    rows[[length(rows) + 1L]] <<- tibble::tibble(
      step = length(rows) + 1L, rule = rule, n_before = before,
      n_excluded = excluded, n_after = sum(keep)
    )
  }
  for (f in c("stationtype", "samplequality", "gearcondition")) {
    allowed <- cfg$inclusion[[f]]
    if (!is.null(allowed)) {
      step(paste0(f, " in {", paste(allowed, collapse = ", "), "}"), !st[[f]] %in% allowed)
    }
  }
  # Zero or missing distances are always flagged, with what could be recovered;
  # the configuration decides whether recovered distances are used.
  no_distance <- is.na(st$distance) | st$distance <= 0
  rec <- recoverable_distance(st)
  method <- cfg$inclusion$distance_recovery
  recovered <- no_distance & (
    (method %in% c("log", "log_or_positions") & !is.na(rec$log)) |
      (method == "log_or_positions" & !is.na(rec$positions))
  )
  flagged <- keep & no_distance
  distance <- tibble::tibble(
    n_zero_or_missing = sum(flagged),
    n_recoverable_from_log = sum(flagged & !is.na(rec$log)),
    n_recoverable_from_positions_only = sum(flagged & is.na(rec$log) & !is.na(rec$positions)),
    recovery = method,
    n_recovered = sum(flagged & recovered)
  )
  if (isTRUE(cfg$inclusion$positive_distance)) {
    step(paste0("distance > 0 (recovery: ", method, ")"), no_distance & !recovered)
  }
  stratum <- station_strata(st, polygons, cfg$data$stratum_label)
  step("start position inside the strata", is.na(stratum))
  # The distance StoX will use: the recorded one, or the recovered one.
  distance_used <- st$distance
  from_log <- recovered & !is.na(rec$log)
  distance_used[from_log] <- rec$log[from_log]
  from_pos <- recovered & !from_log
  distance_used[from_pos] <- rec$positions[from_pos]
  list(keep = keep, stratum = stratum, distance_used = distance_used,
       rules = dplyr::bind_rows(rows), distance = distance)
}

inclusion_counts <- function(st, cfg, polygons) {
  inc <- apply_inclusion(st, cfg, polygons)
  kept <- table(factor(inc$stratum[inc$keep], levels = cfg$data$stratum_names))
  by_stratum <- tibble::tibble(stratum = names(kept), n_kept = as.integer(kept))
  if ("includeintotal" %in% names(polygons)) {
    flag <- polygons$includeintotal[match(by_stratum$stratum,
                                          polygons[[cfg$data$stratum_label]])]
    by_stratum$include_in_total <- flag
  }
  structure(
    list(rules = inc$rules, distance = inc$distance, by_stratum = by_stratum),
    class = "nb_inclusion"
  )
}

#' @export
print.nb_inclusion <- function(x, ...) {
  cat("<nb_inclusion> station counts only\n")
  print(x$rules, n = Inf, width = Inf)
  d <- x$distance
  if (d$n_zero_or_missing > 0) {
    cat(sprintf(paste0("\nFlag: %d station(s) with zero or missing distance; recoverable from ",
                       "the log: %d, from positions only: %d; recovered (%s): %d.\n"),
                d$n_zero_or_missing, d$n_recoverable_from_log,
                d$n_recoverable_from_positions_only, d$recovery, d$n_recovered))
  }
  cat("\nStations kept by stratum:\n")
  print(x$by_stratum, n = Inf)
  invisible(x)
}

# ---- The entry point -----------------------------------------------------------

# Writes StoX's console messages (which can carry file locations) to the run's
# local log instead of the console.
log_stox_messages <- function(expr, log_file) {
  withCallingHandlers(expr, message = function(m) {
    nb_log(log_file, "message", m)
    invokeRestart("muffleMessage")
  })
}

# Pelagic or otherwise non-unique serial numbers would make the key-list filter
# ambiguous: refuse rather than guess.
check_station_keys <- function(survey) {
  if (anyDuplicated(survey$station$serialnumber) > 0L) {
    nb_abort("SX-KEY-01", paste("Serial numbers are not unique across the biotic files, so the stations",
                                "to keep cannot be identified for StoX."))
  }
  if (anyDuplicated(survey$catch[c("serialnumber", "catchsampleid")]) > 0L) {
    nb_abort("SX-KEY-02", "Catch samples with duplicated keys: the survey is refused, not repaired (M1 record).")
  }
  invisible(TRUE)
}

#' Run a StoX swept-area estimate for one survey
#'
#' The single entry point of the `stox_sweptarea` module. It reads the
#' configuration, applies the inclusion rules (the same as
#' [inclusion_summary()]), builds a StoX project from the versioned template
#' under `<root>/stox/<survey>/<run>/`, runs the baseline and the bootstrap
#' headless through RstoxFramework, converts the reports to the Section 9 schema
#' (biomass in tonnes and abundance in millions, by stratum and in total), builds
#' the support table and stages both with [stage_export()]. Nothing is written to
#' `outbox/`: a person reviews the staged files and releases them.
#'
#' In this version the swept width is a fixed value (`swept_width.method:
#' fixed`): StoX 4.2.1 does not take a haul-specific door spread through
#' its swept-area density, so `trawldoorspread` is refused with a message.
#' Biomass comes from catch weights and abundance from length distributions, as
#' two branches of the StoX chain (template `sweptarea`). Every step runs under
#' sanitised errors, with details in `<root>/logs/`.
#'
#' @param config A configuration file path (the hash of the file is recorded),
#'   or an `nb_config` from [read_config()].
#' @inheritParams read_survey
#' @param staging_dir The staging folder; defaults to `staging` under `root`.
#' @return Invisibly, a list with `estimates` (Section 9 table), `support`,
#'   `staged` (the result of [stage_export()]), `project_path` and `run_label`.
#' @export
#' @examples
#' if (requireNamespace("RstoxFramework", quietly = TRUE)) {
#'   root <- tempfile("nansen-root-")
#'   dir.create(file.path(root, "surveys"), recursive = TRUE)
#'   dir.create(file.path(root, "strata"))
#'   sv <- synth_survey(synth_design(excluded = c(pelagic = 3, aborted = 2)), seed = 1)
#'   write_biotic(sv, file.path(root, "surveys", "synthetic-seed1.xml"))
#'   strata <- sv$strata
#'   names(strata)[names(strata) == "stratum"] <- "StratumName"
#'   sf::st_write(strata, file.path(root, "strata", "synthetic-strata.geojson"), quiet = TRUE)
#'   cfg <- read_config(system.file("configs", "synthetic-example.yml",
#'                                  package = "nansenbiomass"))
#'   cfg$bootstrap$replicates <- 5L # few replicates, to keep the example fast
#'   res <- suppressWarnings(run_estimate(cfg, root = root))
#'   res$staged$outcome
#' }
run_estimate <- function(config, root = data_root(), staging_dir = file.path(root, "staging")) {
  cfg <- if (inherits(config, "nb_config")) config else read_config(config)
  hash <- attr(cfg, "config_hash")
  if (is.null(hash)) nb_abort("CF-READ-03", "A configuration object must come from read_config().")
  if (!identical(cfg$swept_width$method, "fixed")) {
    nb_abort("SX-CFG-01", paste("StoX 4.2.1 takes a constant sweep width only; set",
                                "swept_width.method to fixed (haul-specific door spread is not supported)."))
  }
  if (!grepl("^[A-Za-z0-9._-]+$", cfg$survey$label)) {
    nb_abort("SX-CFG-02", "survey.label may contain only letters, digits, '.', '_' and '-'.")
  }
  files <- vapply(cfg$data$biotic, resolve_data_path, character(1), root = root, USE.NAMES = FALSE)
  strata_file <- resolve_data_path(cfg$data$strata, root)
  run_label <- paste0(format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), "-", substr(hash, 1, 8))
  project_path <- file.path(root, "stox", cfg$survey$label, run_label)
  log_dir <- file.path(root, "logs")
  run_log <- file.path(log_dir, paste0("run_estimate-", run_label, ".log"))

  with_sanitised_errors(
    {
      check_stox_ready(cfg)
      survey <- read_biotic(files)
      check_station_keys(survey)
      polygons <- read_strata_polygons(strata_file, cfg$data$stratum_label)
      inc <- apply_inclusion(survey$station, cfg, polygons)
      if (!any(inc$keep)) nb_abort("SX-INC-02", "No station is kept by the inclusion rules.")
      st <- survey$station
      keys <- as.character(st$serialnumber[inc$keep])

      # Recovered distances reach StoX through a translation inside the project.
      old <- st$distance
      recovered <- inc$keep & (is.na(old) | old <= 0) & !is.na(inc$distance_used) & inc$distance_used > 0
      translation <- if (any(recovered)) {
        data.table::data.table(EffectiveTowDistance = as.character(old[recovered]),
                               NewValue = as.character(inc$distance_used[recovered]),
                               HaulKey = as.character(st$serialnumber[recovered]))
      }
      flag <- if ("includeintotal" %in% names(polygons)) {
        polygons$includeintotal[match(cfg$data$stratum_names, polygons[[cfg$data$stratum_label]])]
      } else {
        rep(TRUE, length(cfg$data$stratum_names))
      }
      total_strata <- cfg$data$stratum_names[is.na(flag) | flag]

      built <- build_stox_project(
        cfg, list(biotic_files = files, strata_file = strata_file, keep_keys = keys,
                  translation = translation, total_strata = total_strata),
        project_path
      )
      r <- log_stox_messages(
        RstoxFramework::runProject(project_path, modelNames = c("baseline", "analysis", "report"),
                                   msg = FALSE, try = FALSE),
        run_log
      )

      # The stations StoX kept must be the stations the rules kept.
      hauls <- r$FilterStoxBiotic$Haul
      if (nrow(hauls) != sum(inc$keep)) {
        nb_abort("SX-INC-03", paste0("StoX kept ", nrow(hauls), " stations but the inclusion rules keep ",
                                     sum(inc$keep), "."))
      }
      if (isTRUE(cfg$inclusion$positive_distance) &&
          any(is.na(hauls$EffectiveTowDistance) | hauls$EffectiveTowDistance <= 0)) {
        nb_abort("SX-DIST-01", "Some stations kept still have a zero or missing distance in the StoX project.")
      }

      cv <- code_version_string(built$template_version)
      estimates <- stox_reports_to_estimates(r, cfg, hash, cv)
      support <- stox_support_table(survey, inc$keep, inc$stratum, cfg)
      staged <- stage_export(estimates, support, cfg$data$stratum_names,
                             run_label = paste0(cfg$survey$label, "-", run_label),
                             min_stations = cfg$disclosure$min_stations,
                             min_positive = cfg$disclosure$min_positive,
                             staging_dir = staging_dir)
      invisible(list(estimates = estimates, support = support, staged = staged,
                     project_path = project_path, run_label = run_label))
    },
    log_dir = log_dir,
    code = "SX-RUN-01",
    message = "The estimate could not be completed."
  )
}

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

read_strata_polygons <- function(file) {
  if (grepl("\\.xml$", file, ignore.case = TRUE)) {
    nb_abort("SX-STRATA-01",
             "Stratum polygons inside a StoX 2.7 project.xml are not read here; use a polygon file.")
  }
  sf::st_read(file, quiet = TRUE)
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
#' @param config A configuration file path, or an `nb_config` from
#'   [read_config()].
#' @inheritParams read_survey
#' @return An `nb_inclusion` list: `rules` (a tibble of step, rule, stations
#'   before, excluded and after) and `by_stratum` (stations kept per stratum,
#'   including strata with none).
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
      polygons <- read_strata_polygons(strata_file)
      inclusion_counts(survey$station, cfg, polygons)
    },
    log_dir = file.path(root, "logs"),
    code = "SX-INC-01",
    message = "The inclusion rules could not be applied."
  )
}

inclusion_counts <- function(st, cfg, polygons) {
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
  if (isTRUE(cfg$inclusion$positive_distance)) {
    step("distance > 0", is.na(st$distance) | st$distance <= 0)
  }
  stratum <- station_strata(st, polygons, cfg$data$stratum_label)
  step("start position inside the strata", is.na(stratum))
  kept <- table(factor(stratum[keep], levels = cfg$data$stratum_names))
  structure(
    list(rules = dplyr::bind_rows(rows),
         by_stratum = tibble::tibble(stratum = names(kept), n_kept = as.integer(kept))),
    class = "nb_inclusion"
  )
}

#' @export
print.nb_inclusion <- function(x, ...) {
  cat("<nb_inclusion> station counts only\n")
  print(x$rules, n = Inf, width = Inf)
  cat("\nStations kept by stratum:\n")
  print(x$by_stratum, n = Inf)
  invisible(x)
}

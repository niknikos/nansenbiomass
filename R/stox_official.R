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
#'     )))), file.path(root, "official", "process", "project.json"), auto_unbox = TRUE)
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

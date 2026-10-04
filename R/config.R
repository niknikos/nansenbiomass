# config: survey configuration files (configs/<survey>.yml)
# Data class touched: C3 only (configuration; paths point into the data zone but
# are not opened here).
#
# One YAML file per survey and run. Paths are relative to NANSEN_DATA_ROOT, so no
# path is hard-coded (CLAUDE.md). The schema is documented in read_config().

config_quantities <- c("biomass", "abundance")
config_swept_width_methods <- c("fixed", "trawldoorspread")

cf_abort <- function(code, field, problem) {
  nb_abort(code, paste0("Configuration field `", field, "` ", problem, "."))
}

# TRUE for a single, non-empty string.
is_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)

# Checks a path given relative to the data root, without opening it.
check_config_path <- function(x, field) {
  if (!is_string(x)) cf_abort("CF-PATH-01", field, "must be a single path")
  if (is_absolute_path(x)) cf_abort("CF-PATH-02", field, "must be relative to the data root")
  if (any(strsplit(x, "[/\\\\]")[[1]] == "..")) {
    cf_abort("CF-PATH-02", field, "must not contain '..'")
  }
  invisible(x)
}

# Codes are kept as text, as in the NMDBiotic tables; YAML may read 12 as a number.
as_codes <- function(x, field) {
  if (is.null(x)) return(NULL)
  if (!is.atomic(x) || length(x) == 0L || anyNA(x)) {
    cf_abort("CF-TYPE-02", field, "must be a non-empty list of codes")
  }
  as.character(x)
}

whole_number <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x) && x == round(x)

#' Validate a survey configuration
#'
#' Checks a configuration list (as returned by [read_config()]) against the
#' schema, fills in defaults and normalises codes to text. Paths are checked to
#' be relative to the data root but are not opened, so a configuration can be
#' validated anywhere. Errors name the field at fault, never its value.
#'
#' @param cfg A configuration list.
#' @return The validated configuration, with defaults filled in, as an
#'   `nb_config` list.
#' @export
#' @examples
#' cfg <- read_config(system.file("configs", "synthetic-example.yml",
#'                                package = "nansenbiomass"))
#' validate_config(cfg)$inclusion
validate_config <- function(cfg) {
  if (!is.list(cfg)) nb_abort("CF-TYPE-01", "The configuration must be a list of fields.")
  required <- c("survey", "data", "inclusion", "swept_width", "species", "bootstrap")
  for (f in required) if (is.null(cfg[[f]])) cf_abort("CF-REQ-01", f, "is required")

  # survey
  if (!is_string(cfg$survey$label)) cf_abort("CF-REQ-01", "survey.label", "is required")
  if (!whole_number(cfg$survey$year)) cf_abort("CF-TYPE-01", "survey.year", "must be a year")
  cfg$survey$year <- as.integer(cfg$survey$year)

  # data
  biotic <- cfg$data$biotic
  if (is.null(biotic) || length(biotic) == 0L) cf_abort("CF-REQ-01", "data.biotic", "is required")
  for (b in biotic) check_config_path(b, "data.biotic")
  cfg$data$biotic <- as.character(unlist(biotic))
  check_config_path(cfg$data$strata, "data.strata")
  cfg$data$stratum_label <- if (is.null(cfg$data$stratum_label)) "StratumName" else
    cfg$data$stratum_label
  if (!is_string(cfg$data$stratum_label)) {
    cf_abort("CF-TYPE-01", "data.stratum_label", "must name the polygon attribute holding stratum names")
  }
  strata <- cfg$data$stratum_names
  if (is.null(strata) || !is.atomic(strata) || length(strata) == 0L || anyNA(strata) ||
      any(as.character(strata) == "total") || anyDuplicated(strata)) {
    cf_abort("CF-TYPE-02", "data.stratum_names", "must list unique stratum names (not `total`)")
  }
  cfg$data$stratum_names <- as.character(strata)

  # inclusion rules: allowed codes per field; an absent field means no restriction
  inc <- cfg$inclusion
  for (f in c("stationtype", "samplequality", "gearcondition")) {
    inc[[f]] <- as_codes(inc[[f]], paste0("inclusion.", f))
  }
  inc$positive_distance <- if (is.null(inc$positive_distance)) TRUE else inc$positive_distance
  if (!is.logical(inc$positive_distance) || length(inc$positive_distance) != 1L ||
      is.na(inc$positive_distance)) {
    cf_abort("CF-TYPE-01", "inclusion.positive_distance", "must be true or false")
  }
  cfg$inclusion <- inc

  # swept width
  sw <- cfg$swept_width
  if (!is_string(sw$method) || !sw$method %in% config_swept_width_methods) {
    cf_abort("CF-VAL-01", "swept_width.method", "must be fixed or trawldoorspread")
  }
  if (!is.numeric(sw$fixed_m) || length(sw$fixed_m) != 1L || is.na(sw$fixed_m) ||
      sw$fixed_m <= 0) {
    cf_abort("CF-VAL-02", "swept_width.fixed_m",
             "must be a positive width in metres (the width, or the fallback for missing door spreads)")
  }
  cfg$swept_width <- sw

  # species and quantities
  cfg$species <- as_codes(cfg$species, "species")
  q <- if (is.null(cfg$quantities)) "biomass" else as.character(unlist(cfg$quantities))
  if (length(q) == 0L || !all(q %in% config_quantities)) {
    cf_abort("CF-VAL-01", "quantities", "must be biomass and/or abundance")
  }
  cfg$quantities <- unique(q)

  # bootstrap (D-10)
  bs <- cfg$bootstrap
  if (!whole_number(bs$replicates) || bs$replicates < 1) {
    cf_abort("CF-VAL-02", "bootstrap.replicates", "must be a positive whole number")
  }
  if (!whole_number(bs$seed)) cf_abort("CF-VAL-02", "bootstrap.seed", "must be a whole number")
  cfg$bootstrap <- list(replicates = as.integer(bs$replicates), seed = as.integer(bs$seed))

  # StoX version pin
  cfg$stox$version <- if (is.null(cfg$stox$version)) stox_pinned_version else cfg$stox$version
  if (!is_string(cfg$stox$version) || !grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", cfg$stox$version)) {
    cf_abort("CF-VAL-01", "stox.version", "must be a version such as 4.2.1")
  }

  # disclosure (D-03): the minimums can be raised, never lowered
  dis <- cfg$disclosure
  dis$min_stations <- if (is.null(dis$min_stations)) min_stations_floor else dis$min_stations
  dis$min_positive <- if (is.null(dis$min_positive)) min_positive_floor else dis$min_positive
  if (!whole_number(dis$min_stations) || dis$min_stations < min_stations_floor) {
    cf_abort("CF-VAL-03", "disclosure.min_stations", "must be at least 5 (D-03)")
  }
  if (!whole_number(dis$min_positive) || dis$min_positive < min_positive_floor) {
    cf_abort("CF-VAL-03", "disclosure.min_positive", "must be at least 3 (D-03)")
  }
  cfg$disclosure <- list(min_stations = as.integer(dis$min_stations),
                         min_positive = as.integer(dis$min_positive))

  structure(cfg[c(required, "quantities", "stox", "disclosure")], class = "nb_config")
}

#' Read a survey configuration
#'
#' Reads one survey configuration (`configs/<survey>.yml`) and validates it with
#' [validate_config()]. The schema:
#'
#' * `survey`: `label` (text) and `year`.
#' * `data`: `biotic` (one or more biotic files), `strata` (the stratum polygon
#'   file, for example GeoJSON), both relative to `NANSEN_DATA_ROOT`;
#'   `stratum_label`, the polygon attribute holding stratum names (default
#'   `StratumName`, as in StoX); and `stratum_names`.
#' * `inclusion`: allowed `stationtype`, `samplequality` and `gearcondition`
#'   codes (a field left out means no restriction) and `positive_distance`
#'   (default `true`). See `docs/nansis-codes.md` for the codes (D-09).
#' * `swept_width`: `method` (`fixed` or `trawldoorspread`) and `fixed_m` (the
#'   width in metres, or the fallback where a door spread is missing).
#' * `species`: species codes; `quantities`: `biomass` and/or `abundance`.
#' * `bootstrap`: `replicates` and `seed` (D-10).
#' * `stox`: `version`, the RstoxFramework version the run must use.
#' * `disclosure`: `min_stations` and `min_positive`, at least 5 and 3 (D-03).
#'
#' @param path Path of the configuration file.
#' @return A validated `nb_config` list, with the file's hash in the attribute
#'   `config_hash`.
#' @export
#' @examples
#' cfg <- read_config(system.file("configs", "synthetic-example.yml",
#'                                package = "nansenbiomass"))
#' cfg$survey
read_config <- function(path) {
  if (!is_string(path) || !file.exists(path)) {
    nb_abort("CF-READ-01", "The configuration file does not exist.")
  }
  cfg <- tryCatch(
    yaml::read_yaml(path),
    error = function(e) nb_abort("CF-READ-02", "The configuration file is not valid YAML.")
  )
  cfg <- validate_config(cfg)
  attr(cfg, "config_hash") <- config_hash(path)
  cfg
}

#' Hash of a configuration file
#'
#' The MD5 hash of the configuration file's bytes, recorded in the `config_hash`
#' field of every estimate (docs/spec.md, Section 9).
#'
#' @param path Path of the configuration file.
#' @return The hash, as a 32-character string.
#' @export
#' @examples
#' config_hash(system.file("configs", "synthetic-example.yml", package = "nansenbiomass"))
config_hash <- function(path) {
  if (!is_string(path) || !file.exists(path)) {
    nb_abort("CF-READ-01", "The configuration file does not exist.")
  }
  unname(tools::md5sum(path))
}

#' @export
print.nb_config <- function(x, ...) {
  cat("<nb_config>", x$survey$label, x$survey$year, "\n")
  cat("  biotic files:", length(x$data$biotic), " strata:", length(x$data$stratum_names), "\n")
  for (f in c("stationtype", "samplequality", "gearcondition")) {
    v <- x$inclusion[[f]]
    cat(sprintf("  inclusion %-14s %s\n", paste0(f, ":"),
                if (is.null(v)) "any" else paste(v, collapse = ", ")))
  }
  cat("  positive distance:", x$inclusion$positive_distance, "\n")
  cat("  swept width:", x$swept_width$method, "(", x$swept_width$fixed_m, "m )\n")
  cat("  bootstrap:", x$bootstrap$replicates, "replicates, seed", x$bootstrap$seed, "\n")
  invisible(x)
}

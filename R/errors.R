# errors: sanitised conditions, local logging and data-path resolution
#
# Rule 8 of CLAUDE.md: errors must not echo data. Messages raised here may name
# condition codes, tables, fields and counts, never record values or file paths
# (file names can carry cruise identifiers). Details from third-party code go to
# a local log in the data zone.

# Raises a classed, sanitised error. `message` must be a fixed template: callers
# never interpolate record values into it.
nb_abort <- function(code, message) {
  rlang::abort(
    paste0(code, ": ", message),
    class = "nansenbiomass_error",
    nb_code = code,
    call = NULL
  )
}

# Raises a classed, sanitised warning, under the same rule as nb_abort().
nb_warn <- function(code, message) {
  rlang::warn(
    paste0(code, ": ", message),
    class = "nansenbiomass_warning",
    nb_code = code
  )
}

# Appends one condition, with its full message and call, to a local log file.
nb_log <- function(log_file, kind, cnd) {
  dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
  call_text <- if (is.null(conditionCall(cnd))) {
    ""
  } else {
    paste(deparse(conditionCall(cnd)), collapse = " ")
  }
  entry <- c(
    paste0("[", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "] ", kind),
    paste0("  class: ", paste(class(cnd), collapse = ", ")),
    paste0("  call: ", call_text),
    paste0("  message: ", conditionMessage(cnd)),
    ""
  )
  cat(entry, file = log_file, sep = "\n", append = TRUE)
  invisible(log_file)
}

# Evaluates `expr`, writing every error and warning from third-party code to a
# log file in `log_dir`, and signals only sanitised conditions to the console.
# Errors already raised by nb_abort() pass through unchanged (and are logged).
with_sanitised_errors <- function(expr, log_dir, code, message) {
  log_file <- file.path(
    log_dir,
    paste0("nansenbiomass-", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), ".log")
  )
  n_warnings <- 0L
  result <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        n_warnings <<- n_warnings + 1L
        nb_log(log_file, "warning", w)
        invokeRestart("muffleWarning")
      }
    ),
    # One handler: tryCatch() nests its handlers, so an error re-raised from a
    # separate nansenbiomass_error handler would be caught again by `error`.
    error = function(e) {
      nb_log(log_file, "error", e)
      if (inherits(e, "nansenbiomass_error")) stop(e)
      nb_abort(code, paste0(message, " Details in local log ", basename(log_file), "."))
    }
  )
  if (n_warnings > 0L) {
    nb_warn(
      "LOG-WARN-01",
      paste0(n_warnings, " warning(s) written to local log ", basename(log_file), ".")
    )
  }
  result
}

# Field names are structural, but a wide table pivoted by station would carry
# identifiers in its column names. Names that are not plain identifiers, or that
# contain three or more consecutive digits, are therefore withheld from reports.
safe_field_names <- function(x) {
  ok <- grepl("^[A-Za-z_][A-Za-z0-9_.]*$", x) & !grepl("[0-9]{3,}", x) & nchar(x) <= 40L
  x[!ok] <- "<name withheld>"
  x
}

# Reference codes (station type, sample quality, length measurement and the
# like) are structural and may be reported with their counts. Anything that does
# not look like a short code is withheld, in case a field holds free text.
safe_codes <- function(x) {
  ok <- is.na(x) | grepl("^[A-Za-z0-9_.-]{1,8}$", x)
  x[!ok] <- "<code withheld>"
  x
}

# TRUE in a Claude Code cloud session, which sets CLAUDE_CODE_REMOTE.
in_cloud_session <- function() {
  tolower(Sys.getenv("CLAUDE_CODE_REMOTE")) %in% c("true", "1", "yes")
}

is_absolute_path <- function(path) {
  grepl("^(/|~|[A-Za-z]:[/\\\\]|\\\\\\\\)", path)
}

# Normalises a path that may not exist yet, through its nearest existing parent.
normalize_existing <- function(path) {
  p <- path
  rest <- character(0)
  while (!file.exists(p) && dirname(p) != p) {
    rest <- c(basename(p), rest)
    p <- dirname(p)
  }
  out <- normalizePath(p, winslash = "/", mustWork = FALSE)
  if (length(rest)) out <- paste(c(sub("/$", "", out), rest), collapse = "/")
  out
}

# TRUE if `path` (which need not exist yet) is inside `parent`.
path_is_within <- function(path, parent) {
  path <- normalize_existing(path)
  parent <- normalizePath(parent, winslash = "/", mustWork = TRUE)
  identical(path, parent) || startsWith(path, paste0(sub("/$", "", parent), "/"))
}

# Real data never exist in a cloud session. There, a data root is accepted only
# inside the session's temporary folder, where synthetic tests write their files.
check_cloud_root <- function(root) {
  if (in_cloud_session() && !path_is_within(root, tempdir())) {
    nb_abort(
      "IO-CLOUD-01",
      paste(
        "This is a cloud session (CLAUDE_CODE_REMOTE is set); real data are never",
        "read or staged here. Use a data root inside tempdir() for synthetic data."
      )
    )
  }
  invisible(root)
}

#' Locate the data zone
#'
#' Returns the root of the data zone, taken from the environment variable
#' `NANSEN_DATA_ROOT`. Raw data and full results live under this folder, on the
#' developer's laptop and outside the repository (docs/spec.md, Section 4).
#'
#' @return The normalised absolute path of the data root, as a string. Fails
#'   with a sanitised error if the variable is unset or relative, or if the
#'   folder does not exist.
#' @export
#' @examples
#' old <- Sys.getenv("NANSEN_DATA_ROOT")
#' Sys.setenv(NANSEN_DATA_ROOT = tempdir())
#' data_root()
#' Sys.setenv(NANSEN_DATA_ROOT = old)
data_root <- function() {
  root <- Sys.getenv("NANSEN_DATA_ROOT")
  if (!nzchar(root)) {
    nb_abort("IO-ROOT-01", "NANSEN_DATA_ROOT is not set.")
  }
  if (!is_absolute_path(root)) {
    nb_abort("IO-ROOT-02", "NANSEN_DATA_ROOT must be an absolute path.")
  }
  if (!dir.exists(root)) {
    nb_abort("IO-ROOT-03", "The folder named by NANSEN_DATA_ROOT does not exist.")
  }
  normalizePath(root, winslash = "/")
}

# Resolves a data path given relative to `root`. Absolute paths and `..`
# components are refused, so that a path cannot leave the root.
resolve_data_path <- function(path, root, must_exist = TRUE) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    nb_abort("IO-PATH-01", "The data path must be a single, non-empty string.")
  }
  if (is_absolute_path(path)) {
    nb_abort("IO-PATH-02", "Data paths must be relative to the data root.")
  }
  parts <- strsplit(path, "[/\\\\]")[[1]]
  if (any(parts == "..")) {
    nb_abort("IO-PATH-02", "Data paths must not contain '..'.")
  }
  if (!dir.exists(root)) {
    nb_abort("IO-ROOT-03", "The data root does not exist.")
  }
  check_cloud_root(root)
  full <- file.path(normalizePath(root, winslash = "/"), path)
  if (must_exist && !file.exists(full)) {
    nb_abort("IO-PATH-03", "The data file does not exist under the data root.")
  }
  full
}

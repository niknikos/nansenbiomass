# stox_project: the versioned StoX template and the project builder
# Data class touched: C0 (the generated project lives in the data zone and holds
# station keys and file locations); the template itself is C3.
#
# The chain is described once, in inst/stox/sweptarea/template.json: ordered
# processes, their StoX functions, fixed parameters and named placeholders. The
# builder fills the placeholders from a survey configuration and creates the
# project with RstoxFramework's own functions, so StoX validates every step
# against the installed release. A project is never edited by hand.

stox_packages <- c("RstoxFramework", "RstoxBase", "RstoxData")

stox_template_path <- function() {
  path <- system.file("stox", "sweptarea", "template.json", package = "nansenbiomass")
  if (!nzchar(path)) nb_abort("SX-TPL-01", "The StoX project template is missing from the package.")
  path
}

# JSON arrays of plain values become vectors; everything else stays a list.
simplify_json <- function(x) {
  if (!is.list(x)) return(x)
  x <- lapply(x, simplify_json)
  if (is.null(names(x)) && length(x) > 0L &&
      all(vapply(x, function(e) is.atomic(e) && length(e) == 1L, logical(1)))) {
    return(unlist(x))
  }
  x
}

read_stox_template <- function(path = stox_template_path()) {
  tpl <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE),
                  error = function(e) nb_abort("SX-TPL-02", "The StoX project template is not valid JSON."))
  tpl$processes <- lapply(tpl$processes, simplify_json)
  tpl
}

placeholder_pattern <- "\\{\\{[a-z_]+\\}\\}"

# Replaces `{{name}}` placeholders by the values in `values`. A string that is
# exactly one placeholder takes the value as it is (a table, a number, an empty
# vector); a placeholder left unfilled is an error.
fill_template <- function(x, values) {
  if (is.list(x) && !inherits(x, "data.frame")) return(lapply(x, fill_template, values = values))
  if (is.character(x) && any(grepl(placeholder_pattern, x))) {
    if (length(x) == 1L && grepl(paste0("^", placeholder_pattern, "$"), x)) {
      name <- gsub("[{}]", "", x)
      if (!name %in% names(values)) {
        nb_abort("SX-TPL-03", paste0("The template placeholder `", name, "` has no value."))
      }
      return(values[[name]])
    }
    nb_abort("SX-TPL-04", "A template placeholder is embedded in text; it must stand alone.")
  }
  x
}

# Template processes that apply to this run.
template_active <- function(proc, flags) {
  is.null(proc$when) || isTRUE(flags[[proc$when]])
}

#' The StoX versions behind a run
#'
#' @return A named character vector of the installed RstoxFramework, RstoxBase
#'   and RstoxData versions (`NA` for a package that is not installed).
#' @noRd
stox_versions <- function() {
  vapply(stox_packages, function(p) {
    v <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
    v
  }, character(1))
}

# The StoX packages must be installed at the pinned release, and the
# configuration must ask for that release. Errors name packages and versions
# only, never data.
check_stox_ready <- function(cfg) {
  v <- stox_versions()
  if (anyNA(v)) {
    nb_abort("SX-STOX-01", paste0("The StoX package(s) ", paste(names(v)[is.na(v)], collapse = ", "),
                                  " are not installed."))
  }
  if (!identical(unname(v[["RstoxFramework"]]), cfg$stox$version)) {
    nb_abort("SX-STOX-02", paste0("The configuration asks for RstoxFramework ", cfg$stox$version,
                                  " but ", v[["RstoxFramework"]], " is installed."))
  }
  invisible(v)
}

# Package version, Git commit (where the package sits in a repository), the StoX
# versions and the template version, for the code_version field.
code_version_string <- function(template_version) {
  pkg <- tryCatch(as.character(utils::packageVersion("nansenbiomass")), error = function(e) "unknown")
  dir <- tryCatch(find.package("nansenbiomass"), error = function(e) NA_character_)
  commit <- if (is.na(dir)) "nogit" else tryCatch({
    out <- suppressWarnings(system2("git", c("-C", shQuote(dir), "rev-parse", "--short", "HEAD"),
                                    stdout = TRUE, stderr = FALSE))
    if (length(out) == 1L && grepl("^[0-9a-f]{7,40}$", out)) out else "nogit"
  }, error = function(e) "nogit")
  v <- stox_versions()
  paste0("nansenbiomass ", pkg, "+", commit, "; ",
         paste0(names(v), " ", ifelse(is.na(v), "NA", v), collapse = "; "),
         "; template sweptarea ", template_version)
}

# Builds the StoX project for one run and returns what the runner needs.
#
# `inputs`: biotic_files, strata_file, keep_keys (HaulKey values of the stations
# to keep), translation (NULL or a table of Value, NewValue, HaulKey) and
# total_strata (strata that count towards the total).
build_stox_project <- function(cfg, inputs, project_path) {
  tpl <- read_stox_template()
  q <- cfg$quantities
  flags <- list(biomass = "biomass" %in% q, abundance = "abundance" %in% q,
                distance_translation = !is.null(inputs$translation))
  methods <- c(abundance = "ResampleMeanLengthDistributionData",
               biomass = "ResampleMeanSpeciesCategoryCatchData")
  processes <- c(abundance = "MeanLengthDistribution", biomass = "MeanSpeciesCategoryCatch")
  use <- names(methods)[unlist(flags[names(methods)])]
  keys <- paste0("'", gsub("'", "", inputs$keep_keys), "'", collapse = ",")
  all_in <- setequal(inputs$total_strata, cfg$data$stratum_names)
  values <- list(
    biotic_files = inputs$biotic_files,
    strata_file = inputs$strata_file,
    stratum_label = cfg$data$stratum_label,
    stoxbiotic_process = if (flags$distance_translation) "TranslateStoxBiotic" else "StoxBiotic",
    filter_expression = list(Haul = paste0("HaulKey %in% c(", keys, ")")),
    translation_table = inputs$translation,
    raising_factor_priority = "Weight",
    sweep_width_m = cfg$swept_width$fixed_m,
    replicates = cfg$bootstrap$replicates,
    output_processes = c(biomass = "Biomass", abundance = "Abundance")[use],
    bootstrap_method_table = data.table::data.table(
      ProcessName = unname(processes[use]), ResampleFunction = unname(methods[use]),
      ResampleBy = "Stratum", Seed = cfg$bootstrap$seed
    ),
    # Strata outside the survey get no Survey label and drop out of the total.
    survey_method = if (all_in) "AllStrata" else "Table",
    survey_table = if (all_in) data.table::data.table() else
      data.table::data.table(Stratum = inputs$total_strata, Survey = "Survey")
  )
  values$output_processes <- unname(values$output_processes)

  RstoxFramework::createProject(project_path, ow = TRUE, open = TRUE)
  for (proc in tpl$processes) {
    if (!template_active(proc, flags)) next
    spec <- list(processName = proc$name, functionName = proc$`function`)
    if (!is.null(proc$parameters)) spec$functionParameters <- fill_template(proc$parameters, values)
    if (!is.null(proc$inputs)) spec$functionInputs <- fill_template(proc$inputs, values)
    RstoxFramework::addProcess(project_path, proc$model, values = spec,
                               returnProcessTable = FALSE, add.defaults = TRUE)
  }
  list(project_path = project_path, template_version = tpl$template_version, flags = flags)
}

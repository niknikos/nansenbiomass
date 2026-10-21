# stox_draft: a survey configuration drafted from an official StoX project
# Data class touched: C3 (the project's settings), plus the exporters it calls, which write
# station-level or structural files into the data zone and print counts only.

# The processes of all models of a project.json, as a flat list.
draft_processes <- function(file) {
  j <- jsonlite::read_json(file, simplifyVector = FALSE)$project
  if (is.null(j) || is.null(j$models)) nb_abort("SX-DRAFT-02", "The file is not a StoX project.json.")
  procs <- unlist(lapply(j$models, function(m) m), recursive = FALSE)
  list(procs = procs, versions = unlist(j$RstoxPackageVersion))
}

draft_fun <- function(p) sub("^.*::", "", if (is.null(p$functionName)) "" else p$functionName)

# Values of a table parameter stored as records (a list of lists) or as columns.
draft_column <- function(x, column) {
  if (is.null(x)) return(character(0))
  if (!is.null(x[[column]])) return(as.character(unlist(x[[column]])))
  v <- unlist(lapply(x, function(r) if (is.list(r)) r[[column]]))
  if (is.null(v)) character(0) else as.character(v)
}

# Quoted literals of a clause value (`c("a", "b")`, `"a"`, `'a'`).
draft_literals <- function(x) {
  lit <- regmatches(x, gregexpr("'[^']*'|\"[^\"]*\"", x))[[1]]
  substr(lit, 2L, nchar(lit) - 1L)
}

# The station rules of the template, read from the clauses of the project's haul and station
# filters. Returns the inclusion settings, whether a station exclusion list is present, and the
# clauses (by field name only) that the template cannot reproduce.
draft_inclusion <- function(expressions) {
  inc <- list()
  unresolved <- character(0)
  has_exclusions <- FALSE
  codes <- c(gear = "gear", gearcondition = "gearcondition", samplequality = "samplequality",
             stationtype = "stationtype")
  for (expr in expressions) {
    parts <- split_expression(expr)
    if (is.null(parts)) {
      unresolved <- c(unresolved, "a station filter could not be parsed")
      next
    }
    if (any(!parts$connectors %in% c("&", "&&") & !tolower(parts$connectors) %in% "and")) {
      unresolved <- c(unresolved, "a station filter combines clauses with 'or'; only 'and' is reproduced")
    }
    for (cl in parts$clauses) {
      cl <- trimws(gsub("^\\(+|\\)+$", "", trimws(cl)))
      field <- regmatches(cl, regexpr("^!?\\s*\\(?\\s*[A-Za-z_][A-Za-z0-9_.]*", cl))
      field <- tolower(gsub("[!( ]", "", field))
      if (field %in% c("station", "haulkey")) {
        if (grepl("%notin%|^!|!=", cl)) { has_exclusions <- TRUE; next }
        unresolved <- c(unresolved, paste0("a filter keeps a list of stations (", field, "); the template cannot reproduce a keep-list"))
        next
      }
      if (field %in% names(codes)) {
        m <- regmatches(cl, regexec("^[A-Za-z_.]+\\s*(%in%|==)\\s*(.+)$", cl))[[1]]
        vals <- if (length(m) == 3L) draft_literals(m[3]) else character(0)
        if (length(m) == 3L && length(vals) == 0L && grepl("^-?[0-9.]+$", trimws(m[3]))) vals <- trimws(m[3])
        if (length(vals)) {
          inc[[codes[[field]]]] <- unique(c(inc[[codes[[field]]]], vals))
          next
        }
      }
      if (field == "soaktime") {
        m <- regmatches(cl, regexec("^[A-Za-z_.]+\\s*>=\\s*(-?[0-9.]+)$", cl))[[1]]
        if (length(m) == 2L) { inc$soaktime_min <- as.numeric(m[2]); next }
      }
      unresolved <- c(unresolved, paste0("a filter clause on '", safe_field_names(field),
                                         "' is not one the inclusion rules reproduce"))
    }
  }
  list(inclusion = inc, has_exclusions = has_exclusions, unresolved = unique(unresolved))
}

#' Draft a survey configuration from an official StoX project
#'
#' Reads an official StoX project (`process/project.json`, StoX 3 or later) on the laptop and
#' writes a draft survey configuration for [run_estimate()], with the project's own settings:
#' the station filters (gear, gear condition, sample quality, station type, tow duration and a
#' station exclusion list), the species or species groups, the strata, the biotic file, the
#' biomass route, the sweep width, the bootstrap and its interval. Along the way it exports what
#' the configuration points to into the data zone, with the package's own exporters: the strata
#' ([stox_strata()]), a stored species-group lookup ([stox_translation_table()]) and a station
#' exclusion list ([stox_station_exclusions()]).
#'
#' It prints what it decided (settings only) and, separately, what it could not decide; those
#' points need a person, and the draft is not ready until they are settled. The configuration is
#' written in the repository's `configs/` folder (class C3); the exported files stay in the data
#' zone. Check the draft, then run [reproduce_official()].
#'
#' @param project Path, relative to `root`, of the official StoX project folder.
#' @param name Short name of the survey, used for the configuration and the exported files.
#'   Defaults to the project folder's name.
#' @param out Path of the configuration file to write (in the repository, not the data zone).
#' @param replicates Bootstrap replicates for the configuration; by default the project's.
#' @param overwrite Replace existing output files.
#' @inheritParams read_survey
#' @return An `nb_config_draft`: `file` (the configuration written), `decided` (a named
#'   character vector of settings), `unresolved` (points for a person) and `valid` (whether
#'   [read_config()] accepts the draft).
#' @export
#' @examples
#' if (requireNamespace("RstoxFramework", quietly = TRUE)) {
#'   root <- tempfile("nansen-root-")
#'   dir.create(file.path(root, "official", "process"), recursive = TRUE)
#'   jsonlite::write_json(list(project = list(
#'     RstoxPackageVersion = list("RstoxFramework_4.2.1"),
#'     models = list(baseline = list(
#'       list(processName = "FilterStoxBiotic", functionName = "RstoxData::FilterStoxBiotic",
#'            functionParameters = list(FilterExpression = list(
#'              Haul = "gear %in% c('3270') & samplequality %in% '12'"))),
#'       list(processName = "SweptAreaDensity", functionName = "RstoxBase::SweptAreaDensity",
#'            functionParameters = list(SweptAreaDensityMethod = "TotalCatch",
#'                                      SweepWidthMethod = "Constant", SweepWidth = 20)),
#'       list(processName = "SpeciesCategoryCatch", functionName = "RstoxBase::SpeciesCategoryCatch")
#'     )))), file.path(root, "official", "process", "project.json"), auto_unbox = TRUE)
#'   draft <- stox_config_from_project("official", out = tempfile(fileext = ".yml"), root = root)
#'   draft$unresolved
#' }
stox_config_from_project <- function(project, name = NULL, out = NULL, replicates = NULL,
                                     root = data_root(), overwrite = FALSE) {
  src <- resolve_data_path(project, root)
  name <- if (is.null(name)) basename(src) else name
  if (!grepl("^[A-Za-z0-9._-]+$", name)) nb_abort("SX-DRAFT-01", "`name` may contain only letters, digits, '.', '_' and '-'.")
  out <- if (is.null(out)) file.path("configs", paste0(name, ".yml")) else out
  if (file.exists(out) && !isTRUE(overwrite)) nb_abort("SX-DRAFT-03", "The configuration file exists; use overwrite = TRUE to replace it.")
  with_sanitised_errors(
    {
      file <- locate_stox_project_file(src)
      if (!grepl("\\.json$", file)) nb_abort("SX-DRAFT-02", "Only StoX 3 or later projects (project.json) can be drafted from.")
      pj <- draft_processes(file)
      procs <- pj$procs
      funs <- vapply(procs, draft_fun, character(1))
      par <- function(p) if (is.null(p$functionParameters)) list() else p$functionParameters
      first <- function(fun) { i <- which(funs == fun); if (length(i)) procs[[i[1]]] else NULL }
      decided <- character(0)
      unresolved <- character(0)
      note <- function(...) unresolved <<- c(unresolved, paste0(...))
      decide <- function(key, value) decided[[key]] <<- value

      # Filters: station rules, and the species filter
      station_expr <- character(0)
      species_expr <- character(0)
      for (i in which(funs == "FilterStoxBiotic")) {
        fe <- par(procs[[i]])$FilterExpression
        for (lv in names(fe)) {
          if (lv == "SpeciesCategory") species_expr <- c(species_expr, as.character(fe[[lv]])) else
            if (lv %in% c("Haul", "Station", "Cruise")) station_expr <- c(station_expr, as.character(fe[[lv]])) else
              note("a filter at level '", safe_field_names(lv), "' is not reproduced")
        }
      }
      di <- draft_inclusion(station_expr)
      unresolved <- c(unresolved, di$unresolved)
      inc <- c(di$inclusion, list(positive_distance = TRUE, distance_recovery = "none"))
      for (f in intersect(c("gear", "gearcondition", "samplequality", "stationtype"), names(inc))) {
        decide(paste0("inclusion.", f), paste(inc[[f]], collapse = ", "))
      }
      if (!is.null(inc$soaktime_min)) decide("inclusion.soaktime_min", paste(inc$soaktime_min, "h"))

      # Biotic file: the project's own copy
      rb <- first("ReadBiotic")
      fn <- if (is.null(rb)) character(0) else as.character(unlist(par(rb)$FileNames))
      biotic <- character(0)
      for (f in fn) {
        cand <- if (is_absolute_path(f)) file.path(project, "input", "biotic", basename(f)) else file.path(project, f)
        if (file.exists(file.path(root, cand))) biotic <- c(biotic, gsub("\\\\", "/", cand)) else
          note("a biotic file of the project was not found in its folder")
      }
      if (length(biotic)) decide("data.biotic", paste(length(biotic), "file(s) in the project folder"))
      if (length(biotic) == 0L) note("no biotic file could be located; set data.biotic by hand")

      # Station exclusion list
      if (di$has_exclusions && length(biotic) == 1L) {
        ex_out <- file.path("exclusions", paste0(name, ".txt"))
        ex <- stox_station_exclusions(project, biotic, ex_out, root = root, overwrite = overwrite)
        inc$exclude_stations_file <- ex_out
        decide("inclusion.exclude_stations_file", sprintf("%d listed, %d matched", ex$n_listed, ex$n_matched))
        if (ex$n_unmatched > 0L) note(ex$n_unmatched, " excluded stations of the project match no station in the biotic file")
      } else if (di$has_exclusions) {
        note("the project excludes stations, but the exclusion list needs exactly one biotic file")
      }

      # Strata
      strata_out <- file.path("strata", paste0(name, ".geojson"))
      st <- tryCatch(stox_strata(project, root = root, out = strata_out, overwrite = overwrite),
                     error = function(e) NULL)
      if (is.null(st)) note("the strata could not be read from the project; export them with stox_strata()") else
        decide("data.strata", sprintf("%d strata from %s", length(st$names), st$source))

      # Species or species groups
      redefine <- any(vapply(procs[funs == "RedefineStoxBiotic"], function(p) {
        r <- par(p)$Redefinition
        "SpeciesCategory" %in% draft_column(r, "VariableName") && "catchcategory" %in% draft_column(r, "ReplaceBy")
      }, logical(1)))
      tr_i <- which(funs == "DefineTranslation" & vapply(procs, function(p) {
        identical(as.character(unlist(par(p)$VariableName)), "SpeciesCategory")
      }, logical(1)))
      species_values <- unique(as.character(unlist(lapply(species_expr, draft_literals))))
      groups <- NULL
      if (redefine && length(tr_i) == 1L) {
        tp <- procs[[tr_i]]
        if (length(tp$processData$Translation)) {
          lk <- file.path("lookups", paste0(name, "-groups.csv"))
          tt <- stox_translation_table(project, lk, process = tp$processName, root = root, overwrite = overwrite)
          groups <- list(file = lk, value_column = tt$columns[1], group_column = "NewValue")
          decide("species_groups", sprintf("lookup stored in the project, %d rows", tt$n_rows))
        } else {
          rf <- as.character(unlist(par(tp)$FileName))
          if (length(rf) == 1L && !is_absolute_path(rf) && file.exists(file.path(root, project, rf))) {
            groups <- list(file = gsub("\\\\", "/", file.path(project, rf)),
                           value_column = as.character(unlist(par(tp)$ValueColumn)),
                           group_column = as.character(unlist(par(tp)$NewValueColumn)))
            decide("species_groups", "lookup file in the project folder")
          } else {
            note("the species-group lookup could not be located; set species_groups by hand")
          }
        }
        species <- species_values
      } else {
        if (redefine || length(tr_i)) note("species categories are redefined or translated in a way the draft does not recognise")
        # StoX names a category "<name>/<catch category>/<aphia>/<scientific name>"
        species <- unique(vapply(strsplit(species_values, "/", fixed = TRUE),
                                 function(x) if (length(x) >= 2L) x[2] else x[1], character(1)))
        species <- as.character(species)
      }
      if (length(species) == 0L) note("no species filter found; set species by hand")
      decide("species", sprintf("%d %s", length(species), if (is.null(groups)) "species" else "groups"))
      other_tr <- procs[funs %in% c("TranslateStoxBiotic", "TranslateBiotic", "DefineTranslation")]
      other_vars <- setdiff(unique(unlist(lapply(other_tr, function(p) unlist(par(p)$VariableName)))), c("SpeciesCategory", ""))

      # Biomass route, quantities and lengths
      si <- all(c("Individuals", "SuperIndividuals", "ImputeSuperIndividuals") %in% funs)
      sad <- procs[funs == "SweptAreaDensity"]
      sad_method <- unique(unlist(lapply(sad, function(p) par(p)$SweptAreaDensityMethod)))
      biomass <- list()
      if (si) {
        sp <- par(first("SuperIndividuals")); ip <- par(first("ImputeSuperIndividuals"))
        biomass <- list(method = "super_individuals",
                        distribution_method = if (is.null(sp$DistributionMethod)) "Equal" else sp$DistributionMethod)
        imp <- list(method = if (is.null(ip$ImputationMethod)) "RandomSampling" else ip$ImputationMethod)
        if (!is.null(ip$ImputationLevels)) imp$levels <- as.character(unlist(ip$ImputationLevels))
        if (!is.null(ip$Seed)) imp$seed <- as.integer(ip$Seed)
        stox_names <- c(at_missing = "ImputeAtMissing", to_impute = "ToImpute", by_equal = "ImputeByEqual")
        for (key in names(stox_names)) {
          v <- ip[[stox_names[[key]]]]
          if (!is.null(v)) imp[[key]] <- as.character(unlist(v))
        }
        biomass$imputation <- imp
      } else if ("SpeciesCategoryCatch" %in% funs) {
        biomass <- list(method = "total_catch")
      } else {
        note("no biomass chain was recognised")
      }
      quantities <- c(if (length(biomass)) "biomass", if (si || "LengthDistributed" %in% sad_method) "abundance")
      decide("biomass.method", if (length(biomass)) biomass$method else "none")
      decide("quantities", paste(quantities, collapse = ", "))
      if (length(other_vars)) {
        uses_lengths <- "abundance" %in% quantities || si
        if (uses_lengths) note("the project translates ", paste(safe_field_names(other_vars), collapse = ", "),
                               "; the template does not reproduce that translation") else
          decide("translations ignored", paste0(paste(safe_field_names(other_vars), collapse = ", "),
                                                " (not used by a catch-weight estimate)"))
      }
      rg <- first("RegroupLengthDistribution")
      lengths <- if (!is.null(rg) && !is.null(par(rg)$LengthInterval)) list(interval_cm = as.numeric(par(rg)$LengthInterval))
      ld <- first("LengthDistribution")
      rfp <- if (!is.null(ld)) par(ld)$RaisingFactorPriority
      catch <- list(raising_factor_priority = if (is.null(rfp)) "Weight" else as.character(rfp))

      # Sweep width
      swm <- unique(unlist(lapply(sad, function(p) par(p)$SweepWidthMethod)))
      sww <- unique(unlist(lapply(sad, function(p) par(p)$SweepWidth)))
      swept <- NULL
      if (length(swm) == 1L && swm == "Constant" && length(sww) == 1L) {
        swept <- list(method = "fixed", fixed_m = as.numeric(sww))
        decide("swept_width", paste(sww, "m"))
      } else {
        note("the sweep width is not a single constant; the template takes a constant width only")
        swept <- list(method = "fixed", fixed_m = 1)
      }

      # Bootstrap
      bs <- first("Bootstrap")
      bpar <- if (is.null(bs)) list() else par(bs)
      n_boot <- bpar$NumberOfBootstraps
      seed <- draft_column(bpar$BootstrapMethodTable, "Seed")
      impute_seed <- draft_column(bpar$BaselineSeedTable, "Seed")
      pct <- as.numeric(unique(unlist(lapply(procs[funs == "ReportBootstrap"], function(p) par(p)$Percentages))))
      interval <- if (length(pct) && any(abs(pct - 5) < 1e-9) && any(abs(pct - 95) < 1e-9)) 90L else 95L
      bootstrap <- list(replicates = as.integer(if (!is.null(replicates)) replicates else if (!is.null(n_boot)) n_boot else 100L),
                        seed = as.integer(if (length(seed)) seed[1] else 1L), interval = interval)
      if (length(impute_seed)) bootstrap$impute_seed <- as.integer(impute_seed[1])
      if (is.null(bs)) note("no Bootstrap process; the template always runs a bootstrap") else
        decide("bootstrap", sprintf("%d replicates, seed %d, %d%% interval", bootstrap$replicates, bootstrap$seed, interval))
      if (!length(seed)) note("the bootstrap seed was not found; 1 is used")

      # What the fit check flags as not supported
      fit <- stox_fit_rows(describe_stox_project(project, root = root))
      no <- fit[fit$status == "not supported", , drop = FALSE]
      for (k in seq_len(nrow(no))) note(no$feature[k], ": ", no$detail[k])

      cfg <- list(
        survey = list(label = "auto", year = "auto"),
        data = list(biotic = biotic, strata = strata_out, stratum_label = "stratum"),
        inclusion = inc,
        swept_width = swept,
        species = species
      )
      if (!is.null(groups)) cfg$species_groups <- groups
      cfg$quantities <- quantities
      cfg$catch <- catch
      if (!is.null(lengths)) cfg$lengths <- lengths
      if (length(biomass)) cfg$biomass <- biomass
      cfg$bootstrap <- bootstrap
      cfg$estimate <- list(point = "bootstrap_mean")
      cfg$stox <- list(version = stox_pinned_version)
      cfg$disclosure <- list(min_stations = 5L, min_positive = 3L)

      header <- c(
        paste0("# Drafted by stox_config_from_project() from ", basename(src), " on ", format(Sys.Date()), "."),
        paste0("# Made with ", paste(pj$versions, collapse = "; "), ". Paths are relative to NANSEN_DATA_ROOT."),
        if (length(unresolved)) c("# To settle before a run:", paste0("#  - ", unique(unresolved))) else "# Nothing left to settle.",
        "")
      dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
      body <- gsub(": yes$", ": true", gsub(": no$", ": false", strsplit(yaml::as.yaml(cfg), "\n")[[1]]))
      writeLines(c(header, body), out)
      valid <- tryCatch({ read_config(out); TRUE }, error = function(e) FALSE)
      structure(list(file = out, decided = decided, unresolved = unique(unresolved), valid = valid),
                class = "nb_config_draft")
    },
    log_dir = file.path(root, "logs"),
    code = "SX-DRAFT-04",
    message = "The configuration could not be drafted."
  )
}

#' @export
print.nb_config_draft <- function(x, ...) {
  cat("<nb_config_draft> settings only; written to", x$file, "\n")
  for (k in names(x$decided)) cat(sprintf("  %-34s %s\n", k, x$decided[[k]]))
  cat("Accepted by read_config():", if (isTRUE(x$valid)) "yes" else "no", "\n")
  if (length(x$unresolved)) {
    cat("To settle before a run:\n")
    for (u in x$unresolved) cat("  -", u, "\n")
  } else {
    cat("Nothing left to settle.\n")
  }
  invisible(x)
}

# ---- One call: reproduce an official project -------------------------------------------

repro_fmt <- function(x, digits = 3) ifelse(is.na(x), "NA", formatC(signif(x, digits), digits = digits, format = "fg"))

#' Reproduce an official StoX project in one call
#'
#' Runs the whole reproduction of an official project on the laptop and reports it on one page,
#' for a person to read and pass on. The steps, in an order that spends no time on a
#' configuration that does not match:
#'
#' 1. the inclusion counts ([inclusion_summary()]);
#' 2. our estimate with the baseline as the point estimate and a few replicates (fast);
#' 3. a copy of the official project rerun under the installed StoX ([stox_official_copy_run()]),
#'    with the replicates of the configuration;
#' 4. the baseline comparison, which has no randomness and must be exact: if it is not, the run
#'    stops here and the report says so;
#' 5. our full estimate, with the configuration's replicates and point estimate;
#' 6. the comparison of bootstrap means, with `z` in units of the Monte Carlo error.
#'
#' The report holds counts, ratios and `z` values only, never estimates, station identifiers or
#' positions, and closes with a reading against D-11 (baseline values exact; bootstrap means within
#' Monte Carlo noise). The reading is a summary for the person, who decides. The report is also
#' saved as a text file in the data zone (`reproduction/`), and the full objects are returned for
#' further checks on the laptop.
#'
#' @param config A configuration file or an object from [read_config()].
#' @param project Path, relative to `root`, of the official StoX project folder.
#' @param quick_replicates Replicates of the fast baseline run (step 2).
#' @param cores Cores for the official rerun; by default the project's own setting.
#' @inheritParams read_survey
#' @return An `nb_reproduction`: `report` (the lines printed), `verdict`, and the objects behind it
#'   (`inclusion`, `baseline_run`, `official`, `baseline_comparison`, `estimate`, `comparison`).
#' @export
#' @examples
#' \dontrun{
#' rep <- reproduce_official("configs/survey-B.yml", "stox_official/<project folder>")
#' }
reproduce_official <- function(config, project, root = data_root(), quick_replicates = 3L, cores = NULL) {
  cfg <- if (inherits(config, "nb_config")) config else read_config(config)
  n <- cfg$bootstrap$replicates
  step <- function(i, what) message(sprintf("Step %d of 6: %s", i, what))
  out <- list()

  step(1, "inclusion counts")
  out$inclusion <- inclusion_summary(cfg, root = root)

  step(2, "our baseline run (fast)")
  cfg_b <- cfg
  cfg_b$estimate$point <- "baseline"
  cfg_b$bootstrap$replicates <- as.integer(quick_replicates)
  out$baseline_run <- suppressMessages(run_estimate(cfg_b, root = root))

  step(3, "the official project, rerun under the installed StoX (the longest step)")
  out$official <- stox_official_copy_run(project, root = root, replicates = n, cores = cores)
  ref <- official_reports_to_table(out$official$reports)

  step(4, "baseline comparison")
  out$baseline_comparison <- tryCatch(compare_estimates(out$baseline_run$estimates, ref, "baseline"),
                                      error = function(e) NULL)
  cb <- out$baseline_comparison
  base_ok <- is.null(cb) || all(is.na(cb$ratio) | abs(cb$ratio - 1) < 1e-6)

  if (base_ok) {
    step(5, "our full run")
    out$estimate <- run_estimate(cfg, root = root)
    step(6, "comparison of bootstrap means")
    out$comparison <- tryCatch(compare_estimates(out$estimate$estimates, ref, "mean", replicates = c(n, n)),
                               error = function(e) NULL)
  }

  # The report: counts, ratios and z only
  label <- if (!is.null(out$baseline_run$estimates$survey_id)) out$baseline_run$estimates$survey_id[1] else cfg$survey$label
  r <- out$inclusion$rules
  bs <- out$inclusion$by_stratum
  lines <- c(
    sprintf("Reproduction of %s (survey %s), %s", basename(project), label, format(Sys.time(), "%Y-%m-%d %H:%M")),
    sprintf("Configuration hash %s; %d replicates on both sides", attr(cfg, "config_hash"), n),
    "", "Inclusion (stations):",
    sprintf("  %-45s %5d -> %5d", r$rule, r$n_before, r$n_after),
    sprintf("  strata: %d, with stations: %d, without: %d", nrow(bs), sum(bs$n_kept > 0), sum(bs$n_kept == 0))
  )
  if (any(ref$unit_assumed)) lines <- c(lines, "  note: some official reports state no unit; their scale was assumed")
  lines <- c(lines, "", "Baseline (deterministic; must be exact):")
  if (is.null(cb)) {
    lines <- c(lines, "  the official project has no baseline report to compare with")
  } else {
    dev <- abs(cb$ratio - 1)
    lines <- c(lines, sprintf("  cells compared: %d, exact: %d, largest deviation: %s",
                              sum(!is.na(cb$ratio)), sum(dev < 1e-6, na.rm = TRUE),
                              repro_fmt(max(c(0, dev), na.rm = TRUE))))
  }
  cm <- out$comparison
  if (!base_ok) {
    lines <- c(lines, "  STOPPED: the baseline differs, so the configuration does not yet match the project.")
    verdict <- "not reproduced: the baseline differs"
  } else if (is.null(cm)) {
    lines <- c(lines, "", "Bootstrap means: the official project has no bootstrap report to compare with")
    verdict <- if (is.null(cb)) "not checked: nothing to compare" else "baseline reproduced; no bootstrap to compare"
  } else {
    z <- cm$z[!is.na(cm$z)]
    share <- if (length(z)) mean(abs(z) < 2) else NA_real_
    tot <- cm[cm$stratum == "total", , drop = FALSE]
    lines <- c(lines, "", "Bootstrap means (within Monte Carlo noise if |z| < about 2):",
               sprintf("  cells with z: %d, |z| < 2: %s%%, mean z: %s, range: %s to %s", length(z),
                       if (is.finite(share)) sprintf("%.0f", 100 * share) else "NA", repro_fmt(if (length(z)) mean(z) else NA),
                       repro_fmt(if (length(z)) min(z) else NA), repro_fmt(if (length(z)) max(z) else NA)),
               "  totals (species or group, quantity, ratio, z, CV ratio):",
               sprintf("    %-22s %-9s %8s %8s %8s", tot$species_code, tot$quantity, repro_fmt(tot$ratio),
                       repro_fmt(tot$z), repro_fmt(tot$cv_ratio)))
    worst <- suppressWarnings(max(abs(tot$z), na.rm = TRUE))
    verdict <- if (is.finite(share) && share >= 0.9 && (!is.finite(worst) || worst <= 3)) {
      "consistent with D-11: baseline exact, bootstrap means within Monte Carlo noise"
    } else {
      "needs review: bootstrap means outside the noise expected under D-11"
    }
  }
  if (!is.null(out$estimate)) {
    lines <- c(lines, "", sprintf("Airlock: full export %s, total-only export %s", out$estimate$staged$outcome,
                                  if (is.null(out$estimate$staged_total_only)) "not needed" else out$estimate$staged_total_only$outcome))
  }
  lines <- c(lines, "", paste("Reading:", verdict), "The person running the comparison decides whether it is accepted.")

  dir <- file.path(root, "reproduction")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file <- file.path(dir, paste0(gsub("[^A-Za-z0-9._-]", "_", label), "-", format(Sys.time(), "%Y%m%dT%H%M%S"), ".txt"))
  writeLines(lines, file)
  structure(c(list(report = lines, verdict = verdict, file = file), out), class = "nb_reproduction")
}

#' @export
print.nb_reproduction <- function(x, ...) {
  cat(x$report, sep = "\n")
  cat("\n(saved in the data zone)\n")
  invisible(x)
}

# stox_report: StoX report tables to the Section 9 schema, and the support table
# Data class touched: C0 in, C2 candidates out (staged, never released here).

# StoX names a species category "<name>/<code>/<aphia>/<scientific name>". The
# code is the element that matches one of the configured species codes.
species_from_category <- function(category, codes) {
  vapply(strsplit(as.character(category), "/", fixed = TRUE), function(p) {
    hit <- intersect(p, codes)
    if (length(hit)) hit[[1]] else NA_character_
  }, character(1))
}

# One report table as a plain data frame with `stratum` (or "total"),
# `species_code` and the requested statistic columns.
tidy_report <- function(dt, codes, stratum = NULL, cols) {
  df <- as.data.frame(dt)
  # Totals are grouped by Survey; strata left out of the survey carry no label
  if ("Survey" %in% names(df)) df <- df[!is.na(df$Survey), , drop = FALSE]
  out <- data.frame(
    stratum = if (is.null(stratum)) as.character(df$Stratum) else rep(stratum, nrow(df)),
    species_code = species_from_category(df$SpeciesCategory, codes),
    stringsAsFactors = FALSE
  )
  for (nm in names(cols)) {
    hit <- grep(cols[[nm]], names(df), value = TRUE)
    if (length(hit) != 1L) nb_abort("SX-REP-01", paste0("The StoX report has no unique column for ", nm, "."))
    out[[nm]] <- as.numeric(df[[hit]])
  }
  out[!is.na(out$species_code), , drop = FALSE]
}

# Converts the four reports of each quantity (baseline and bootstrap, by
# stratum and total) into Section 9 rows. The estimate is the baseline value;
# the CV is the bootstrap SD over the bootstrap mean; the interval is the 2.5% to
# 97.5% percentile interval of the bootstrap.
stox_reports_to_estimates <- function(reports, cfg, config_hash, code_version,
                                      run_time = Sys.time()) {
  codes <- cfg$species
  units <- c(biomass = "tonnes", abundance = "millions")
  scale <- c(biomass = 1e-3, abundance = 1e-6)
  rows <- lapply(cfg$quantities, function(qty) {
    Q <- c(biomass = "Biomass", abundance = "Abundance")[[qty]]
    get <- function(prefix, part) reports[[paste0(prefix, Q, part)]]
    base <- rbind(
      tidy_report(get("Report", "ByStratum"), codes, cols = c(value = "_sum$")),
      tidy_report(get("Report", "Total"), codes, stratum = "total", cols = c(value = "_sum$"))
    )
    boot_cols <- c(mean = "_sum_mean$", sd = "_sum_sd$", lower = "_sum_2\\.5%$", upper = "_sum_97\\.5%$")
    boot <- rbind(
      tidy_report(get("ReportBootstrap", "ByStratum"), codes, cols = boot_cols),
      tidy_report(get("ReportBootstrap", "Total"), codes, stratum = "total", cols = boot_cols)
    )
    m <- merge(base, boot, by = c("stratum", "species_code"), all.x = TRUE, sort = FALSE)
    m <- m[!is.na(m$value), , drop = FALSE]
    tibble::tibble(
      survey_id = cfg$survey$label,
      year = cfg$survey$year,
      species_code = m$species_code,
      stratum = m$stratum,
      method = "stox_sweptarea",
      quantity = qty,
      value = m$value * scale[[qty]],
      unit = units[[qty]],
      cv = ifelse(is.finite(m$mean) & m$mean > 0, m$sd / m$mean, NA_real_),
      ci_lower = m$lower * scale[[qty]],
      ci_upper = m$upper * scale[[qty]],
      ci_type = "bootstrap_percentile_95",
      config_hash = config_hash,
      code_version = code_version,
      run_time = as.POSIXct(run_time, tz = "UTC"),
      disclosure = NA_character_
    )
  })
  dplyr::bind_rows(rows)
}

# Stations and stations with a positive catch per stratum and configured species,
# from the stations the estimate used (D-03). The strata of the configuration all
# appear, with zero where nothing was kept.
stox_support_table <- function(survey, keep, stratum, cfg) {
  kept_serial <- survey$station$serialnumber[keep]
  kept_stratum <- stratum[keep]
  catch <- survey$catch
  dplyr::bind_rows(lapply(cfg$species, function(sp) {
    pos <- unique(catch$serialnumber[catch$catchcategory %in% sp & !is.na(catch$catchweight) &
                                       catch$catchweight > 0])
    tibble::tibble(
      survey_id = cfg$survey$label, year = cfg$survey$year, species_code = sp,
      stratum = cfg$data$stratum_names,
      n_stations = vapply(cfg$data$stratum_names, function(s) sum(kept_stratum == s, na.rm = TRUE),
                          integer(1), USE.NAMES = FALSE),
      n_positive = vapply(cfg$data$stratum_names, function(s) {
        sum(kept_stratum == s & kept_serial %in% pos, na.rm = TRUE)
      }, integer(1), USE.NAMES = FALSE)
    )
  }))
}

# stox_report: StoX report tables to the Section 9 schema, and the support table
# Data class touched: C0 in, C2 candidates out (staged, never released here).

# StoX units (RstoxData::getUnitOptions()) and their factors to tonnes (mass) and to millions
# of individuals (cardinality). Results from projects that report in different units are
# made comparable with these; a unit not listed stops the conversion, it is never guessed.
stox_unit_factors <- list(
  biomass = c(mcg = 1e-12, mg = 1e-9, g = 1e-6, kg = 1e-3, ton = 1, kiloton = 1e3, megaton = 1e6),
  abundance = c(individuals = 1e-6, "10^3 individuals" = 1e-3, "10^6 individuals" = 1,
                "10^9 individuals" = 1e3, "10^12 individuals" = 1e6)
)

stox_unit_factor <- function(unit, quantity) {
  known <- stox_unit_factors[[quantity]]
  u <- trimws(as.character(unit))
  bad <- unique(u[is.na(u) | !u %in% names(known)])
  if (length(bad)) {
    nb_abort("SX-UNIT-01", paste0("A ", quantity, " report states the unit '", paste(bad, collapse = "', '"),
                                  "', which the package does not convert (known: ",
                                  paste(names(known), collapse = ", "), ")."))
  }
  unname(known[u])
}

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
tidy_report <- function(dt, codes, stratum = NULL, cols, quantity) {
  df <- as.data.frame(dt)
  # Totals are grouped by Survey; strata left out of the survey carry no label
  if ("Survey" %in% names(df)) df <- df[!is.na(df$Survey), , drop = FALSE]
  out <- data.frame(
    stratum = if (is.null(stratum)) as.character(df$Stratum) else rep(stratum, nrow(df)),
    species_code = species_from_category(df$SpeciesCategory, codes),
    stringsAsFactors = FALSE
  )
  # every report of the template states its unit; values are converted to tonnes or millions
  if (!"Unit" %in% names(df)) nb_abort("SX-UNIT-02", "A StoX report of the template states no unit.")
  factor <- stox_unit_factor(df$Unit, quantity)
  for (nm in names(cols)) {
    hit <- grep(cols[[nm]], names(df), value = TRUE)
    if (length(hit) != 1L) nb_abort("SX-REP-01", paste0("The StoX report has no unique column for ", nm, "."))
    out[[nm]] <- as.numeric(df[[hit]]) * factor
  }
  out[!is.na(out$species_code), , drop = FALSE]
}

# Converts the four reports of each quantity (baseline and bootstrap, by
# stratum and total) into Section 9 rows. The estimate is the baseline value (or
# the bootstrap mean, when the configuration asks for it); the CV is the bootstrap
# SD over the bootstrap mean; the interval is the 2.5% to 97.5% percentile
# interval of the bootstrap (90% when configured).
stox_reports_to_estimates <- function(reports, cfg, config_hash, code_version,
                                      run_time = Sys.time(), unsampled = character(0)) {
  codes <- cfg$species
  units <- c(biomass = "tonnes", abundance = "millions")
  from_bootstrap <- identical(cfg$estimate$point, "bootstrap_mean")
  interval <- bootstrap_interval(cfg)
  rows <- lapply(cfg$quantities, function(qty) {
    Q <- c(biomass = "Biomass", abundance = "Abundance")[[qty]]
    get <- function(prefix, part) reports[[paste0(prefix, Q, part)]]
    base <- rbind(
      tidy_report(get("Report", "ByStratum"), codes, cols = c(value = "_sum$"), quantity = qty),
      tidy_report(get("Report", "Total"), codes, stratum = "total", cols = c(value = "_sum$"), quantity = qty)
    )
    lim <- c((100 - interval) / 2, 100 - (100 - interval) / 2)
    pct <- gsub(".", "\\.", format(lim, trim = TRUE, drop0trailing = TRUE), fixed = TRUE)
    boot_cols <- c(mean = "_sum_mean$", sd = "_sum_sd$", lower = paste0("_sum_", pct[1], "%$"),
                   upper = paste0("_sum_", pct[2], "%$"))
    boot <- rbind(
      tidy_report(get("ReportBootstrap", "ByStratum"), codes, cols = boot_cols, quantity = qty),
      tidy_report(get("ReportBootstrap", "Total"), codes, stratum = "total", cols = boot_cols, quantity = qty)
    )
    m <- merge(base, boot, by = c("stratum", "species_code"), all.x = TRUE, sort = FALSE)
    m <- m[!is.na(m$value), , drop = FALSE]
    # StoX reports no row for a stratum and species without catch. A sampled stratum is a
    # zero for that species; a selected stratum with no station kept is not dropped but has
    # no estimate (NA).
    grid <- expand.grid(stratum = c(cfg$data$stratum_names, "total"), species_code = codes,
                        stringsAsFactors = FALSE)
    absent <- grid[!paste(grid$stratum, grid$species_code) %in% paste(m$stratum, m$species_code), ,
                   drop = FALSE]
    if (nrow(absent)) {
      none <- absent$stratum %in% unsampled |
        (absent$stratum == "total" & all(cfg$data$stratum_names %in% unsampled))
      fill <- ifelse(none, NA_real_, 0)
      m <- dplyr::bind_rows(m, data.frame(
        stratum = absent$stratum, species_code = absent$species_code,
        value = fill, mean = fill, sd = fill, lower = fill, upper = fill,
        stringsAsFactors = FALSE))
    }
    tibble::tibble(
      survey_id = cfg$survey$label,
      year = cfg$survey$year,
      species_code = m$species_code,
      stratum = m$stratum,
      method = "stox_sweptarea",
      quantity = qty,
      value = if (from_bootstrap) m$mean else m$value,
      unit = units[[qty]],
      cv = ifelse(is.finite(m$mean) & m$mean > 0, m$sd / m$mean, NA_real_),
      ci_lower = m$lower,
      ci_upper = m$upper,
      ci_type = paste0("bootstrap_percentile_", interval),
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
stox_support_table <- function(survey, keep, stratum, cfg, groups = NULL) {
  kept_serial <- survey$station$serialnumber[keep]
  kept_stratum <- stratum[keep]
  catch <- survey$catch
  dplyr::bind_rows(lapply(cfg$species, function(sp) {
    # a species group counts a catch of any of its catch categories
    codes <- if (is.null(groups)) sp else groups$value[groups$group == sp]
    pos <- unique(catch$serialnumber[as.character(catch$catchcategory) %in% codes & !is.na(catch$catchweight) &
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

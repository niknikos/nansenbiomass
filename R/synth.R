# synth: generate synthetic surveys from known spatial fields
# Data class touched: C3 only (docs/spec.md, Section 4).
#
# Every value here is generated from closed-form fields and a fixed seed. Nothing
# is derived from real records (CLAUDE.md, rule 6). Identifiers are plainly
# artificial, and the domain sits in the open South Pacific, far from any
# programme area, with shelf depths that do not exist there.

synth_crs <- function(design) {
  sprintf(
    "+proj=laea +lat_0=%s +lon_0=%s +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs",
    design$centre[["lat"]], design$centre[["lon"]]
  )
}

#' Design of a synthetic survey
#'
#' Returns the parameters from which [synth_survey()] generates a survey. The
#' defaults describe a 200 x 120 km domain in a Lambert azimuthal equal-area
#' projection (km units) centred at 30 S, 120 W, with four rectangular strata,
#' three synthetic species and a Tweedie observation model.
#'
#' @param strata A tibble with one row per stratum and the columns `stratum`,
#'   `xmin`, `xmax`, `ymin`, `ymax` (km) and `n_stations`.
#' @param species A tibble with one row per species and the columns
#'   `species_code`, `commonname`, `log_peak` (log of the peak density, kg per
#'   km2), `depth_opt` and `depth_sd` (m), `sigma` and `range_km` of the spatial
#'   effect, `meanlog` and `sdlog` of length (m), and the length-weight
#'   parameters `lw_a` and `lw_b` (weight in kg, length in m).
#' @param family Observation model for catches: `"tweedie"` or `"delta_gamma"`.
#' @param tweedie_p,tweedie_phi Power and dispersion of the Tweedie model.
#' @param dg_scale,dg_cv Delta-gamma model: the probability of a positive catch
#'   is `1 - exp(-mu / dg_scale)`; positive catches are gamma with this CV.
#' @param swept_width_km Mean effective swept width of the trawl, in km. Each
#'   tow's width varies by up to 10% around it and is recorded as the door
#'   spread (`trawldoorspread`, in m), so that the field and the catches agree.
#' @param distance_nmi Range of towed distances, in nautical miles.
#' @param split_prob Probability that a positive catch is recorded as two
#'   disjoint catch parts.
#' @param max_measured Maximum number of fish measured for length per catch part.
#' @param year Survey year.
#' @param grid_km Resolution of the integration grid for the true values, in km.
#' @return An `nb_synth_design` list.
#' @export
#' @examples
#' synth_design()
synth_design <- function(
    strata = tibble::tibble(
      stratum = c("SYN-A", "SYN-B", "SYN-C", "SYN-D"),
      xmin = c(-100, -100, 0, 0),
      xmax = c(0, 0, 100, 100),
      ymin = c(-60, 0, -60, 0),
      ymax = c(0, 60, 0, 60),
      n_stations = c(12L, 15L, 10L, 8L)
    ),
    species = tibble::tibble(
      species_code = c("SYN001", "SYN002", "SYN003"),
      commonname = c("synthetic species one", "synthetic species two",
                     "synthetic species three"),
      log_peak = log(c(2000, 800, 150)),
      depth_opt = c(100, 250, 60),
      depth_sd = c(60, 80, 30),
      sigma = c(0.6, 0.5, 1.0),
      range_km = c(25, 30, 15),
      meanlog = log(c(0.30, 0.45, 0.20)),
      sdlog = c(0.25, 0.20, 0.30),
      lw_a = c(10, 9, 12),
      lw_b = c(3, 3, 3)
    ),
    family = c("tweedie", "delta_gamma"),
    tweedie_p = 1.5,
    tweedie_phi = 2,
    dg_scale = 5,
    dg_cv = 0.8,
    swept_width_km = 0.02,
    distance_nmi = c(1.4, 1.6),
    split_prob = 0.15,
    max_measured = 100L,
    year = 2000L,
    grid_km = 1) {
  family <- match.arg(family)
  structure(
    list(
      centre = c(lat = -30, lon = -120),
      domain = c(xmin = min(strata$xmin), xmax = max(strata$xmax),
                 ymin = min(strata$ymin), ymax = max(strata$ymax)),
      strata = strata,
      species = species,
      family = family,
      tweedie_p = tweedie_p,
      tweedie_phi = tweedie_phi,
      dg_scale = dg_scale,
      dg_cv = dg_cv,
      swept_width_km = swept_width_km,
      distance_nmi = distance_nmi,
      split_prob = split_prob,
      max_measured = as.integer(max_measured),
      year = as.integer(year),
      grid_km = grid_km,
      mission = list(
        missiontype = "4", platform = "9999", missionnumber = 1L,
        cruise = "SYNTH0001", platformname = "SYNTHETIC"
      ),
      # NANSIS conventions for a preselected swept-area bottom-trawl station:
      # stationtype 12, samplequality 12 (usable for biomass analysis), gear OK.
      station_codes = list(stationtype = "12", samplequality = "12", gearcondition = "1",
                           gear = "9999")
    ),
    class = "nb_synth_design"
  )
}

#' @export
print.nb_synth_design <- function(x, ...) {
  cat("<nb_synth_design> synthetic survey design\n")
  cat("  domain (km):", paste(names(x$domain), x$domain, collapse = ", "), "\n")
  cat("  strata:", paste0(x$strata$stratum, " (", x$strata$n_stations, ")", collapse = ", "), "\n")
  cat("  species:", paste(x$species$species_code, collapse = ", "), "\n")
  cat("  family:", x$family, "\n")
  invisible(x)
}

# Bottom depth (m) at km coordinates: deepening offshore (x), with one bank.
synth_depth <- function(x, y) {
  base <- 20 + 480 * (pmin(pmax(x + 100, 0), 200) / 200)^1.5
  bank <- 80 * exp(-((x - 40)^2 + (y - 10)^2) / (2 * 20^2))
  pmax(base - bank, 10)
}

# Process-convolution field: a weighted sum of Gaussian kernels.
kernel_sum <- function(x, y, field) {
  d2 <- outer(x, field$cx, "-")^2 + outer(y, field$cy, "-")^2
  drop(exp(-d2 / (2 * field$range_km^2)) %*% field$w)
}

# Draws the kernels of one species' spatial effect and standardises it on the
# integration grid, so that it has mean 0 and standard deviation `sigma` there.
make_field <- function(sp, design, grid) {
  pad <- 2 * sp$range_km
  dom <- design$domain
  n_kernels <- 80L
  field <- list(
    cx = stats::runif(n_kernels, dom[["xmin"]] - pad, dom[["xmax"]] + pad),
    cy = stats::runif(n_kernels, dom[["ymin"]] - pad, dom[["ymax"]] + pad),
    w = stats::rnorm(n_kernels),
    range_km = sp$range_km,
    sigma = sp$sigma
  )
  raw <- kernel_sum(grid$x, grid$y, field)
  field$centre <- mean(raw)
  field$scale <- stats::sd(raw)
  field
}

field_value <- function(x, y, field) {
  field$sigma * (kernel_sum(x, y, field) - field$centre) / field$scale
}

# True density (kg per km2) of species i at km coordinates.
density_at <- function(sv, i, x, y) {
  sp <- sv$design$species[i, ]
  depth <- synth_depth(x, y)
  exp(sp$log_peak - 0.5 * ((depth - sp$depth_opt) / sp$depth_sd)^2 +
        field_value(x, y, sv$fields[[i]]))
}

#' True density of a synthetic survey
#'
#' Evaluates the known density field of one synthetic species at any point of
#' the survey's projected coordinate system (km).
#'
#' @param sv An `nb_synth` object from [synth_survey()].
#' @param species_code Code of one synthetic species.
#' @param x,y Coordinates in km, in the survey's projection (`sv$crs`).
#' @return A numeric vector of densities in kg per km2.
#' @export
#' @examples
#' sv <- synth_survey(seed = 1)
#' synth_density(sv, "SYN001", x = c(-50, 50), y = c(0, 0))
synth_density <- function(sv, species_code, x, y) {
  i <- match(species_code, sv$design$species$species_code)
  if (is.na(i) || length(i) != 1L) {
    nb_abort("SY-ARG-01", "`species_code` must name one species of the design.")
  }
  density_at(sv, i, x, y)
}

# Tweedie draws as compound Poisson-gamma sums, with mean mu.
rtweedie <- function(mu, p, phi) {
  lambda <- mu^(2 - p) / (phi * (2 - p))
  shape <- (2 - p) / (p - 1)
  scale <- phi * (p - 1) * mu^(p - 1)
  n <- stats::rpois(length(mu), lambda)
  y <- numeric(length(mu))
  pos <- n > 0
  y[pos] <- stats::rgamma(sum(pos), shape = n[pos] * shape, scale = scale[pos])
  y
}

# Delta-gamma draws with mean mu.
rdeltagamma <- function(mu, scale, cv) {
  prob <- 1 - exp(-mu / scale)
  present <- stats::runif(length(mu)) < prob
  y <- numeric(length(mu))
  shape <- 1 / cv^2
  y[present] <- stats::rgamma(sum(present), shape = shape,
                              scale = mu[present] / prob[present] / shape)
  y
}

mean_weight <- function(sp) {
  sp$lw_a * exp(sp$lw_b * sp$meanlog + sp$lw_b^2 * sp$sdlog^2 / 2)
}

# Places stations uniformly in each stratum and builds the tows.
make_stations <- function(design) {
  st <- design$strata
  n <- sum(st$n_stations)
  idx <- rep(seq_len(nrow(st)), st$n_stations)
  x <- stats::runif(n, st$xmin[idx], st$xmax[idx])
  y <- stats::runif(n, st$ymin[idx], st$ymax[idx])
  distance <- round(stats::runif(n, design$distance_nmi[1], design$distance_nmi[2]), 2)
  heading <- stats::runif(n, 0, 2 * pi)
  dist_km <- distance * 1.852
  door_m <- round(design$swept_width_km * 1000 * stats::runif(n, 0.9, 1.1), 1)
  tibble::tibble(
    stratum = st$stratum[idx],
    x = x, y = y,
    x_end = x + dist_km * sin(heading),
    y_end = y + dist_km * cos(heading),
    distance = distance,
    trawldoorspread = door_m,
    swept_area_km2 = dist_km * door_m / 1000
  )
}

to_lonlat <- function(x, y, crs) {
  pts <- sf::st_as_sf(data.frame(x = x, y = y), coords = c("x", "y"), crs = crs)
  xy <- sf::st_coordinates(sf::st_transform(pts, 4326))
  list(lon = round(xy[, 1], 4), lat = round(xy[, 2], 4))
}

strata_polygons <- function(design, crs) {
  st <- design$strata
  polys <- lapply(seq_len(nrow(st)), function(i) {
    ring <- matrix(c(st$xmin[i], st$ymin[i], st$xmax[i], st$ymin[i], st$xmax[i], st$ymax[i],
                     st$xmin[i], st$ymax[i], st$xmin[i], st$ymin[i]), ncol = 2, byrow = TRUE)
    sf::st_polygon(list(ring))
  })
  geom <- sf::st_segmentize(sf::st_sfc(polys, crs = crs), dfMaxLength = 5)
  sf::st_sf(stratum = st$stratum, area_km2 = (st$xmax - st$xmin) * (st$ymax - st$ymin),
            geometry = sf::st_transform(geom, 4326))
}

integration_grid <- function(design) {
  dom <- design$domain
  g <- design$grid_km
  xs <- seq(dom[["xmin"]] + g / 2, dom[["xmax"]] - g / 2, by = g)
  ys <- seq(dom[["ymin"]] + g / 2, dom[["ymax"]] - g / 2, by = g)
  grid <- expand.grid(x = xs, y = ys)
  st <- design$strata
  grid$stratum <- NA_character_
  for (i in seq_len(nrow(st))) {
    inside <- grid$x >= st$xmin[i] & grid$x < st$xmax[i] &
      grid$y >= st$ymin[i] & grid$y < st$ymax[i]
    grid$stratum[inside] <- st$stratum[i]
  }
  grid$area_km2 <- g^2
  tibble::as_tibble(grid)
}

# Integrates the true density over the grid, by stratum and in total.
true_values <- function(sv, grid) {
  design <- sv$design
  rows <- lapply(seq_len(nrow(design$species)), function(i) {
    sp <- design$species[i, ]
    kg <- density_at(sv, i, grid$x, grid$y) * grid$area_km2
    by_stratum <- tibble::tibble(stratum = grid$stratum, kg = kg) |>
      dplyr::filter(!is.na(.data$stratum)) |>
      dplyr::summarise(kg = sum(.data$kg), .by = "stratum")
    by_stratum <- dplyr::bind_rows(by_stratum, tibble::tibble(stratum = "total", kg = sum(by_stratum$kg)))
    dplyr::bind_rows(
      dplyr::mutate(by_stratum, quantity = "biomass", value = .data$kg / 1000, unit = "tonnes"),
      dplyr::mutate(by_stratum, quantity = "abundance", value = .data$kg / mean_weight(sp) / 1e6,
                    unit = "millions")
    ) |>
      dplyr::mutate(species_code = sp$species_code)
  })
  dplyr::bind_rows(rows) |>
    dplyr::mutate(survey_id = design$mission$cruise, year = design$year) |>
    dplyr::select("survey_id", "year", "species_code", "stratum", "quantity", "value", "unit")
}

# Individuals of one catch part: lengths (m) and weights (kg).
make_individuals <- function(sp, n) {
  length_m <- round(stats::rlnorm(n, sp$meanlog, sp$sdlog), 3)
  weight <- sp$lw_a * length_m^sp$lw_b * stats::rlnorm(n, 0, 0.1)
  list(length = length_m, individualweight = round(pmax(weight, 0.001), 3),
       sex = sample(c("1", "2"), n, replace = TRUE))
}

#' Generate a synthetic survey
#'
#' Simulates a stratified random bottom-trawl survey from known spatial fields
#' (docs/spec.md, Section 8, tier 2). Density depends on depth and on a smooth
#' spatial effect built independently of sdmTMB; catches follow the design's
#' observation model; lengths and weights follow a length-weight relationship.
#' The true biomass and abundance, integrated over the strata, are returned with
#' the survey so that estimators can be checked against them.
#'
#' All identifiers are plainly artificial, and the survey is flagged as
#' synthetic, which [write_biotic()] requires.
#'
#' @param design An `nb_synth_design` from [synth_design()].
#' @param seed Random seed. The same seed and design give the same survey.
#' @return An `nb_synth` list with `survey` (an `nb_survey`), `truth` (true
#'   biomass in tonnes and abundance in millions, by stratum and in total),
#'   `strata` (an `sf` object of stratum polygons in WGS84), `stations` (station
#'   positions in km, with stratum and swept area), `crs` (the projection),
#'   `fields`, `design` and `seed`.
#' @export
#' @examples
#' sv <- synth_survey(seed = 1)
#' sv
#' sv$truth
synth_survey <- function(design = synth_design(), seed = 1L) {
  if (!inherits(design, "nb_synth_design")) {
    nb_abort("SY-ARG-02", "`design` must come from synth_design().")
  }
  withr::with_seed(seed, synth_survey_impl(design, seed))
}

synth_survey_impl <- function(design, seed) {
  crs <- synth_crs(design)
  grid <- integration_grid(design)
  sv <- list(design = design, crs = crs, seed = seed)
  sv$fields <- lapply(seq_len(nrow(design$species)), function(i) {
    make_field(design$species[i, ], design, grid)
  })
  stations <- make_stations(design)
  n_st <- nrow(stations)
  start <- to_lonlat(stations$x, stations$y, crs)
  end <- to_lonlat(stations$x_end, stations$y_end, crs)
  m <- design$mission
  dates <- as.Date(sprintf("%d-05-01", design$year)) + (seq_len(n_st) - 1L) %/% 4L
  hours <- 6L + 3L * ((seq_len(n_st) - 1L) %% 4L)
  times <- sprintf("%02d:15:00.000Z", hours)
  stop_times <- sprintf("%02d:45:00.000Z", hours)
  # Tows last 30 minutes; the log runs on from an arbitrary 1000 nmi.
  log_start <- round(1000 + cumsum(c(0, stations$distance[-n_st] + 20)), 2)
  mission_keys <- tibble::tibble(missiontype = m$missiontype, startyear = design$year,
                                 platform = m$platform, missionnumber = m$missionnumber)
  station <- tibble::tibble(
    mission_keys[rep(1L, n_st), ],
    serialnumber = 90000L + seq_len(n_st),
    station = seq_len(n_st),
    stationstartdate = dates,
    stationstarttime = times,
    stationtype = design$station_codes$stationtype,
    latitudestart = start$lat, longitudestart = start$lon,
    latitudeend = end$lat, longitudeend = end$lon,
    bottomdepthstart = round(synth_depth(stations$x, stations$y), 1),
    bottomdepthstop = round(synth_depth(stations$x_end, stations$y_end), 1),
    fishingdepthmin = NA_real_,
    gear = design$station_codes$gear,
    gearcondition = design$station_codes$gearcondition,
    samplequality = design$station_codes$samplequality,
    distance = stations$distance,
    stationstopdate = dates,
    stationstoptime = stop_times,
    fishingdepthmax = NA_real_,
    vesselspeed = round(stations$distance / 0.5, 1),
    logstart = log_start,
    logstop = log_start + stations$distance,
    verticaltrawlopening = round(stats::runif(n_st, 4.5, 5.5), 1),
    trawldoorspread = stations$trawldoorspread,
    haulvalidity = NA_character_,
    gearno = NA_integer_
  )
  stations$serialnumber <- station$serialnumber

  catch_rows <- list()
  ind_rows <- list()
  for (i in seq_len(nrow(design$species))) {
    sp <- design$species[i, ]
    mu <- density_at(sv, i, stations$x, stations$y) * stations$swept_area_km2
    y <- if (design$family == "tweedie") {
      rtweedie(mu, design$tweedie_p, design$tweedie_phi)
    } else {
      rdeltagamma(mu, design$dg_scale, design$dg_cv)
    }
    y <- round(y, 3)
    w_mean <- mean_weight(sp)
    for (j in which(y > 0)) {
      parts <- if (stats::runif(1) < design$split_prob) {
        share <- stats::runif(1, 0.3, 0.7)
        round(c(y[j] * share, y[j] - y[j] * share), 3)
      } else {
        y[j]
      }
      parts <- parts[parts > 0]
      for (k in seq_along(parts)) {
        count <- max(1L, as.integer(round(parts[k] / w_mean)))
        n_meas <- min(count, design$max_measured)
        fish <- make_individuals(sp, n_meas)
        ls_weight <- if (n_meas == count) parts[k] else min(sum(fish$individualweight), parts[k])
        catchsampleid <- length(catch_rows) + 1L
        catch_rows[[catchsampleid]] <- tibble::tibble(
          serialnumber = station$serialnumber[j],
          catchsampleid = catchsampleid,
          catchcategory = sp$species_code,
          commonname = sp$commonname,
          aphia = NA_character_,
          catchpartnumber = k,
          sampletype = NA_character_,
          catchweight = parts[k],
          catchcount = count,
          lengthsampleweight = round(ls_weight, 3),
          lengthsamplecount = n_meas,
          scientificname = paste("Synthetica", tolower(sp$species_code)),
          lengthmeasurement = NA_character_,
          catchproducttype = NA_character_,
          sampleproducttype = NA_character_,
          raisingfactor = 1,
          specimensamplecount = n_meas
        )
        ind_rows[[catchsampleid]] <- tibble::tibble(
          serialnumber = station$serialnumber[j],
          catchsampleid = catchsampleid,
          specimenid = seq_len(n_meas),
          length = fish$length,
          individualweight = fish$individualweight,
          sex = fish$sex,
          lengthresolution = NA_character_,
          individualproducttype = NA_character_
        )
      }
    }
  }
  # Order and number catch samples by station, as in an NMDBiotic file.
  catch <- dplyr::bind_rows(catch_rows)
  individual <- dplyr::bind_rows(ind_rows)
  catch$old_id <- catch$catchsampleid
  catch <- dplyr::arrange(catch, .data$serialnumber, .data$old_id)
  catch$catchsampleid <- seq_len(nrow(catch))
  individual$catchsampleid <- catch$catchsampleid[match(individual$catchsampleid, catch$old_id)]
  individual <- dplyr::arrange(individual, .data$serialnumber, .data$catchsampleid,
                               .data$specimenid)
  catch$old_id <- NULL
  catch <- dplyr::bind_cols(mission_keys[rep(1L, nrow(catch)), ], catch)
  individual <- dplyr::bind_cols(mission_keys[rep(1L, nrow(individual)), ], individual)
  mission <- tibble::tibble(
    mission_keys,
    cruise = m$cruise,
    platformname = m$platformname,
    missionstartdate = min(dates),
    missionstopdate = max(dates)
  )
  tables <- list(
    mission = mission[table_columns("mission")],
    station = station[table_columns("station")],
    catch = catch[table_columns("catch")],
    individual = individual[table_columns("individual")]
  )
  tables$individual$specimenid <- as.integer(tables$individual$specimenid)
  tables$catch$catchpartnumber <- as.integer(tables$catch$catchpartnumber)
  sv$survey <- new_survey(tables, synthetic = TRUE,
                          namespace = "http://www.imr.no/formats/nmdbiotic/v3.1")
  sv$truth <- true_values(sv, grid)
  sv$strata <- strata_polygons(design, crs)
  sv$stations <- stations[c("serialnumber", "stratum", "x", "y", "distance", "trawldoorspread",
                            "swept_area_km2")]
  structure(
    sv[c("survey", "truth", "strata", "stations", "crs", "fields", "design", "seed")],
    class = "nb_synth"
  )
}

#' @export
print.nb_synth <- function(x, ...) {
  cat("<nb_synth> synthetic survey, seed ", x$seed, ", family ", x$design$family, "\n", sep = "")
  print(x$survey)
  invisible(x)
}

# ---- Writing ------------------------------------------------------------------

xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

# Child elements of one row, skipping missing values.
xml_fields <- function(df, fields, indent) {
  parts <- lapply(fields, function(f) {
    v <- df[[f]]
    txt <- if (inherits(v, "Date")) paste0(format(v, "%Y-%m-%d"), "Z") else format_plain(v)
    ifelse(is.na(v), "", sprintf("%s<%s>%s</%s>\n", indent, f, xml_escape(txt), f))
  })
  do.call(paste0, parts)
}

format_plain <- function(v) {
  if (is.double(v)) {
    out <- formatC(v, format = "fg", digits = 10, flag = "")
    trimws(out)
  } else {
    as.character(v)
  }
}

#' Write a synthetic survey as NMDBiotic XML
#'
#' Writes a synthetic survey in the NMDBiotic v3.1 format, so that the reader can
#' be tested on the same file format as real data. Only surveys flagged as
#' synthetic are written: the package cannot be used to copy real data into a
#' new raw-data file (CLAUDE.md, rule 6).
#'
#' @param x An `nb_synth` object, or a synthetic `nb_survey`.
#' @param file Path of the XML file to write.
#' @return The path, invisibly.
#' @export
#' @examples
#' file <- tempfile(fileext = ".xml")
#' write_biotic(synth_survey(seed = 1), file)
write_biotic <- function(x, file) {
  survey <- if (inherits(x, "nb_synth")) x$survey else x
  if (!inherits(survey, "nb_survey") || !isTRUE(attr(survey, "synthetic"))) {
    nb_abort("SY-WRITE-01", "write_biotic() writes synthetic surveys only.")
  }
  s <- biotic_schema()
  el <- function(tbl) s$field[s$table == tbl & s$source == "element"]
  ind <- survey$individual
  ind_xml <- sprintf("        <individual specimenid=\"%d\">\n%s        </individual>\n",
                     ind$specimenid, xml_fields(ind, el("individual"), "          "))
  ind_by_catch <- split(ind_xml, paste(ind$serialnumber, ind$catchsampleid))
  ca <- survey$catch
  ca_body <- xml_fields(ca, el("catch"), "        ")
  ca_inds <- vapply(paste(ca$serialnumber, ca$catchsampleid), function(k) {
    paste(ind_by_catch[[k]], collapse = "")
  }, character(1))
  ca_xml <- sprintf("      <catchsample catchsampleid=\"%d\">\n%s%s      </catchsample>\n",
                    ca$catchsampleid, ca_body, ca_inds)
  ca_by_station <- split(ca_xml, ca$serialnumber)
  st <- survey$station
  st_body <- xml_fields(st, el("station"), "      ")
  st_catches <- vapply(as.character(st$serialnumber), function(k) {
    paste(ca_by_station[[k]], collapse = "")
  }, character(1))
  st_xml <- sprintf("    <fishstation serialnumber=\"%d\">\n%s%s    </fishstation>\n",
                    st$serialnumber, st_body, st_catches)
  mi <- survey$mission
  if (nrow(mi) != 1L) {
    nb_abort("SY-WRITE-02", "write_biotic() writes surveys with exactly one mission.")
  }
  mi_xml <- sprintf(
    "  <mission missiontype=\"%s\" startyear=\"%d\" platform=\"%s\" missionnumber=\"%d\">\n%s%s  </mission>\n",
    xml_escape(mi$missiontype), mi$startyear, xml_escape(mi$platform), mi$missionnumber,
    xml_fields(mi, el("mission"), "    "), paste(st_xml, collapse = "")
  )
  lines <- c(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
    "<!-- SYNTHETIC SURVEY generated by nansenbiomass::synth_survey(); not real data -->",
    "<missions xmlns=\"http://www.imr.no/formats/nmdbiotic/v3.1\">",
    sub("\n$", "", mi_xml),
    "</missions>"
  )
  writeLines(lines, file, useBytes = TRUE)
  invisible(file)
}

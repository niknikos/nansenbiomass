# synth: synthetic surveys from known spatial fields.

sv <- synth_survey(seed = 1)

test_that("the same seed gives the same survey, and another seed does not", {
  again <- synth_survey(seed = 1)
  expect_identical(again$survey$catch, sv$survey$catch)
  expect_identical(again$truth, sv$truth)
  other <- synth_survey(seed = 2)
  expect_false(identical(other$survey$catch$catchweight, sv$survey$catch$catchweight))
})

test_that("identifiers and positions are plainly artificial", {
  st <- sv$survey$station
  expect_equal(sv$survey$mission$cruise, "SYNTH0001")
  expect_equal(sv$survey$mission$platformname, "SYNTHETIC")
  expect_true(all(startsWith(sv$survey$catch$catchcategory, "SYN")))
  expect_true(all(st$latitudestart > -31 & st$latitudestart < -29))
  expect_true(all(st$longitudestart > -122 & st$longitudestart < -118))
  expect_true(isTRUE(attr(sv$survey, "synthetic")))
})

test_that("units follow NMDBiotic: length in m, weights in kg", {
  ind <- sv$survey$individual
  expect_true(all(ind$length > 0.02 & ind$length < 3))
  k <- 100 * (ind$individualweight * 1000) / (ind$length * 100)^3
  expect_true(all(k > 0.2 & k < 5))
  expect_true(all(sv$survey$station$stationtype == "12"))
  expect_true(all(sv$survey$station$samplequality == "12"))
  expect_true(all(sv$survey$catch$raisingfactor == 1))
  st <- sv$survey$station
  expect_true(all(abs(st$trawldoorspread - 20) <= 2.05))
  expect_equal(sv$stations$swept_area_km2, st$distance * 1.852 * st$trawldoorspread / 1000)
  expect_true(all(sv$survey$station$distance >= 1.4 & sv$survey$station$distance <= 1.6))
})

test_that("strata and truth are consistent", {
  expect_s3_class(sv$strata, "sf")
  expect_equal(sf::st_crs(sv$strata)$epsg, 4326L)
  expect_equal(sv$strata$stratum, c("SYN-A", "SYN-B", "SYN-C", "SYN-D"))
  bio <- sv$truth[sv$truth$quantity == "biomass", ]
  for (sp in unique(bio$species_code)) {
    b <- bio[bio$species_code == sp, ]
    expect_equal(sum(b$value[b$stratum != "total"]), b$value[b$stratum == "total"])
  }
  expect_true(all(sv$truth$value > 0))
})

test_that("truth for a constant field equals density times area", {
  des <- synth_design()
  des$species$sigma <- 0
  des$species$depth_sd <- 1e8
  const <- synth_survey(des, seed = 3)
  bio <- const$truth[const$truth$quantity == "biomass" & const$truth$stratum == "SYN-A", ]
  area <- 100 * 60
  expect_equal(bio$value, exp(des$species$log_peak) * area / 1000, tolerance = 1e-6)
  expect_equal(synth_density(const, "SYN002", 10, 10), exp(des$species$log_peak[2]),
               tolerance = 1e-6)
})

test_that("a stratified mean on a large synthetic survey recovers the truth", {
  des <- synth_design(max_measured = 1L, split_prob = 0)
  des$strata$n_stations <- rep(150L, 4)
  big <- synth_survey(des, seed = 4)
  catch <- big$survey$catch |>
    dplyr::filter(.data$catchcategory == "SYN001") |>
    dplyr::summarise(kg = sum(.data$catchweight), .by = "serialnumber")
  dens <- big$stations |>
    dplyr::left_join(catch, by = "serialnumber") |>
    dplyr::mutate(kg = dplyr::coalesce(.data$kg, 0), d = .data$kg / .data$swept_area_km2)
  est <- dens |>
    dplyr::summarise(m = mean(.data$d), v = stats::var(.data$d) / dplyr::n(), .by = "stratum") |>
    dplyr::mutate(area = 6000)
  total_t <- sum(est$m * est$area) / 1000
  se_t <- sqrt(sum(est$v * est$area^2)) / 1000
  truth <- big$truth$value[big$truth$species_code == "SYN001" & big$truth$stratum == "total" &
                             big$truth$quantity == "biomass"]
  expect_lt(abs(total_t - truth), 3 * se_t)
})

test_that("the delta-gamma observation model also gives a valid survey", {
  dg <- synth_survey(synth_design(family = "delta_gamma"), seed = 5)
  expect_false(any(validate_survey(dg$survey)$status == "fail"))
})

test_that("write_biotic() refuses anything not flagged synthetic", {
  f <- withr::local_tempfile(fileext = ".xml")
  write_biotic(sv, f)
  unflagged <- read_biotic(f)
  cnd <- expect_error(write_biotic(unflagged, f), class = "nansenbiomass_error")
  expect_equal(cnd$nb_code, "SY-WRITE-01")
  expect_error(write_biotic(list(), f), class = "nansenbiomass_error")
  expect_match(readLines(f, n = 2)[2], "SYNTHETIC SURVEY")
})

test_that("excluded stations are added without changing the design or the truth", {
  plain <- synth_survey(seed = 1)
  ex <- synth_survey(synth_design(excluded = c(pelagic = 4, aborted = 3, zero_distance = 2)),
                     seed = 1)
  expect_identical(ex$truth, plain$truth)
  st <- ex$stations
  expect_equal(sum(st$design_station), 45L)
  expect_equal(sum(!st$design_station), 9L)
  expect_equal(sum(st$stationtype == "11" & st$samplequality == "14"), 4L)
  expect_equal(sum(st$samplequality == "5" & st$gearcondition == "9"), 3L)
  expect_equal(sum(ex$survey$station$distance == 0), 2L)
  # Synthetic exports count only the design stations
  sup <- synth_export(ex)$support
  expect_equal(sum(sup$n_stations[sup$species_code == "SYN001"]), 45L)
  expect_error(synth_design(excluded = c(pelagic = -1)), class = "nansenbiomass_error")
})

# Survey configurations: reading, validation and hashing. Synthetic only.

example <- system.file("configs", "synthetic-example.yml", package = "nansenbiomass")
if (!nzchar(example)) example <- test_path("../../inst/configs/synthetic-example.yml")
sentinel <- "SENTINEL_7f3a9c"

raw <- function() yaml::read_yaml(example)
code_of <- function(expr) expect_error(expr, class = "nansenbiomass_error")$nb_code

test_that("the synthetic example reads, validates and is hashed", {
  cfg <- read_config(example)
  expect_s3_class(cfg, "nb_config")
  expect_equal(cfg$survey$year, 2000L)
  expect_equal(cfg$inclusion$samplequality, "12")
  expect_equal(cfg$inclusion$gearcondition, c("1", "2"))
  expect_true(cfg$inclusion$positive_distance)
  expect_equal(cfg$bootstrap, list(replicates = 100L, seed = 1L))
  expect_equal(cfg$disclosure, list(min_stations = 5L, min_positive = 3L))
  expect_equal(attr(cfg, "config_hash"), unname(tools::md5sum(example)))
  expect_match(config_hash(example), "^[0-9a-f]{32}$")
})

test_that("the hash changes when the file changes", {
  f <- withr::local_tempfile(fileext = ".yml")
  file.copy(example, f)
  h1 <- config_hash(f)
  cat("# edited\n", file = f, append = TRUE)
  expect_false(identical(config_hash(f), h1))
})

test_that("codes are kept as text and defaults are filled in", {
  x <- raw()
  x$inclusion <- list(samplequality = c(12, 1))
  x$quantities <- NULL
  x$stox <- NULL
  x$disclosure <- NULL
  cfg <- validate_config(x)
  expect_equal(cfg$inclusion$samplequality, c("12", "1"))
  expect_null(cfg$inclusion$stationtype)
  expect_true(cfg$inclusion$positive_distance)
  expect_equal(cfg$quantities, "biomass")
  expect_equal(cfg$stox$version, stox_pinned_version)
  expect_equal(cfg$disclosure$min_stations, 5L)
})

test_that("invalid configurations fail by code, naming fields but not values", {
  x <- raw(); x$bootstrap <- NULL
  expect_equal(code_of(validate_config(x)), "CF-REQ-01")
  x <- raw(); x$data$biotic <- paste0("/data/", sentinel, ".xml")
  cnd <- expect_error(validate_config(x), class = "nansenbiomass_error")
  expect_equal(cnd$nb_code, "CF-PATH-02")
  expect_match(conditionMessage(cnd), "data.biotic")
  expect_false(grepl(sentinel, conditionMessage(cnd)))
  x <- raw(); x$data$strata <- "../outside/strata.geojson"
  expect_equal(code_of(validate_config(x)), "CF-PATH-02")
  x <- raw(); x$data$stratum_names <- c("SYN-A", "total")
  expect_equal(code_of(validate_config(x)), "CF-TYPE-02")
  x <- raw(); x$swept_width$method <- "wingspread"
  expect_equal(code_of(validate_config(x)), "CF-VAL-01")
  x <- raw(); x$swept_width$fixed_m <- 0
  expect_equal(code_of(validate_config(x)), "CF-VAL-02")
  x <- raw(); x$quantities <- "density"
  expect_equal(code_of(validate_config(x)), "CF-VAL-01")
  x <- raw(); x$bootstrap$replicates <- 0
  expect_equal(code_of(validate_config(x)), "CF-VAL-02")
  x <- raw(); x$inclusion$distance_recovery <- "guess"
  expect_equal(code_of(validate_config(x)), "CF-VAL-01")
  x <- raw(); x$stox$version <- "latest"
  expect_equal(code_of(validate_config(x)), "CF-VAL-01")
})

test_that("the D-03 minimums can be raised but not lowered", {
  x <- raw(); x$disclosure$min_stations <- 4
  expect_equal(code_of(validate_config(x)), "CF-VAL-03")
  x <- raw(); x$disclosure$min_positive <- 2
  expect_equal(code_of(validate_config(x)), "CF-VAL-03")
  x <- raw(); x$disclosure$min_stations <- 8
  expect_equal(validate_config(x)$disclosure$min_stations, 8L)
})

test_that("missing or unreadable files fail by code", {
  expect_equal(code_of(read_config(file.path(tempdir(), "none.yml"))), "CF-READ-01")
  f <- withr::local_tempfile(fileext = ".yml")
  writeLines(c("survey: [unclosed", sentinel), f)
  cnd <- expect_error(read_config(f), class = "nansenbiomass_error")
  expect_equal(cnd$nb_code, "CF-READ-02")
  expect_false(grepl(sentinel, conditionMessage(cnd)))
})

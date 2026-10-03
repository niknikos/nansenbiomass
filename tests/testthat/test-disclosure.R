# disclosure: acceptance check 2 of M1. Each deliberately disclosive synthetic
# export must be rejected with its expected code (docs/spec.md, Section 11).

sv <- synth_survey(seed = 1)
strata <- sv$design$strata$stratum
cand <- synth_export(sv)
clean <- cand$export[cand$export$species_code == "SYN001", ]
support <- cand$support
sentinel <- "SENTINEL_7f3a9c"

failed <- function(report) report$check_id[report$status == "fail"]

test_that("a clean synthetic export passes every check", {
  r <- check_disclosure(clean, support, strata)
  expect_s3_class(r, "nb_disclosure_report")
  expect_equal(attr(r, "outcome"), "pass")
  expect_equal(nrow(r), 12L)
  expect_equal(attr(r, "min_stations"), 5L)
  expect_equal(attr(r, "min_positive"), 3L)
})

test_that("coordinates and station identifiers are rejected", {
  bad <- clean
  bad$latitudestart <- -30.1234
  bad$serialnumber <- 90001L
  expect_true(all(c("DC-FLD-01", "DC-FLD-04") %in% failed(check_disclosure(bad, support, strata))))
})

test_that("record-level rows are rejected", {
  per_station <- clean[rep(1, 12), ]
  expect_true("DC-AGG-01" %in% failed(check_disclosure(per_station, support, strata)))
  per_station$stratum <- paste0("ST", 1:12)
  expect_true("DC-VAL-01" %in% failed(check_disclosure(per_station, support, strata)))
})

test_that("cells with too few stations or no support are rejected", {
  few <- support
  few$n_stations[few$stratum == "SYN-D"] <- 3L
  few$n_positive[few$stratum == "SYN-D"] <- pmin(few$n_positive[few$stratum == "SYN-D"], 3L)
  expect_true("DC-AGG-02" %in% failed(check_disclosure(clean, few, strata)))
  no_support <- support[support$stratum != "SYN-C", ]
  expect_true("DC-AGG-02" %in% failed(check_disclosure(clean, no_support, strata)))
  expect_true("DC-AGG-02" %in% failed(check_disclosure(clean, support, strata,
                                                       min_stations = 13)))
})

test_that("a withheld stratum recoverable from the total is rejected (differencing)", {
  one_out <- clean[clean$stratum != "SYN-D", ]
  expect_true("DC-AGG-03" %in% failed(check_disclosure(one_out, support, strata)))
  two_out <- clean[!clean$stratum %in% c("SYN-C", "SYN-D"), ]
  expect_equal(attr(check_disclosure(two_out, support, strata), "outcome"), "pass")
  no_total <- clean[clean$stratum != "total" & clean$stratum != "SYN-D", ]
  expect_equal(attr(check_disclosure(no_total, support, strata), "outcome"), "pass")
})

test_that("cells resting on too few positive stations are rejected", {
  sparse <- support
  sparse$n_positive[sparse$species_code == "SYN001" & sparse$stratum == "SYN-C"] <- 2L
  expect_true("DC-AGG-04" %in% failed(check_disclosure(clean, sparse, strata)))
  none <- support
  none$n_positive[none$species_code == "SYN001" & none$stratum == "SYN-C"] <- 0L
  expect_equal(attr(check_disclosure(clean, none, strata), "outcome"), "pass")
})

test_that("spatial objects and list columns are rejected", {
  pts <- sf::st_as_sf(cbind(clean, lon = -120, lat = -30), coords = c("lon", "lat"), crs = 4326)
  f <- failed(check_disclosure(pts, support, strata))
  expect_true(all(c("DC-FLD-01", "DC-FLD-03", "DC-FLD-04") %in% f))
})

test_that("text resembling coordinates is rejected", {
  bad <- clean
  bad$survey_id[1] <- "30.1234S"
  expect_true("DC-VAL-02" %in% failed(check_disclosure(bad, support, strata)))
})

test_that("wrong types and missing required values are rejected", {
  bad <- clean
  bad$value <- as.character(bad$value)
  expect_true("DC-TYP-01" %in% failed(check_disclosure(bad, support, strata)))
  bad <- clean
  bad$config_hash[1] <- NA
  expect_true("DC-TYP-02" %in% failed(check_disclosure(bad, support, strata)))
  expect_true("DC-FLD-02" %in% failed(check_disclosure(clean[-1], support, strata)))
})

test_that("the D-03 minimums can be raised but not lowered", {
  code_of <- function(expr) expect_error(expr, class = "nansenbiomass_error")$nb_code
  expect_equal(code_of(check_disclosure(clean, support, strata, min_stations = 4)), "DC-ARG-01")
  expect_equal(code_of(check_disclosure(clean, support, strata, min_positive = 2)), "DC-ARG-02")
  expect_equal(code_of(check_disclosure(clean, support[1:3], strata)), "DC-ARG-03")
  expect_equal(code_of(check_disclosure(clean, support, c(strata, "total"))), "DC-ARG-07")
})

test_that("reports name fields and counts, never values", {
  bad <- clean
  bad$species_code[1] <- sentinel
  bad$note <- sentinel
  r <- check_disclosure(bad, support, strata)
  printed <- paste(utils::capture.output(print(r)), collapse = "\n")
  expect_false(grepl(sentinel, printed))
  expect_match(printed, "note")
})

test_that("column names that could carry identifiers are withheld from the report", {
  wide <- clean
  wide[["stn_90001"]] <- 1
  wide[["-30.12"]] <- 1
  r <- check_disclosure(wide, support, strata)
  printed <- paste(utils::capture.output(print(r)), r$fields, collapse = "\n")
  expect_true("DC-FLD-01" %in% failed(r))
  expect_false(grepl("90001", printed))
  expect_false(grepl("-30.12", printed, fixed = TRUE))
  expect_match(r$fields[r$check_id == "DC-FLD-01"], "<name withheld>", fixed = TRUE)
})

test_that("stage_export writes a passing export with the outcome recorded", {
  staging <- withr::local_tempdir()
  st <- stage_export(clean, support, strata, "synthetic-run", staging_dir = staging)
  expect_equal(st$outcome, "pass")
  out <- utils::read.csv(file.path(staging, "synthetic-run", "estimates.csv"))
  expect_equal(names(out), estimate_schema()$field)
  expect_true(all(out$disclosure == "pass:rules-v1:min5:pos3"))
  expect_true(file.exists(file.path(staging, "synthetic-run", "disclosure-report.txt")))
})

test_that("a failing export is not staged, and replaces an earlier pass", {
  staging <- withr::local_tempdir()
  stage_export(clean, support, strata, "run", staging_dir = staging)
  bad <- clean
  bad$latitudestart <- -30.1
  st <- stage_export(bad, support, strata, "run", staging_dir = staging)
  expect_equal(st$outcome, "fail")
  expect_false(file.exists(file.path(staging, "run", "estimates.csv")))
  expect_true(file.exists(file.path(staging, "run", "disclosure-report.csv")))
})

test_that("staging refuses outbox folders, unsafe labels and non-temporary cloud paths", {
  code_of <- function(expr) expect_error(expr, class = "nansenbiomass_error")$nb_code
  base <- withr::local_tempdir()
  expect_equal(code_of(stage_export(clean, support, strata, "run",
                                    staging_dir = file.path(base, "outbox", "x"))), "DC-STAGE-01")
  expect_equal(code_of(stage_export(clean, support, strata, "../run", staging_dir = base)),
               "DC-STAGE-02")
  withr::local_envvar(CLAUDE_CODE_REMOTE = "true")
  outside <- file.path(R.home(), "nansenbiomass-staging-test")
  expect_equal(code_of(stage_export(clean, support, strata, "run", staging_dir = outside)),
               "IO-CLOUD-01")
  expect_false(dir.exists(outside))
})

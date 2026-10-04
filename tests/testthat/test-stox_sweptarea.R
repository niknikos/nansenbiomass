# stox_sweptarea on synthetic projects only. The project files below are
# artificial: they imitate the layout of StoX projects, with planted identifiers,
# a planted polygon and a planted path that must never appear in any output.

sentinel <- "SENTINEL_7f3a9c"

xml_project <- c(
  '<?xml version="1.0" encoding="UTF-8"?>',
  '<project xmlns="http://www.imr.no/formats/stox/v1" template="UserDefined" stoxversion="2.7">',
  '  <model name="baseline">',
  '    <process name="ReadBioticXML">',
  '      <function>ReadBioticXML</function>',
  paste0('      <parameter name="FileName1">input/biotic/', sentinel, '.xml</parameter>'),
  '    </process>',
  '    <process name="FilterBiotic">',
  '      <function>FilterBiotic</function>',
  paste0('      <parameter name="FishStationExpr">samplequality == 12 and serialno != \'', sentinel, '\'</parameter>'),
  '      <parameter name="CatchExpr">species not in [90001,90002]</parameter>',
  '    </process>',
  '    <process name="SweptAreaDensity">',
  '      <function>SweptAreaDensity</function>',
  '      <parameter name="SweepWidthMethod">Constant</parameter>',
  '      <parameter name="SweepWidth">25</parameter>',
  '      <parameter name="Note">a free-text note mentioning a cruise</parameter>',
  '    </process>',
  '  </model>',
  '  <model name="r">',
  '    <process name="runBootBiotic">',
  '      <function>runBootstrap</function>',
  '      <parameter name="nboots">500</parameter>',
  '      <parameter name="seed">1234</parameter>',
  '    </process>',
  '  </model>',
  '  <processdata>',
  '    <bioticassignment>',
  paste0('      <stationweight assignmentid="1" station="', sentinel, '/1">1.0</stationweight>'),
  '      <stationweight assignmentid="1" station="SYN/2">1.0</stationweight>',
  '    </bioticassignment>',
  '    <stratumpolygon>',
  paste0('      <value polygonkey="SYN-A" polygonvariable="polygon">MULTIPOLYGON (((', sentinel, ' 1, 2 2, 3 3)))</value>'),
  '    </stratumpolygon>',
  '  </processdata>',
  '</project>'
)

json_project <- list(project = list(
  RstoxPackageVersion = list("RstoxFramework_4.2.1", "RstoxBase_2.2.1", "RstoxData_2.2.1"),
  models = list(
    baseline = list(
      list(processName = "ReadBiotic", functionName = "RstoxData::ReadBiotic",
           functionInputs = setNames(list(), character(0)),
           functionParameters = list(FileNames = paste0("input/biotic/", sentinel, ".xml")),
           processParameters = list(enabled = TRUE),
           processData = list()),
      list(processName = "FilterStoxBiotic", functionName = "RstoxData::FilterStoxBiotic",
           functionInputs = list(StoxBioticData = "StoxBiotic"),
           functionParameters = list(FilterExpression = list(
             Haul = paste0("HaulQuality == 12 & HaulKey != '", sentinel, "'")
           )),
           processParameters = list(enabled = TRUE),
           processData = list()),
      list(processName = "DefineBioticPSU", functionName = "RstoxBase::DefineBioticPSU",
           functionInputs = setNames(list(), character(0)),
           functionParameters = list(DefinitionMethod = "Stratum"),
           processParameters = list(enabled = TRUE),
           processData = list(BioticPSU = list(
             list(Stratum = "SYN-A", PSU = "PSU1", Haul = sentinel),
             list(Stratum = "SYN-A", PSU = "PSU2", Haul = "SYN-2")
           )))
    ),
    analysis = list(
      list(processName = "Bootstrap", functionName = "RstoxFramework::Bootstrap",
           functionInputs = setNames(list(), character(0)),
           functionParameters = list(NumberOfBootstraps = 200, BootstrapMethodTable = list(
             list(ProcessName = "LengthDistribution", ResampleFunction = "ResampleHauls", Seed = 1)
           )),
           processParameters = list(enabled = TRUE),
           processData = list())
    )
  )
))

write_project <- function(root, format = c("xml", "json")) {
  format <- match.arg(format)
  dir.create(file.path(root, "official", "process"), recursive = TRUE)
  if (format == "xml") {
    writeLines(xml_project, file.path(root, "official", "process", "project.xml"))
  } else {
    jsonlite::write_json(json_project, file.path(root, "official", "process", "project.json"),
                         auto_unbox = TRUE, pretty = TRUE)
  }
}

all_text <- function(d) {
  paste(c(utils::capture.output(print(d)), unlist(d$processes), unlist(d$process_data),
          d$versions), collapse = "\n")
}

test_that("a StoX 2.7 project.xml is described without its data", {
  root <- withr::local_tempdir()
  write_project(root, "xml")
  d <- describe_stox_project("official", root = root)
  expect_s3_class(d, "nb_stox_description")
  expect_equal(d$format, "StoX 2.7 (project.xml)")
  p <- d$processes
  expect_setequal(unique(p$model), c("baseline", "r"))
  expect_equal(p$value[p$parameter == "SweepWidthMethod"], "Constant")
  expect_equal(p$value[p$parameter == "SweepWidth"], "25")
  expect_equal(p$value[p$parameter == "nboots"], "500")
  expect_equal(p$value[p$parameter == "seed"], "1234")
  expect_equal(p$value[p$parameter == "FileName1"], "<withheld: path>")
  expect_equal(p$value[p$parameter == "Note"], "<withheld: text>")
  fs <- p[p$parameter == "FishStationExpr", ]
  expect_equal(fs$value, "<withheld: expression>")
  expect_equal(fs$fields, "samplequality, serialno")
  expect_equal(p$fields[p$parameter == "CatchExpr"], "species")
  expect_setequal(d$process_data$element, c("bioticassignment", "stratumpolygon"))
  expect_equal(d$process_data$n_entries[d$process_data$element == "bioticassignment"], 2L)
  expect_false(grepl(sentinel, all_text(d)))
  expect_false(grepl("90001|MULTIPOLYGON", all_text(d)))
})

test_that("a StoX 3 or later project.json is described without its data", {
  root <- withr::local_tempdir()
  write_project(root, "json")
  d <- describe_stox_project("official", root = root)
  expect_equal(d$format, "StoX >= 3 (project.json)")
  expect_true("RstoxFramework_4.2.1" %in% d$versions)
  p <- d$processes
  expect_equal(p$value[p$parameter == "FileNames"], "<withheld: path>")
  hf <- p[p$parameter == "FilterExpression$Haul", ]
  expect_equal(hf$value, "<withheld: expression>")
  expect_equal(hf$fields, "HaulQuality, HaulKey")
  expect_equal(p$value[p$parameter == "DefinitionMethod"], "Stratum")
  expect_equal(p$value[p$parameter == "NumberOfBootstraps"], "200")
  expect_equal(p$value[p$parameter == "BootstrapMethodTable[1]$ResampleFunction"], "ResampleHauls")
  expect_equal(p$value[p$parameter == "input:StoxBioticData"], "StoxBiotic")
  expect_equal(d$process_data$element, "DefineBioticPSU")
  expect_equal(d$process_data$n_entries, 2L)
  expect_false(grepl(sentinel, all_text(d)))
})

test_that("a project file can be named directly, and wrong targets fail by code", {
  root <- withr::local_tempdir()
  write_project(root, "xml")
  d <- describe_stox_project("official/process/project.xml", root = root)
  expect_equal(d$format, "StoX 2.7 (project.xml)")
  dir.create(file.path(root, "flat"))
  writeLines(xml_project, file.path(root, "flat", "project.xml"))
  expect_equal(describe_stox_project("flat", root = root)$format, "StoX 2.7 (project.xml)")
  dir.create(file.path(root, "empty"))
  code_of <- function(expr) expect_error(expr, class = "nansenbiomass_error")$nb_code
  expect_equal(code_of(describe_stox_project("empty", root = root)), "SX-READ-02")
  writeLines("<notaproject/>", file.path(root, "other.xml"))
  expect_equal(code_of(describe_stox_project("other.xml", root = root)), "SX-READ-03")
  writeLines(paste0("<project><broken ", sentinel), file.path(root, "broken.xml"))
  cnd <- expect_error(describe_stox_project("broken.xml", root = root),
                      class = "nansenbiomass_error")
  expect_equal(cnd$nb_code, "SX-READ-01")
  expect_false(grepl(sentinel, conditionMessage(cnd)))
})

test_that("settings are classified conservatively", {
  expect_equal(classify_stox_value("Method", "LengthDistributed")$status, "shown")
  expect_equal(classify_stox_value("Seed", 1234)$value, "1234")
  expect_equal(classify_stox_value("Label", "ST2019104")$status, "withheld")
  expect_equal(classify_stox_value("FileName", "C:\\data\\x.txt")$value, "<withheld: path>")
  expect_equal(expression_fields("a == 'X123' & b %in% c(1, 2) | is.na(c3)"), c("a", "b", "c3"))
})

# ---- Inclusion rules ----------------------------------------------------------

example_config <- function() {
  f <- system.file("configs", "synthetic-example.yml", package = "nansenbiomass")
  if (!nzchar(f)) f <- test_path("../../inst/configs/synthetic-example.yml")
  f
}

synthetic_root <- function(excluded = c(pelagic = 4, aborted = 3, zero_distance = 2),
                           envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = envir)
  dir.create(file.path(root, "surveys"))
  dir.create(file.path(root, "strata"))
  sv <- synth_survey(synth_design(excluded = excluded), seed = 1)
  write_biotic(sv, file.path(root, "surveys", "synthetic-seed1.xml"))
  strata <- sv$strata
  names(strata)[names(strata) == "stratum"] <- "StratumName"
  sf::st_write(strata, file.path(root, "strata", "synthetic-strata.geojson"), quiet = TRUE)
  list(root = root, sv = sv)
}

test_that("inclusion_summary() excludes exactly the planted stations", {
  s <- synthetic_root()
  inc <- inclusion_summary(example_config(), root = s$root)
  expect_s3_class(inc, "nb_inclusion")
  r <- inc$rules
  expect_equal(r$n_before[1], 54L)
  expect_equal(r$n_excluded[r$rule == "stationtype in {12}"], 4L)
  expect_equal(r$n_excluded[r$rule == "samplequality in {12}"], 3L)
  expect_equal(r$n_excluded[r$rule == "gearcondition in {1, 2}"], 0L)
  expect_equal(r$n_excluded[r$rule == "distance > 0 (recovery: none)"], 2L)
  expect_equal(inc$distance$n_zero_or_missing, 2L)
  expect_equal(inc$distance$n_recoverable_from_log, 2L)
  expect_equal(inc$distance$n_recovered, 0L)
  expect_match(paste(utils::capture.output(print(inc)), collapse = "\n"),
               "Flag: 2 station\\(s\\) with zero or missing distance")
  expect_equal(r$n_excluded[r$rule == "start position inside the strata"], 0L)
  expect_equal(r$n_after[nrow(r)], 45L)
  design <- s$sv$design$strata
  expect_equal(inc$by_stratum$n_kept[match(design$stratum, inc$by_stratum$stratum)],
               design$n_stations)
})

test_that("rules left out of the configuration exclude nothing", {
  s <- synthetic_root(excluded = c(pelagic = 2))
  cfg <- read_config(example_config())
  cfg$inclusion$stationtype <- NULL
  cfg$inclusion$samplequality <- NULL
  inc <- inclusion_summary(cfg, root = s$root)
  expect_false(any(grepl("stationtype|samplequality", inc$rules$rule)))
  expect_equal(inc$rules$n_after[nrow(inc$rules)], 47L)
})

test_that("stations outside the strata are counted, and polygons need the label", {
  s <- synthetic_root(excluded = c(pelagic = 0))
  cfg <- read_config(example_config())
  cfg$data$stratum_names <- c("SYN-A", "SYN-B", "SYN-C", "SYN-D")
  polys <- sf::st_read(file.path(s$root, "strata", "synthetic-strata.geojson"), quiet = TRUE)
  sf::st_write(polys[polys$StratumName != "SYN-D", ],
               file.path(s$root, "strata", "three.geojson"), quiet = TRUE)
  cfg$data$strata <- "strata/three.geojson"
  inc <- inclusion_summary(cfg, root = s$root)
  expect_equal(inc$rules$n_excluded[inc$rules$rule == "start position inside the strata"], 8L)
  expect_equal(inc$by_stratum$n_kept[inc$by_stratum$stratum == "SYN-D"], 0L)
  cfg$data$stratum_label <- "Name"
  cnd <- expect_error(inclusion_summary(cfg, root = s$root), class = "nansenbiomass_error")
  expect_equal(cnd$nb_code, "SX-STRATA-02")
})

test_that("zero distances are recovered from the log, or from positions, when configured", {
  s <- synthetic_root(excluded = c(zero_distance = 3))
  cfg <- read_config(example_config())
  cfg$inclusion$distance_recovery <- "log"
  inc <- inclusion_summary(cfg, root = s$root)
  expect_equal(inc$distance$n_recovered, 3L)
  expect_equal(inc$rules$n_excluded[grepl("^distance", inc$rules$rule)], 0L)
  expect_equal(inc$rules$n_after[nrow(inc$rules)], 48L)

  # Without usable logs, only positions can recover the distance
  xml <- file.path(s$root, "surveys", "synthetic-seed1.xml")
  x <- readLines(xml)
  x <- x[!grepl("<logstop>", x)]
  writeLines(x, xml)
  inc <- inclusion_summary(cfg, root = s$root)
  expect_equal(inc$distance$n_recoverable_from_log, 0L)
  expect_equal(inc$distance$n_recoverable_from_positions_only, 3L)
  expect_equal(inc$distance$n_recovered, 0L)
  cfg$inclusion$distance_recovery <- "log_or_positions"
  expect_equal(inclusion_summary(cfg, root = s$root)$distance$n_recovered, 3L)
})

test_that("position distances are great-circle distances in nautical miles", {
  # One minute of latitude is one nautical mile
  expect_equal(position_distance_nmi(-30, -120, -30 + 1 / 60, -120), 1, tolerance = 1e-3)
})

test_that("strata can be read from a StoX 2.7 project.xml, with includeintotal", {
  skip_if_not_installed("RstoxBase")
  s <- synthetic_root(excluded = c(pelagic = 0))
  wkt <- sf::st_as_text(sf::st_geometry(s$sv$strata))
  include <- c("true", "true", "true", "false")
  values <- unlist(lapply(seq_along(wkt), function(i) c(
    sprintf('      <value polygonkey="%s" polygonvariable="includeintotal">%s</value>',
            s$sv$strata$stratum[i], include[i]),
    sprintf('      <value polygonkey="%s" polygonvariable="polygon">%s</value>',
            s$sv$strata$stratum[i], wkt[i])
  )))
  dir.create(file.path(s$root, "stox_official", "synthetic", "process"), recursive = TRUE)
  writeLines(c('<?xml version="1.0" encoding="UTF-8"?>',
               '<project xmlns="http://www.imr.no/formats/stox/v1">',
               '  <processdata>', '    <stratumpolygon>', values, '    </stratumpolygon>',
               '  </processdata>', '</project>'),
             file.path(s$root, "stox_official", "synthetic", "process", "project.xml"))
  cfg <- read_config(example_config())
  cfg$data$strata <- "stox_official/synthetic/process/project.xml"
  inc <- inclusion_summary(cfg, root = s$root)
  expect_equal(inc$rules$n_after[nrow(inc$rules)], 45L)
  expect_equal(inc$by_stratum$include_in_total, c(TRUE, TRUE, TRUE, FALSE))
})

# ---- run_estimate(): template, project, estimate, staging ----------------------

skip_if_no_stox <- function() {
  skip_if_not_installed("RstoxFramework")
  skip_if_not_installed("RstoxBase")
  skip_if_not_installed("RstoxData")
  skip_if_not_installed("data.table")
}

quick_config <- function(replicates = 5L) {
  cfg <- read_config(example_config())
  cfg$bootstrap$replicates <- as.integer(replicates)
  cfg
}

# The design-based estimate computed directly in R from the synthetic tables:
# catch weight per swept area at each station, stratum means, times stratum area.
direct_biomass <- function(sv, species, width_m = 20) {
  st <- sv$stations[sv$stations$design_station, ]
  ca <- sv$survey$catch[sv$survey$catch$catchcategory == species, ]
  kg <- tapply(ca$catchweight, ca$serialnumber, sum)
  st$kg <- ifelse(as.character(st$serialnumber) %in% names(kg),
                  kg[as.character(st$serialnumber)], 0)
  st$dens <- st$kg / (st$distance * 1.852 * width_m / 1000)
  area <- with(sv$design$strata, (xmax - xmin) * (ymax - ymin))
  names(area) <- sv$design$strata$stratum
  by <- tapply(st$dens, st$stratum, mean)
  by * area[names(by)] / 1000
}

test_that("the template fills completely and refuses an unfilled placeholder", {
  tpl <- read_stox_template()
  expect_equal(tpl$template, "sweptarea")
  expect_match(tpl$template_version, "^[0-9]+\\.[0-9]+\\.[0-9]+$")
  expect_equal(fill_template(list(a = "{{x}}", b = list(c = 1)), list(x = 1:2)),
               list(a = 1:2, b = list(c = 1)))
  expect_error(fill_template(list(a = "{{missing}}"), list(x = 1)), "SX-TPL-03")
  expect_error(fill_template(list(a = "text {{x}}"), list(x = 1)), "SX-TPL-04")
  # every placeholder the template uses is one the builder supplies
  used <- unique(unlist(regmatches(
    jsonlite::toJSON(tpl$processes, auto_unbox = TRUE),
    gregexpr("\\{\\{[a-z_]+\\}\\}", jsonlite::toJSON(tpl$processes, auto_unbox = TRUE))
  )))
  expect_setequal(
    gsub("[{}]", "", used),
    c("biotic_files", "translation_table", "stoxbiotic_process", "filter_expression",
      "strata_file", "stratum_label", "raising_factor_priority", "sweep_width_m", "cores",
      "bootstrap_method_table", "replicates", "output_processes", "survey_method",
      "survey_table")
  )
})

test_that("run_estimate() reproduces the direct estimate and stages a passing export", {
  skip_if_no_stox()
  s <- synthetic_root()
  res <- suppressWarnings(run_estimate(quick_config(), root = s$root,
                                       staging_dir = file.path(s$root, "staging")))
  est <- res$estimates
  expect_equal(res$staged$outcome, "pass")
  expect_true(file.exists(file.path(s$root, "staging", basename(dirname(res$staged$files[1])),
                                    "estimates.csv")))
  expect_false(any(grepl("outbox", list.dirs(s$root), ignore.case = TRUE)))
  expect_true(dir.exists(res$project_path))

  # Section 9 fields and values
  expect_setequal(names(est), estimate_schema()$field)
  expect_true(all(est$method == "stox_sweptarea"))
  expect_setequal(unique(est$quantity), c("biomass", "abundance"))
  expect_equal(unique(est$unit[est$quantity == "biomass"]), "tonnes")
  expect_equal(unique(est$unit[est$quantity == "abundance"]), "millions")
  expect_equal(unique(est$ci_type), "bootstrap_percentile_95")
  expect_match(unique(est$config_hash), "^[0-9a-f]{32}$")
  expect_equal(unique(est$config_hash), config_hash(example_config()))
  expect_match(unique(est$code_version), "RstoxFramework 4\\.2\\.1; RstoxBase 2\\.2\\.1; RstoxData 2\\.2\\.1")
  expect_match(unique(est$code_version), "template sweptarea [0-9.]+$")
  expect_true(all(est$value > 0))

  # The baseline estimate equals the direct design-based estimate, which uses
  # the same stations: the planted pelagic, aborted and zero-distance stations
  # are therefore excluded.
  for (sp in sv_species <- s$sv$design$species$species_code) {
    direct <- direct_biomass(s$sv, sp)
    got <- est[est$species_code == sp & est$quantity == "biomass" & est$stratum != "total", ]
    # StoX reports no row for a stratum where the species was not caught
    expect_equal(got$value, as.numeric(direct[got$stratum]), tolerance = 1e-6)
    expect_true(all(direct[setdiff(names(direct), got$stratum)] == 0))
    tot <- est$value[est$species_code == sp & est$quantity == "biomass" & est$stratum == "total"]
    expect_equal(tot, sum(direct), tolerance = 1e-6)
  }

  # Support table: the 45 design stations (of 54 in the file) were used
  sup <- res$support
  expect_equal(sum(sup$n_stations[sup$species_code == "SYN001"]), 45L)
  expect_equal(sum(sup$n_stations[sup$species_code == "SYN001"]),
               inclusion_summary(example_config(), root = s$root)$rules$n_after[4])
})

test_that("recovered distances and includeintotal reach the StoX project", {
  skip_if_no_stox()
  s <- synthetic_root(excluded = c(zero_distance = 2))
  # (1) two stations with a zero distance are used when positions can recover it
  cfg <- quick_config(2L)
  cfg$inclusion$distance_recovery <- "log_or_positions"
  res <- suppressWarnings(run_estimate(cfg, root = s$root,
                                       staging_dir = file.path(s$root, "staging")))
  expect_equal(sum(res$support$n_stations[res$support$species_code == "SYN001"]), 47L)
  expect_equal(res$staged$outcome, "pass")

  # (2) a stratum flagged includeintotal = false is left out of the total only
  wkt <- sf::st_as_text(sf::st_geometry(s$sv$strata))
  include <- c("true", "true", "true", "false")
  values <- unlist(lapply(seq_along(wkt), function(i) c(
    sprintf('      <value polygonkey="%s" polygonvariable="includeintotal">%s</value>',
            s$sv$strata$stratum[i], include[i]),
    sprintf('      <value polygonkey="%s" polygonvariable="polygon">%s</value>',
            s$sv$strata$stratum[i], wkt[i])
  )))
  dir.create(file.path(s$root, "stox_official", "synthetic", "process"), recursive = TRUE)
  writeLines(c('<?xml version="1.0" encoding="UTF-8"?>',
               '<project xmlns="http://www.imr.no/formats/stox/v1">',
               '  <processdata>', '    <stratumpolygon>', values, '    </stratumpolygon>',
               '  </processdata>', '</project>'),
             file.path(s$root, "stox_official", "synthetic", "process", "project.xml"))
  cfg <- quick_config(2L)
  cfg$data$strata <- "stox_official/synthetic/process/project.xml"
  res <- suppressWarnings(run_estimate(cfg, root = s$root,
                                       staging_dir = file.path(s$root, "staging2")))
  b <- res$estimates[res$estimates$species_code == "SYN001" & res$estimates$quantity == "biomass", ]
  by <- setNames(b$value, b$stratum)
  expect_equal(by[["total"]], sum(by[c("SYN-A", "SYN-B", "SYN-C")]), tolerance = 1e-6)
  expect_lt(by[["total"]], sum(by[c("SYN-A", "SYN-B", "SYN-C", "SYN-D")]))
})

test_that("run_estimate() refuses what it cannot do, with sanitised errors", {
  s <- synthetic_root()
  cfg <- quick_config()
  door <- cfg
  door$swept_width$method <- "trawldoorspread"
  expect_error(run_estimate(door, root = s$root), "SX-CFG-01")

  skip_if_no_stox()
  old <- cfg
  old$stox$version <- "9.9.9"
  expect_error(run_estimate(old, root = s$root), "SX-STOX-02")

  # the same file twice gives non-unique serial numbers: refused, not guessed
  dup <- cfg
  dup$data$biotic <- rep(dup$data$biotic, 2)
  err <- tryCatch(run_estimate(dup, root = s$root), error = function(e) e)
  expect_s3_class(err, "nansenbiomass_error")
  expect_match(conditionMessage(err), "SX-KEY-01")
  expect_false(grepl("[0-9]{5}", conditionMessage(err)))
})

test_that("bootstrap cores default to the machine's minus 2, and are configurable", {
  cfg <- read_config(example_config())
  expected <- max(1L, min(parallel::detectCores() - 2L, 50L))
  expect_equal(bootstrap_cores(cfg), expected)
  cfg$bootstrap$cores <- 3L
  expect_equal(bootstrap_cores(cfg), 3L)
  cfg$bootstrap$replicates <- 2L
  expect_equal(bootstrap_cores(cfg), 2L) # never more cores than replicates
  x <- yaml::read_yaml(example_config())
  x$bootstrap$cores <- 0
  expect_error(validate_config(x), "CF-VAL-02")
  x$bootstrap$cores <- NULL
  x$catch$raising_factor_priority <- "Mass"
  expect_error(validate_config(x), "CF-VAL-01")
})

test_that("the bootstrap does not depend on the number of cores", {
  skip_if_no_stox()
  skip_if(parallel::detectCores() < 2, "needs two cores")
  s <- synthetic_root(excluded = c(pelagic = 1))
  run <- function(cores, dir) {
    cfg <- quick_config(4L)
    cfg$bootstrap$cores <- as.integer(cores)
    suppressWarnings(run_estimate(cfg, root = s$root, staging_dir = file.path(s$root, dir)))$estimates
  }
  one <- run(1, "st1")
  two <- run(2, "st2")
  cols <- c("species_code", "stratum", "quantity", "value", "cv", "ci_lower", "ci_upper")
  expect_equal(one[cols], two[cols])
})

# ---- stox_key_settings() --------------------------------------------------------

key_project <- function(sentinel) {
  fp <- function(...) list(...)
  proc <- function(name, fun, params = list(), inputs = setNames(list(), character(0)), data = list()) {
    list(processName = name, functionName = fun, functionInputs = inputs,
         functionParameters = params, processParameters = list(enabled = TRUE), processData = data)
  }
  list(project = list(
    RstoxPackageVersion = list("RstoxFramework_4.2.1"),
    models = list(
      baseline = list(
        proc("TranslateBiotic", "RstoxData::TranslateBiotic",
             list(VariableName = "catchcategory", Note = paste("free text", sentinel))),
        proc("FilterStoxBiotic", "RstoxData::FilterStoxBiotic",
             list(FilterExpression = list(Haul = paste0("HaulQuality == 12 & HaulKey != '", sentinel, "'")))),
        proc("BioticPSU", "RstoxBase::DefineBioticPSU", list(DefinitionMethod = "StationToPSU"),
             data = list(BioticPSU = list(list(Stratum = "S1", PSU = "P1", Haul = sentinel)))),
        proc("LengthDistribution", "RstoxBase::LengthDistribution",
             list(LengthDistributionType = "Normalized", RaisingFactorPriority = "Weight")),
        proc("AbundanceDensity", "RstoxBase::SweptAreaDensity",
             list(SweptAreaDensityMethod = "LengthDistributed", SweepWidthMethod = "Constant",
                  SweepWidth = 18.5, DensityType = "AreaNumberDensity"))
      ),
      analysis = list(
        proc("Bootstrap", "RstoxFramework::Bootstrap",
             list(NumberOfBootstraps = 500, BootstrapMethodTable = list(
               list(ProcessName = "MeanLengthDistribution",
                    ResampleFunction = "ResampleMeanLengthDistributionData", Seed = 1234))))
      )
    )
  ))
}

test_that("stox_key_settings() picks out the settings and withholds the rest", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "official", "process"), recursive = TRUE)
  jsonlite::write_json(key_project(sentinel), file.path(root, "official", "process", "project.json"),
                       auto_unbox = TRUE, pretty = TRUE)
  k <- stox_key_settings("official", root = root)
  expect_s3_class(k, "nb_stox_settings")
  s <- k$settings
  val <- function(item, par) s$value[s$item == item & s$parameter == par]
  expect_equal(val("sweep width and density", "SweepWidthMethod"), "Constant")
  expect_equal(val("sweep width and density", "SweepWidth"), "18.5")
  expect_equal(val("raising and length distribution", "RaisingFactorPriority"), "Weight")
  expect_true(any(s$item == "bootstrap" & s$parameter == "NumberOfBootstraps" & s$value == "500"))
  expect_true(any(s$item == "bootstrap" & grepl("Seed", s$parameter) & s$value == "1234"))
  expect_true(any(s$item == "filters (fields only)" & grepl("HaulQuality", s$fields)))
  expect_true(any(s$item == "translations"))
  expect_equal(k$not_found, character(0))
  expect_equal(k$chain$process[k$chain$model == "baseline"][1], "TranslateBiotic")
  # nothing withheld by describe_stox_project() comes back
  txt <- paste(c(utils::capture.output(print(k)), unlist(k$settings), unlist(k$process_data)),
               collapse = "\n")
  expect_false(grepl(sentinel, txt, fixed = TRUE))
  expect_false(grepl("HaulQuality == 12", txt, fixed = TRUE))
  expect_match(txt, "withheld")
  # the same result from a description
  expect_equal(stox_key_settings(describe_stox_project("official", root = root))$settings, s)
})

test_that("stox_key_settings() reports the items a project does not contain", {
  root <- withr::local_tempdir()
  write_project(root, "xml")
  k <- stox_key_settings("official", root = root)
  expect_true("translations" %in% k$not_found)
  expect_true("sweep width and density" %in% setdiff(names(stox_settings_rules), k$not_found))
  expect_output(print(k), "Not found in this project")
  expect_false(grepl(sentinel, paste(utils::capture.output(print(k)), collapse = "\n"), fixed = TRUE))
})

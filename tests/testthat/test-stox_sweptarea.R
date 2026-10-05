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
  # the coded clause is shown, the identifier clause is not
  expect_equal(fs$value, "samplequality == 12 and <withheld clause>")
  expect_equal(fs$status, "partly withheld")
  expect_equal(fs$fields, "samplequality, serialno")
  expect_equal(p$value[p$parameter == "CatchExpr"], "species not in [90001,90002]")
  expect_equal(p$fields[p$parameter == "CatchExpr"], "species")
  expect_setequal(d$process_data$element, c("bioticassignment", "stratumpolygon"))
  expect_equal(d$process_data$n_entries[d$process_data$element == "bioticassignment"], 2L)
  expect_false(grepl(sentinel, all_text(d)))
  expect_false(grepl("MULTIPOLYGON", all_text(d)))
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
  # a listed stratum that is not in the polygons is refused (a typo would otherwise go unnoticed)
  expect_error(inclusion_summary(cfg, root = s$root), "SX-STRATA-05|SX-INC-01")
  cfg$data$stratum_names <- NULL
  inc <- inclusion_summary(cfg, root = s$root)
  expect_equal(inc$rules$n_excluded[inc$rules$rule == "start position inside the strata"], 8L)
  expect_equal(inc$by_stratum$stratum, c("SYN-A", "SYN-B", "SYN-C"))
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
direct_biomass <- function(sv, species, width_m = 20, exclude = NULL) {
  st <- sv$stations[sv$stations$design_station, ]
  st <- st[!as.character(st$serialnumber) %in% exclude, ]
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
      "species_filter_expression", "strata_file", "stratum_label", "raising_factor_priority",
      "length_interval", "length_process", "sweep_width_m", "cores", "si_distribution_method",
      "impute_method", "impute_at_missing", "impute_to", "impute_by_equal", "impute_levels",
      "impute_seed", "baseline_seed_table", "bootstrap_method_table", "replicates",
      "output_processes", "survey_method", "survey_table")
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
  expect_match(unique(est$code_version), "template sweptarea 2\\.1\\.0$")
  expect_true(all(est$value >= 0))

  # The baseline estimate equals the direct design-based estimate, which uses
  # the same stations: the planted pelagic, aborted and zero-distance stations
  # are therefore excluded.
  for (sp in sv_species <- s$sv$design$species$species_code) {
    direct <- direct_biomass(s$sv, sp)
    got <- est[est$species_code == sp & est$quantity == "biomass" & est$stratum != "total", ]
    # a sampled stratum where the species was not caught is a zero
    expect_equal(got$value, as.numeric(direct[got$stratum]), tolerance = 1e-6)
    expect_setequal(got$stratum, s$sv$design$strata$stratum)
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
             list(FilterExpression = list(Haul = paste0("samplequality == 12 & HaulKey != '", sentinel, "'")))),
        proc("BioticPSU", "RstoxBase::DefineBioticPSU", list(DefinitionMethod = "StationToPSU"),
             data = list(BioticPSU = list(list(Stratum = "S1", PSU = "P1", Haul = sentinel)))),
        proc("LengthDistribution", "RstoxBase::LengthDistribution",
             list(LengthDistributionType = "Normalized", RaisingFactorPriority = "Weight")),
        proc("Regroup", "RstoxBase::RegroupLengthDistribution", list(LengthInterval = 2)),
        proc("SuperIndividuals", "RstoxBase::SuperIndividuals", list(DistributionMethod = "Equal")),
        proc("ImputeSuperIndividuals", "RstoxBase::ImputeSuperIndividuals",
             list(ImputationMethod = "RandomLengthConditional", Seed = 1)),
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
  expect_true(any(s$item == "filters (fields only)" & grepl("samplequality", s$fields)))
  expect_true(any(s$value == "samplequality == 12 & <withheld clause>"))
  expect_true(any(s$item == "translations"))
  bi <- s[s$item == "length groups, quantities and individuals (biomass route)", ]
  expect_equal(bi$value[bi$parameter == "LengthInterval"], "2")
  expect_equal(bi$value[bi$parameter == "ImputationMethod"], "RandomLengthConditional")
  expect_equal(k$not_found, character(0))
  # the bookkeeping parameters stay in the data but not in the printed output
  expect_true("enabled" %in% s$parameter)
  expect_false(any(grepl("showInMap|fileOutput", utils::capture.output(print(k)))))
  expect_equal(k$chain$process[k$chain$model == "baseline"][1], "TranslateBiotic")
  # nothing withheld by describe_stox_project() comes back
  txt <- paste(c(utils::capture.output(print(k)), unlist(k$settings), unlist(k$process_data)),
               collapse = "\n")
  expect_false(grepl(sentinel, txt, fixed = TRUE))
  expect_match(txt, "samplequality == 12 & <withheld clause>", fixed = TRUE)
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

# ---- Filter expressions: what may be shown ---------------------------------------

show <- function(x) {
  r <- redact_expression(x)
  if (r$n_shown == 0L) "<withheld: expression>" else r$text
}

test_that("coded clauses in filters are shown and identifier clauses are not", {
  # shown: coded fields and distance or depth thresholds, with few short values
  expect_equal(show("samplequality == 12"), "samplequality == 12")
  expect_equal(show("stationtype %in% c(12) & samplequality in [12,13] | gearcondition == 1"),
               "stationtype %in% c(12) & samplequality in [12,13] | gearcondition == 1")
  expect_equal(show("species not in [90001,90002]"), "species not in [90001,90002]")
  expect_equal(show("gearcondition %in% \"1\""), "gearcondition %in% \"1\"")        # a bare value
  expect_equal(show("Gear %in% c(\"3032\", \"3033\") & gearcondition %in% \"1\" & samplequality %in% \"12\""),
               "Gear %in% c(\"3032\", \"3033\") & gearcondition %in% \"1\" & samplequality %in% \"12\"")
  expect_equal(show("serialno %in% \"12\""), "<withheld: expression>")
  expect_equal(show("distance > 0.5 and gear == 3270"), "distance > 0.5 and gear == 3270")
  expect_equal(show("(stationtype == 12) and (gearcondition == 1)"),
               "(stationtype == 12) and (gearcondition == 1)")
  # identifier fields are replaced, whatever the value looks like
  expect_equal(show("samplequality == 12 and serialno != 'X1'"),
               "samplequality == 12 and <withheld clause>")
  expect_equal(show("HaulKey %in% c('1','2')"), "<withheld: expression>")
  expect_equal(show("station == 7 | samplequality == 12"), "<withheld clause> | samplequality == 12")
  for (f in c("serialno", "serialnumber", "StationKey", "HaulKey", "cruise", "missionnumber",
              "platform", "latitudestart", "longitudestart", "catchsampleid", "specimenid")) {
    expect_equal(show(paste0(f, " == 12")), "<withheld: expression>")
  }
})

test_that("long lists, long values and odd syntax in filters are withheld", {
  expect_equal(show("samplequality in [1,2,3,4,5,6]"), "<withheld: expression>")
  expect_equal(show("samplequality %in% c(1,2,3,4,5)"), "samplequality %in% c(1,2,3,4,5)")
  expect_equal(show("samplequality == 'ST2019104'"), "<withheld: expression>")   # 9 characters
  expect_equal(show("samplequality == 'SENTINEL_7f3a9c'"), "<withheld: expression>")
  # a connector inside a quoted value cannot smuggle a clause through
  expect_equal(show("serialno != 'a & samplequality == 12'"), "<withheld: expression>")
  expect_equal(show("serialno != 'a | gear == 1 and gear == 2'"), "<withheld: expression>")
  # functions, arithmetic and anything else that is not a plain comparison
  expect_equal(show("is.na(samplequality)"), "<withheld: expression>")
  expect_equal(show("samplequality == 12 + serialno"), "<withheld: expression>")
  expect_equal(show("distance == 1"), "distance == 1")
  expect_equal(show("distance > abc"), "<withheld: expression>")   # a threshold must be a number
  expect_equal(show("samplequality == 12 and serialno != 'unbalanced"), "<withheld: expression>")
  expect_equal(show(strrep("samplequality == 12 and ", 20)), "<withheld: expression>")
  expect_equal(show(paste(rep("samplequality == 12", 13), collapse = " & ")), "<withheld: expression>")
  expect_equal(show(strrep("x", 500)), "<withheld: expression>")
})

test_that("classify_stox_value() reports shown, partly withheld and withheld expressions", {
  expect_equal(classify_stox_value("FilterExpression", "samplequality == 12")$status, "shown")
  expect_equal(classify_stox_value("FilterExpression", "samplequality == 12 & HaulKey != 'a'")$status,
               "partly withheld")
  expect_equal(classify_stox_value("FilterExpression", "HaulKey != 'a'")$status, "withheld")
  expect_equal(classify_stox_value("FilterExpression", "HaulKey != 'a'")$value, "<withheld: expression>")
  expect_equal(classify_stox_value("FishStationExpr", "HaulKey != 'a'")$fields, "HaulKey")
  # the operator word notin is not a field
  expect_equal(classify_stox_value("FilterExpression", "Station %notin% c('a', 'b')")$fields, "Station")
})

# ---- Template 2.0.0: species filter, super-individual route -----------------------

test_that("the species categories of the configured species are found as StoX names them", {
  skip_if_no_stox()
  s <- synthetic_root(excluded = c(pelagic = 1))
  f <- file.path(s$root, "surveys", "synthetic-seed1.xml")
  cats <- stox_species_categories(f, c("SYN001", "SYN003", "NOPE"))
  expect_length(cats, 2L)
  expect_setequal(species_from_category(cats, c("SYN001", "SYN003")), c("SYN001", "SYN003"))
  expect_length(stox_species_categories(f, "NOPE"), 0L)
})

test_that("only the configured species are estimated, and hauls without them stay in the mean", {
  skip_if_no_stox()
  s <- synthetic_root()
  cfg <- quick_config(3L)
  cfg$species <- "SYN003"       # a patchy species: absent at some stations
  res <- suppressWarnings(run_estimate(cfg, root = s$root,
                                       staging_dir = file.path(s$root, "staging")))
  expect_setequal(unique(res$estimates$species_code), "SYN003")
  direct <- direct_biomass(s$sv, "SYN003")
  b <- res$estimates[res$estimates$quantity == "biomass" & res$estimates$stratum != "total", ]
  # the stratum means include the stations where the species was not caught
  expect_equal(b$value, as.numeric(direct[b$stratum]), tolerance = 1e-6)
  tot <- res$estimates$value[res$estimates$quantity == "biomass" & res$estimates$stratum == "total"]
  expect_equal(tot, sum(direct), tolerance = 1e-6)
  # a configuration whose species are not in the files is refused
  cfg$species <- "NOPE"
  expect_error(run_estimate(cfg, root = s$root), "SX-RUN-01|SX-SPC-01")
})

test_that("biomass through super-individuals agrees with the catch-weight route", {
  skip_if_no_stox()
  s <- synthetic_root()
  a <- suppressWarnings(run_estimate(quick_config(3L), root = s$root,
                                     staging_dir = file.path(s$root, "stA")))
  cfg <- quick_config(3L)
  cfg$biomass$method <- "super_individuals"
  cfg$lengths$interval_cm <- 2
  b <- suppressWarnings(run_estimate(cfg, root = s$root, staging_dir = file.path(s$root, "stB")))
  expect_equal(b$staged$outcome, "pass")
  expect_match(unique(b$estimates$code_version), "template sweptarea 2\\.1\\.0$")
  key <- c("species_code", "stratum", "quantity")
  m <- merge(a$estimates[c(key, "value", "unit")], b$estimates[c(key, "value", "unit")], by = key,
             suffixes = c(".catch", ".si"))
  expect_equal(nrow(m), nrow(a$estimates))
  expect_equal(m$unit.catch, m$unit.si)
  # abundance follows the same chain in both routes
  ab <- m[m$quantity == "abundance", ]
  expect_equal(ab$value.si, ab$value.catch, tolerance = 1e-9)
  # biomass: different estimators of the same quantity, within a few per cent in total
  bt <- m[m$quantity == "biomass" & m$stratum == "total", ]
  expect_true(all(abs(bt$value.si / bt$value.catch - 1) < 0.05))
  # and within 10 per cent in every stratum
  bs <- m[m$quantity == "biomass" & m$stratum != "total" & m$value.catch > 0, ]
  expect_true(all(abs(bs$value.si / bs$value.catch - 1) < 0.10))
})

test_that("the bootstrap mean can be reported as the estimate", {
  skip_if_no_stox()
  s <- synthetic_root(excluded = c(pelagic = 1))
  cfg <- quick_config(6L)
  base <- suppressWarnings(run_estimate(cfg, root = s$root, staging_dir = file.path(s$root, "st1")))
  cfg$estimate$point <- "bootstrap_mean"
  mean_ <- suppressWarnings(run_estimate(cfg, root = s$root, staging_dir = file.path(s$root, "st2")))
  key <- c("species_code", "stratum", "quantity")
  m <- merge(base$estimates[c(key, "value", "ci_lower", "ci_upper")],
             mean_$estimates[c(key, "value")], by = key, suffixes = c(".base", ".mean"))
  expect_false(isTRUE(all.equal(m$value.base, m$value.mean)))
  # the bootstrap mean is close to the baseline for a large cell
  big <- m$quantity == "biomass" & m$stratum == "total" & m$species_code == "SYN001"
  expect_lt(abs(m$value.mean[big] / m$value.base[big] - 1), 0.25)
})

# ---- Station exclusions ----------------------------------------------------------

test_that("station exclusion lists are read from plain filter clauses only", {
  expect_equal(parse_station_exclusions("Station %notin% c('a/1-2', 'b-3')"), c("a/1-2", "b-3"))
  expect_equal(parse_station_exclusions('!Station %in% c("a", "b")'), c("a", "b"))
  expect_equal(parse_station_exclusions("!(Station %in% c('a','b'))"), c("a", "b"))
  expect_equal(parse_station_exclusions("Station != 'a' & Station != 'b'"), c("a", "b"))
  expect_equal(parse_station_exclusions("samplequality == 12 & Station %notin% c('x')"), "x")
  expect_equal(parse_station_exclusions("Station %notin% c('a & b', 'c | d')"), c("a & b", "c | d"))
  expect_null(parse_station_exclusions("samplequality == 12 & gear %in% c(1, 2)"))
  expect_null(parse_station_exclusions("StationKey %notin% c('x')"))   # another field
  # anything else on the field is refused, without echoing the key
  for (bad in c("Station == 'SENTINEL_KEY'", "Station %in% c('SENTINEL_KEY')",
                "Station %notin% SENTINEL_KEY", "Station %notin% c('SENTINEL_KEY', other)",
                "Station %notin% c('SENTINEL_KEY"))  {
    err <- tryCatch(parse_station_exclusions(bad), error = function(e) e)
    expect_s3_class(err, "nansenbiomass_error")
    expect_match(conditionMessage(err), "SX-EXC-04")
    expect_false(grepl("SENTINEL_KEY", conditionMessage(err)))
  }
})

write_exclusion_project <- function(root, keys, expr = NULL, name = "official") {
  dir.create(file.path(root, name, "process"), recursive = TRUE)
  expr <- if (is.null(expr)) paste0("Station %notin% c(", paste0("'", keys, "'", collapse = ", "), ")") else expr
  jsonlite::write_json(
    list(project = list(models = list(baseline = list(list(
      processName = "FilterStoxBiotic", functionName = "RstoxData::FilterStoxBiotic",
      functionParameters = list(FilterExpression = list(Station = expr))
    ))))),
    file.path(root, name, "process", "project.json"), auto_unbox = TRUE
  )
}

synthetic_station_keys <- function(root) {
  sb <- suppressWarnings(RstoxData::StoxBiotic(RstoxData::ReadBiotic(
    file.path(root, "surveys", "synthetic-seed1.xml"))))
  sb$Station$Station
}

test_that("stox_station_exclusions() writes serial numbers and prints counts only", {
  skip_if_no_stox()
  s <- synthetic_root()
  keys <- synthetic_station_keys(s$root)
  write_exclusion_project(s$root, c(keys[1:2], "SENTINEL_KEY"))
  warned <- NULL
  res <- withCallingHandlers(
    stox_station_exclusions("official", "surveys/synthetic-seed1.xml", "exclusions/s.txt", root = s$root),
    warning = function(w) { warned <<- conditionMessage(w); invokeRestart("muffleWarning") }
  )
  expect_match(warned, "LOG-WARN-01")
  expect_false(grepl("SENTINEL_KEY", warned))
  expect_s3_class(res, "nb_exclusions")
  expect_equal(unlist(unclass(res)[c("n_listed", "n_matched", "n_unmatched", "n_serials")],
                      use.names = FALSE), c(3L, 2L, 1L, 2L))
  lines <- readLines(file.path(s$root, "exclusions", "s.txt"))
  expect_equal(lines[-1], c("90001", "90002"))
  expect_true(startsWith(lines[1], "#"))
  # neither the keys nor the planted identifier reach the printed object or the log
  shown <- paste(utils::capture.output(print(res)), collapse = "\n")
  expect_false(grepl("SENTINEL_KEY|SYNTH0001", shown))
  logs <- paste(unlist(lapply(list.files(file.path(s$root, "logs"), full.names = TRUE), readLines)),
                collapse = "\n")
  expect_false(grepl("SENTINEL_KEY", logs))
  # refusals
  expect_error(stox_station_exclusions("official", "surveys/synthetic-seed1.xml", "exclusions/s.txt",
                                       root = s$root), "SX-EXC-05")
  write_exclusion_project(s$root, character(0), expr = "samplequality == 12", name = "nolist")
  expect_error(stox_station_exclusions("nolist", "surveys/synthetic-seed1.xml", "exclusions/n.txt",
                                       root = s$root), "SX-EXC-07")
  dir.create(file.path(s$root, "v27", "process"), recursive = TRUE)
  writeLines(xml_project, file.path(s$root, "v27", "process", "project.xml"))
  expect_error(stox_station_exclusions("v27", "surveys/synthetic-seed1.xml", "exclusions/x.txt",
                                       root = s$root), "SX-EXC-06")
})

test_that("an exclusion file leaves the listed stations out of the counts and of the estimate", {
  skip_if_no_stox()
  s <- synthetic_root()
  keys <- synthetic_station_keys(s$root)
  write_exclusion_project(s$root, keys[1:2])
  stox_station_exclusions("official", "surveys/synthetic-seed1.xml", "exclusions/s.txt", root = s$root)
  cfg <- quick_config(3L)
  cfg$inclusion$exclude_stations_file <- "exclusions/s.txt"
  inc <- inclusion_summary(cfg, root = s$root)
  r <- inc$rules
  expect_equal(r$n_excluded[grepl("^not in the exclusion list", r$rule)], 2L)
  expect_match(r$rule[grepl("exclusion", r$rule)], "2 listed, 2 in the files")
  expect_equal(r$n_after[nrow(r)], 43L)
  res <- suppressWarnings(run_estimate(cfg, root = s$root, staging_dir = file.path(s$root, "st")))
  expect_equal(sum(res$support$n_stations[res$support$species_code == "SYN001"]), 43L)
  direct <- direct_biomass(s$sv, "SYN001", exclude = c("90001", "90002"))
  got <- res$estimates[res$estimates$species_code == "SYN001" & res$estimates$quantity == "biomass" &
                         res$estimates$stratum != "total", ]
  expect_equal(got$value, as.numeric(direct[got$stratum]), tolerance = 1e-6)
  # the identifiers are not in the staged files
  est_file <- res$staged$files[grepl("estimates\\.csv$", res$staged$files)]
  expect_length(est_file, 1L)
  expect_false(grepl("serial|exclu", paste(readLines(est_file), collapse = "\n"), ignore.case = TRUE))
})

test_that("a bad exclusion file or path is refused without echoing its content", {
  s <- synthetic_root()
  dir.create(file.path(s$root, "exclusions"))
  writeLines(c("# comment", "12345", "SENTINEL KEY WITH SPACES"), file.path(s$root, "exclusions", "bad.txt"))
  cfg <- quick_config(3L)
  cfg$inclusion$exclude_stations_file <- "exclusions/bad.txt"
  err <- tryCatch(inclusion_summary(cfg, root = s$root), error = function(e) e)
  expect_match(conditionMessage(err), "SX-EXC-02")
  expect_false(grepl("SENTINEL", conditionMessage(err)))
  x <- yaml::read_yaml(example_config())
  x$inclusion$exclude_stations_file <- "C:/data/exclusions.txt"
  expect_error(validate_config(x), "CF-PATH-02")
  x$inclusion$exclude_stations_file <- "../exclusions.txt"
  expect_error(validate_config(x), "CF-PATH-02")
  cfg$inclusion$exclude_stations_file <- "exclusions/missing.txt"
  expect_error(inclusion_summary(cfg, root = s$root), "IO-PATH-03|SX-INC-01")
})

# ---- Official project copy run and comparison -----------------------------------

test_that("StoX report tables are converted to a comparable table", {
  reports <- list(
    base_stratum = data.frame(Stratum = c("A", "B"), SpeciesCategory = "n/SP1/NA/s",
                              Biomass_sum = c(2e6, 3e6)),
    boot_total = data.frame(Survey = c("Survey", NA), SpeciesCategory = "n/SP1/NA/s",
                            Abundance_sum_mean = c(5e6, 9e6), Abundance_sum_sd = c(5e5, 1e5),
                            Abundance_sum_cv = c(0.1, 0.01), `Abundance_sum_2.5%` = c(4e6, 1e6),
                            `Abundance_sum_97.5%` = c(6e6, 2e6), check.names = FALSE),
    by_length = data.frame(Stratum = "A", IndividualTotalLength = 10, Abundance_sum = 1e6),
    not_a_report = data.frame(x = 1)
  )
  tab <- official_reports_to_table(reports)
  expect_setequal(tab$process, c("base_stratum", "boot_total"))      # the length report is skipped
  b <- tab[tab$process == "base_stratum", ]
  expect_equal(b$kind, c("baseline", "baseline"))
  expect_equal(b$baseline, c(2, 3))                                  # grams to tonnes
  expect_equal(b$quantity, c("biomass", "biomass"))
  t <- tab[tab$process == "boot_total", ]
  expect_equal(nrow(t), 1L)                                          # the row outside the survey is dropped
  expect_equal(t$stratum, "total")
  expect_equal(t$kind, "bootstrap")
  expect_equal(c(t$mean, t$sd, t$cv, t$lower, t$upper), c(5, 0.5, 0.1, 4, 6))   # millions; the CV is not scaled
  expect_equal(official_reports_to_table(reports, scale = c(biomass = 1, abundance = 1))$baseline[1], 2e6)
  # an empty row (a stratum without a catch of the species) is dropped
  empty <- list(r = data.frame(Stratum = c("A", "C"), SpeciesCategory = c("n/SP1/NA/s", NA),
                               Biomass_sum = c(2e6, NA)))
  expect_equal(official_reports_to_table(empty)$stratum, "A")
})

test_that("compare_estimates() reports ratios, flags, and refuses empty comparisons", {
  ours <- tibble::tibble(species_code = c("SP1", "SP1", "SP2"), stratum = c("A", "total", "A"),
                         quantity = "biomass", value = c(2.01, 5.5, 1), cv = c(0.2, 0.1, 0.3))
  ref <- official_reports_to_table(list(
    base = data.frame(Stratum = c("A", "A"), SpeciesCategory = c("n/SP1/NA/s", "n/SP2/NA/s"),
                      Biomass_sum = c(2e6, 1e6)),
    tot = data.frame(Survey = "Survey", SpeciesCategory = "n/SP1/NA/s", Biomass_sum = 5e6)
  ))
  cmp <- compare_estimates(ours, ref)
  expect_s3_class(cmp, "nb_comparison")
  expect_equal(nrow(cmp), 3L)
  expect_equal(cmp$ratio[cmp$stratum == "A" & cmp$species_code == "SP1"], 1.005)
  expect_equal(cmp$within[cmp$stratum == "A" & cmp$species_code == "SP1"], TRUE)
  expect_equal(cmp$within[cmp$stratum == "total"], FALSE)            # 5.5 against 5 is outside 1%
  expect_equal(cmp$ratio[cmp$species_code == "SP2"], 1)
  expect_true(all(is.na(cmp$cv_ratio)))                              # the reference has no CV
  expect_output(print(cmp), "3 rows, 3 with a ratio, 2 within tolerance")
  expect_equal(compare_estimates(ours, ref, tolerance = 0.2)$within, rep(TRUE, 3))
  expect_error(compare_estimates(ours, ref, "mean"), "SX-CMP-01")    # no bootstrap values
  other <- ref; other$stratum <- "Z"
  expect_error(compare_estimates(ours, other), "SX-CMP-02")
})

test_that("a copy of a project reproduces our estimates and the original is left untouched", {
  skip_if_no_stox()
  s <- synthetic_root()
  cfg <- quick_config(3L)
  cfg$biomass$method <- "super_individuals"
  cfg$lengths$interval_cm <- 2
  res <- suppressWarnings(run_estimate(cfg, root = s$root, staging_dir = file.path(s$root, "st")))
  rel <- sub(paste0("^", normalizePath(s$root, winslash = "/"), "/"), "",
             normalizePath(res$project_path, winslash = "/"))
  files <- list.files(res$project_path, recursive = TRUE, full.names = TRUE)
  before <- tools::md5sum(files[!grepl("/output/", files)])
  run <- suppressWarnings(stox_official_copy_run(rel, root = s$root, replicates = 3L))
  expect_s3_class(run, "nb_official_run")
  # the original project is unchanged, the copy is a separate folder in the data zone
  after <- tools::md5sum(files[!grepl("/output/", files)])
  expect_equal(unname(after), unname(before))
  expect_true(dir.exists(file.path(s$root, run$folder)))
  expect_true(startsWith(run$folder, "stox_official_check/copy-"))
  expect_true(length(run$reports) >= 4L)
  expect_false(is.null(run$reference))
  # same engine and same input: the baseline values are ours
  with_value <- res$estimates[!is.na(res$estimates$value) & res$estimates$value > 0, ]
  cmp <- compare_estimates(with_value, official_reports_to_table(run$reports), "baseline")
  expect_equal(nrow(cmp), nrow(with_value))
  expect_equal(cmp$ratio, rep(1, nrow(cmp)), tolerance = 1e-9)
  # nothing but names and counts is printed
  shown <- paste(utils::capture.output(print(run)), collapse = "\n")
  expect_match(shown, "rows")
  expect_false(grepl("SYNTH0001|SYN001|SYN-A|/tmp|Rtmp", shown))
  # refusals
  expect_error(stox_official_copy_run("surveys/synthetic-seed1.xml", root = s$root), "SX-OFF-02")
  expect_error(stox_official_copy_run(rel, root = s$root, replicates = 0), "SX-OFF-03")
})

test_that("the StoX packages are attached before use, also after a session detached them", {
  skip_if_no_stox()
  for (p in c("RstoxFramework", "RstoxData", "RstoxBase")) {
    nm <- paste0("package:", p)
    if (nm %in% search()) detach(nm, character.only = TRUE)
  }
  expect_false("package:RstoxBase" %in% search())
  check_stox_ready(list(stox = list(version = stox_pinned_version)))
  expect_true(all(paste0("package:", c("RstoxBase", "RstoxData", "RstoxFramework")) %in% search()))
  # and a run works straight after a detach
  for (p in c("RstoxFramework", "RstoxData", "RstoxBase")) detach(paste0("package:", p), character.only = TRUE)
  s <- synthetic_root(excluded = c(pelagic = 1))
  res <- suppressWarnings(run_estimate(quick_config(2L), root = s$root,
                                       staging_dir = file.path(s$root, "st")))
  expect_gt(nrow(res$estimates), 0L)   # whether the export passes the airlock is a separate question
})

# ---- Strata from a project or a polygon file ------------------------------------------

strata_feature_list <- function(root, label = "polygonName") {
  sv <- synth_survey(seed = 1)
  st <- sv$strata[c("stratum", "geometry")]
  names(st)[1] <- label
  f <- file.path(root, "tmp.geojson")
  sf::st_write(st, f, quiet = TRUE)
  jsonlite::read_json(f, simplifyVector = FALSE)
}

test_that("strata are read from a polygon file, and written for the configuration", {
  s <- synthetic_root()
  d <- stox_strata("strata/synthetic-strata.geojson", root = s$root, out = "strata/clean.geojson")
  expect_s3_class(d, "nb_strata")
  expect_equal(d$label, "StratumName")
  expect_equal(d$names, c("SYN-A", "SYN-B", "SYN-C", "SYN-D"))
  written <- sf::st_read(file.path(s$root, "strata", "clean.geojson"), quiet = TRUE)
  expect_equal(written$stratum, d$names)
  expect_equal(sf::st_crs(written)$epsg, 4326L)
  expect_output(print(d), "SYN-A, SYN-B, SYN-C, SYN-D")
  expect_error(stox_strata("strata/synthetic-strata.geojson", root = s$root, out = "strata/clean.geojson"),
               "SX-STR-02")
  expect_equal(stox_strata("strata/synthetic-strata.geojson", root = s$root, out = "strata/clean.geojson",
                           overwrite = TRUE)$names, d$names)
  expect_error(stox_strata("strata/synthetic-strata.geojson", root = s$root, label = "nope"), "SX-STR-03")
})

test_that("strata are read from the process data of a project.json and from a project folder", {
  s <- synthetic_root()
  fc <- strata_feature_list(s$root)
  dir.create(file.path(s$root, "p1", "process"), recursive = TRUE)
  jsonlite::write_json(list(project = list(models = list(baseline = list(list(
    processName = "DefineStratumPolygon", functionName = "RstoxBase::DefineStratumPolygon",
    functionParameters = list(StratumNameLabel = "polygonName"), processData = fc
  ))))), file.path(s$root, "p1", "process", "project.json"), auto_unbox = TRUE)
  d <- stox_strata("p1", root = s$root)
  expect_equal(d$source, "the project's process data")
  expect_equal(d$label, "polygonName")
  expect_equal(d$names, c("SYN-A", "SYN-B", "SYN-C", "SYN-D"))
  expect_equal(stox_strata("p1/process/project.json", root = s$root)$names, d$names)
  # a project folder whose file has no process data but whose output holds the polygons
  dir.create(file.path(s$root, "p2", "process"), recursive = TRUE)
  jsonlite::write_json(list(project = list(models = list(baseline = list(list(
    processName = "DefineStratumPolygon", functionName = "RstoxBase::DefineStratumPolygon"))))),
    file.path(s$root, "p2", "process", "project.json"), auto_unbox = TRUE)
  dir.create(file.path(s$root, "p2", "output", "baseline", "DefineStratumPolygon"), recursive = TRUE)
  file.copy(file.path(s$root, "strata", "synthetic-strata.geojson"),
            file.path(s$root, "p2", "output", "baseline", "DefineStratumPolygon", "StratumPolygon.geojson"))
  d2 <- stox_strata("p2", root = s$root)
  expect_equal(d2$source, "a polygon file in the project folder")
  expect_equal(d2$names, d$names)
  # nothing to find
  dir.create(file.path(s$root, "p3", "process"), recursive = TRUE)
  writeLines("{}", file.path(s$root, "p3", "process", "project.json"))
  expect_error(stox_strata("p3", root = s$root), "SX-STR-01")
})

test_that("strata are read from a StoX 2.7 project.xml with their includeintotal flag", {
  skip_if_not_installed("RstoxBase")
  s <- synthetic_root()
  wkt <- sf::st_as_text(sf::st_geometry(s$sv$strata))
  include <- c("true", "true", "true", "false")
  values <- unlist(lapply(seq_along(wkt), function(i) c(
    sprintf('      <value polygonkey="%s" polygonvariable="includeintotal">%s</value>', s$sv$strata$stratum[i], include[i]),
    sprintf('      <value polygonkey="%s" polygonvariable="polygon">%s</value>', s$sv$strata$stratum[i], wkt[i]))))
  dir.create(file.path(s$root, "v27", "process"), recursive = TRUE)
  writeLines(c('<?xml version="1.0" encoding="UTF-8"?>', '<project xmlns="http://www.imr.no/formats/stox/v1">',
               '  <processdata>', '    <stratumpolygon>', values, '    </stratumpolygon>', '  </processdata>', '</project>'),
             file.path(s$root, "v27", "process", "project.xml"))
  d <- stox_strata("v27/process/project.xml", root = s$root)
  expect_equal(d$names, c("SYN-A", "SYN-B", "SYN-C", "SYN-D"))
  expect_equal(unname(d$include_in_total), c(TRUE, TRUE, TRUE, FALSE))
})

test_that("stratum names can be left out of the configuration", {
  s <- synthetic_root()
  x <- yaml::read_yaml(example_config())
  x$data$stratum_names <- NULL
  cfg <- validate_config(x)
  expect_null(cfg$data$stratum_names)
  expect_equal(validate_config(cfg), cfg)
  inc <- inclusion_summary(cfg, root = s$root)
  expect_equal(inc$by_stratum$stratum, c("SYN-A", "SYN-B", "SYN-C", "SYN-D"))
  expect_equal(sum(inc$by_stratum$n_kept), 45L)
  bad <- cfg; bad$data$stratum_label <- "nope"
  expect_error(inclusion_summary(bad, root = s$root), "SX-STRATA-02|SX-INC-01")
})

test_that("a run without stratum names in the configuration gives the same estimates", {
  skip_if_no_stox()
  s <- synthetic_root(excluded = c(pelagic = 1))
  with <- suppressWarnings(run_estimate(quick_config(2L), root = s$root, staging_dir = file.path(s$root, "a")))
  cfg <- quick_config(2L)
  cfg$data$stratum_names <- NULL
  without <- suppressWarnings(run_estimate(cfg, root = s$root, staging_dir = file.path(s$root, "b")))
  cols <- c("species_code", "stratum", "quantity", "value")
  expect_equal(without$estimates[cols], with$estimates[cols])
})

test_that("super-individual reports leave out individuals without a weight, as the official reports do", {
  skip_if_no_stox()
  root <- withr::local_tempdir()
  dir.create(file.path(root, "surveys")); dir.create(file.path(root, "strata"))
  sv <- synth_survey(synth_design(), seed = 1)
  full <- sv$survey
  part <- sv$survey
  withr::with_seed(1, part$individual$individualweight[sample(nrow(part$individual), nrow(part$individual) %/% 3)] <- NA)
  strata <- sv$strata
  names(strata)[names(strata) == "stratum"] <- "StratumName"
  sf::st_write(strata, file.path(root, "strata", "synthetic-strata.geojson"), quiet = TRUE)
  cfg <- quick_config(2L)
  cfg$species <- "SYN001"
  cfg$biomass <- list(method = "super_individuals", distribution_method = "HaulDensity",
                      imputation = list(at_missing = "IndividualTotalLength", to_impute = "IndividualTotalLength",
                                        by_equal = "LengthResolution", seed = 1))
  cfg <- validate_config(cfg)
  attr(cfg, "config_hash") <- config_hash(example_config())
  total <- function(survey, dir) {
    write_biotic(survey, file.path(root, "surveys", "synthetic-seed1.xml"))
    r <- suppressWarnings(run_estimate(cfg, root = root, staging_dir = file.path(root, dir)))$estimates
    r$value[r$stratum == "total" & r$quantity == "biomass"]
  }
  b_full <- total(full, "a")
  b_part <- total(part, "b")
  expect_true(is.finite(b_part))     # missing weights are removed, not propagated
  expect_lt(b_part, b_full)          # and the biomass is lower than with all weights
})

# ---- StoX WKT strata, the gear rule and the total-only export -------------------------

test_that("a StoX WKT stratum file is read like a polygon file", {
  s <- synthetic_root()
  wkt <- sf::st_as_text(sf::st_geometry(sf::st_transform(s$sv$strata, 4326)), digits = 15)
  writeLines(paste0(s$sv$strata$stratum, "\t", wkt), file.path(s$root, "strata", "synthetic.wkt"))
  d <- stox_strata("strata/synthetic.wkt", root = s$root, out = "strata/from-wkt.geojson")
  expect_equal(d$source, "a StoX stratum WKT file")
  expect_equal(d$names, c("SYN-A", "SYN-B", "SYN-C", "SYN-D"))
  expect_equal(sf::st_read(file.path(s$root, "strata", "from-wkt.geojson"), quiet = TRUE)$stratum, d$names)
  cfg <- quick_config()
  cfg$data$strata <- "strata/synthetic.wkt"
  cfg$data$stratum_label <- "stratum"
  inc_wkt <- inclusion_summary(cfg, root = s$root)
  inc_geo <- inclusion_summary(quick_config(), root = s$root)
  expect_equal(inc_wkt$by_stratum$n_kept, inc_geo$by_stratum$n_kept)
  writeLines(c("A", "B\tnot a polygon"), file.path(s$root, "strata", "bad.wkt"))
  expect_error(stox_strata("strata/bad.wkt", root = s$root), "SX-STRATA-04|SX-STR-04")
})

test_that("the gear rule keeps the stations of the listed gears", {
  s <- synthetic_root()
  cfg <- quick_config()
  cfg$inclusion$gear <- "9999"
  expect_equal(inclusion_summary(cfg, root = s$root)$rules$n_after |> utils::tail(1), 45L)
  cfg$inclusion$gear <- c("1111", "2222")
  r <- inclusion_summary(cfg, root = s$root)$rules
  expect_equal(r$n_excluded[grepl("^gear in", r$rule)], 54L - 4L - 3L - 0L)   # all that passed the code rules
  expect_equal(utils::tail(r$n_after, 1), 0L)
  x <- yaml::read_yaml(example_config())
  x$inclusion$gear <- 3032
  expect_equal(validate_config(x)$inclusion$gear, "3032")
})

test_that("when the full export fails the airlock, the totals are staged on their own", {
  skip_if_no_stox()
  s <- synthetic_root(excluded = c(pelagic = 1))
  cfg <- quick_config(2L)
  cfg$species <- "SYN001"
  cfg$disclosure$min_stations <- 11L        # strata with fewer stations cannot be released
  res <- suppressWarnings(run_estimate(cfg, root = s$root, staging_dir = file.path(s$root, "st")))
  expect_equal(res$staged$outcome, "fail")
  expect_false(is.null(res$staged_total_only))
  expect_equal(res$staged_total_only$outcome, "pass")
  est_file <- res$staged_total_only$files[grepl("estimates\\.csv$", res$staged_total_only$files)]
  rel <- utils::read.csv(est_file)
  expect_equal(unique(rel$stratum), "total")
  expect_setequal(unique(rel$quantity), c("biomass", "abundance"))
  # and a run whose export passes has no separate totals
  ok <- suppressWarnings(run_estimate(quick_config(2L), root = s$root, staging_dir = file.path(s$root, "ok")))
  if (identical(ok$staged$outcome, "pass")) expect_null(ok$staged_total_only)
})

# ---- Selecting the strata of the surveyed EEZs ----------------------------------------

test_that("only the selected strata are counted: stations elsewhere are left out", {
  s <- synthetic_root()
  cfg <- quick_config()
  cfg$data$stratum_names <- c("SYN-A", "SYN-B")
  inc <- inclusion_summary(cfg, root = s$root)
  r <- inc$rules
  expect_equal(r$n_excluded[grepl("^stratum among the 2 selected of 4", r$rule)], 18L)   # 10 in C, 8 in D
  expect_equal(utils::tail(r$n_after, 1), 27L)
  expect_equal(inc$by_stratum$stratum, c("SYN-A", "SYN-B"))
  expect_equal(inc$by_stratum$n_kept, c(12L, 15L))
  # the same selection by pattern, and both together (union, in file order)
  p <- quick_config()
  p$data$stratum_names <- NULL
  p$data$stratum_pattern <- "^SYN-[AB]$"
  expect_equal(inclusion_summary(p, root = s$root)$by_stratum$stratum, c("SYN-A", "SYN-B"))
  b <- quick_config()
  b$data$stratum_names <- "SYN-D"
  b$data$stratum_pattern <- "^SYN-[AB]$"
  expect_equal(inclusion_summary(b, root = s$root)$by_stratum$stratum, c("SYN-A", "SYN-B", "SYN-D"))
  # no selection: all strata, no selection step
  all <- inclusion_summary(quick_config(), root = s$root)
  expect_false(any(grepl("selected of", all$rules$rule)))
  expect_equal(nrow(all$by_stratum), 4L)
})

test_that("regions of strata are selected from the stations, with unsampled strata kept in", {
  s <- synthetic_root()
  strata <- sf::st_read(file.path(s$root, "strata", "synthetic-strata.geojson"), quiet = TRUE)
  strata$StratumName <- c("N_50-100m", "N_100-200m", "S_50-100m", "S_100-200m")[
    match(strata$StratumName, c("SYN-A", "SYN-B", "SYN-C", "SYN-D"))]
  sf::st_write(strata, file.path(s$root, "strata", "synthetic-strata.geojson"),
               delete_dsn = TRUE, quiet = TRUE)
  cfg <- quick_config()
  cfg$data$stratum_names <- NULL
  cfg$data$stratum_regions <- "^(.*)_[0-9]+-[0-9]+m$"
  # stations in every region: all strata
  inc <- inclusion_summary(cfg, root = s$root)
  expect_equal(nrow(inc$by_stratum), 4L)
  # all stations of the S_100 stratum excluded: the region S is still surveyed, so S_100 stays
  d <- s$sv$stations[s$sv$stations$design_station & s$sv$stations$stratum == "SYN-D", ]
  dir.create(file.path(s$root, "exclusions"))
  writeLines(as.character(d$serialnumber), file.path(s$root, "exclusions", "d.txt"))
  cfg$inclusion$exclude_stations_file <- "exclusions/d.txt"
  inc <- inclusion_summary(cfg, root = s$root)
  expect_setequal(inc$by_stratum$stratum, c("N_50-100m", "N_100-200m", "S_50-100m", "S_100-200m"))
  expect_equal(inc$by_stratum$n_kept[inc$by_stratum$stratum == "S_100-200m"], 0L)
  # no station kept in region S: its strata are not selected
  c1 <- s$sv$stations[s$sv$stations$design_station & s$sv$stations$stratum %in% c("SYN-C", "SYN-D"), ]
  writeLines(as.character(c1$serialnumber), file.path(s$root, "exclusions", "d.txt"))
  inc <- inclusion_summary(cfg, root = s$root)
  expect_setequal(inc$by_stratum$stratum, c("N_50-100m", "N_100-200m"))
  # a pattern that does not describe every stratum name is refused
  cfg$data$stratum_regions <- "^(N)_[0-9]+-[0-9]+m$"
  expect_error(inclusion_summary(cfg, root = s$root), "SX-STRATA-07|SX-INC-01")
  raw <- yaml::read_yaml(example_config())
  raw$data$stratum_regions <- "no group"
  expect_error(validate_config(raw), "CF-VAL-01")
})

test_that("a selected stratum that is not sampled stays in the estimates with an NA value", {
  skip_if_no_stox()
  s <- synthetic_root()
  d <- s$sv$stations[s$sv$stations$design_station & s$sv$stations$stratum == "SYN-D", ]
  dir.create(file.path(s$root, "exclusions"))
  writeLines(as.character(d$serialnumber), file.path(s$root, "exclusions", "d.txt"))
  cfg <- quick_config(2L)
  cfg$inclusion$exclude_stations_file <- "exclusions/d.txt"
  expect_message(
    res <- suppressWarnings(run_estimate(cfg, root = s$root, staging_dir = file.path(s$root, "st"))),
    "no station kept")
  e <- res$estimates[res$estimates$species_code == "SYN001" & res$estimates$quantity == "biomass", ]
  expect_true("SYN-D" %in% e$stratum)
  expect_true(is.na(e$value[e$stratum == "SYN-D"]))
  expect_false(anyNA(e$value[e$stratum != "SYN-D"]))
  expect_equal(res$support$n_stations[res$support$stratum == "SYN-D" & res$support$species_code == "SYN001"], 0L)
  # nothing is staged for the unsampled stratum
  if (!is.null(res$staged$files)) {
    est_file <- res$staged$files[grepl("estimates\\.csv$", res$staged$files)]
    if (length(est_file)) expect_false(any(grepl("SYN-D", readLines(est_file))))
  }
})

test_that("a selection that cannot be applied is refused", {
  s <- synthetic_root()
  x <- quick_config(); x$data$stratum_names <- c("SYN-A", "NOT-THERE")
  expect_error(inclusion_summary(x, root = s$root), "SX-STRATA-05|SX-INC-01")
  y <- quick_config(); y$data$stratum_names <- NULL; y$data$stratum_pattern <- "^nothing"
  expect_error(inclusion_summary(y, root = s$root), "SX-STRATA-03|SX-INC-01")
  raw <- yaml::read_yaml(example_config())
  raw$data$stratum_pattern <- "([unbalanced"
  expect_error(suppressWarnings(validate_config(raw)), "CF-VAL-01")
  raw$data$stratum_pattern <- NULL
  raw$data$stratum_pattern <- c("a", "b")
  expect_error(validate_config(raw), "CF-VAL-01")
})

test_that("a run estimates the selected strata only, from a WKT strata file", {
  skip_if_no_stox()
  s <- synthetic_root()
  wkt <- sf::st_as_text(sf::st_geometry(sf::st_transform(s$sv$strata, 4326)), digits = 15)
  writeLines(paste0(s$sv$strata$stratum, "\t", wkt), file.path(s$root, "strata", "synthetic.wkt"))
  cfg <- quick_config(2L)
  cfg$data$strata <- "strata/synthetic.wkt"
  cfg$data$stratum_label <- "stratum"
  cfg$data$stratum_names <- NULL
  cfg$data$stratum_pattern <- "^SYN-[AB]$"
  res <- suppressWarnings(run_estimate(cfg, root = s$root, staging_dir = file.path(s$root, "st")))
  est <- res$estimates
  expect_setequal(unique(est$stratum), c("SYN-A", "SYN-B", "total"))
  expect_setequal(unique(res$support$stratum), c("SYN-A", "SYN-B"))
  expect_equal(sum(res$support$n_stations[res$support$species_code == "SYN001"]), 27L)
  direct <- direct_biomass(s$sv, "SYN001")[c("SYN-A", "SYN-B")]
  b <- est[est$species_code == "SYN001" & est$quantity == "biomass", ]
  expect_equal(b$value[b$stratum %in% c("SYN-A", "SYN-B")][order(b$stratum[b$stratum %in% c("SYN-A", "SYN-B")])],
               as.numeric(direct), tolerance = 1e-6)
  expect_equal(b$value[b$stratum == "total"], sum(direct), tolerance = 1e-6)
  # StoX received the selected polygons only
  wkt_in_project <- readLines(file.path(res$project_path, "strata-selected.wkt"))
  expect_length(wkt_in_project, 2L)
  expect_false(any(grepl("SYN-C|SYN-D", wkt_in_project)))
})

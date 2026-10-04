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

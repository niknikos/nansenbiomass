# Sanitised conditions, local logging and data-path resolution. Synthetic only.

sentinel <- "SENTINEL_7f3a9c"

test_that("nb_abort raises a classed error carrying its code", {
  cnd <- expect_error(nb_abort("XX-TEST-01", "A fixed message."), class = "nansenbiomass_error")
  expect_equal(cnd$nb_code, "XX-TEST-01")
  expect_match(conditionMessage(cnd), "^XX-TEST-01: A fixed message\\.$")
})

test_that("third-party errors reach the log but not the console", {
  log_dir <- withr::local_tempdir()
  cnd <- expect_error(
    with_sanitised_errors(stop("value ", sentinel, " is bad"), log_dir, "XX-TEST-02", "It failed."),
    class = "nansenbiomass_error"
  )
  expect_equal(cnd$nb_code, "XX-TEST-02")
  expect_false(grepl(sentinel, conditionMessage(cnd)))
  log_text <- readLines(list.files(log_dir, full.names = TRUE))
  expect_true(any(grepl(sentinel, log_text)))
})

test_that("third-party warnings are logged and summarised without values", {
  log_dir <- withr::local_tempdir()
  w <- expect_warning(
    res <- with_sanitised_errors({
      warning("odd value ", sentinel)
      42
    }, log_dir, "XX-TEST-03", "Unused."),
    class = "nansenbiomass_warning"
  )
  expect_equal(res, 42)
  expect_false(grepl(sentinel, conditionMessage(w)))
  expect_true(any(grepl(sentinel, readLines(list.files(log_dir, full.names = TRUE)))))
})

test_that("sanitised errors from the package pass through unchanged", {
  log_dir <- withr::local_tempdir()
  cnd <- expect_error(
    with_sanitised_errors(nb_abort("XX-TEST-04", "Own error."), log_dir, "XX-TEST-05", "Other."),
    class = "nansenbiomass_error"
  )
  expect_equal(cnd$nb_code, "XX-TEST-04")
})

test_that("data_root() reports an unset, relative or missing root by code", {
  withr::local_envvar(NANSEN_DATA_ROOT = "")
  expect_equal(expect_error(data_root(), class = "nansenbiomass_error")$nb_code, "IO-ROOT-01")
  withr::local_envvar(NANSEN_DATA_ROOT = "relative/path")
  expect_equal(expect_error(data_root(), class = "nansenbiomass_error")$nb_code, "IO-ROOT-02")
  withr::local_envvar(NANSEN_DATA_ROOT = file.path(tempdir(), "does-not-exist-9d2"))
  expect_equal(expect_error(data_root(), class = "nansenbiomass_error")$nb_code, "IO-ROOT-03")
  root <- withr::local_tempdir()
  withr::local_envvar(NANSEN_DATA_ROOT = root)
  expect_equal(data_root(), normalizePath(root, winslash = "/"))
})

test_that("data paths cannot leave the root", {
  root <- withr::local_tempdir()
  code_of <- function(expr) expect_error(expr, class = "nansenbiomass_error")$nb_code
  expect_equal(code_of(resolve_data_path("/etc/passwd", root)), "IO-PATH-02")
  expect_equal(code_of(resolve_data_path("C:/data/biotic.xml", root)), "IO-PATH-02")
  expect_equal(code_of(resolve_data_path("../biotic.xml", root)), "IO-PATH-02")
  expect_equal(code_of(resolve_data_path("a/../../biotic.xml", root)), "IO-PATH-02")
  expect_equal(code_of(resolve_data_path("missing.xml", root)), "IO-PATH-03")
  expect_equal(code_of(resolve_data_path(c("a", "b"), root)), "IO-PATH-01")
})

test_that("a cloud session reads only from a root inside tempdir()", {
  withr::local_envvar(CLAUDE_CODE_REMOTE = "true")
  outside <- R.home()
  cnd <- expect_error(resolve_data_path("biotic.xml", outside), class = "nansenbiomass_error")
  expect_equal(cnd$nb_code, "IO-CLOUD-01")
  inside <- withr::local_tempdir()
  file.create(file.path(inside, "biotic.xml"))
  expect_true(file.exists(resolve_data_path("biotic.xml", inside)))

  withr::local_envvar(CLAUDE_CODE_REMOTE = "")
  cnd <- expect_error(resolve_data_path("biotic.xml", outside), class = "nansenbiomass_error")
  expect_equal(cnd$nb_code, "IO-PATH-03")
})

test_that("path_is_within handles paths that do not exist yet", {
  parent <- withr::local_tempdir()
  expect_true(path_is_within(file.path(parent, "new", "folder"), parent))
  expect_false(path_is_within(R.home(), parent))
})

test_that("field names that could carry identifiers are withheld", {
  expect_equal(safe_field_names(c("value", "stratum", "stn_90001", "-30.12", "a b", "cv2")),
               c("value", "stratum", "<name withheld>", "<name withheld>", "<name withheld>",
                 "cv2"))
})

test_that("find_awk_executables reports executables that exist", {
  found <- find_awk_executables()
  expect_type(found, "character")
  expect_true(all(
    names(found) %in% c("mawk", "gawk", "awk", "original-awk")
  ))
  expect_true(all(file.exists(found)))
  expect_true("awk" %in% names(found))
})

test_that("set_awk stores an executable quoted", {
  local_config()
  path <- unname(Sys.which("awk"))
  suppressMessages(set_awk(path))
  expect_equal(get_awk(), shQuote(path))

  executable <- local_awk_symlink("my (tools)")
  suppressMessages(set_awk(executable))
  expect_equal(get_awk(), shQuote(executable))
})

test_that("set_awk stores shell text verbatim where told not to quote", {
  local_config()
  text <- paste("LC_ALL=C", shQuote(Sys.which("awk")))
  suppressMessages(set_awk(text, quote = FALSE))
  expect_equal(get_awk(), text)
})

test_that("set_awk errors naming an executable that does not run", {
  local_config()
  expect_error(
    set_awk("this_executable_does_not_exist"),
    "this_executable_does_not_exist"
  )
  expect_null(.filterread_env$config$awk)
})

test_that("set_awk suggests quote = FALSE for shell text", {
  local_config()
  expect_error(
    set_awk("LC_ALL=C awk"),
    "shell text rather than one path or name, pass quote = FALSE"
  )
})

test_that("set_awk suggests quoting a path it was told not to quote", {
  local_config()
  executable <- local_awk_symlink("my (tools)")
  expect_error(set_awk(executable, quote = FALSE), "Quote any path")
})

test_that("set_awk errors on anything but one executable", {
  local_config()
  expect_error(set_awk(c("mawk", "gawk")), "one character string")
  expect_error(set_awk(NA_character_), "one character string")
  expect_error(set_awk(""), "one character string")
})

test_that("probe_awk finds only the divergences gawk shows", {
  skip_if_not(nzchar(Sys.which("gawk")))
  expect_equal(
    probe_awk("gawk --posix"),
    "a hex value such as `0x10` in a field, read as a number"
  )
  expect_equal(probe_awk("gawk"), character(0))
})

test_that("probe_awk finds the interval divergence in gawk --traditional", {
  skip_if_not(nzchar(Sys.which("gawk")))
  version_line <- system2("gawk", "--version", stdout = TRUE)[1L]
  version <- regmatches(version_line, regexpr("[0-9]+\\.[0-9]+", version_line))
  skip_if(
    package_version("5.2") <= package_version(version),
    "gawk 5.2 accepts an interval under --traditional"
  )
  expect_equal(
    probe_awk("gawk --traditional"),
    "an interval `{n}` in a regex pattern"
  )
})

test_that("probe_awk ignores what the shell writes to stderr", {
  awk_executable <- paste("echo noise >&2;", shQuote(Sys.which("awk")))
  expect_no_error(probe_awk(awk_executable))
})

test_that("probe_awk errors on output that does not answer each probe once", {
  expect_error(
    probe_awk("printf 'filterRead-probe\\n1\\n'; true"),
    "Could not run the awk executable"
  )
  expect_error(
    probe_awk("printf 'filterRead-probe\\n1\\n1\\n1\\n1\\n'; true"),
    "Could not run the awk executable"
  )
  expect_error(
    probe_awk("printf 'filterRead-probe\\n1\\n1\\nnoise\\n'; true"),
    "Could not run the awk executable"
  )
})

test_that("the default prefers mawk, else awk", {
  local_config()
  local_mocked_bindings(probe_awk = function(awk_executable) character(0))
  mawk_path <- "/usr/bin/mawk"
  local_mocked_bindings(
    Sys.which = function(names) setNames(mawk_path, names),
    .package = "base"
  )
  expect_equal(get_awk(), "mawk")

  mawk_path <- ""
  .filterread_env$awk <- NULL
  expect_equal(get_awk(), "awk")
})

test_that("get_awk resolves once a session", {
  local_config()
  probes <- 0L
  local_mocked_bindings(
    probe_awk = function(awk_executable) {
      probes <<- probes + 1L
      character(0)
    }
  )

  get_awk()
  get_awk()

  expect_equal(probes, 1L)
})

test_that("set_awk and get_awk report the divergences they find", {
  local_config()
  local_mocked_bindings(
    probe_awk = function(awk_executable) names(AWK_R_DIVERGENCE_PROBES)[1L]
  )
  expect_message(get_awk(), "an interval `{n}`", fixed = TRUE)

  path <- unname(Sys.which("awk"))
  expect_message(set_awk(path), "an interval `{n}`", fixed = TRUE)

  .filterread_env$awk <- NULL
  expect_message(get_awk(), shQuote(path), fixed = TRUE)
})

test_that("get_awk warns on an executable that no longer runs", {
  local_config()
  executable <- local_awk_symlink("tools")
  suppressMessages(set_awk(executable))
  unlink(executable)
  .filterread_env$awk <- NULL

  expect_warning(
    awk_executable <- get_awk(),
    "Could not run the awk executable"
  )
  expect_equal(awk_executable, shQuote(executable))
})

test_that("set_awk(NULL) returns to the default and keeps the other settings", {
  local_config()
  local_mocked_bindings(invalidate_chain_cache = function() NULL)
  default <- suppressMessages(get_awk())
  chain_path <- withr::local_tempdir()
  set_chain_path(chain_path)
  suppressMessages(set_awk(unname(Sys.which("awk"))))

  set_awk(NULL)

  expect_equal(suppressMessages(get_awk()), default)
  expect_equal(get_chain_path(), normalizePath(chain_path))
})

test_that("reset_config returns to the default", {
  local_config()
  default <- suppressMessages(get_awk())
  suppressMessages(set_awk(unname(Sys.which("awk"))))

  reset_config()

  expect_equal(suppressMessages(get_awk()), default)
})

test_that("reloading the configuration clears the session's awk executable", {
  local_config()
  suppressMessages(set_awk(unname(Sys.which("awk"))))
  saved <- shQuote(local_awk_symlink("saved"))
  config <- .filterread_env$config
  config$awk <- saved
  save_config(config)

  .filterread_env$config_loaded <- FALSE

  expect_equal(suppressMessages(get_awk()), saved)
})

test_that("a persisted executable survives the configuration reload", {
  local_config()
  path <- unname(Sys.which("awk"))
  suppressMessages(set_awk(path))

  .filterread_env$config_loaded <- FALSE

  expect_equal(suppressMessages(get_awk()), shQuote(path))
})

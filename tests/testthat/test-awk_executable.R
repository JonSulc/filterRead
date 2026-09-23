test_that("find_awk_executables reports executables that exist", {
  found <- find_awk_executables()
  expect_type(found, "character")
  expect_true(all(
    names(found) %in% c("mawk", "gawk", "awk", "original-awk")
  ))
  expect_true(all(file.exists(found)))
  expect_true("awk" %in% names(found))
})

test_that("get_awk returns the configured executable", {
  local_config()
  set_awk("custom-awk", validate = FALSE)
  expect_equal(get_awk(), "custom-awk")
})

test_that("get_awk finds an installed executable when none is configured", {
  local_config()
  expect_true(nzchar(Sys.which(get_awk())))
})

test_that("get_awk probes for an executable once a session", {
  local_config()
  probes <- 0L
  local_mocked_bindings(
    Sys.which = function(names) {
      probes <<- probes + 1L
      ""
    },
    .package = "base"
  )

  get_awk()
  get_awk()

  expect_equal(probes, 1L)
})

test_that("set_awk errors naming an executable it cannot find", {
  local_config()
  expect_error(
    set_awk("this_executable_does_not_exist"),
    "this_executable_does_not_exist"
  )
})

test_that("set_awk errors on anything but one executable", {
  local_config()
  expect_error(set_awk(c("mawk", "gawk")), "one character string")
  expect_error(set_awk(NA_character_), "one character string")
  expect_error(set_awk("", validate = FALSE), "one character string")
})

test_that("set_awk quotes a validated path holding a space", {
  local_config()
  spacey_path <- file.path(withr::local_tempdir(), "my tools")
  dir.create(spacey_path)
  executable <- file.path(spacey_path, "mawk")
  file.symlink(Sys.which("awk"), executable)

  set_awk(executable)

  expect_equal(get_awk(), shQuote(executable))
  expect_equal(
    system(paste(get_awk(), "'BEGIN { print 1 }'"), intern = TRUE),
    "1"
  )
})

test_that("set_awk(NULL) returns to automatic resolution", {
  local_config()
  automatic <- get_awk()
  set_awk("custom-awk", validate = FALSE)

  set_awk(NULL)

  expect_equal(get_awk(), automatic)
})

test_that("a persisted executable survives the configuration reload", {
  local_config()
  set_awk("custom-awk", validate = FALSE)

  .filterread_env$config_loaded <- FALSE

  expect_equal(get_awk(), "custom-awk")
})

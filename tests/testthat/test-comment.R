commented_file <- "~/rcp_storage/common//Users/sulc//data/gwas_summary_statistics//raw/ieu-b-110.vcf.gz"

test_that("Commented lines are correctly ignored", {
  expect_equal(
    new_file_interface(commented_file) |>
      head(),
    data.table::data.table(
      chr = 1,
      pos = 10177,
      ID = "rs1264289758",
      ref = "AC",
      alt = "A",
      QUAL = ".",
      FILTER = "PASS",
      INFO = "AF=0.601001",
      FORMAT = "ES:SE:LP:AF:ID",
      "ieu-b-110" = "-0.00481799:0.00310296:0.920819:0.601001:rs1264289758"
    )
  )
})

test_that("get_prefix_cmds works with default parameters", {
  finterface <- list(comment_prefix = "^##", drop_prefix = "^#")
  result <- get_prefix_cmds(finterface)
  expected <- c("grep -v '^##'", "awk '{gsub(/^#/, \"\"); print}'")
  expect_equal(result, expected)
})

test_that("get_prefix_cmds works with custom comment_prefix", {
  finterface <- list(comment_prefix = "^//", drop_prefix = "^#")
  result <- get_prefix_cmds(finterface)
  expected <- c("grep -v '^//'", "awk '{gsub(/^#/, \"\"); print}'")
  expect_equal(result, expected)
})

test_that("get_prefix_cmds works with custom drop_prefix", {
  finterface <- list(comment_prefix = "^##", drop_prefix = "^%")
  result <- get_prefix_cmds(finterface)
  expected <- c("grep -v '^##'", "awk '{gsub(/^%/, \"\"); print}'")
  expect_equal(result, expected)
})

test_that("get_prefix_cmds works with both custom parameters", {
  finterface <- list(comment_prefix = "^//", drop_prefix = "^%")
  result <- get_prefix_cmds(finterface)
  expected <- c("grep -v '^//'", "awk '{gsub(/^%/, \"\"); print}'")
  expect_equal(result, expected)
})

test_that("get_prefix_cmds handles NULL comment_prefix", {
  finterface <- list(comment_prefix = NULL, drop_prefix = "^#")
  result <- get_prefix_cmds(finterface)
  expected <- "awk '{gsub(/^#/, \"\"); print}'"
  expect_equal(result, expected)
})

test_that("get_prefix_cmds handles NULL drop_prefix", {
  finterface <- list(comment_prefix = "^##", drop_prefix = NULL)
  result <- get_prefix_cmds(finterface)
  expected <- "grep -v '^##'"
  expect_equal(result, expected)
})

test_that("get_prefix_cmds handles both NULL parameters", {
  finterface <- list(comment_prefix = NULL, drop_prefix = NULL)
  result <- get_prefix_cmds(finterface)
  expected <- NULL
  expect_equal(result, expected)
})

test_that("get_prefix_cmds handles special regex characters", {
  finterface <- list(comment_prefix = "\\*\\*", drop_prefix = "\\*")
  result <- get_prefix_cmds(finterface)
  expected <- c("grep -v '\\*\\*'", "awk '{gsub(/\\*/, \"\"); print}'")
  expect_equal(result, expected)
})

test_that("get_prefix_cmds handles empty strings", {
  finterface <- list(comment_prefix = "", drop_prefix = "")
  result <- get_prefix_cmds(finterface)
  expected <- c("grep -v ''", "awk '{gsub(//, \"\"); print}'")
  expect_equal(result, expected)
})

test_that("get_prefix_cmds handles complex patterns", {
  finterface <- list(comment_prefix = "^<!--", drop_prefix = "^\\/\\/")
  result <- get_prefix_cmds(finterface)
  expected <- c("grep -v '^<!--'", "awk '{gsub(/^\\/\\//, \"\"); print}'")
  expect_equal(result, expected)
})

test_that("get_prefix_cmds handles patterns with spaces", {
  finterface <- list(comment_prefix = "# TODO", drop_prefix = "# ")
  result <- get_prefix_cmds(finterface)
  expected <- c("grep -v '# TODO'", "awk '{gsub(/# /, \"\"); print}'")
  expect_equal(result, expected)
})

test_that("awk_load_file_cmd works with non-gzipped file, no nlines, only_read=FALSE", {
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = "^##",
    drop_prefix = "^#"
  )
  result <- awk_load_file_cmd(finterface)
  expected <- "grep -v '^##' test.txt | awk '{gsub(/^#/, \"\"); print}' | awk"
  expect_equal(result, expected)
})

test_that("awk_load_file_cmd works with non-gzipped file, no nlines, only_read=TRUE", {
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = "^##",
    drop_prefix = "^#"
  )
  result <- awk_load_file_cmd(finterface, only_read = TRUE)
  expected <- "grep -v '^##' test.txt | awk '{gsub(/^#/, \"\"); print}'"
  expect_equal(result, expected)
})

test_that("awk_load_file_cmd works with gzipped file, no nlines, only_read=FALSE", {
  finterface <- list(
    gzipped = TRUE,
    filename = "test.txt.gz",
    comment_prefix = "^##",
    drop_prefix = "^#"
  )
  result <- awk_load_file_cmd(finterface)
  expected <- "zcat test.txt.gz | grep -v '^##' | awk '{gsub(/^#/, \"\"); print}' | awk"
  expect_equal(result, expected)
})

test_that("awk_load_file_cmd works with gzipped file, no nlines, only_read=TRUE", {
  finterface <- list(
    gzipped = TRUE,
    filename = "test.txt.gz",
    comment_prefix = "^##",
    drop_prefix = "^#"
  )
  result <- awk_load_file_cmd(finterface, only_read = TRUE)
  expected <- "zcat test.txt.gz | grep -v '^##' | awk '{gsub(/^#/, \"\"); print}'"
  expect_equal(result, expected)
})

test_that("awk_load_file_cmd works with non-gzipped file, with nlines, only_read=FALSE", {
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = "^##",
    drop_prefix = "^#"
  )
  result <- awk_load_file_cmd(finterface, nlines = 100)
  expected <- "grep -v '^##' test.txt | awk '{gsub(/^#/, \"\"); print}' | head -n 100 | awk"
  expect_equal(result, expected)
})

test_that("awk_load_file_cmd works with non-gzipped file, with nlines, only_read=TRUE", {
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = "^##",
    drop_prefix = "^#"
  )
  result <- awk_load_file_cmd(finterface, nlines = 100, only_read = TRUE)
  expected <- "grep -v '^##' test.txt | awk '{gsub(/^#/, \"\"); print}' | head -n 100"
  expect_equal(result, expected)
})

test_that("awk_load_file_cmd works with gzipped file, with nlines", {
  finterface <- list(
    gzipped = TRUE,
    filename = "test.txt.gz",
    comment_prefix = "^##",
    drop_prefix = "^#"
  )
  result <- awk_load_file_cmd(finterface, nlines = 100)
  expected <- "bash -c \"head -n 100 < <(zcat test.txt.gz 2>/dev/null | grep -v '^##' | awk '{gsub(/^#/, \\\"\\\"); print}')\" | awk"
  expect_equal(result, expected)
})

test_that("awk_load_file_cmd works with custom prefix parameters", {
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = "^//",
    drop_prefix = "^%"
  )
  result <- awk_load_file_cmd(finterface)
  expected <- "grep -v '^//' test.txt | awk '{gsub(/^%/, \"\"); print}' | awk"
  expect_equal(result, expected)
})

test_that("awk_load_file_cmd handles NULL comment_prefix", {
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = NULL,
    drop_prefix = "^#"
  )
  result <- awk_load_file_cmd(finterface)
  expected <- "awk '{gsub(/^#/, \"\"); print}' test.txt | awk"
  expect_equal(result, expected)
})

test_that("awk_load_file_cmd handles NULL drop_prefix", {
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = "^##",
    drop_prefix = NULL
  )
  result <- awk_load_file_cmd(finterface)
  expected <- "grep -v '^##' test.txt | awk"
  expect_equal(result, expected)
})

test_that("awk_load_file_cmd handles both NULL prefix parameters", {
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = NULL,
    drop_prefix = NULL
  )
  result <- awk_load_file_cmd(finterface)
  expected <- "awk"
  expect_equal(result, expected)
})

test_that("awk_load_file_cmd handles both NULL prefix parameters with only_read=TRUE", {
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = NULL,
    drop_prefix = NULL
  )
  result <- awk_load_file_cmd(finterface, only_read = TRUE)
  expected <- "cat test.txt"
  expect_equal(result, expected)
})

test_that("detect_prefix_from_first_line detects comment prefix", {
  # Create a mock file interface for testing
  finterface <- list(gzipped = FALSE, filename = "test_file")

  # Mock the first line to return a line starting with ##
  with_mocked_bindings(
    system = function(cmd, intern = TRUE) {
      if (grepl("head -n 1", cmd)) {
        return("##fileformat=VCFv4.2")
      }
      return(character(0))
    },
    .package = "base",
    {
      result <- detect_comment_prefix(finterface)
      expected <- "^##"
      expect_equal(result, expected)
    }
  )
})

test_that("detect_prefix_from_first_line detects drop prefix", {
  # Create a mock file interface for testing
  finterface <- list(gzipped = FALSE, filename = "test_file")
  comment_prefix <- "^##"

  # Mock system call to return header line with # prefix
  with_mocked_bindings(
    system = function(cmd, intern = TRUE) {
      if (grepl("grep -v", cmd)) {
        return("#CHROM\tPOS\tID\tREF\tALT")
      }
      return(character(0))
    },
    .package = "base",
    {
      result <- detect_drop_prefix(finterface, comment_prefix)
      expected <- "^#"
      expect_equal(result, expected)
    }
  )
})

test_that("get_prefix_cmds uses detected prefixes from finterface", {
  finterface <- list(
    comment_prefix = "^##",
    drop_prefix = "^#"
  )
  result <- get_prefix_cmds(finterface)
  expected <- c("grep -v '^##'", "awk '{gsub(/^#/, \"\"); print}'")
  expect_equal(result, expected)
})

test_that("get_prefix_cmds handles NULL prefixes", {
  finterface <- list(
    comment_prefix = NULL,
    drop_prefix = NULL
  )
  result <- get_prefix_cmds(finterface)
  expected <- NULL
  expect_equal(result, expected)
})

test_that("get_prefix_cmds handles only comment_prefix", {
  finterface <- list(
    comment_prefix = "^//",
    drop_prefix = NULL
  )
  result <- get_prefix_cmds(finterface)
  expected <- "grep -v '^//'"
  expect_equal(result, expected)
})

test_that("get_prefix_cmds handles only drop_prefix", {
  finterface <- list(
    comment_prefix = NULL,
    drop_prefix = "^#"
  )
  result <- get_prefix_cmds(finterface)
  expected <- "awk '{gsub(/^#/, \"\"); print}'"
  expect_equal(result, expected)
})

# Create package-private environment for caching config
.filterread_env <- new.env(parent = emptyenv())
.filterread_env$config_loaded <- FALSE

# Build name constants
.valid_builds <- c("b36", "b37", "b38")

# Mapping from canonical build names to UCSC names
.build_to_ucsc <- c(b36 = "hg18", b37 = "hg19", b38 = "hg38")

# Mapping from synonyms to canonical build names
.build_synonyms <- c(
  # Canonical names
  b36 = "b36", b37 = "b37", b38 = "b38",
  # UCSC/hg names
  hg18 = "b36", hg19 = "b37", hg38 = "b38",
  # GRCh names
  GRCh36 = "b36", GRCh37 = "b37", GRCh38 = "b38",
  grch36 = "b36", grch37 = "b37", grch38 = "b38"
)

#' Normalize build name to canonical form
#'
#' Converts build name synonyms (hg38, GRCh38, etc.) to canonical form (b38).
#'
#' @param build Character string specifying genome build
#' @param allow_null Logical, whether to allow NULL input (returns NULL)
#' @return Canonical build name (b36, b37, or b38)
#' @export
#' @examples
#' normalize_build("hg38")
#' normalize_build("GRCh37")
#' normalize_build("b37")
normalize_build <- function(build, allow_null = TRUE) {
  if (is.null(build)) {
    if (allow_null) {
      return(NULL)
    }
    stop("Build cannot be NULL")
  }

  canonical <- .build_synonyms[build]
  if (is.na(canonical)) {
    stop(
      sprintf("Unknown build '%s'. ", build),
      "Valid builds: ", paste(names(.build_synonyms), collapse = ", ")
    )
  }
  unname(canonical)
}

# Create an active binding: this function is run when filterread_env$config is
# accessed
makeActiveBinding(
  "config",
  \(new) {
    if (!missing(new)) {
      .filterread_env$config_data <- new
      .filterread_env$config_loaded <- TRUE
      save_config(new)
    } else if (!.filterread_env$config_loaded) {
      .filterread_env$config_data <- load_config()
      .filterread_env$config_loaded <- TRUE
    }
    .filterread_env$config_data
  },
  env = .filterread_env
)

#' Get filterRead configuration directory
#' @keywords internal
get_config_dir <- function() {
  tools::R_user_dir("filterRead", "config")
}

#' Get path to config file
#' @keywords internal
get_config_file <- function() {
  file.path(get_config_dir(), "config.rds")
}

#' Load config
#' @keywords internal
get_config <- function(force = FALSE, warn = FALSE) {
  if (force) {
    # Load from disk (auto cached in env)
    load_config()
  }

  .filterread_env$config
}

#' Get package config
#' @keywords internal
load_config <- function() {
  config_file <- get_config_file()
  if (!file.exists(config_file)) {
    return(list())
  }
  config <- readRDS(config_file)

  invisible(config)
}

#' Save configuration to disk
#' @keywords internal
save_config <- function(config) {
  config_dir <- get_config_dir()
  if (!dir.exists(config_dir)) {
    dir.create(config_dir, recursive = TRUE)
  }
  saveRDS(config, get_config_file())
  invisible(config)
}

#' Set dbSNP reference file path
#' @param path Path to dbSNP reference files directory
#' @export
set_dbsnp_path <- function(
  path = tools::R_user_dir("filterRead", "data")
) {
  config <- .filterread_env$config

  config$dbsnp_path <- normalizePath(path)

  # Save to disk
  save_config(config)

  # Update cache
  .filterread_env$config <- config

  invisible(config)
}

#' Get dbSNP reference file path
#' @param warn Logical, whether to throw a warning if the path does not exist
#' @export
get_dbsnp_path <- function(warn = TRUE) {
  config <- .filterread_env$config

  if (is.null(config$dbsnp_path)) {
    default_path <- tools::R_user_dir("filterRead", "data") |>
      file.path("dbsnp")

    if (warn) {
      warning(
        sprintf(
          "dbSNP path not configured, using default (%s). ",
          default_path
        ),
        "Use set_dbsnp_path() to set it."
      )
    }

    config$dbsnp_path <- default_path
    .filterread_env$config <- config
  }
  config$dbsnp_path
}

#' Set chain files path
#' @param path Path to directory containing UCSC chain files
#' @export
set_chain_path <- function(
  path = tools::R_user_dir("filterRead", "data")
) {
  config <- .filterread_env$config

  config$chain_path <- normalizePath(path, mustWork = FALSE)

  # Save to disk
  save_config(config)

  # Update cache
  .filterread_env$config <- config

  invisible(config)
}

#' Get chain files path
#' @param warn Logical, whether to throw a warning if the path is not configured
#' @export
get_chain_path <- function(warn = TRUE) {
  config <- .filterread_env$config

  if (is.null(config$chain_path)) {
    default_path <- tools::R_user_dir("filterRead", "data") |>
      file.path("ucsc")

    if (warn) {
      warning(
        sprintf(
          "Chain file path not configured, using default (%s). ",
          default_path
        ),
        "Use set_chain_path() to set it."
      )
    }

    config$chain_path <- default_path
    .filterread_env$config <- config
  }
  config$chain_path
}

#' Reset filterRead configuration
#' @export
reset_config <- function() {
  config_file <- get_config_file()
  if (file.exists(config_file)) {
    file.remove(config_file)
  }
  .filterread_env$config_loaded <- FALSE
  invisible()
}

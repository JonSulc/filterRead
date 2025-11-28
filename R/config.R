# Create package-private environment for caching config
.filterread_env <- new.env(parent = emptyenv())
.filterread_env$config_loaded <- FALSE

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

#' Reset filterRead configuration
#' @export
reset_config <- function() {
  config_file <- get_config_file()
  if (file.exists(config_file)) {
    file.remove(config_file)
  }
  invisible()
}

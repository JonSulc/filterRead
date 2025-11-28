#' @importFrom purrr walk

# dbSNP metadata structure
.dbsnp_registry <- list(
  base_url = "https://ftp.ncbi.nih.gov/snp/organisms",
  builds = list(
    b37 = list(
      path = "human_9606_b151_GRCh37p13/VCF",
      files = list(
        common = list(
          remote = "00-common_all.vcf.gz",
          local = "00-common_all_b37.vcf.gz"
        ),
        all = list(
          remote = "00-All.vcf.gz",
          local = "00-All_b37.vcf.gz"
        )
      )
    ),
    b38 = list(
      path = "human_9606_b151_GRCh38p7/VCF",
      files = list(
        common = list(
          remote = "00-common_all.vcf.gz",
          local = "00-common_all_b38.vcf.gz"
        ),
        all = list(
          remote = "00-All.vcf.gz",
          local = "00-All_b38.vcf.gz"
        )
      )
    )
  )
)

#' Get dbSNP file URL
#' @param build Build version ("b37" or "b38")
#' @param type Type of SNP reference set (common = .01 <= MAF)
#'   ("common" or "all")
#' @keywords internal
get_dbsnp_url <- function(
  build = c("b37", "b38"),
  type = c("common", "all")
) {
  build <- match.arg(build, several.ok = TRUE)
  type <- match.arg(type)
  build_info <- .dbsnp_registry$builds[[build]]
  file_info <- build_info$files[[type]]

  file.path(
    .dbsnp_registry$base_url,
    build_info$path,
    file_info$remote
  )
}

#' Get dbSNP local filename
#' @param build Build version ("b37" or "b38")
#' @param type Type of SNP reference set (common = .01 <= MAF)
#'   ("common" or "all")
#' @keywords internal
get_dbsnp_filename <- function(
  build = c("b37", "b38"),
  type = c("common", "all")
) {
  build <- match.arg(build, several.ok = TRUE)
  type <- match.arg(type)
  if (1 < length(build)) {
    return(sapply(build, get_dbsnp_filename, type = type))
  }
  build_info <- .dbsnp_registry$builds[[build]]
  file_info <- build_info$files[[type]]

  file_info$local
}

#' Get all dbSNP files to download
#' @param build Character vector of builds ("b37", "b38")
#' @param type Type of SNP reference set (common = .01 <= MAF)
#'   ("common" or "all")
#' @keywords internal
get_dbsnp_downloads <- function(
  build = c("b37", "b38"),
  type = c("common", "all")
) {
  build <- match.arg(build, several.ok = TRUE)
  type <- match.arg(type)
  if (1 < length(build)) {
    return(
      setNames(nm = build) |>
        lapply(get_dbsnp_downloads, type = type) |>
        unlist(recursive = FALSE)
    )
  }

  url <- get_dbsnp_url(build, type)
  filename <- get_dbsnp_filename(build, type)

  list(
    vcf = list(
      url = url,
      filename = filename,
      md5_file = list(
        url = paste0(url, ".md5"),
        filename = paste0(filename, ".md5")
      )
    ),
    index = list(
      url = paste0(url, ".tbi"),
      filename = paste0(filename, ".tbi")
    )
  )
}

#' Verify MD5 checksum of a file
#' @param file_path Path to file to verify
#' @param md5_path Path to MD5 checksum file
#' @keywords internal
md5_matches <- function(
  file_path,
  md5_path = paste0(file_path, ".md5")
) {
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }

  if (!file.exists(md5_path)) {
    warning("MD5 file does not exist: ", md5_path)
    return(FALSE)
  }

  # Read MD5 file (format: "md5sum  filename")
  md5_line <- readLines(md5_path, n = 1, warn = FALSE)
  expected_md5 <- strsplit(md5_line, "\\s+")[[1]][1]

  # Calculate actual MD5
  actual_md5 <- tools::md5sum(file_path) |>
    unname()

  if (tolower(actual_md5) == tolower(expected_md5)) {
    message("  MD5 checksum verified")
    TRUE
  } else {
    warning(
      sprintf(
        "MD5 mismatch for %s\n  Expected: %s\n  Actual: %s",
        basename(file_path),
        expected_md5,
        actual_md5
      ),
      call. = FALSE
    )
    FALSE
  }
}

#' Download a file with retry logic
#' @param url URL to download from
#' @param dest_file Destination file path
#' @param max_tries Maximum number of retry attempts
#' @param timeout Timeout in seconds for each attempt
#' @keywords internal
download_with_retry <- function(
  url,
  dest_file,
  md5_file = NULL,
  max_tries = 3,
  timeout = 3600
) {
  if (!is.null(md5_file)) {
    # If MD5 file is provided, download it first
    md5_dest_file <- file.path(
      dirname(dest_file),
      md5_file$filename
    )
    download_with_retry(
      url = md5_file$url,
      dest_file = md5_dest_file,
      md5_file = NULL,
      max_tries = max_tries,
      timeout = timeout
    )
  }

  # Try curl package first (most reliable for large files)
  use_curl <- requireNamespace("curl", quietly = TRUE)

  for (attempt in seq_len(max_tries)) {
    if (attempt > 1) {
      message(
        "  Attempt ", attempt, "/", max_tries
      )
      Sys.sleep(2)
    }

    tryCatch(
      {
        if (use_curl) {
          # curl package with resume capability
          handle <- curl::new_handle(
            timeout = timeout,
            connecttimeout = 300,
            low_speed_time = 60,
            low_speed_limit = 1000
          )

          # Enable resume if partial file exists
          if (file.exists(dest_file)) {
            size <- file.size(dest_file)
            message("  Resuming download from byte ", size)
            curl::handle_setopt(handle, resume_from = size)
          }

          curl::curl_download(
            url,
            dest_file,
            handle = handle,
            quiet = FALSE
          )
        } else {
          # Fallback to download.file with libcurl
          old_timeout <- getOption("timeout")
          options(timeout = timeout)
          on.exit(options(timeout = old_timeout), add = TRUE)

          method <- if (capabilities("libcurl")) {
            "libcurl"
          } else {
            "auto"
          }

          download.file(
            url,
            dest_file,
            mode = "wb",
            method = method,
            quiet = FALSE
          )
        }

        # Verify file was created and has content
        if (!file.exists(dest_file)) {
          stop("Downloaded file is missing")
        } else if (file.size(dest_file) == 0) {
          stop("Downloaded file is empty")
        } else if (!is.null(md5_file)) {
          if (!md5_matches(dest_file, md5_dest_file)) {
            stop("MD5 sum doesn't match expected")
          }
        }
        return()
      },
      error = function(e) {
        # Only remove file if using download.file
        # (curl with resume needs the partial file)
        if (!use_curl && file.exists(dest_file)) {
          file.remove(dest_file)
        }
        if (attempt == max_tries) {
          stop(e$message)
        }
      }
    )
  }
}

#' Setup dbSNP reference files
#' @param download Logical, download files if missing
#' @param build Character vector of builds to download
#'   (default: c("b37", "b38"))
#' @param type Type of SNP reference set (common = .01 <= MAF)
#'   ("common" or "all")
#' @param timeout Timeout in seconds for each download (default: 3600)
#' @param max_tries Maximum number of retry attempts per file
#'   (default: 3)
#' @export
setup_dbsnp <- function(
  build = c("b37", "b38"),
  type = c("common", "all"),
  max_tries = 3,
  timeout = 3600
) {
  build <- match.arg(build, several.ok = TRUE)
  type <- match.arg(type)

  path <- get_dbsnp_path(warn = FALSE)

  if (!dir.exists(path)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  }

  downloads <- get_dbsnp_downloads(build, type)

  message(
    sprintf(
      "Downloading %d dbSNP files to: %s",
      length(downloads),
      path
    )
  )

  failed_downloads <- character()

  for (dl in downloads) {
    dest_file <- file.path(path, dl$filename)

    if (file.exists(dest_file)) {
      if (is.null(dl$md5_file)) {
        message("Skipping existing file: ", dl$filename)
        next
      }

      md5_dest_file <- file.path(path, dl$md5_file$filename)
      if (!file.exists(md5_dest_file)) {
        download_with_retry(
          url = dl$md5_file$url,
          dest_file = md5_dest_file,
          max_tries = max_tries,
          timeout = timeout
        )
      }
      message("Verifying existing file: ", dl$filename)

      # Skip download if file exists and passes MD5 verification
      if (md5_matches(dest_file, md5_dest_file)) {
        next
      } else {
        message("  Redownloading due to MD5 mismatch")
        file.remove(dest_file)
      }
    }

    message(
      sprintf("Downloading: %s", dl$filename)
    )


    success <- tryCatch(
      {
        download_with_retry(
          dl$url,
          dest_file,
          md5_file = dl$md5_file,
          max_tries = max_tries,
          timeout = timeout
        )
        message("  Complete")

        TRUE
      },
      error = function(e) {
        warning(
          sprintf(
            "Failed to download %s after %d attempts: %s",
            dl$filename,
            max_tries,
            e$message
          ),
          call. = FALSE
        )
        FALSE
      }
    )

    if (!success) {
      failed_downloads <- c(failed_downloads, dl$filename)
    }
  }

  if (length(failed_downloads) > 0) {
    message("\nFailed downloads:")
    for (f in failed_downloads) {
      message(sprintf("  - %s", f))
    }
    message(
      "\nYou can retry by running setup_dbsnp() again, ",
      "or download manually from:"
    )
    message(.dbsnp_registry$base_url)
  } else {
    message("\nAll files downloaded successfully!")
  }

  invisible(path)
}

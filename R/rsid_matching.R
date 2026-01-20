# =============================================================================
# RSID Matching and dbSNP Integration
# =============================================================================
# This file consolidates all RSID-related functionality for files that use
# RSID-based indexing instead of chromosome/position coordinates. These
# functions handle dbSNP lookup, tabix queries, and awk code generation for
# RSID matching.

#' Check if file requires RSID-based matching
#'
#' Determines if file has RSID column but lacks chr/pos columns,
#' requiring dbSNP lookup for genomic region filtering.
#'
#' @param finterface File interface object
#' @param force If TRUE, recalculate even if cached
#'
#' @return TRUE if file needs RSID matching for genomic queries
#' @keywords internal
needs_rsid_matching <- function(
  finterface,
  force = FALSE
) {
  # Return cached value if available
  if (!force & "needs_rsid_matching" %in% names(finterface)) {
    return(finterface$needs_rsid_matching)
  }
  fcolumn_names <- finterface$column_info[
    !is.na(input_name),
    standard_name
  ]
  # Need RSID matching if: has RSID but missing chr AND/OR pos
  "rsid" %in% fcolumn_names &
    !all(c("chr", "pos") %in% fcolumn_names)
}
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
  type = c("common", "all"),
  full_path = FALSE
) {
  build <- match.arg(build, several.ok = TRUE)
  type <- match.arg(type)
  if (1 < length(build)) {
    return(
      sapply(build, get_dbsnp_filename, type = type, full_path = full_path)
    )
  }
  build_info <- .dbsnp_registry$builds[[build]]
  file_info <- build_info$files[[type]]

  if (!full_path) {
    return(file_info$local)
  }
  file.path(get_dbsnp_path(), file_info$local)
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
#' @param md5_file Optional list containing the 'filename' and 'url' of the MD5
#'   file to check the download
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

#' Download dbSNP reference files to the path defined by get_dbsnp_path()
#' @param build Character vector of builds to download
#'   (default: c("b37", "b38"))
#' @param type Type of SNP reference set (common = .01 <= MAF)
#'   ("common" or "all")
#' @param max_tries Maximum number of retry attempts per file
#'   (default: 3)
#' @param timeout Timeout in seconds for each download (default: 3600)
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
tabix_colnames <- c(
  "chr",
  "pos",
  "rsid",
  "ref",
  "alt",
  "qual",
  "filter",
  "info"
)


get_tabix_process_substitution <- function(
  chr,
  start,
  end,
  dbsnp_filename
) {
  stopifnot(length(start) == length(end))
  stopifnot(length(chr) == 1 | length(chr) == length(start))
  regions <- sprintf(
    sprintf(
      "%s%s",
      drop_chr_prefix(chr),
      ifelse(is.na(start) & is.na(end),
        "",
        sprintf(
          ":%s-%s",
          ifelse(is.na(start) | is.infinite(start), "", start),
          ifelse(is.na(end) | is.infinite(end), "", end)
        )
      )
    )
  )
  sprintf(
    "<(tabix %s %s)",
    dbsnp_filename,
    paste(regions, collapse = " ")
  )
}
or_filter_condition_rsid <- function(
  fcondition1,
  fcondition2
) {
  list(condition = unname(c(fcondition1, fcondition2)))
}

#' Wrap condition in parentheses
#'
#' Adds parentheses around a condition for correct operator precedence.
#'
#' @param fcondition Condition list with `condition` element
#' @return Same list with condition wrapped in parentheses
#' @keywords internal
lp_filter_condition <- function(
  fcondition
) {
  fcondition$condition <- sprintf("(%s)", fcondition$condition)
  fcondition
}

#' Generic condition combiner
#'
#' Combines two condition lists with a specified operator, merging their
#' variable_arrays and additional_files.
#'
#' Convert filter condition with RSID matching to awk data.table
#'
#' Handles files that use RSID-based indexing (files without chr/pos columns
#' that require dbSNP lookup). For RSID files with genomic conditions, uses
#' tabix to query dbSNP and creates awk code to match RSIDs.
#'
#' @param fcondition A filter_condition object
#' @param rsid_bash_index Awk column ref for RSID column (e.g., "$3")
#' @param index Counter for multiple genomic blocks (for unique array names)
#'
#' @return data.table with columns:
#'   - index: block index for unique naming
#'   - awk_code_block: awk code to load RSID array from dbSNP
#'   - print_prefix: awk code to prepend chr/pos to output
#'   - process_substitution: tabix query command
#'   - rsid_condition: awk condition to check RSID membership
#'   - condition: additional non-genomic conditions
#'   - variable_arrays, additional_files: from eval_fcondition_w_gregions
#' @keywords internal
fcondition_and_rsid_to_awk <- function(
  fcondition,
  rsid_bash_index = get_file_interface(fcondition)$column_info[
    "rsid",
    bash_index,
    on = "name"
  ],
  index = 0
) {
  # RSID-based indexing should only be applied if required, i.e.,
  # if the file is RSID-indexed and there is a genomic condition applied
  rsid_indexed <- needs_rsid_matching(get_file_interface(fcondition))
  full_genome_condition <- is_full_genome(
    genomic_regions(fcondition, recursive = TRUE)
  )
  no_genome_condition <- has_no_gregions(fcondition)

  # Standard path: no RSID matching needed
  if (!rsid_indexed || full_genome_condition || no_genome_condition) {
    return(eval_fcondition_w_gregions(
      fcondition,
      finterface = get_file_interface(fcondition)
    ) |>
      data.table::as.data.table())
  }

  # Recursive case: handle OR of multiple genomic blocks
  if (!is_single_genomic_block(fcondition)) {
    stopifnot(fcondition[[1]] == as.symbol("or_filter_condition"))
    return(rbind(
      fcondition_and_rsid_to_awk(
        fcondition[[2]],
        rsid_bash_index,
        index = index
      ),
      fcondition_and_rsid_to_awk(
        fcondition[[3]],
        rsid_bash_index,
        index = index + 1
      )
    ))
  }

  # Single genomic block with RSID matching:
  # 1. Query dbSNP via tabix for RSIDs in the region
  # 2. Generate awk code to load RSIDs into array
  # 3. Generate condition to filter by RSID membership
  genomic_regions(fcondition)[
    ,
    .(
      index = index,
      # awk code block loads RSID -> chr:pos mapping from dbSNP tabix output
      # NR == FNR detects when reading the first file (dbSNP output)
      awk_code_block = paste0(
        "if (NR == FNR%s) {\n",
        "  rsid%i[$3]=$1 OFS $2\n",  # rsid array: RSID -> "chr\tpos"
        "}"
      ) |>
        sprintf(
          ifelse(index == 0,
            "",
            paste0(" + ", index)
          ),
          index
        ),
      # Prefix to prepend chr/pos to output lines
      print_prefix = sprintf(
        "rsid%i[%s] OFS ",
        index,
        rsid_bash_index
      ),
      # Tabix query as bash process substitution
      process_substitution = get_tabix_process_substitution(
        chr = chr,
        start = start,
        end = end,
        dbsnp_filename = get_dbsnp_filename(
          build = build(get_file_interface(fcondition)),
          full_path = TRUE
        )
      ),
      # Condition: only print if RSID exists in loaded array
      rsid_condition = sprintf("%s in rsid%i", rsid_bash_index, index)
    )
  ] |>
    cbind(data.table::as.data.table(
      eval_fcondition_w_gregions(
        fcondition,
        finterface = get_file_interface(fcondition)
      )
    ))
}

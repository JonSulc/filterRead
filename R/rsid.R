#' @import data.table

dbsnp_file <- "~/rcp_storage/common/Users/abadreddine/data/dbSNP/GCF_000001405.40.gz"

chromosome_names <- paste("tabix -l", dbsnp_file) |>
  system(intern = TRUE)
chromosome_names <- chromosome_names[grepl("^NC_0+[0-2][0-9][.][0-9]+$", chromosome_names)]
chromosome_names <- chromosome_names |>
  setNames(sub("^NC_0+([1-2]?[0-9])[.][0-9]+$", "\\1", chromosome_names))

if ("23" %in% names(chromosome_names))
  names(chromosome_names)[names(chromosome_names) == "23"] <- "x"
if ("24" %in% names(chromosome_names))
  names(chromosome_names)[names(chromosome_names) == "24"] <- "y"

tabix_colnames <- c("chr", "pos", "rsid", "ref", "alt", "qual", "filter", "info")

awk_get_rsid_list <- function(
  chr,
  start,
  stop,
  rsid_bash_index,
  chr_names = chromosome_names
) {
  process_substitution <- get_tabix_process_substitution(
    chr   = chr,
    start = start,
    stop  = stop
  )
  print_prefix <- sprintf("tabix[%s] OFS ", rsid_bash_index)
  awk_code_block <- "if (NR == FNR) {
    tabix[$3]=$1 OFS $2
  }"
}

get_tabix_process_substitution <- function(
  chr,
  start,
  stop,
  dbsnp_filename = dbsnp_file,
  chr_names = chromosome_names
) {
  chr_name <- chr_names[tolower(chr)]
  stopifnot(length(start) == length(stop))
  stopifnot(length(chr_name) == 1 | length(chr_name) == length(start))
  regions <- sprintf(
    "%s:%i-%i",
    chr_name,
    start, stop
  )
  sprintf(
    "<(tabix %s %s)",
    dbsnp_filename,
    paste(regions, collapse = " ")
  )
}

# TODO Current (partial) implementation requires a genomic range, see if
#  by condition is possible, otherwise implement in summary_stats

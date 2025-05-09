#' @import data.table

dbsnp_file <- "~/rcp_storage/common/Users/abadreddine/data/dbSNP/GCF_000001405.40.gz"

chromosome_names <- paste("tabix -l", dbsnp_file) |>
  system(intern = TRUE)
chromosome_names <- chromosome_names[grepl("^NC_", chromosome_names)]
chromosome_names <- chromosome_names |>
  setNames(sub("^NC_[0-9]+[.][0-9]+$", "MT",
               sub("^NC_0+([1-2]?[0-9])[.][0-9]+$", "\\1", chromosome_names)))

if ("23" %in% names(chromosome_names))
  names(chromosome_names)[names(chromosome_names) == "23"] <- "X"
if ("24" %in% names(chromosome_names))
  names(chromosome_names)[names(chromosome_names) == "24"] <- "Y"

tabix_colnames <- c("chr", "pos", "rsid", "ref", "alt", "qual", "filter", "info")

is_single_genomic_range_block <- function(
  fcondition
) {
  # Determine whether all non-genomic conditions apply to the same genomic range
  if (!is.call(fcondition)) return(TRUE)
  if (is_and_block(fcondition)) return(TRUE)
  if (!has_non_genomic_condition(fcondition)) return(TRUE)
  if (!has_chromosome_condition(fcondition)
      & !has_position_condition(fcondition)) return(TRUE)
  if (fcondition[[1]] == as.symbol("or_filter_condition")) {
    return(
      (is_single_genomic_range_block(fcondition[[2]])
       & !has_genomic_condition(fcondition[[3]]))
      | (is_single_genomic_range_block(fcondition[[3]])
         & !has_genomic_condition(fcondition[[2]]))
    )
  }
  FALSE
}

# awk_get_rsid_list <- function(
#   fcondition,
#   rsid_bash_index,
#   index = 0,
#   chr_names = chromosome_names
# ) {
#   genomic_range <- attr(fcondition, "genomic_range")
#   if (is.null(genomic_range) & is_single_genomic_range_block(fcondition))
#     return()
#
#   if (!is_single_genomic_range_block(fcondition)) {
#     stopifnot(fcondition[[1]] == as.symbol("or_filter_condition"))
#     to_return <- awk_get_rsid_list(fcondition[[2]],
#                                    rsid_bash_index,
#                                    index     = index,
#                                    chr_names = chr_names)
#     return(
#       rbind(to_return,
#             awk_get_rsid_list(fcondition[[3]],
#                               rsid_bash_index,
#                               index     = max(to_return$index) + 1,
#                               chr_names = chr_names))
#     )
#   }
#
#   genomic_range[
#     ,
#     .(
#       index                = index,
#       awk_code_block       = paste0("if (NR == FNR%s) {\n",
#                                     "    rsid%i[$3]=$1 OFS $2\n",
#                                     "  }") |>
#         sprintf(ifelse(index == 0,
#                        "",
#                        paste0(" + ", index)),
#                 index),
#       print_prefix         = sprintf("rsid%i[%s] OFS ",
#                                      index,
#                                      rsid_bash_index),
#       process_substitution = get_tabix_process_substitution(
#         chr   = chr,
#         start = start,
#         end   = end,
#         chr_names = chr_names
#       ),
#       rsid_condition       = sprintf("%s in rsid%i", rsid_bash_index, index)
#     )
#   ]
# }

get_tabix_process_substitution <- function(
  chr,
  start,
  end,
  dbsnp_filename = dbsnp_file,
  chr_names = chromosome_names
) {
  chr_name <- chr_names[toupper(chr)]
  stopifnot(length(start) == length(end))
  stopifnot(length(chr_name) == 1 | length(chr_name) == length(start))
  regions <- sprintf(
    sprintf("%s%s",
            chr_name,
            ifelse(is.na(start) & is.na(end),
                   "",
                   sprintf(":%s-%s",
                           ifelse(is.na(start) | is.infinite(start), "", start),
                           ifelse(is.na(end) | is.infinite(end), "", end))))
  )
  sprintf(
    "<(tabix %s %s)",
    dbsnp_filename,
    paste(regions, collapse = " ")
  )
}

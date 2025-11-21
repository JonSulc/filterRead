#' @import data.table

# Dictionary to standardize the column names across various sources
summary_stats_column_names <- list(
  variant_id = c("variant_id"),
  chr = c(
    "chr",
    "SNPChr",
    "CHROM",
    "chromosome",
    "Chromosome",
    "CHR",
    "ISmet_chr",
    "Chr",
    "Chromsome", # Yup...
    "chrom",
    "WMHmet_chr",
    "CHROMOSOME",
    "BImet_chr",
    "#CHROM"
  ),
  pos = c(
    "pos",
    "SNPPos",
    "position",
    "GENPOS",
    "Position",
    "POS",
    "ISmet_position",
    "BP",
    "Pos",
    "Pos_b37",
    "MapInfo",
    "Physical_Location",
    "PhysPos",
    "chromStart",
    "WMHmet_position",
    "POSITION",
    "BImet_pos"
  ),
  ref = c(
    "ref",
    "OtherAllele",
    "other_allele.outcome",
    "other_allele.exposure",
    "nea",
    "ALLELE0",
    "NEA",
    "Noncoded_allele",
    "Non_coded_allele",
    "noncoded_all",
    "Other_allele",
    "WMHmet_NonEffAllele",
    "BImet_ref.noncoded",
    "REF"
  ),
  alt = c(
    "alt",
    "AssessedAllele",
    "effect_allele.outcome",
    "effect_allele.exposure",
    "ea",
    "ALLELE1",
    "SNPEffectAllele",
    "Effect_allele",
    "Effect allele (EA)",
    "EA",
    "effallele",
    "Coded_allele",
    "coded_all",
    "ALT"
  ),
  effect = c(
    "effect",
    "slope",
    "beta",
    "BETA",
    "MetaBeta",
    "b",
    "Estimate_Effect",
    "genoxt2d.b", # MVP summary statistics, T2D complications
    "Effect",
    "Est",
    "Estimate_effect",
    "Beta"
  ),
  pval = c(
    "pval",
    "pval_nominal",
    "Pvalue",
    "p",
    "MetaP",
    "P_value",
    "P value",
    "genoxt2d.p", # MVP summary statistics, T2D complications
    "PVAL",
    "P",
    "P.value",
    "ISmet_pvalue",
    "WMHmet_P",
    "BImet_meta.p",
    "P-value"
  ),
  log10p = c(
    "LOG10P",
    "LOG10_P"
  ),
  effect_se = c(
    "effect_se",
    "slope_se",
    "se",
    "SE",
    "MetaSE",
    "b_se",
    "StdErr"
  ),
  zscore = c(
    "Zscore",
    "Test statistic",
    "WMHmet_ZScore"
  ),
  odds_ratio = c("OR"),
  allele1 = c(
    "Allele_1",
    "Allele 1",
    "Allele1",
    "a1",
    "A1",
    "allele1",
    "ISmet_allele1"
  ),
  allele2 = c(
    "Allele_2",
    "Allele 2",
    "Allele2",
    "a2",
    "A2",
    "allele2",
    "ISmet_allele2"
  )
)

# In some CHARGE files, there is no position column but it is encoded in other
# columns (in ~/Databases/CHARGE/authorized_data/deflated_organized/submission)
# The name of the list element indicates the column name in the file
summary_stats_encoded_columns <- list(
  MarkerName = list(
    # e.g., sub20200527/accumbens_eur_z_ldsc_unrestricted_NG05SEP19.out
    list(
      pattern = "%s:%s",
      regex = "^([^:]{1,2}):([0-9]+)$",
      encoded_names = list(c("chr", "pos")),
      delimiter = ":"
    ),

    # e.g., sub20180725/SVE.european.results.metal.csv
    list(
      pattern = "%s-c%s:%s-123",
      regex = "^(b3[6-8])-c([^:]{1,2}):([0-9]+)-[0-9]+$",
      encoded_names = list(c("build", "chr", "pos")),
      delimiter = "-c|:|-"
    ),

    # e.g., sub20190511/invnormFT4_overall_150611_invvar1.txt-QCfiltered_GC.rsid.txt
    list(
      pattern = "%s:%s:SNP",
      regex = "^([^:]+):([0-9]+):[^:]+$",
      encoded_names = list(c("chr", "pos")),
      delimiter = ":"
    ),

    # e.g., sub20201231_01/BP-ICE_EUR_SBP_transformed_15-04-2020.txt
    list(
      pattern = "%s:%i:a:c",
      regex = "^([^:]{1,2}):([0-9]+):[a-zA-Z]+:[a-zA-Z]+$",
      encoded_names = list(c("chr", "pos")),
      delimiter = ":"
    )
  ),
  chr_colon_pos = list(
    # e.g., sub20180818/vv.results.metal.txt
    list(
      pattern = "%s:%s",
      regex = "^([^:]{1,2}):([0-9]+)$",
      encoded_names = list(c("chr", "pos")),
      delimiter = ":"
    )
  ),
  Chr_Pos = list(
    # e.g., sub20200523/FVIIactivity_EA_AA_trans.csv
    list(
      pattern = "%s:%s",
      regex = "^([^:]{1,2}):([0-9]+)(?::ID)?$",
      encoded_names = list(c("chr", "pos")),
      delimiter = ":"
    )
  )
)
summary_stats_standard_names_dt <- data.table::data.table(
  standard_name = names(summary_stats_column_names)
)[
  ,
  .(input_name = summary_stats_column_names[[standard_name]]),
  by = standard_name
]
summary_stats_standard_names_dt[
  standard_name == "chr",
  possible_prefixes := .("chr")
][]

summary_stats_encoded_columns_dt <- data.table::data.table(
  input_name    = names(summary_stats_encoded_columns),
  standard_name = names(summary_stats_encoded_columns)
)[
  ,
  data.table::rbindlist(summary_stats_encoded_columns[[input_name]]),
  by = input_name
]

summary_stats_rsid_dt <- data.table::data.table(
  input_name    = c("rsid", "id", "MarkerName", "ID"),
  standard_name = "rsid",
  pattern       = "rs%s",
  regex         = "^(rs[0-9]+)$"
)

summary_stats_standard_names_dt <- list(
  summary_stats_standard_names_dt,
  summary_stats_encoded_columns_dt,
  summary_stats_rsid_dt
) |>
  data.table::rbindlist(fill = TRUE, use.names = TRUE)



# In other files (e.g., sub20171222/appendicularleanmass.results.metal.txt)
# there are only rsids, currently unsupported

# TODO Check specific files to expand handled formats
# Ideally to support:
#   sub20181231/fgf23gwas.model1.txt
#     -> Has quoted rsids
#   sub20181227/Plaque_meta_032218.csv
#     -> Yields duplicate column names and fails when filtering.
#   sub20190101/1KG_CRP_GWAS_AJHG_2018.txt
#     -> data.table::fread (in head) throws a warning because only 5 column
#        names but 6 columns are returned
# Absolute nightmare, not supported, no support intended:
#   MarkerName contains a combination of rsid, chr:pos:I/D
#   and whatever TF 72.6021656097915 is...:
#   sub20200329/Hispanic.QRS.GWAS.FullResults.GenomicControl.Final.csv

#' @import data.table

# Dictionary to standardize the column names across various sources
summary_stats_column_names <- list(
  variant_id = c("variant_id"),
  chr = c("chr",
          "SNPChr",
          "CHROM",
          "chromosome",
          "Chromosome",
          "CHR",
          "ISmet_chr",
          "Chr",
          "Chromsome" # Yup...
          ),

  pos = c("pos",
          "SNPPos",
          "position",
          "GENPOS",
          "Position",
          "POS",
          "ISmet_position",
          "BP",
          "Pos",
          "Pos_b37",
          "MapInfo"),

  ref = c("ref",
          "OtherAllele",
          "other_allele.outcome",
          "other_allele.exposure",
          "nea",
          "ALLELE0",
          "NEA",
          "Noncoded_allele",
          "noncoded_all",
          "Other_allele"),

  alt = c("alt",
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
          "coded_all"),

  effect = c("effect",
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
             "Beta"),

  pval = c("pval",
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
           "ISmet_pvalue"),

  log10p = c("LOG10P"),

  effect_se = c("effect_se",
                "slope_se",
                "se",
                "SE",
                "MetaSE",
                "b_se",
                "StdErr"),

  zscore = c("Zscore",
             "Test statistic"),

  odds_ratio = c("OR")
)

summary_stats_extra_column_names <- list(
  allele1 = c("Allele_1",
              "Allele 1",
              "Allele1",
              "a1",
              "A1",
              "allele1",
              "ISmet_allele1"),
  allele2 = c("Allele_2",
              "Allele 2",
              "Allele2",
              "a2",
              "A2",
              "allele2",
              "ISmet_allele2"),
  position_encoding = data.table::data.table(
    column_name = c("MarkerName"),
    regex       = c("^chr[^:]+:([0-9]+):[a-zA-Z]")
  )
)

summary_stats_prefixes <- list(
  chr = "chr"
)

# filterRead

Fast file filtering using `awk` for efficient data loading with
`data.table::fread`.

## Overview

`filterRead` is an R package that provides a high-performance interface for
loading GWAS summary statistics from file. It's main features are the
standardized interface and fast filtering prior to loading in memory,
powered by `awk`. This simplifies the loading of summary statistics from
different sources, by invisibly and efficiently handling differences in column
names, compression, field separator, and more, returning a data.table with
standardized names for downstream analyses.

## Features

- **Fast filtering**: Leverages `awk` to filter rows before loading
  into R
- **Automatic file detection**: Handles CSV, TSV, and gzipped files
  automatically
- **Column naming**: Automatically converts columns to standardized names
- **Missing columns**: Missing columns (e.g., `pos`) are inferred
  from existing columns (e.g., variant_id)
- **Genomic data support**: Built-in support for chromosome/position
  filtering and RSID matching
- **R-style syntax**: Write filter conditions using familiar R
  expressions
- **Complex conditions**: Supports logical operators (`&`, `|`),
  comparisons, and pattern matching
- **Memory efficient**: Only loads filtered rows into memory

## Installation

```r
# Install from local source
remotes::install_github("JonSulc/filterRead")
```

## Quick Start

### Basic Usage

```r
library(filterRead)

# Create a file interface
# This doesn't load any data, only checks the file formatting
finterface <- new_file_interface("gwas_summary_stats.txt.gz")

# Check how the processed table will look
head(finterface)

# Filter using R-style conditions and load into memory
significant_hits <- finterface[chr == 1 & pval < 5e-8]
genomic_regions <- finterface[
  (chr == 2 & 12345 < pos & pos < 23456) |
  (chr == 3 & 42 < pos & pos < 4242)
]
```

## How It Works

1. **File interface creation**: Automatically detects file format,
   separator, column names, and structure
2. **Condition parsing**: R expressions are parsed and translated to
   `awk`-compatible filter conditions
3. **Efficient filtering**: `awk` processes the file and filters rows
   before data reaches R
4. **Data loading**: Only filtered rows are loaded using
   `data.table::fread`
5. **Header reconstructed**: The header names are filled in with standard
   names
6. **Missing data**: Where possible missing fields are reconstructed from other
   columns (e.g., extracting `pos` from `MarkerName`)

### Standard column names

- **chr**: Chromosome
- **pos**: Position
- **ref**: Reference (non-effect) allele
- **alt**: Alternate (effect) allele
- **effect**: Estimated effect of the allele
- **pval**: P-value
- **effect_se**: Standard error of the effect
- **log10p**: Negative log10 p-value
- **zscore**: Z-score
- **odds_ratio**: Odds ratio

## System Requirements

- R (>= 4.0.0)
- `awk` (available on most Unix-like systems)
- `tabix` (for RSID matching functionality)
- A SNP reference file for RSID indexing

### R Package Dependencies

- data.table (>= 1.16.0)
- purrr
- rlang
- stringr

## License

GPL (>= 3)

## Author

Jonathan Sulc (<jonsulc@gmail.com>)

## Contributing

Contributions are welcome! Please feel free to submit issues or pull
requests.

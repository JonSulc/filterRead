# filterRead

Fast file filtering using `awk` for efficient data loading with
`data.table::fread`.

## Overview

`filterRead` is an R package that provides a high-performance interface
for loading large tab/comma-separated GWAS summary statistics files with
filtering conditions. By translating R-style filter expressions to `awk`
commands, the package filters data before loading it into R, dramatically
reducing memory usage and load times for large datasets. Different and
missing column names are automatically converted and/or completed.

## Features

- **Fast filtering**: Leverages `awk` to filter rows before loading
  into R
- **R-style syntax**: Write filter conditions using familiar R
  expressions
- **Automatic file detection**: Handles CSV, TSV, and gzipped files
  automatically
- **Column naming**: Automatically converts columns to standardized names
- **Missing columns**: Missing columns (e.g., `chr` and `pos`) are obtained
  from existing columns (e.g., variant_id)
- **Genomic data support**: Built-in support for chromosome/position
  filtering and RSID matching
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
finterface <- new_file_interface("data.csv")

# Check how the processed table looks
head(finterface)

# Filter using R-style conditions
filtered_data <- finterface[chr == 1 & pval < 0.05]
```

### Genomic Data

```r
# Load genomic data with chromosome/position filtering
gwas_file <- new_file_interface("gwas_summary_stats.txt.gz")

# Filter by chromosome and position range
chr1_data <- gwas_file[chr == 1 & 1000000 < pos & pos < 2000000]

# Filter by p-value threshold
significant <- gwas_file[pval < 5e-8]
```

### RSID Matching

```r
# Filter by specific RSIDs (requires tabix-indexed reference)
rsids <- c("rs12345", "rs67890", "rs11111")
snp_data <- gwas_file[rsid %in% rsids]
```

## How It Works

1. **File interface creation**: Automatically detects file format,
   separator, column names, and structure
2. **Condition parsing**: R expressions are parsed and translated to
   `awk`-compatible filter conditions
3. **Efficient filtering**: `awk` processes the file and filters rows
   before data reaches R
4. **Data loading**: Only header and filtered rows are loaded using
   `data.table::fread`

## Advanced Features

### Column Encoding

The package handles encoded columns that need splitting or recoding:

```r
# Automatically handles complex column formats
# (e.g., quoted values, special separators)
finterface <- new_file_interface("complex_format.txt")
data <- finterface[condition == TRUE]
```

### IEU GWAS Format

Built-in parsing for IEU GWAS database format files:

```r
# Automatically detects and parses IEU GWAS format
ieugwas_file <- new_file_interface("ieugwas_data.vcf.gz")
filtered <- ieugwas_file[pval < 1e-5]
```

### Complex Filter Conditions

```r
# Combine multiple conditions
finterface[
  (chr == 1 | chr == 2) &
  pval < 0.01 &
  (b < -0.1 | 0.1 < b)
]
```

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

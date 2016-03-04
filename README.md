# snpTools

> My R package for general tasks when working with SNP array data

## Table of Contents

1. [Summary of tools](#summary-of-tools)
2. [Installation](#installation)
3. [Example](#example)

## Summary of tools

Currently, snpTools contains:

- `merge_geno()` Merges any number of genotyping datasets 
and ensures SNPs from each set are merged in the correct order.
- `recode_geno()` Converts an ACTG coded SNP to a A/B coded SNP, provided a recoding table. The function can be vectorized to apply to a full dataset of n SNPs
- `count_geno()` Converts a SNP dataset in A/B format to the dosage of a chosen allele.
- `filter_geno()` Filters a SNP dataset, filtering out SNPs that do not meet a particular call rate, are fixed, or have an extreme MAF (to be implemented).
- `format_gpGeno()` Imports raw genotyping data (in the format such as those provided by GeneSeek)
- `fimpute_run()` Runs FImpute from within R, allowing for convenient genotype phasing and imputing from genotyping data and a SNP map.

## Installation

To install and attach snpTools, from within the R interpreter use:

```r
devtools::install_github("funkhou9/snpTools")
library(snpTools)
```
This assumes you have the `devtools` package installed. If not, use `install.packages("devtools")` to install `devtools` from CRAN.

For more information on any particular function, use:

```r
?fimpute_run
```

## Example

`fimpute_run()` can be used to phase genotypes, given a genotyping dataset and SNP map with:

```r
fimpute_run(geno = <geno>, 
			map = <map>, 
			path = "/path/to/FImpute",
			exclude_chr = "1 2 3 4 5 6 7 9 10 11 12 13 14 15 16 17 18 19 20 21")
```	

- `<geno>` may be a data.frame or matrix containing SNPs in columns and individuals in rows. SNPs must be represented as dosages.
- `<map>` must be a data.frame containing SNP information, with columns labeled as "chr" and "pos", and rownames containing the name of each SNP.
- Use the `path` argument to specify the location of the FImpute binary.
- Use the `exclude_chr` to specify a character string containing the chromosomes you want to exclude in the genotype phasing. In the example we are only phasing SNPs present on chromosome 8.
- Use the optional `output_folder` argument to specify the name and location of FImpute output. By default, output will be saved in the current working directory within a directory named `fimpute_run`

The above code will invoke FImpute, so you should see standard FImpute stdout:

```
 .--------------------------------------.
|  .---.--.--                  .         |
|  |      |                   _|_        |
|  |---   |  .--.--. .,-. .  . |  .-.    |
|  |      |  |  |  | |   )|  | | (.-'    |
|  '    --'--'  '  `-|`-' `--`-`-'`--'   |
|                    |                   |
|                    '                   |
|              Version 2.2               |
|                                        |
|           Mehdi Sargolzaei,            |
|  Jacques Chesnais and Flavio Schenkel  |
|           Semex Alliance, ON           |
|     CGIL, University of Guelph, ON     |
|                                        |
|       Last update: Jan 16, 2014        |
 `--------------------------------------'
 ```

#' Synbreed geno formatting converter
#'
#' Function to convert genotype file from long format to matrix format suitable for Synbreeds
#' gpData object.
#'
#' @param data_file Path to raw genotyping file in long format
#' @param num_ids Numeric. Number of individuals in file
#' @param num_snps Numeric. Number of SNPs per individual
#' @param head Numeric. Number of header lines.
#' @param snp_col Numeric. Indicates which column in file contains SNP names
#' @param id_col Numeric. Indicates which column in file contains individual names
#' @param allele1_col Numeric. Indicates which column contains genotyping for allele 1.
#' @param allele2_col Numeric. Indicates which column contains genotyping for allele 2.
#'
#' @return Genotype matrix with individuals in rows and snps in columns
#' @export
format_gpGeno <- function(data_file, num_ids, num_snps,
                          head = 10, snp_col = 1, id_col = 2, allele1_col = 3, allele2_col = 4) {
  # Read in long file
  raw_data <- read.table(data_file, skip = head, header = FALSE, sep = '\t')

  # Paste together alleles to make genotypes
  genotypes <- paste(raw_data[, allele1_col], raw_data[, allele2_col], sep = '/')

  # Get vector of SNP names in file
  snps <- unique(raw_data[, snp_col])

  # Get vector of ID names in file
  ids <- unique(raw_data[, id_col])

  # Create matrix with IDs in rows and SNPs in columns. Assign col/row names
  gMat <- matrix(data = genotypes, nrow = num_ids, ncol = num_snps, byrow = TRUE)
  colnames(gMat) <- snps
  rownames(gMat) <- ids

  return(gMat)
}

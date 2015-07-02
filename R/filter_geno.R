#' Filters genotypes based on various criteria
#' 
#' Before using genotypes in models, this function can be used to filter out SNPs that are
#'  fixed, have a low call rate, etc.
#'
#' @param geno matrix of genotyping data with individuals in rows and SNPs in columns.
#'  Formatted in dosage of B format.
#' @param call_rate numeric proportion indicating the call rate that must be met
#' @param fix boolean. If TRUE, remove SNPs that are fixed among all animals
#' @return geno matrix with removed (filtered out) SNPs
#' @export
filter_geno <- function(geno, call_rate = 0.9, fix = TRUE) {
  
  # Calculate 'call_rate' for each SNP
  call <- apply(geno, 2, function(x) { sum(!is.na(x)) / length(x) })
  
  # Calculate the number of unique genotypes for each SNP
  num_uniq <- apply(geno, 2, function(x) { length(unique(x[!is.na(x)])) })
  
  # Apply filters
  geno_filtered <- geno[, call > call_rate]
  # Idx of those SNPs still in geno that have at least two genotypes present
  idx <- colnames(geno_filtered) %in% names(num_uniq[num_uniq != 1])
  
  geno_filtered <- geno_filtered[, idx]
  return(geno_filtered)
}
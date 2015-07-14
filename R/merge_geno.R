#' Merges SNP array data
#' 
#' Merges any number of genotyping datasets 
#' and ensures SNPs from each set are merged in the correct order.
#' 
#' Assumes IDs are in rows of the dataset and SNPs are in columns.
#'  
#' @param ... matrix of SNP array datasets
#' @return matrix of merged SNP array data
#' @import magrittr
#' @export
merge_geno <- function(...) {
  
  # Put datasets in a list
  genos <- list(...)
  
  # Find SNPs common among all datasets
  common_snps <- lapply(genos, colnames) %>%
                    Reduce(intersect, .)
  
  # Produce new list of datasets composed of only common SNPs
  genos_new <- lapply(genos, function(x) x[, common_snps])
  
  # Merge!
  do.call(rbind, genos_new)
}

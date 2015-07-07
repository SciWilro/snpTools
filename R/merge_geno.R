#' Merges SNP array data
#' 
#' Merges any number of genotyping datasets 
#' and ensures SNPs from each set are merged in the correct order.
#' 
#' Assumes IDs are in rows of the dataset and SNPs are in columns.
#'  
#' @param ... matrix of SNP array datasets
#' @return matrix of merged SNP array data
#' @export
merge_geno <- function(...) {
  
  names_each <- c()
  
  # Put datasets in a list
  genos <- list(...)
  
  # Get SNP order of first dataset
  SNP_names <- colnames(genos[[1]])
  
  # Is each dataset composed of the same SNPs?
  name_check <-
    lapply(genos, function(x) {
      SNP_names %in% colnames(x) & length(SNP_names) == length(colnames(x))
  })
  if(!any(unlist(name_check))) stop("Datasets aren't compatible")
  
  # Make new list of datasets with SNPs arranged according
  #   to the first
  genos_new <- lapply(genos, function(x) x[, SNP_names])
  
  # Merge!
  do.call(rbind, genos_new)
}

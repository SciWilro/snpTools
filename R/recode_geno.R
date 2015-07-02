#' recode_geno - converts an ACTG coded SNP to a A/B coded SNP
#' 
#' @param SNPname character of SNP name to recode
#' @param geno matrix of genotyping data with individuals in rows and SNPs in columns.
#'  Formatted in ATCG "forward" format
#' @param table data.frame consisting of an equivalency table needed for conversion
#' @param Bdose boolean. If true, returns dosage of allele B. If false, returns A/B.
#' @return numeric/character vector with recoded genotypes for all individuals for a single
#'  SNP in SNPname
recode_geno <- function(SNPname, geno, table, Bdose = FALSE) {
  
  # Genotypes for all individuals
  genotype <- geno[, SNPname]
  
  # Check if genotype available
  if (is.null(genotype)) {
    warning("SNP name not in genotype matrix")
    return(NA)
  }
  
  # Separate alleles
  A1 <- substr(genotype, 1, 1)
  A2 <- substr(genotype, 3, 3)
  alleles <- names(table(c(A1, A2)))
  
  # Genotypes must consist of combination of AGTCs
  if(!all(alleles %in% c("A", "G", "T", "C", "-"))) {
    warning("Must enter valid genotype")
    return(NA)
  }
  
  # Marker must be in table
  if(!SNPname %in% table[,1]) {
    warning("SNP name not present in equivalency table")
    return(NULL)
  } else {
    
    # If SNPname is in table, extract that SNP
    ref <- table[table[,1] == SNPname, 7]
    
    #Recoding according to table using dose of B if Bdose == T
    #else - "A/B" genotype format returned
    if(Bdose) {
      A1[A1 == "N" | A1 == "-"] <- NA
      A1 <- c(0, 1)[(A1 == ref) + 1]
      A2[A2 == "N" | A2 == "-"] <- NA
      A2 <- c(0, 1)[(A2 == ref) + 1]
      return(A1 + A2)
      
    } else {
      A1[A1 == "N" | A1 == "-"] <- NA
      A1 <- c("A", "B")[(A1 == ref) + 1]
      A2[A2 == "N" | A2 == "-"] <- NA
      A2 <- c("A", "B")[(A2 == ref) + 1]
      return(paste(A1, A2, sep = "/"))
    }
  }
}
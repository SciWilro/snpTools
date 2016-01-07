#' Counts dosages of alleles
#' 
#' Counts the dosage of a particular allele when genotyping data is in A/B format
#' @param geno matrix of genotypes with SNPs in columns and samples in rows
#' @param count character ("A" or "B") for which allele will be counted
#' @return a new genotype matrix with counts of a particular allele rather than A/B coding
#' @export
count_geno <- function(geno, count = "B") {
  
  # For each column, extract alleles and count dosages
  apply(geno, 2, 
        function(x) {
          a1 <- substr(x, 1, 1)
          a2 <- substr(x, 3, 3)
          
          # Detect missing calls
          a1[a1 == "N" | a1 == "-"] <- NA
          a2[a2 == "N" | a2 == "-"] <- NA
          
          # Change coding to dosages and return sum
          a1 <- c(0, 1)[(a1 == count) + 1]
          a2 <- c(0, 1)[(a2 == count) + 1]
          a1 + a2
        })
}
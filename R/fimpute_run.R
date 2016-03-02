#' Run Fimpute
#' 
#' Invoke Fimpute for imputation and genotype phasing from within R. Uses R objects for genotypes,
#' maps, and pedigrees to construct Fimpute inputs, and executes an Fimpute run. Fimpute is required
#' to be on your PATH, or otherwise specify the location of the Fimpute binary with the path argument
#' 
#' @param geno a matrix or data.frame with SNPs in columns and individuals in rows. Genotypes must
#' be coded as dosage of allele 'b' {0, 1, 2}.
#' @param map a data.frame containing SNP map information for each SNP present in geno
#' @param ped a data.frame pedigree providing family information for each individual in geno. The first
#' column of the pedigree is for ID, second is for sire/father ID, and third is for dam/mother ID.
#' @param path a character represting the path to the FImpute binary. If omitted, assumes FImpute binary
#' resides along PATH.
#' @export
fimpute_run <- function(geno,
                        map,
                        ped = NULL,
                        path = NULL) {

    # Check if required objects meet criteria
    if(!any(colnames(map) == c("chr", "pos")))
      stop("Check map argument. It should have the columns 'chr' and 'map'")
  
    # Ensure chromosomes are numeric (change sex chromosomes if necessary)
    map$chr[map$chr == 0] <- NA
    map$chr[map$chr == "X"] <- 19
    map$chr[map$chr == "Y"] <- 20
    map$chr[is.na(map$chr)] <- 21
    
    # IDs of individuals cannot contain spaces, replace with underscores
    rownames(geno) <- gsub(" ", "_", rownames(geno))
    
    # Check if SNPs present in geno are present in the map
    idx <- colnames(geno) %in% rownames(map)
    
    if (any(!idx))
      warning(paste(sum(!idx), "SNPs present in geno were not present in map, and were removed"))
    
    # Only keep SNPs in geno if they are present in map
    geno <- geno[, idx]
    
    # Similar to map, NAs in geno are not permitted. Replace with the "missing integer"
    geno[is.na(geno)] <- 5
    
    # Obtain order of SNPs in map as they appear in geno (needed for part of output known as
    # "snp info file")
    chip <- match(rownames(map), colnames(geno))
    
    # For any SNPs in the map but not in the geno, replace with 0.
    chip[is.na(chip)] <- 0
    
    # Create the vector of the chip number (1). Again required for "snp info file"
    chip_number <- rep(1, nrow(geno))
    
    # Create ID vector
    id <- rownames(geno)
    
    # Paste all the genotypes for each animal together
    call <- apply(geno, 1, paste, collapse = "")
    
    # Assemble required output (ID, chip, and genotypes (represented as one long string))
    genotype <- data.frame(id,
                           chip_number,
                           call)
    colnames(genotype) <- c("ID", "Chip", "Call")
    
    snp_info_list <- list(map, 
                          chip,
                          genotype)
    
    names(snp_info_list) <- c("map", "chip", "geno")
    return(snp_info_list)
}
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
#' column of the pedigree is for ID, second is for sire/father ID, third is for dam/mother ID, fourth
#' is sex of ID {"M" or "F"}. If ids in geno are not present in pedigree, they will be added to the
#' end with missing parent information and a "M" sex. Correct sex information should only be required
#' when imputing/phasing the sex chromosomes.
#' @param path a character represting the path to the FImpute binary. If omitted, assumes FImpute binary
#' resides along PATH.
#' @param groups a list of character vectors with the names of IDs meant to be processed as groups. 
#' Names of list should be the names of the groups, with each element containing IDs.
#' @param exclude_chr a character vector of chromosomes to exclude
#' @param output_folder a character providing the location of desired FImpute output
#' @param parent logical use TRUE if only parents identified from the pedigree are to be reference animals
#' @param reference logical use TRUE if you want `groups` to be used to specify the reference animals
#' used by FImpute. Not compatable with parent TRUE
#' @param keep_fimpute logical use TRUE to remove associated files used to run FImpute.
#' @export
fimpute_run <- function(geno,
                        map,
                        ped = NULL,
                        path = NULL,
                        groups = NULL,
                        exclude_chr = NULL,
                        output_folder = getwd(),
                        parent = FALSE,
                        reference = FALSE,
                        keep_fimpute = FALSE) {

    message("Preparing input files for FImpute")

    fimpute_files <- c()
    
    # Check if required objects meet criteria
    if(!any(colnames(map) == c("chr", "pos")))
      stop("Check map argument. It should only have two columns: 'chr' and 'pos'")
  
    # Ensure chromosomes are numeric character values (change sex chromosomes if necessary) and replace
    # 0 positions with NA
    map$chr <- as.character(map$chr)
    map$chr[map$chr == 0] <- NA
    map$chr[map$chr == "X"] <- 19
    map$chr[map$chr == "Y"] <- 20
    map$chr[is.na(map$chr)] <- 21
    map$pos[map$pos == 0] <- NA
    
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
    genotypes <- data.frame(id,
                            chip_number,
                            call)
    colnames(genotypes) <- c("ID", "Chip", "Call")

    # Write FImpute input components and run -------------------------------------------------------
    # 1. SNP info
    snp_info <- data.frame(rownames(map),
                           map,
                           chip)
    colnames(snp_info) <- c("SNP_ID", "Chr", "Pos", "Chip1")
    
    write.table(snp_info,
                file = "snp_info_fimpute.txt", 
                quote = FALSE, 
                sep = '\t', 
                col.names = TRUE, 
                row.names = FALSE)
    
    # 2. Pedigree file, if provided
    if (!is.null(ped)) {
      if (!is.data.frame(ped) | ncol(ped) != 4)
        stop("ped argument supplied, but it does not appear to be a d.f. with 4 columns")
      
      # Ensure IDs in pedigree contain no spaces. Replace with "_".
      ped[, 1] <- gsub(" ", "_", ped[, 1])
      ped[, 2] <- gsub(" ", "_", ped[, 2])
      ped[, 3] <- gsub(" ", "_", ped[, 3])
      
      # Append missing pedigree IDs. 
      if (any(!(rownames(geno) %in% unlist(ped)))) {
        extra_ids <- rownames(geno)[!(rownames(geno) %in% unlist(ped))]
        extra_ped <- data.frame(extra_ids,
                                rep(0, length(extra_ids)),
                                rep(0, length(extra_ids)),
                                rep("M", length(extra_ids)))
        colnames(extra_ped) <- colnames(ped)
        ped <- rbind(ped, extra_ped)
      }
      
      # Write pedigree to disk for fimpute run
      write.table(ped, file = "trio_ped_fimpute.txt", 
                  quote = FALSE, 
                  sep = '\t', 
                  col.names = TRUE, 
                  row.names = FALSE)
    }
    
    # 4. Write reference file (if reference is TRUE) for each group
    if (reference) {
      for (i in 1:length(groups)) {
        write.table(as.data.frame(groups[[i]]),
                    file = paste0(names(groups)[i],
                                  "_reference.txt"),
                    quote = FALSE,
                    col.names = FALSE,
                    row.names = FALSE)
      }
    }
    
    # 3. Geno info and submission file for each element in groups
    if (!is.null(groups)) {
      for (i in 1:length(groups)) {
        
        # Ensure IDs in each group do not contain spaces
        groups[[i]] <- gsub(" ", "_", groups[[i]])
        
        group_i <- genotypes[ genotypes[, 1] %in% groups[[i]], ]
        write.table(group_i,
                    file = paste0(names(groups)[i], "_geno_fimpute.txt"),
                    quote = FALSE,
                    sep = '\t',
                    col.names = TRUE,
                    row.names = FALSE)
        
        # One submission file for each group
        file_name <- paste0(names(groups)[i], "_fimpute_run.txt")
        file_con <- file(file_name)
        writeLines(
          c(paste0('title="', names(groups)[i], '";'), 
            paste0('genotype_file="', names(groups)[i], '_geno_fimpute.txt";'),
            'snp_info_file="snp_info_fimpute.txt";',
            if (!is.null(ped))
              paste0('ped_file="trio_ped_fimpute.txt";'),
            paste0('output_folder="', output_folder, '/', names(groups)[i], '_fimpute_run";'),
            if (!is.null(exclude_chr))
              paste0('exclude_chr= ', exclude_chr, ';'),
            'save_hap_lib;',
            if (parent)
              paste0('ref = 1000 /parent;'),
            if (reference)
              paste0('ref ="', names(groups)[i], '_reference.txt";'),
            'njob=5;'),
          con = file_con
        )
        close(file_con)
        
        # Invoke FImpute for each group
        if (!is.null(path))
          system(paste0(path, "/FImpute ", names(groups)[i], "_fimpute_run.txt"))
        else
          system(paste0("FImpute ", names(groups)[i], "_fimpute_run.txt"))
        
        # Add file
      }
    } else {
      write.table(genotypes,
                  file = "geno_fimpute.txt",
                  quote = FALSE,
                  sep = '\t',
                  col.names = TRUE,
                  row.names = FALSE)
      
      # Write a single submission file
      file_con <- file("fimpute_run.txt")
      writeLines(
        c('title="fimpute";',
          'genotype_file="geno_fimpute.txt";',
          'snp_info_file="snp_info_fimpute.txt";',
          if (!is.null(ped))
            paste0('ped_file="trio_ped_fimpute.txt";'),
          paste0('output_folder="', output_folder, '/fimpute_run";'),
          if (!is.null(exclude_chr))
            paste0('exclude_chr= ', exclude_chr, ';'),
          'save_hap_lib;',
          if (parent)
            paste0('ref = 1000 /parent;'),
          'njob=5;'),
        con = file_con
      )
      close(file_con)
      
      # Invoke FImpute
      if (!is.null(path))
        system(paste0(path, "/FImpute fimpute_run.txt"))
      else
        system("FImpute fimpute_run.txt")
    }
}





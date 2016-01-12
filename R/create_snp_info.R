#' Create snp_info file required as FImpute imput
#' 
#' Author: Sebastian Casiro
#' 
#' @param map data.frame containing SNP map
#' @param geno data.frame or matrix containing genotypes (animals in rows, SNPs in columns)
#' @return object nearly ready to be written to a table and used as FImpute input as the snp_info file
#' @export
create_snp_info<-function(map=map,geno=genotype,gpData=NULL,chip_number=1){
  #Check if there is a gpData object or not
  if (is.null(gpData)){
    map<-map
    geno<-geno
  }
  
  
  if (!(is.null(gpData))){
    map<-gpData$map
    geno<-gpData$geno
  }
  
  map$chr[is.na(map$chr)]<-21
  #Check if the columns of the G matrix are present or not in the map
  presence<-colnames(geno)%in%rownames(map)
  #check if all the SNP are present or not
  #This is crucial, because we will have a dosage in the G matrix, so we have to be sure that the SNP
  #Is in the map (Thus has a position and a chromosome)
  for (i in 1:length(presence)){
    if (presence[i]!=TRUE) {warning ("Some SNP are not present and were taken out")}
  }
  #Filter Geno by presence
  geno<-geno[,presence]
  #Write 5 instead of NA values
  geno[is.na(geno)]<-5
  
  
  
  #Match the rownames of the map with the column names of the Geno. This will check the order of apparence
  #Of the SNP in the columns in G as they appare in the rows of the map. This will give a positional order
  #That will be the position of that snp in the chip.
  chip<-match(rownames(map),colnames(geno))
  #Check if the order vector has NA replace it with 0
  chip[is.na(chip)]<-0
  
  #Create the vector of the chip number
  chip_number<-rep(chip_number,nrow(geno))
  
  #Create ID vector
  ID<-rownames(geno)
  
  #geno must be a data frame
  geno<-data.frame(geno)
  #Create a vector with the names of the SNP
  cols<-colnames(geno)
  
  #Paste all the dosages for one animal toghether
  geno$Call<-apply(geno[,cols],1, paste,collapse="")
  dim(geno)
  
  # remove the unnecessary rows
  geno<-geno[,!(colnames(geno)%in%cols)]
  #Merge all the data into a data frame because to write a table it should be a
  #data frame or matrix and it was a list
  geno<-data.frame(ID,chip_number,geno)
  #Change columnames
  colnames(geno)<-c("ID","Chip","Call")
  
  #Create a List with all the Output
  snp_info_list<-list(map,chip,geno)
  names(snp_info_list)<-c("map","chip","geno")
  return(snp_info_list)
}
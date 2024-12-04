# Code used to check the identifications made in our study against GBIF to detect any
# outdated terms, or potential errors. 

# NOTE AS I WAS IN CHARGE OF DOING THIS ANALYSIS FOR THE VERTEBRATE DATA COLLECTED USING
# AUDIOMOTHS AND CAMERA TRAPS, I USED THE CODE THAT CHECKS SPECIES NAMES. FOR DATA ON
# IVERTEBRATES THE CODE MUST BE ADJUSTED TO CHECK THE ORDER NAMES.

rm(list=ls())

library("rgbif")  

# reset working directory to outside of the project 
getwd()
setwd("C:/Users/charl/Documents/University/Year 5 Masters/Semester 1/Professional Skills for Ecologists/EIA Assessment Write-up/GBIF")

# import the data that needs to be checked
vertebrates_messy <- read.csv("vertebrates_tech.csv")
head(vertebrates_messy)

# list unique species in the data
species <- unique(vertebrates_messy$scientificName)
# save the number of species present in the data
n_species <- length(species)

# Initialize the data frames for a species that is sure to be in the taxonomy
spp.check.ok <- name_backbone("Pipistrellus pygmaeus", verbose = TRUE, strict = TRUE)
spp.check.ok <- spp.check.ok[-c(1:nrow(spp.check.ok)), ]  # Remove all the rows
spp.check.ok <- spp.check.ok[,-which(names(spp.check.ok) %in% c("acceptedUsageKey","family","genus","familyKey","genusKey","class","classKey"))]

# Initialize a data frame for a bad word (not a species name)
spp.check.bad <- name_backbone("xxx", verbose = TRUE, strict = TRUE)
spp.check.bad <- spp.check.bad[-1, ]  # Remove this row

# Loop over species
for (i in 1:n_species) {
  # Try-catch block to handle potential errors during each loop
  
  # i <- 0
  # 
  # i <- i+1
  # print(i)
  
  try({
    # i <- i+1
    toto <- name_backbone(species[i], verbose = TRUE, strict = TRUE)  # Check species against GBIF backbone taxonomy
    # print(toto)
    if(nrow(toto)>1)
      toto <- toto[which(toto$rank=="SPECIES"),]
    # print(toto)
    if(ncol(toto) > ncol(spp.check.ok))
      toto <- toto[,-which(names(toto) %in% c("acceptedUsageKey","class","classKey","family","genus","familyKey","genusKey"))]
    
    # Ensure the result has the same number of columns as spp.check.ok
    if (ncol(toto) == ncol(spp.check.ok)) {
      toto <- toto[,names(spp.check.ok)]
      # Check the status field
      if ("ACCEPTED" %in% toto$status) {
        spp.check.ok <- rbind(spp.check.ok, toto[toto$status == "ACCEPTED", ])
      } else if ("SYNONYM" %in% toto$status) {
        warning(paste("Species", species[i], "is a synonym"))
        spp.check.ok <- rbind(spp.check.ok, toto[toto$status == "SYNONYM", ][1, ])
      } else if ("DOUBTFUL" %in% toto$status) {
        warning(paste("Species", species[i], "is doubtful"))
        spp.check.ok <- rbind(spp.check.ok, toto[toto$status == "DOUBTFUL", ][1, ])
      } else {
        stop("Status unknown for species:", species[i])
      }
    } else {
      spp.check.bad <- rbind(spp.check.bad, toto)
    } 
    
    # print(spp.check.ok)
  }, silent = TRUE)  # Continue the loop even if an error occurs
  #setTxtProgressBar(pb, i)  # Update the progress bar
}

# check if there are any synonyms for any of the species in the data set
length(which(spp.check.ok$status=="SYNONYM"))
# check if there are any doubtfull species
length(which(spp.check.ok$status=="DOUBTFUL"))





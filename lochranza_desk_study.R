# desk study completed prior to in person habitat assessments and data collection

getwd()
setwd("C:/Users/charl/Documents/University/Year 5 Masters/Semester 1/Professional Skills for Ecologists/2. EIA Assessment Write-up/NBN Atlas")
getwd()


# read all the data from NBN Atlas in
north_areareport <- read.delim('proposed_northsite_area_report.txt', sep=",", header = T)
north_biodiversityactionplan <- read.delim('proposed_northsite_biodiversityactionplan.txt', sep=",", header = T, row.names = NULL)
north_biodiversityactionplan <- subset(north_biodiversityactionplan, select = -c(row.names))
north_redlistend <- read.delim('proposed_northsite_redlistGBend.txt', sep=",", header = T)
north_RSPB <- read.delim('proposed_northsite_RSPB.txt', sep=",", header = T)
north_scotbiodiversity <- read.delim('proposed_northsite_scottishbiodiversitylist.txt', sep=",", header = T)
south_areareport <- read.delim('proposed_southsite_area_report.txt', sep=",", header = T)
south_biodiversityactionplan <- read.delim('proposed_southsite_biodiversityactionplan.txt', sep=",", header = T)
south_invasives <- read.delim('proposed_southsite_invasives.txt', sep=",", header = T)
south_redlistcritend <- read.delim('proposed_southsite_redlistGBcritend.txt', sep=",", header = T, row.names = NULL)
south_redlistend <- read.delim('proposed_southsite_redlistGBend.txt', sep=",", header = T, row.names = NULL)
south_RSPB <- read.delim('proposed_southsite_RSPB.txt', sep=",", header = T)
south_scotbiodiversity <- read.delim('proposed_southsite_scottishbiodiversitylist.txt', sep=",", header = T)

# add a column to describe the source of the data on the NBN Atlas website
north_biodiversityactionplan$source <- list("Biodiversity Action Plan UK list of priority species")
north_redlistend$source <- list("Red List GB post 2001 - Endangered - based on IUCN guidelines")
north_RSPB$source <- list("RSPB Priority Species")
north_scotbiodiversity$source <- list("Scottish Biodiversity List")
south_biodiversityactionplan$source <- list("Biodiversity Action Plan UK list of priority species")
south_invasives$source <- list("Invasive Species")
south_redlistcritend$source <- list("Red List GB post 2001 - Critically Endangered (possibly extinct) - based on IUCN guidelines")
south_redlistend$source <- list("Red List GB post 2001 - Endangered - based on IUCN guidelines")
south_RSPB$source <- list("RSPB Priority Species")
south_scotbiodiversity$source <- list("Scottish Biodiversity List")

# add a column to describe the source of the data on the NBN Atlas website
north_biodiversityactionplan$slope <- list("North")
north_redlistend$slope <- list("North")
north_RSPB$slope <- list("North")
north_scotbiodiversity$slope <- list("North")
south_biodiversityactionplan$slope <- list("South")
south_invasives$slope <- list("south")
south_redlistcritend$slope <- list("South")
south_redlistend$slope <- list("South")
south_RSPB$slope <- list("South")
south_scotbiodiversity$slope <- list("South")

# combine the data sets
NBN_Atlas_data <- rbind(north_biodiversityactionplan, north_redlistend, north_RSPB, north_scotbiodiversity, south_biodiversityactionplan, south_invasives, south_redlistcritend, south_redlistend, south_RSPB, south_scotbiodiversity)
NBN_Atlas_data$source <- as.character(NBN_Atlas_data$source)
NBN_Atlas_data$slope <- as.character(NBN_Atlas_data$slope)
NBN_Atlas_data$Invasive <- as.character(NBN_Atlas_data$Invasive)


# save this data to the project
setwd("C:/Users/charl/Documents/University/Year 5 Masters/Semester 1/Professional Skills for Ecologists/2. EIA Assessment Write-up/lochranza_EIA_data/Data")

# save as 'NBN_atlas_data.csv'
write.csv(NBN_Atlas_data, "NBN_atlas_data.csv", row.names = F)


# rearrange data set
library(dplyr)
library(tidyr)

NBN_data <- read.csv("./Data/NBN_atlas_data.csv")

NBN_data_rearranged <- NBN_data %>%
  # Create a new column classifying the sources into desired categories
  mutate(Source_Category = case_when(
    source == "Biodiversity Action Plan UK list of priority species" ~ "Biodiversity Action Plan UK list of priority species",
    source == "Red List GB post 2001 - Endangered - based on IUCN guidelines" ~ "Red List - Endangered",
    source == "RSPB Priority Species" ~ "RSPB Priority Species",
    source == "Scottish Biodiversity List" ~ "Scottish Biodiversity List",
    source == "Invasive Species" ~ "Invasive Species",
    source == "Red List GB post 2001 - Critically Endangered (possibly extinct) - based on IUCN guidelines" ~ "Red List - Critically Endangered",
    TRUE ~ "Other"  # Catch-all for anything unexpected
  )) %>%
  # Spread the data so each source category becomes a column
  pivot_wider(
    names_from = Source_Category,
    values_from = Number.of.records,
    values_fill = 0  # Fill missing values with 0
  ) %>%
  # Arrange columns as per your required format
  select(Species.Name, 
         "Biodiversity Action Plan UK list of priority species",
         "Red List - Endangered", 
         "RSPB Priority Species", 
         "Scottish Biodiversity List", 
         "Invasive Species", 
         "Red List - Critically Endangered",
         "slope")

# NBN_data_rearranged <- NBN_data_rearranged %>%
#   # Create a new column to indicate the slope (North = 'N', South = 'S')
#   mutate(slope2 = case_when(
#     grepl("North", slope, ignore.case = TRUE) ~ "N",
#     grepl("South", slope, ignore.case = TRUE) ~ "S",
#     TRUE ~ NA_character_
#   )) %>%
#   # Remove records with missing Slope data
#   filter(!is.na(slope)) %>%
#   
#   # Group by Species and Source Columns and summarize slope presence (N, S, B)
#   group_by(Species.Name) %>%
#   summarize(
#     across(
#       c("Biodiversity Action Plan UK list of priority species",
#         "Red List - Endangered", 
#         "RSPB Priority Species", 
#         "Scottish Biodiversity List", 
#         "Invasive Species", 
#         "Red List - Critically Endangered"),
#       ~ case_when(
#         all(slope2 == "N" & !is.na(.)) ~ "N",
#         all(slope2 == "S" & !is.na(.)) ~ "S",
#         any(slope2 == "N") & any(slope2 == "S") ~ "B",
#         TRUE ~ NA_character_
#       ),
#       .names = "slope_{.col}"  # Renaming columns with a Slope prefix
#     ),
#     Site = paste(unique(slope), collapse = ", ")
#   ) %>%
#   ungroup()
# 
# # remove the no longer needed 'slope' column
# NBN_data_rearranged <- NBN_data_rearranged %>%
#   select(-Site)

# save as 'NBN_atlas_data_summarised.csv'
write.csv(NBN_data_rearranged, "./Data/NBN_atlas_data_summarised.csv", row.names = F)

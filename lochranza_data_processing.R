rm(list=ls())

# organisation of the raw data on recorded bat calls taken from the audiomoths

# check workingdirectory is correct
getwd()
setwd("C:/Users/charl/Documents/University/Year 5 Masters/Semester 1/Professional Skills for Ecologists/1. Field Course/audiomoth_bats/Results")
getwd()
list.files()

# import each of the audiofiles for audio 2 day 1, into one list
file_names <- list.files(path = "day1_audio2", pattern = "\\.csv$", full.names = FALSE)

# change working directory to the folder containing audio 2 data from day 1
setwd("C:/Users/charl/Documents/University/Year 5 Masters/Semester 1/Professional Skills for Ecologists/1. Field Course/audiomoth_bats/Results/day1_audio2")

# create empty object for the processed data
day1_audio2 <- list()

# add additional information required for the database
audio <- list("audio2")
slope <- list("north")
coord_lat <- list("55.7074570")
coord_long <- list("-5.2842280")

# loop through all files in this folder, collating the relevant information and
# saving into the new dataframe
for (i in file_names)  { # loop through list of files
  data <- read.csv(i) # for each file, read it in
  data$time_stamp <- i # save the name as the time_stamp to differentiate each set
  data$audiomoth <- as.character(audio) # add the audiomoth ID to new column called 'audiomoth'
  data$site_ID <- slope # add the slope data is from to the new column called 'site_ID'
  data$lattitude <- as.character(coord_lat) # add the GPS data for this recording
  data$longitude <- as.character(coord_long) # add the GPS data for this recording
  day1_audio2[[i]] <- data # save the completed data frame into the new object
  
}

# print new dataframe to check the code has worked
day1_audio2

# combine the dataframes in the new object into one data frame
library(dplyr)
day1_audio2 <- bind_rows(day1_audio2)
day1_audio2


# repeat process for audio 3 day 1
setwd("C:/Users/charl/Documents/University/Year 5 Masters/Semester 1/Professional Skills for Ecologists/1. Field Course/audiomoth_bats/Results")
file_names <- list.files(path = "day1_audio3", pattern = "\\.csv$", full.names = FALSE)
length(file_names)

setwd("C:/Users/charl/Documents/University/Year 5 Masters/Semester 1/Professional Skills for Ecologists/1. Field Course/audiomoth_bats/Results/day1_audio3")

day1_audio3 <- list()
audio <- list("audio3")
slope <- list("south")
coord_lat <- list("55.6984290")
coord_long <- list("-5.2862850")


for (i in file_names)  {
  data <- read.csv(i)
  data$time_stamp <- i
  data$audiomoth <- as.character(audio)
  data$site_ID <- slope
  data$lattitude <- as.character(coord_lat)
  data$longitude <- as.character(coord_long)
  day1_audio3[[i]] <- data
  
}

day1_audio3

day1_audio3 <- bind_rows(day1_audio3)
day1_audio3


# repeat process for audio 4 day 1
setwd("C:/Users/charl/Documents/University/Year 5 Masters/Semester 1/Professional Skills for Ecologists/1. Field Course/audiomoth_bats/Results")
file_names <- list.files(path = "day1_audio4", pattern = "\\.csv$", full.names = FALSE)
length(file_names)

setwd("C:/Users/charl/Documents/University/Year 5 Masters/Semester 1/Professional Skills for Ecologists/1. Field Course/audiomoth_bats/Results/day1_audio4")

day1_audio4 <- list()
audio <- list("audio4")
slope <- list("south")
coord_lat <- list("55.6970540")
coord_long <- list("-5.2839410")

for (i in file_names)  {
  data <- read.csv(i)
  data$time_stamp <- i
  data$audiomoth <- as.character(audio)
  data$site_ID <- slope
  data$lattitude <- as.character(coord_lat)
  data$longitude <- as.character(coord_long)
  day1_audio4[[i]] <- data
}

day1_audio4

day1_audio4 <- bind_rows(day1_audio4)
day1_audio4

# combine day 1 data into one dataframe
day1_data <- rbind(day1_audio2, day1_audio3, day1_audio4)


# repeat for audio 1 day 2
setwd("C:/Users/charl/Documents/University/Year 5 Masters/Semester 1/Professional Skills for Ecologists/1. Field Course/audiomoth_bats/Results")
file_names <- list.files(path = "day2_audio1", pattern = "\\.csv$", full.names = FALSE)
length(file_names)

setwd("C:/Users/charl/Documents/University/Year 5 Masters/Semester 1/Professional Skills for Ecologists/1. Field Course/audiomoth_bats/Results/day2_audio1")

day2_audio1 <- list()
audio <- list("audio1")
slope <- list("south")
coord_lat <- list("55.7009080")
coord_long <- list("-5.2836060")

for (i in file_names)  {
  data <- read.csv(i)
  data$time_stamp <- i
  data$audiomoth <- as.character(audio)
  data$site_ID <- slope
  data$lattitude <- as.character(coord_lat)
  data$longitude <- as.character(coord_long)
  day2_audio1[[i]] <- data
}

day2_audio1

day2_audio1 <- bind_rows(day2_audio1)
day2_audio1

# repeat the process for audio 2 day 2
setwd("C:/Users/charl/Documents/University/Year 5 Masters/Semester 1/Professional Skills for Ecologists/1. Field Course/audiomoth_bats/Results")
file_names <- list.files(path = "day2_audio2", pattern = "\\.csv$", full.names = FALSE)
length(file_names)

setwd("C:/Users/charl/Documents/University/Year 5 Masters/Semester 1/Professional Skills for Ecologists/1. Field Course/audiomoth_bats/Results/day2_audio2")

day2_audio2 <- list()
audio <- list("audio2")
slope <- list("south")
coord_lat <- list("55.6989950")
coord_long <- list("-5.2823140")

for (i in file_names)  {
  data <- read.csv(i)
  data$time_stamp <- i
  data$audiomoth <- as.character(audio)
  data$site_ID <- slope
  data$lattitude <- as.character(coord_lat)
  data$longitude <- as.character(coord_long)
  day2_audio2[[i]] <- data
}

day2_audio2

day2_audio2 <- bind_rows(day2_audio2)
day2_audio2

# repeat this process for audio 3 day two
setwd("C:/Users/charl/Documents/University/Year 5 Masters/Semester 1/Professional Skills for Ecologists/1. Field Course/audiomoth_bats/Results")
file_names <- list.files(path = "day2_audio3", pattern = "\\.csv$", full.names = FALSE)
length(file_names)

setwd("C:/Users/charl/Documents/University/Year 5 Masters/Semester 1/Professional Skills for Ecologists/1. Field Course/audiomoth_bats/Results/day2_audio3")

day2_audio3 <- list()
audio <- list("audio3")
slope <- list("north")
coord_lat <- list("55.7055170")
coord_long <- list("-5.2824580")

for (i in file_names)  {
  data <- read.csv(i)
  data$time_stamp <- i
  data$audiomoth <- as.character(audio)
  data$site_ID <- slope
  data$lattitude <- as.character(coord_lat)
  data$longitude <- as.character(coord_long)
  day2_audio3[[i]] <- data
}

day2_audio3

day2_audio3 <- bind_rows(day2_audio3)
day2_audio3

# repeat process for audio 4 day 2
setwd("C:/Users/charl/Documents/University/Year 5 Masters/Semester 1/Professional Skills for Ecologists/1. Field Course/audiomoth_bats/Results")
file_names <- list.files(path = "day2_audio4", pattern = "\\.csv$", full.names = FALSE)
length(file_names)

setwd("C:/Users/charl/Documents/University/Year 5 Masters/Semester 1/Professional Skills for Ecologists/1. Field Course/audiomoth_bats/Results/day2_audio4")

day2_audio4 <- list()
audio <- list("audio4")
slope <- list("north")
coord_lat <- list("55.7031450")
coord_long <- list("-5.2790140")

for (i in file_names)  {
  data <- read.csv(i)
  data$time_stamp <- i
  data$audiomoth <- as.character(audio)
  data$site_ID <- slope
  data$lattitude <- as.character(coord_lat)
  data$longitude <- as.character(coord_long)
  day2_audio4[[i]] <- data
}

day2_audio4

day2_audio4 <- bind_rows(day2_audio4)
day2_audio4

# combine day 2 data into one dataframe
day2_data <- rbind(day2_audio1, day2_audio2, day2_audio3, day2_audio4)


# combine both of these into one data frame
all_bat_data <- rbind(day1_data, day2_data)
# print new dataframe to check if this has worked
all_bat_data

# save finished dataframe to documents
setwd("C:/Users/charl/Documents/University/Year 5 Masters/Semester 1/Professional Skills for Ecologists/1. Field Course/audiomoth_bats")
# ensure data types are correct
all_bat_data <- apply(all_bat_data, 2, as.character)
# save as 'audiomoth_data_combined.csv'
write.csv(all_bat_data, "./Results/audiomoth_data_combined.csv", row.names = T)

setwd("C:/Users/charl/Documents/University/Year 5 Masters/Semester 1/Professional Skills for Ecologists/1. Field Course/audiomoth_bats/Results")
all_bat_data <- read.csv("audiomoth_data_combined.csv")
all_bat_data
class(all_bat_data$X)
all_bat_data$X <- as.character(all_bat_data$X)

setwd("C:/Users/charl/Documents/University/Year 5 Masters/Semester 1/Professional Skills for Ecologists/1. Field Course/audiomoth_bats")
all_bat_data <- apply(all_bat_data, 2, as.character)
write.csv(all_bat_data, "./Results/audiomoth_data_combined.csv", row.names = T)
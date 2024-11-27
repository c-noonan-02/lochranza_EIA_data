# clear environment
rm(list=ls())

# import packages needed
library(ggplot2)
library(dplyr)
library(vegan)
library(tidyr)
library(tibble)
# ...

# import full data set
arran_data_full <- read.csv('./Data/arran_data_full.csv')
# check the data set
head(arran_data_full)

# check data types
str(arran_data_full)
arran_data_full$eventID <- as.factor(arran_data_full$eventID)
arran_data_full$site <- as.factor(arran_data_full$site)
arran_data_full$samplingProtocol <- as.factor(arran_data_full$samplingProtocol)
arran_data_full$vernacularName <- as.factor(arran_data_full$vernacularName)
arran_data_full$individualCount <- as.numeric(arran_data_full$individualCount)
# ... need to finish
str(arran_data_full)

# create subsets of the data
invert_data <- arran_data_full %>% slice(1:235)
vert_data <- arran_data_full %>% slice(236:302)
# for total number of vertebrate species use vert_data
# but for counts or diversity yse vert_count_data
vert_count_data <- vert_data %>% slice(47:67)

# ANALYSIS OF INVERTEBRATE BIODIVERSITY 
invert_data <- invert_data %>%
  filter(order != "")

invert_abundances <- invert_data %>%
  group_by(site, order) %>%
  summarise(total_count = sum(individualCount), .groups = "drop")

invert_alpha_div <- invert_data %>%
  group_by(site) %>% # Group by site
  summarise(no_orders = n_distinct(order), .groups = "drop") # Count unique species

ggplot(invert_abundances, aes(x = order, y = total_count, fill = site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_text(
    aes(label = total_count),
    position = position_dodge(width = 0.9), # Adjust text position to align with bars
    vjust = -0.5, # Position text slightly above the bars
    size = 3      # Adjust text size
  ) +
  labs(x = "Order", y = "Order Abundance") +
  scale_fill_manual(values = c("south" = "purple", "north" = "purple4")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# ANALYSIS OF VERTEBRATE BIODIVERSITY
 
# remove camera trap data due to small sample size
vert_count_data2 <- vert_count_data %>%
  filter(samplingProtocol != "Camera_traps")

vert_abundances <- vert_count_data2 %>%
  group_by(site, vernacularName) %>%
  summarise(total_count = sum(individualCount), .groups = "drop")

vert_abundances_zeros <- data.frame(site = character(0), vernacularName = character(0), total_count = integer(0))
#vert_abundances_zeros1 <- data.frame(site = "north", vernacularName = "Red_Deer", total_count = 0)
vert_abundances_zeros2 <- data.frame(site = "south", vernacularName = "Brown_long-eared_bat", total_count = 0)
#vert_abundances_zeros3 <- data.frame(site = "south", vernacularName = "European_Robin", total_count = 0)
vert_abundances_zeros4 <- data.frame(site = "south", vernacularName = "Soprano_pipistrelle", total_count = 0)
vert_abundances_zeros <- rbind(vert_abundances_zeros, vert_abundances_zeros2, vert_abundances_zeros4)
#vert_abundances_zeros <- rbind(vert_abundances_zeros, vert_abundances_zeros1, vert_abundances_zeros2, vert_abundances_zeros3, vert_abundances_zeros4)

vert_abundances <- rbind(vert_abundances, vert_abundances_zeros)

ggplot(vert_abundances, aes(x = vernacularName, y = total_count, fill = site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_text(
    aes(label = total_count),
    position = position_dodge(width = 0.9), # Adjust text position to align with bars
    vjust = -0.5, # Position text slightly above the bars
    size = 3      # Adjust text size
  ) +
  labs(x = "Species", y = "Species Count") +
  scale_fill_manual(values = c("south" = "purple", "north" = "purple4")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# then can do calls per audiomoth!
vert_abundances_by_device <- vert_count_data2 %>%
  group_by(site, eventID, vernacularName) %>%
  summarise(total_count = sum(individualCount), .groups = "drop")

# remove outlier audiomoth
vert_abundances_by_device <- vert_abundances_by_device %>%
  filter(total_count <= 1000)

ggplot(vert_abundances_by_device, aes(x = vernacularName, y = total_count, fill = site)) +
  geom_boxplot(width = 0.4) +
  geom_jitter(width = 0.3, colour = "purple3", alpha = 0.3) +
  labs(x = "Proposed Site", y = "Number of calls recorded per audiomoth device") +
  #scale_x_discrete(labels = c("North Slope", "South Slope")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

vert_alpha_div <- vert_data %>%
  group_by(site) %>% # Group by site
  summarise(no_species = n_distinct(vernacularName), .groups = "drop") # Count unique species
#plot this!!!


# ALPHA DIVERSITY
# Summarise the number of species per site
unique(arran_data_full$class)
arran_data_full <- arran_data_full %>%
  filter(individualCount != 0)
unique(arran_data_full$class)

arran_alpha_div <- arran_data_full %>%
  group_by(site, class) %>% # Group by site
  summarise(no_species = n_distinct(vernacularName), .groups = "drop") # Count unique species

ggplot(arran_alpha_div, aes(x = class, y = no_species, fill = site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_text(
    aes(label = no_species),
    position = position_dodge(width = 0.9), # Adjust text position to align with bars
    vjust = -0.5, # Position text slightly above the bars
    size = 3      # Adjust text size
  ) +
  labs(x = "Class", y = "Species Count") +
  scale_fill_manual(values = c("south" = "purple", "north" = "purple4")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
# overall pretty similar - but south fairing slightly better?


# BETA DIVERSITY - SHANNONS

# rearrange data set
arran_data_rearranged <- arran_alpha_div %>%
  tidyr::pivot_wider(names_from = class, values_from = no_species, values_fill = 0) %>%
  column_to_rownames(var = "site")
# Set the 'site' column as row names using tibble's function
rownames(arran_data_rearranged) <- arran_data_rearranged$site
#arran_data_rearranged <- arran_data_rearranged[, -1]  # Remove the 'site' column

# shannons
arran_shannons <- diversity(arran_data_rearranged[,1:11])
arran_shannons <- as.numeric(arran_shannons)

shannons_south <- arran_shannons[2]
shannons_north <- arran_shannons[1]

arran_diversity_indicies <- data.frame(diversity_index = character(0), south = numeric(0), north = numeric(0))
arran_shannons <- data.frame(diversity_index = "shannons", south = shannons_south, north = shannons_north)
arran_diversity_indicies <- rbind(arran_diversity_indicies, arran_shannons)

# simpsons
arran_simpsons <- diversity(arran_data_rearranged[,1:11], index = "simpson")
arran_simpsons <- as.numeric(arran_simpsons)

simpsons_south <- arran_simpsons[2]
simpsons_north <- arran_simpsons[1]

arran_simpsons <- data.frame(diversity_index = "simpsons", south = simpsons_south, north = simpsons_north)
arran_diversity_indicies <- rbind(arran_diversity_indicies, arran_simpsons)
# south has higher diversity

arran_diversity_indicies

# HAVE DONE BY SLOPE, WHAT ABOUT BY SAMPLE BY SLOPE AS IN MY DISS?
# could then use more interesting plots like boxplots

# ALPHA DIVERSITY
# Summarise the number of species per site
unique(arran_data_full$class)
arran_data_full <- arran_data_full %>%
  filter(individualCount != 0)
unique(arran_data_full$class)

sample_class <- arran_data_full %>%
  group_by(site, eventID, class) %>% # Group by site
  summarise(no_species = n_distinct(vernacularName), .groups = "drop") # Count unique species

ggplot(sample_class, aes(x = site, y = no_species)) +
  geom_boxplot(width = 0.4, fill = "thistle") +
  geom_jitter(width = 0.3, colour = "purple3", alpha = 0.3) +
  labs(x = "Proposed Site", y = "Number of classes per sampling event") +
  scale_x_discrete(labels = c("North Slope", "South Slope")) +
  theme_bw()

unique(arran_data_full$order)
arran_data_full <- arran_data_full %>%
  filter(individualCount != 0)
arran_data_full <- arran_data_full %>%
  filter(order != "")
unique(arran_data_full$order)

sample_order <- arran_data_full %>%
  group_by(site, eventID, order) %>% # Group by site
  summarise(no_species = n_distinct(vernacularName), .groups = "drop") # Count unique species

ggplot(sample_order, aes(x = site, y = no_species)) +
  geom_boxplot(width = 0.4, fill = "thistle") +
  geom_jitter(width = 0.3, colour = "purple3", alpha = 0.3) +
  labs(x = "Proposed Site", y = "Number of orders per sampling event") +
  scale_x_discrete(labels = c("North Slope", "South Slope")) +
  theme_bw()


# DIVERSITY
# rearrange dataset
# don't take from alpha, take from main dataframe
sample_data_rearranged <- sample_class %>%
  tidyr::pivot_wider(names_from = order, values_from = no_species, values_fill = 0) %>%
  column_to_rownames(var = "site")



# stuff
#select(-genre, -spotify_monthly_listeners, -year_founded)
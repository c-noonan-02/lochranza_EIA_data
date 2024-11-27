# clear environment
rm(list=ls())

# import packages needed
library(ggplot2)
library(dplyr)
library(vegan)
library(tidyr)
library(tibble)
library(cowplot)
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

vert_data <- arran_data_full %>% slice(236:302)
# for total number of vertebrate species use vert_data
# but for counts or diversity yse vert_count_data
vert_count_data <- vert_data %>% slice(47:67)




# ANALYSIS OF INVERTEBRATE BIODIVERSITY 
# subset the data on invertebrates
invert_data <- arran_data_full %>% slice(1:235)

# remove any instances witch could not be identified to order
invert_data <- invert_data %>%
  filter(order != "")

# abundances of each invertebrate order
invert_abundances <- invert_data %>%
  group_by(site, order) %>%
  summarise(total_count = sum(individualCount), .groups = "drop")
# add zeros for orders not present on one slope but present on another
invert_abundances <- invert_abundances %>%
  complete(order, site, fill = list(total_count = 0))

# plot abundances of each invertebrate order
ggplot(invert_abundances, aes(x = order, y = total_count, fill = site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  ylim(0,85) +
  geom_text(
    aes(label = total_count),
    position = position_dodge(width = 0.9), # Adjust text position to align with bars
    vjust = -0.5, # Position text slightly above the bars
    size = 3      # Adjust text size
  ) +
  labs(x = "Order", y = "Order Abundance") +
  scale_fill_manual(values = c("Slope A" = "purple", "Slope B" = "purple4"), name = "Proposed Site") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
# still need to adjust text size?
# change Key labels and heading
# spread out bars?

# calculate alpha diversity (here number of unique orders) for each site
invert_alpha_div <- invert_data %>%
  group_by(site) %>% # Group by site
  summarise(no_orders = n_distinct(order), .groups = "drop") # Count unique species

# analysis of invertebrate data - sweep netting
# subset of dataset including only data collected on terrestrial invertebrates using sweep netting techniques
invert_sweep_data1 <- invert_data[(1:26),]
invert_sweep_data2 <- invert_data[(49:126),]
invert_sweep_data <- rbind(invert_sweep_data1, invert_sweep_data2)
# remove any instances witch could not be identified to order
invert_sweep_data <- invert_sweep_data %>%
  filter(order != "")
# abundances of each invertebrate order
invert_sweep_abundances <- invert_sweep_data %>%
  group_by(site, order) %>%
  summarise(total_count = sum(individualCount), .groups = "drop")
# add zeros for orders not present on one slope but present on another
invert_sweep_abundances <- invert_sweep_abundances %>%
  complete(order, site, fill = list(total_count = 0))
# plot this data
invert_sweep_abundances_plot <- ggplot(invert_sweep_abundances, aes(x = order, y = total_count, fill = site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  ylim(0, 85) +            # Set y-axis limits from 0 to 500
  geom_text(
    aes(label = total_count),
    position = position_dodge(width = 0.9), # Adjust text position to align with bars
    vjust = -0.5, # Position text slightly above the bars
    size = 3      # Adjust text size
  ) +
  labs(x = "Order", y = "Order Abundance", title = "Terrestrial Sweep Netting") +
  scale_fill_manual(values = c("Slope A" = "purple", "Slope B" = "purple4"), name = "Proposed Site") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
# calculate alpha diversity ( number of unique orders) for each site
invert_sweep_div <- invert_sweep_data %>%
  group_by(site) %>% # Group by site
  summarise(no_orders = n_distinct(order), .groups = "drop") # Count unique species

# analysis of invertebrate data - moth traps
invert_moth_data <- invert_data[(27:48), ]
# remove any instances witch could not be identified to order
invert_moth_data <- invert_moth_data %>%
  filter(order != "")
# abundances of each invertebrate order
invert_moth_abundances <- invert_moth_data %>%
  group_by(site, order) %>%
  summarise(total_count = sum(individualCount), .groups = "drop")
# add zeros for orders not present on one slope but present on another
invert_moth_abundances <- invert_moth_abundances %>%
  complete(order, site, fill = list(total_count = 0))
# plot this data
invert_moth_abundances_plot <- ggplot(invert_moth_abundances, aes(x = order, y = total_count, fill = site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  ylim(0, 18) +
  geom_text(
    aes(label = total_count),
    position = position_dodge(width = 0.9), # Adjust text position to align with bars
    vjust = -0.5, # Position text slightly above the bars
    size = 3      # Adjust text size
  ) +
  labs(x = "Order", y = "Order Abundance", title = "Moth Trap Sampling") +
  scale_fill_manual(values = c("Slope A" = "purple", "Slope B" = "purple4"), name = "Proposed Site") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
# calculate alpha diversity ( number of unique orders) for each site
invert_moth_div <- invert_moth_data %>%
  group_by(site) %>% # Group by site
  summarise(no_orders = n_distinct(order), .groups = "drop") # Count unique species

# analysis of invertebrate data - streams
invert_stream_data <- invert_data[(127:204), ]
# remove any instances witch could not be identified to order
invert_stream_data <- invert_stream_data %>%
  filter(order != "")
# abundances of each invertebrate order
invert_stream_abundances <- invert_stream_data %>%
  group_by(site, order) %>%
  summarise(total_count = sum(individualCount), .groups = "drop")
# add zeros for orders not present on one slope but present on another
invert_stream_abundances <- invert_stream_abundances %>%
  complete(order, site, fill = list(total_count = 0))
# plot this data
invert_stream_abundances_plot <- ggplot(invert_stream_abundances, aes(x = order, y = total_count, fill = site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  ylim(0, 17) +
  geom_text(
    aes(label = total_count),
    position = position_dodge(width = 0.9), # Adjust text position to align with bars
    vjust = -0.5, # Position text slightly above the bars
    size = 3      # Adjust text size
  ) +
  labs(x = "Order", y = "Order Abundance", title = "Stream Kick Sampling") +
  scale_fill_manual(values = c("Slope A" = "purple", "Slope B" = "purple4"), name = "Proposed Site") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
# calculate alpha diversity ( number of unique orders) for each site
invert_stream_div <- invert_stream_data %>%
  group_by(site) %>% # Group by site
  summarise(no_orders = n_distinct(order), .groups = "drop") # Count unique species

# analysis of invertebrate data - bog
invert_bog_data <- invert_data[(205:223), ]
# remove any instances witch could not be identified to order
invert_bog_data <- invert_bog_data %>%
  filter(order != "")
# abundances of each invertebrate order
invert_bog_abundances <- invert_bog_data %>%
  group_by(site, order) %>%
  summarise(total_count = sum(individualCount), .groups = "drop")
# add zeros for orders not present on one slope but present on another
invert_bog_abundances <- invert_bog_abundances %>%
  complete(order, site, fill = list(total_count = 0))
# plot this data
invert_bog_abundances_plot <- ggplot(invert_bog_abundances, aes(x = order, y = total_count, fill = site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  ylim(0, 25) +
  geom_text(
    aes(label = total_count),
    position = position_dodge(width = 0.9), # Adjust text position to align with bars
    vjust = -0.5, # Position text slightly above the bars
    size = 3      # Adjust text size
  ) +
  labs(x = "Order", y = "Order Abundance", title = "Bog Sampling") +
  scale_fill_manual(values = c("Slope A" = "purple", "Slope B" = "purple4"), name = "Proposed Site") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
# calculate alpha diversity ( number of unique orders) for each site
invert_bog_div <- invert_bog_data %>%
  group_by(site) %>% # Group by site
  summarise(no_orders = n_distinct(order), .groups = "drop") # Count unique species

# combine all invert habitat order abundances
# call relevant plots, in panel display
plot_grid(invert_sweep_abundances_plot, invert_moth_abundances_plot, invert_stream_abundances_plot, invert_bog_abundances_plot, labels = "AUTO")

# combine all invert habitat order diversities
invert_sweep_div$sample <- c("Terrestrial Sweep Netting")
invert_moth_div$sample <- c("Moth Traps")
invert_stream_div$sample <- c("Stream Kick Sampling")
invert_bog_div_nrow <- data.frame(site = "Slope B", no_orders = 0)
invert_bog_div <- rbind(invert_bog_div, invert_bog_div_nrow)
invert_bog_div$sample <- c("Bog Sampling")
invert_alpha_div$sample <- c("Total Slope Sample")
invert_sample_div <- rbind(invert_sweep_div, invert_moth_div, invert_stream_div, invert_bog_div, invert_alpha_div)
# plot invert div data
ggplot(invert_sample_div, aes(x = sample, y = no_orders, fill = site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  ylim(0, 18) +
  geom_text(
    aes(label = no_orders),
    position = position_dodge(width = 0.9), # Adjust text position to align with bars
    vjust = -0.5, # Position text slightly above the bars
    size = 3      # Adjust text size
  ) +
  labs(x = "Sampling Method", y = "Number of invertebrate orders") + 
  scale_fill_manual(values = c("Slope A" = "purple", "Slope B" = "purple4"), name = "Proposed Site") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


# ANALYSIS OF VERTEBRATE BIODIVERSITY
 
# remove camera trap data due to small sample size
vert_count_data <- vert_count_data %>%
  filter(samplingProtocol != "Camera traps")

# rearrange for total count for each species by slope
vert_abundances <- vert_count_data %>%
  group_by(site, vernacularName) %>%
  summarise(total_count = sum(individualCount), .groups = "drop")

# add zeros for orders not present on one slope but present on another
vert_abundances <- vert_abundances %>%
  mutate(vernacularName = as.character(vernacularName),
         site = as.character(site)
  ) %>%
  complete(vernacularName, site, fill = list(total_count = 0))


ggplot(vert_abundances, aes(x = vernacularName, y = total_count, fill = site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  ylim(0,17600) +
  geom_text(
    aes(label = total_count),
    position = position_dodge(width = 0.9), # Adjust text position to align with bars
    vjust = -0.5, # Position text slightly above the bars
    size = 3      # Adjust text size
  ) +
  labs(x = "Bat Species*", y = "Species Call Occurrence") +
  scale_fill_manual(values = c("Slope A" = "purple", "Slope B" = "purple4"), name = "Proposed Site") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# then can do calls per audiomoth!
# no - not informative, data too messy/ sample too small

# number of vertebrate species present on each slope, from all data
vert_alpha_div <- vert_data %>%
  group_by(site) %>% # Group by site
  summarise(no_species = n_distinct(vernacularName), .groups = "drop") # Count unique species

# transect vertebrate data
vert_transect_data <- vert_data[(1:46), ]
# no abundance data for this sampling technique, so cannot plot this
# calculate alpha diversity (number of unique orders) for each site
vert_transect_div <- vert_transect_data %>%
  group_by(site) %>% # Group by site
  summarise(no_species = n_distinct(vernacularName), .groups = "drop") # Count unique species

# audiomoth vertebrate data
vert_audiomoth_data <- vert_data[(47:65), ]
# abundances done above
# calculate alpha diversity (number of unique orders) for each site
vert_audiomoth_div <- vert_audiomoth_data %>%
  group_by(site) %>% # Group by site
  summarise(no_species = n_distinct(vernacularName), .groups = "drop") # Count unique species

# camera trap vertebrate data
vert_camera_data <- vert_data[(66:67), ]
# not doing abundances as only 2 species
# calculate alpha diversity (number of unique orders) for each site
vert_camera_div <- vert_camera_data %>%
  group_by(site) %>% # Group by site
  summarise(no_species = n_distinct(vernacularName), .groups = "drop") # Count unique species

# combine all invert habitat order diversities
vert_transect_div$sample <- c("Transect Walking")
vert_audiomoth_div$sample <- c("Audiomoths")
vert_camera_div$sample <- c("Camera Traps")
vert_alpha_div$sample <- c("Total Slope Sample")
vert_total_div <- vert_alpha_div
vert_sample_div <- rbind(vert_transect_div, vert_audiomoth_div, vert_camera_div, vert_total_div)

# plot vert div data
ggplot(vert_sample_div, aes(x = sample, y = no_species, fill = site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  ylim(0, 26) +
  geom_text(
    aes(label = no_species),
    position = position_dodge(width = 0.9), # Adjust text position to align with bars
    vjust = -0.5, # Position text slightly above the bars
    size = 3      # Adjust text size
  ) +
  labs(x = "Sampling Method", y = "Number of vertebrate species") + 
  scale_fill_manual(values = c("Slope A" = "purple", "Slope B" = "purple4"), name = "Proposed Site") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
# still need to adjust text sizes



# BETA DIVERSITY

# remove vertebrate presence data
arran_data_full2 <- arran_data_full[-(236:281), ]

arran_class_abundance <- arran_data_full2 %>%
  group_by(site, class) %>% # Group by site
  summarise(total_count = sum(individualCount), .groups = "drop") # sum abundance of each unique class

# remove empty rows of data - i.e. samples where nothing was found
arran_class_abundance <- arran_class_abundance %>%
  filter(total_count != 0)

# rearrange data set
arran_data_rearranged <- arran_class_abundance %>%
  tidyr::pivot_wider(names_from = class, values_from = total_count, values_fill = 0) %>%
  column_to_rownames(var = "site")

# Set the 'site' column as row names using tibble's function
rownames(arran_data_rearranged) <- arran_data_rearranged$site
#arran_data_rearranged <- arran_data_rearranged[, -1]  # Remove the 'site' column

# shannons
arran_shannons <- diversity(arran_data_rearranged[,1:11])
arran_shannons <- as.numeric(arran_shannons)

shannons_slopeA <- arran_shannons[2]
shannons_slopeB <- arran_shannons[1]

arran_diversity_indicies <- data.frame(diversity_index = character(0), Slope_A = numeric(0), Slope_B = numeric(0))
arran_shannons <- data.frame(diversity_index = "shannons", Slope_A = shannons_slopeA, Slope_B = shannons_slopeB)
arran_diversity_indicies <- rbind(arran_diversity_indicies, arran_shannons)

# simpsons
arran_simpsons <- diversity(arran_data_rearranged[,1:11], index = "simpson")
arran_simpsons <- as.numeric(arran_simpsons)

simpsons_slopeA <- arran_simpsons[2]
simpsons_slopeB <- arran_simpsons[1]

arran_simpsons <- data.frame(diversity_index = "simpsons", Slope_A = simpsons_slopeA, Slope_B = simpsons_slopeB)
arran_diversity_indicies <- rbind(arran_diversity_indicies, arran_simpsons)
# Slope A has higher diversity

arran_diversity_indicies

# rearrange to long form to plot
arran_diversity_long <- arran_diversity_indicies %>%
  pivot_longer(cols = starts_with("Slope"), names_to = "Slope", values_to = "Diversity") %>%
  rename(Index = diversity_index)

# plot diversity indices
ggplot(arran_diversity_long, aes(x = Slope, y = Diversity)) +
  geom_point(aes(colour = Index), na.rm = T, size = 3) +
  labs(x = "Proposed Site", y = "Class Diversity", colour = "Diversity Indices") +
  scale_x_discrete(labels = c("Slope A", "Slope B"))+
  scale_colour_manual(values = c("thistle2", "thistle")) +
  theme_bw()


# repeat for sweep netting only

# repeat for moth trap only
invert_moth_abundances
# rearrange data set
invert_moth_data_rearranged <- invert_moth_abundances %>%
  tidyr::pivot_wider(names_from = order, values_from = total_count, values_fill = 0) %>%
  column_to_rownames(var = "site")
# Set the 'site' column as row names using tibble's function
rownames(invert_moth_data_rearranged) <- invert_moth_data_rearranged$site
# shannons
invert_moth_shannons <- diversity(invert_moth_data_rearranged)
invert_moth_shannons_slopeA <- invert_moth_shannons[2]
invert_moth_shannons_slopeB <- invert_moth_shannons[1]
# simpsons
invert_moth_simpsons <- diversity(invert_moth_data_rearranged, index = "simpson")
invert_moth_simpsons_slopeA <- invert_moth_simpsons[2]
invert_moth_simpsons_slopeB <- invert_moth_simpsons[1]
# combine moth indices
invert_moth_div_indices <- data.frame(diversity_index = character(0), Slope_A = numeric(0), Slope_B = numeric(0), sample = character(0))
invert_moth_shannons <- data.frame(diversity_index = "shannons", Slope_A = invert_moth_shannons_slopeA, Slope_B = invert_moth_shannons_slopeB, sample = "Moth Traps")
invert_moth_simpsons <- data.frame(diversity_index = "simpsons", Slope_A = invert_moth_simpsons_slopeA, Slope_B = invert_moth_simpsons_slopeB, sample = "Moth Traps")
invert_moth_diversity_indices <- rbind(invert_moth_div_indices, invert_moth_shannons, invert_moth_simpsons)

# repeat for streams only
invert_stream_abundances
# rearrange data set
invert_stream_data_rearranged <- invert_stream_abundances %>%
  tidyr::pivot_wider(names_from = order, values_from = total_count, values_fill = 0) %>%
  column_to_rownames(var = "site")
# Set the 'site' column as row names using tibble's function
rownames(invert_stream_data_rearranged) <- invert_stream_data_rearranged$site
# shannons
invert_stream_shannons <- diversity(invert_stream_data_rearranged)
invert_stream_shannons_slopeA <- invert_stream_shannons[2]
invert_stream_shannons_slopeB <- invert_stream_shannons[1]
# simpsons
invert_stream_simpsons <- diversity(invert_stream_data_rearranged, index = "simpson")
invert_stream_simpsons_slopeA <- invert_stream_simpsons[2]
invert_stream_simpsons_slopeB <- invert_stream_simpsons[1]
# combine stream indices
invert_stream_div_indices <- data.frame(diversity_index = character(0), Slope_A = numeric(0), Slope_B = numeric(0), sample = character(0))
invert_stream_shannons <- data.frame(diversity_index = "shannons", Slope_A = invert_stream_shannons_slopeA, Slope_B = invert_stream_shannons_slopeB, sample = "Stream Kick Sampling")
invert_stream_simpsons <- data.frame(diversity_index = "simpsons", Slope_A = invert_stream_simpsons_slopeA, Slope_B = invert_stream_simpsons_slopeB, sample = "Stream Kick Sampling")
invert_stream_diversity_indices <- rbind(invert_stream_div_indices, invert_stream_shannons, invert_stream_simpsons)

# repeat for bog only
invert_bog_abundances
# rearrange data set
invert_bog_data_rearranged <- invert_bog_abundances %>%
  tidyr::pivot_wider(names_from = order, values_from = total_count, values_fill = 0) %>%
  column_to_rownames(var = "site")
# Set the 'site' column as row names using tibble's function
rownames(invert_bog_data_rearranged) <- invert_bog_data_rearranged$site
# shannons
invert_bog_shannons <- diversity(invert_bog_data_rearranged)
invert_bog_shannons_slopeA <- invert_bog_shannons[2]
invert_bog_shannons_slopeB <- invert_bog_shannons[1]
# simpsons
invert_bog_simpsons <- diversity(invert_bog_data_rearranged, index = "simpson")
invert_bog_simpsons_slopeA <- invert_bog_simpsons[2]
invert_bog_simpsons_slopeB <- invert_bog_simpsons[1]
# combine bog indices
invert_bog_div_indices <- data.frame(diversity_index = character(0), Slope_A = numeric(0), Slope_B = numeric(0), sample = character(0))
invert_bog_shannons <- data.frame(diversity_index = "shannons", Slope_A = invert_stream_shannons_slopeA, Slope_B = invert_stream_shannons_slopeB, sample = "Bog Sampling")
invert_bog_simpsons <- data.frame(diversity_index = "simpsons", Slope_A = invert_stream_simpsons_slopeA, Slope_B = invert_stream_simpsons_slopeB, sample = "Bog Sampling")
invert_bog_diversity_indices <- rbind(invert_stream_div_indices, invert_stream_shannons, invert_stream_simpsons)

# repeat for bats only
# abundances of each invertebrate order
vert_audiomoth_abundances <- vert_abundances
# rearrange data set
vert_audiomoth_data_rearranged <- vert_audiomoth_abundances %>%
  tidyr::pivot_wider(names_from = vernacularName, values_from = total_count, values_fill = 0) %>%
  column_to_rownames(var = "site")
# Set the 'site' column as row names using tibble's function
rownames(vert_audiomoth_data_rearranged) <- vert_audiomoth_data_rearranged$site
# shannons
vert_audiomoth_shannons <- diversity(vert_audiomoth_data_rearranged)
vert_audiomoth_shannons_slopeA <- vert_audiomoth_shannons[2]
vert_audiomoth_shannons_slopeB <- vert_audiomoth_shannons[1]
# simpsons
vert_audiomoth_simpsons <- diversity(vert_audiomoth_data_rearranged, index = "simpson")
vert_audiomoth_simpsons_slopeA <- vert_audiomoth_simpsons[2]
vert_audiomoth_simpsons_slopeB <- vert_audiomoth_simpsons[1]
# combine audiomoth indices
vert_audiomoth_div_indices <- data.frame(diversity_index = character(0), Slope_A = numeric(0), Slope_B = numeric(0), sample = character(0))
vert_audiomoth_shannons <- data.frame(diversity_index = "shannons", Slope_A = vert_audiomoth_shannons_slopeA, Slope_B = vert_audiomoth_shannons_slopeB, sample = "Audiomoths")
vert_audiomoth_simpsons <- data.frame(diversity_index = "simpsons", Slope_A = vert_audiomoth_simpsons_slopeA, Slope_B = vert_audiomoth_simpsons_slopeB, sample = "Audiomoths")
vert_audiomoth_diversity_indices <- rbind(vert_audiomoth_div_indices, vert_audiomoth_shannons, vert_audiomoth_simpsons)

all_sample_div <- rbind(invert_moth_diversity_indices, invert_stream_diversity_indices, invert_bog_diversity_indices, vert_audiomoth_diversity_indices)
#vert_sample_div <- rbind(invert_sweep_diversity_indices, invert_moth_diversity_indices, invert_stream_diversity_indices, invert_bog_diversity_indices, vert_audiomoth_diversity_indices)




# stuff
#select(-genre, -spotify_monthly_listeners, -year_founded)
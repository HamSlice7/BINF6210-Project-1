####load the necessary libraries----
library("tidyverse")
library("readr")



#### create a table of species and latutide to get an idea of the data
sort(table(df_t$lat), decreasing = TRUE)


####organizing testudines data frame----

#load Testudines data frame
df_t <- read_tsv("testudines_bold_data.txt")

#get a data frame consisting of species name and latitude while excluding all NA values
df_testudines_lat_species <- na.omit(df_t[c("species_name", "lat")])


####subdivide the data frame into 15 degree intervals of latitude from -60 degrees latitdude to 60 degrees latitude ----

#selecting  species data for latitudes 0-15
df_testudines_015 <- df_testudines_lat_species[df_testudines_lat_species$lat >= 0 & df_testudines_lat_species$lat <= 15,]

#selecting species data for latitudes 15-30 (should it be 16-30?)
df_testudines_1630 <- df_testudines_lat_species[df_testudines_lat_species$lat > 15 & df_testudines_lat_species$lat <= 30,]

#selecting species data for latitudes 31-45 
df_testudines_3145 <- df_testudines_lat_species[df_testudines_lat_species$lat > 30 & df_testudines_lat_species$lat <= 45,]

#selecting species data for latitudes 46-60
df_testudines_4660 <- df_testudines_lat_species[df_testudines_lat_species$lat > 45 & df_testudines_lat_species$lat <= 60,]




####find the number of unique species in each latitudinal subdivision----

#finding the number of unique species from latitudes 0-15
unique_015 <- unique(df_testudines_015$species_name)
length(unique_015)

#finding the number of unique species from latitudes 16-30
unique_1630 <- unique(df_testudines_1630$species_name)
length(unique_1630)

#finding the number of unique species from latitudes 31-45
unique_3145 <- unique(df_testudines_3145$species_name)
length(unique_3145)

#finding the number of unique species from latitdues 46-60
unique_4660 <- unique(df_testudines_4660$species_name)
length(unique_4660)


####plot the number of species of testudines against latitude----

x <- c(length(unique_015), length(unique_1630), length(unique_3145), length(unique_4660))
y <- c("0-15", "16-30", "31-45", "46-60")

?barplot
barplot(height = x, names.arg = y, col = "darkgreen", main = "Species richness of Testudines and Latitude", xlab = "Latitdues", ylab = "Number of Species")

####Species richness and country (shows under-representation maybe)----
df_t_country_species = na.omit(df_t[c( "processid","species_name", "country", "lat")])


testudines_species_count_country <- aggregate(processid ~ country, data = df_t_country_species, FUN = length)


####Number of samples based on latitude----
df_t_id_lat = na.omit(df_t[c("processid", "lat")])

lat_bins <- cut(df_t_id_lat$lat, breaks = c(-90, -75, -60, -45, -30, -15, 0, 15, 30, 45, 60, 75, 90))

num_of_samples_lat <- table(lat_bins)

num_of_samples_lat


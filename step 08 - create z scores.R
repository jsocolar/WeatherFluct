#### Preparation of Z-Scores Bird Dataset
## April 11, 2019
## Austin Spence

# Set working directory

#setwd("C:/Users/Tingley Lab_2/Dropbox/BBS_Project") #Tingley lab computer
#setwd("/Users/austinspence/Dropbox/BBS_Project")
# Load in the full data set

load("./BBS_Data/breed_postbreed_month_weather.Rdata")


# A function that takes a column name, a list of species for which z-scores are desired,
# and a lag_weather dataframe as input, and gives as output a lag_weather dataframe with a 
# column of z-scores appended, giving the z-score for the desired species and NA elsewhere
# Repeated application of lag_weather <- zsc(lag_weather, column_name, z_score_species) for the 
# desired columns will give the desired final output
zsc <- function(lag_weather, column_name, z_score_species){
  oldcol <- which(names(lag_weather) == column_name)
  newcol <- rep(NA, nrow(lag_weather))
  for(i in z_score_species){
    lag_SP <- lag_weather[which(lag_weather$aou == i), ]
    unique_routes <- lag_SP[!duplicated(lag_SP$routeID),]
    unique_routes$zscores <- scale(unique_routes[, oldcol])
    for(j in unique_routes$routeID){
      newcol[lag_weather$routeID == j & lag_weather$aou == i] <- unique_routes$zscores[unique_routes$routeID == j]
    }
  }
  lag_weather <- cbind(lag_weather, newcol)
  names(lag_weather)[ncol(lag_weather)] <- paste0(column_name, "_zscore")
  return(lag_weather)
}




#### Create the z_scores for desired columns across all the sites (gives you if it is in warm or cold part of range)
aou_species <- unique(breed_postbreed_month_weather$aou)

# Breeding weather
full_birds <- zsc(lag_weather = breed_postbreed_month_weather, column_name = c("breeding_mean_high"), z_score_species = aou_species)  # where is it at in the species breeding high thermal range?
full_birds <- zsc(lag_weather = full_birds, column_name = c("breeding_mean_low"), z_score_species = aou_species)                   # where is it at in the species breeding low thermal range?
full_birds <- zsc(lag_weather = full_birds, column_name = c("breeding_mean_precip"), z_score_species = aou_species)                # where is it at in the species breeding precipitation range?
full_birds <- zsc(lag_weather = full_birds, column_name = c("breeding_mean_swe"), z_score_species = aou_species)                   # where is it at in the species breeding snow range?

# Post-breeding weather
full_birds <- zsc(lag_weather = full_birds, column_name = c("post_breeding_mean_high"), z_score_species = aou_species)                  # where is it at in the species post-breeding high thermal range?
full_birds <- zsc(lag_weather = full_birds, column_name = c("post_breeding_mean_low"), z_score_species = aou_species)                   # where is it at in the species post-breeding low thermal range?
full_birds <- zsc(lag_weather = full_birds, column_name = c("post_breeding_mean_precip"), z_score_species = aou_species)                # where is it at in the species post-breeding precipitation range?
full_birds <- zsc(lag_weather = full_birds, column_name = c("post_breeding_mean_swe"), z_score_species = aou_species)                   # where is it at in the species post-breeding snow range?

# July/August weather
full_birds <- zsc(lag_weather = full_birds, column_name = c("mean_high_jul_aug"), z_score_species = aou_species)                   # where is it at in the species jul/aug high thermal range?
full_birds <- zsc(lag_weather = full_birds, column_name = c("mean_precip_jul_aug"), z_score_species = aou_species)                 # where is it at in the species jul/aug low thermal range?





#### Create the z_scores manually to see if it was an anomolous temperatre/precip that year
# Breeding weather
full_birds$breeding_avg_high_year_1_zscore <- (full_birds$breeding_avg_high_year_1 - full_birds$breeding_mean_high)/full_birds$breeding_sd_high # was this specific year an anomoly for mean high temp?
full_birds$breeding_avg_low_year_1_zscore <- (full_birds$breeding_avg_low_year_1 - full_birds$breeding_mean_low)/full_birds$breeding_sd_low # was this specific year an anomoly for mean low temp?
full_birds$breeding_avg_precip_year_1_zscore <- (full_birds$breeding_avg_precip_year_1 - full_birds$breeding_mean_precip)/full_birds$breeding_sd_precip # was this specific year an anomoly for mean precip?
full_birds$breeding_avg_swe_year_1_zscore <- (full_birds$breeding_avg_swe_year_1 - full_birds$breeding_mean_swe)/full_birds$breeding_sd_swe # was this specific year an anomoly for mean precip?

# Post-breeding weather
full_birds$post_breeding_avg_high_year_1_zscore <- (full_birds$post_breeding_avg_high_year_1 - full_birds$post_breeding_mean_high)/full_birds$post_breeding_sd_high # was this specific year an anomoly for mean high temp?
full_birds$post_breeding_avg_low_year_1_zscore <- (full_birds$post_breeding_avg_low_year_1 - full_birds$post_breeding_mean_low)/full_birds$post_breeding_sd_low # was this specific year an anomoly for mean low temp?
full_birds$post_breeding_avg_precip_year_1_zscore <- (full_birds$post_breeding_avg_precip_year_1 - full_birds$post_breeding_mean_precip)/full_birds$post_breeding_sd_precip # was this specific year an anomoly for mean precip?
full_birds$post_breeding_avg_swe_year_1_zscore <- (full_birds$post_breeding_avg_swe_year_1 - full_birds$post_breeding_mean_swe)/full_birds$post_breeding_sd_swe # was this specific year an anomoly for mean precip?

# July/August weather
full_birds$monthly_avg_high_13_year_1_zscore <- (full_birds$monthly_avg_high_13_year_1 - full_birds$mean_high_jul_aug)/full_birds$sd_high_jul_aug #was this specific year an anomoly for jul/aug high temp?
full_birds$monthly_avg_precip_13_year_1_zscore <- (full_birds$monthly_avg_precip_13_year_1 - full_birds$mean_precip_jul_aug)/full_birds$sd_precip_jul_aug #was this specific year an anomoly for jul/aug high precip?

#save(full_birds, file = "BBS_Data/full_birds_dataset.Rdata")

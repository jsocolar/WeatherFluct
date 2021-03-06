#### Creating the weather variables based on average breeding times rather than months
## Created March 15, 2019
## Austin

setwd("C:/Users/Tingley Lab_2/Dropbox/BBS_Project") #Tingley lab computer

# load in the weather data

load("BBS_routes_sp.Rdata")
load("wd_list.Rdata")

## Make the route ID and delete some unimportant info
BBS_routes_sp$routeID <- rep(NA, dim(BBS_routes_sp)[1])
BBS_routes_sp$routeID <- paste(BBS_routes_sp$countrynum, BBS_routes_sp$statenum, BBS_routes_sp$Route, sep=".")
BBS_routes_sp <- BBS_routes_sp[ , -c(1:5)]

# Pull the weather data for each unique breeding day found from Jacob's projected breeding date code 
# based on latitude and elevation - include up to 45 days out from the start of that breeding date.
# At the end, find the average across all years for these predicted breeding dates. 

for(i in 1:dim(BBS_routes_sp)[1]){
  wd <- wd_list[[i]]
  wd_1 <- wd[wd$yday > 85 & wd$yday < 132, ] # weather data for jday breeding prediction
  wd_2 <- wd[wd$yday > 86 & wd$yday < 133, ] #  weather data for jday breeding prediction
  wd_3 <- wd[wd$yday > 87 & wd$yday < 134, ] #  weather data for jday breeding prediction
  wd_4 <- wd[wd$yday > 88 & wd$yday < 135, ] #  weather data for jday breeding prediction
  wd_5 <- wd[wd$yday > 89 & wd$yday < 136, ] #  weather data for jday breeding prediction
  wd_6 <- wd[wd$yday > 90 & wd$yday < 137, ] #  weather data for jday breeding prediction
  wd_7 <- wd[wd$yday > 91 & wd$yday < 138, ] #  weather data for jday breeding prediction
  wd_8 <- wd[wd$yday > 92 & wd$yday < 139, ] #  weather data for jday breeding prediction
  wd_9 <- wd[wd$yday > 93 & wd$yday < 140, ] #  weather data for jday breeding prediction
  wd_10 <- wd[wd$yday > 94 & wd$yday < 141, ] #  weather data for jday breeding prediction
  wd_11 <- wd[wd$yday > 95 & wd$yday < 142, ] #  weather data for jday breeding prediction
  wd_12 <- wd[wd$yday > 96 & wd$yday < 143, ] #  weather data for jday breeding prediction
  wd_13 <- wd[wd$yday > 97 & wd$yday < 144, ] #  weather data for jday breeding prediction
  wd_14 <- wd[wd$yday > 98 & wd$yday < 145, ] #  weather data for jday breeding prediction
  wd_15 <- wd[wd$yday > 99 & wd$yday < 146, ] # weather data for jday breeding prediction
  wd_16 <- wd[wd$yday > 100 & wd$yday < 147, ] # weather data for jday breeding prediction
  wd_17 <- wd[wd$yday > 101 & wd$yday < 148, ] # weather data for jday breeding prediction
  wd_18 <- wd[wd$yday > 102 & wd$yday < 149, ] # weather data for jday breeding prediction
  wd_19 <- wd[wd$yday > 103 & wd$yday < 150, ] # weather data for jday breeding prediction
  wd_20 <- wd[wd$yday > 104 & wd$yday < 151, ] # weather data for jday breeding prediction
  wd_21 <- wd[wd$yday > 105 & wd$yday < 152, ] # weather data for jday breeding prediction
  wd_22 <- wd[wd$yday > 106 & wd$yday < 153, ] # weather data for jday breeding prediction
  wd_23 <- wd[wd$yday > 107 & wd$yday < 154, ] # weather data for jday breeding prediction
  wd_24 <- wd[wd$yday > 108 & wd$yday < 155, ] # weather data for jday breeding prediction
  wd_25 <- wd[wd$yday > 109 & wd$yday < 156, ] # weather data for jday breeding prediction
  wd_26 <- wd[wd$yday > 110 & wd$yday < 157, ] # weather data for jday breeding prediction
  wd_27 <- wd[wd$yday > 111 & wd$yday < 158, ] # weather data for jday breeding prediction
  wd_28 <- wd[wd$yday > 112 & wd$yday < 159, ] # weather data for jday breeding prediction
  wd_29 <- wd[wd$yday > 113 & wd$yday < 160, ] # weather data for jday breeding prediction
  wd_30 <- wd[wd$yday > 114 & wd$yday < 161, ] # weather data for jday breeding prediction
  wd_31 <- wd[wd$yday > 115 & wd$yday < 162, ] # weather data for jday breeding prediction
  wd_32 <- wd[wd$yday > 116 & wd$yday < 163, ] # weather data for jday breeding prediction
  wd_33 <- wd[wd$yday > 117 & wd$yday < 164, ] # weather data for jday breeding prediction
  wd_34 <- wd[wd$yday > 118 & wd$yday < 165, ] # weather data for jday breeding prediction
  wd_35 <- wd[wd$yday > 119 & wd$yday < 166, ] # weather data for jday breeding prediction
  wd_36 <- wd[wd$yday > 120 & wd$yday < 167, ] # weather data for jday breeding prediction
  wd_37 <- wd[wd$yday > 121 & wd$yday < 168, ] # weather data for jday breeding prediction
  wd_38 <- wd[wd$yday > 122 & wd$yday < 169, ] # weather data for jday breeding prediction
  wd_39 <- wd[wd$yday > 123 & wd$yday < 170, ] # weather data for jday breeding prediction
  wd_40 <- wd[wd$yday > 124 & wd$yday < 171, ] # weather data for jday breeding prediction
  wd_41 <- wd[wd$yday > 125 & wd$yday < 172, ] # weather data for jday breeding prediction
  wd_42 <- wd[wd$yday > 126 & wd$yday < 173, ] # weather data for jday breeding prediction
  wd_43 <- wd[wd$yday > 127 & wd$yday < 174, ] # weather data for jday breeding prediction
  wd_44 <- wd[wd$yday > 128 & wd$yday < 175, ] # weather data for jday breeding prediction
  wd_45 <- wd[wd$yday > 129 & wd$yday < 176, ] # weather data for jday breeding prediction
  wd_46 <- wd[wd$yday > 130 & wd$yday < 177, ] # weather data for jday breeding prediction
  wd_47 <- wd[wd$yday > 131 & wd$yday < 178, ] # weather data for jday breeding prediction
  wd_48 <- wd[wd$yday > 132 & wd$yday < 179, ] # weather data for jday breeding prediction
  wd_49 <- wd[wd$yday > 133 & wd$yday < 180, ] # weather data for jday breeding prediction
  wd_50 <- wd[wd$yday > 134 & wd$yday < 181, ] # weather data for jday breeding prediction
  wd_51 <- wd[wd$yday > 135 & wd$yday < 182, ] # weather data for jday breeding prediction
  wd_52 <- wd[wd$yday > 136 & wd$yday < 183, ] # weather data for jday breeding prediction
  wd_53 <- wd[wd$yday > 137 & wd$yday < 184, ] # weather data for jday breeding prediction
  wd_54 <- wd[wd$yday > 138 & wd$yday < 185, ] # weather data for jday breeding prediction
  wd_55 <- wd[wd$yday > 139 & wd$yday < 186, ] # weather data for jday breeding prediction
  wd_56 <- wd[wd$yday > 140 & wd$yday < 187, ] # weather data for jday breeding prediction
  wd_57 <- wd[wd$yday > 141 & wd$yday < 188, ] # weather data for jday breeding prediction
  wd_58 <- wd[wd$yday > 142 & wd$yday < 189, ] # weather data for jday breeding prediction
  wd_59 <- wd[wd$yday > 143 & wd$yday < 190, ] # weather data for jday breeding prediction
  wd_60 <- wd[wd$yday > 144 & wd$yday < 191, ] # weather data for jday breeding prediction
  wd_61 <- wd[wd$yday > 145 & wd$yday < 192, ] # weather data for jday breeding prediction
  wd_62 <- wd[wd$yday > 156 & wd$yday < 203, ] # weather data for jday breeding prediction
  wd_63 <- wd[wd$yday > 159 & wd$yday < 206, ] # weather data for jday breeding prediction
  wd_64 <- wd[wd$yday > 203 & wd$yday < 250, ] # weather data for jday breeding prediction
  
  # Averages across years during the predicted breeding dates
  for(j in 1:64){
    eval(parse(text = paste0("BBS_routes_sp$mean_breeding_high_",j,"[",i,"] <- mean(wd_",j,"$tmax..deg.c)"))) #site mean high for breeding time
    eval(parse(text = paste0("BBS_routes_sp$sd_breeding_high_",j,"[",i,"] <- sd(wd_",j,"$tmax..deg.c)"))) #site sd high for breeding time
    
    eval(parse(text = paste0("BBS_routes_sp$mean_breeding_low_",j,"[",i,"] <- mean(wd_",j,"$tmin..deg.c)"))) #site mean min for breeding time
    eval(parse(text = paste0("BBS_routes_sp$sd_breeding_low_",j,"[",i,"] <- sd(wd_",j,"$tmin..deg.c)"))) #site sd min for breeding time
    
    eval(parse(text = paste0("BBS_routes_sp$mean_breeding_precip_",j,"[",i,"] <- mean(wd_",j,"$prcp..mm.day.)"))) #site mean precip for breeding time
    eval(parse(text = paste0("BBS_routes_sp$sd_breeding_precip_",j,"[",i,"] <- sd(wd_",j,"$prcp..mm.day.)"))) #site sd precip for breeding time
    
    eval(parse(text = paste0("BBS_routes_sp$mean_breeding_swe_",j,"[",i,"] <- mean(wd_",j,"$swe..kg.m.2)"))) #site mean swe for breeding time
    eval(parse(text = paste0("BBS_routes_sp$sd_breeding_swe_",j,"[",i,"] <- sd(wd_",j,"$swe..kg.m.2)"))) #site sd swe for breeding time
  }
  print(i)
}


# Create averages within each year for the predicted breeding dates
# Switch from spatialPointsDataFrame to DataFrame
BBS_routes_data <- BBS_routes_sp@data
BBS_routes_data$lat <- BBS_routes_sp@coords[ , 1]
BBS_routes_data$lon <- BBS_routes_sp@coords[ , 2]
BBS_weather <- BBS_routes_data

# Prepare columns for average within one year
for(j in 1:64){
  eval(parse(text = paste0("BBS_weather$breeding_avg_high_",j, "<- NA"))) #avg max
  eval(parse(text = paste0("BBS_weather$breeding_max_high_",j, "<- NA"))) #highest max
  eval(parse(text = paste0("BBS_weather$breeding_avg_low_",j, "<- NA"))) #avg min
  eval(parse(text = paste0("BBS_weather$breeding_min_low_",j, "<- NA"))) #lowest min
  eval(parse(text = paste0("BBS_weather$breeding_avg_precip_",j, "<- NA"))) #mean precip
  eval(parse(text = paste0("BBS_weather$breeding_avg_swe_",j, "<- NA"))) #mean swe
}


# Create a list of replicated data.frames, one for each year
# Populate the list as appropriate, and then rBind its elements together.
# Per Jacob, the row indexing is much simpler this way, compared to rBinding and then populating.
df.list <- list()
for(i in 1980:2016){
  BBS_weather_y <- BBS_weather
  BBS_weather_y$year <- i
  df.list[[i-1979]] <- BBS_weather_y
}

# Populate the list of data.frames
for(i in 1:dim(BBS_routes_sp)[1]){
  print(i)
  wd <- wd_list[[i]]
  wd_1 <- wd[wd$yday > 85 & wd$yday < 132, ] # weather data for jday breeding prediction
  wd_2 <- wd[wd$yday > 86 & wd$yday < 133, ] #  weather data for jday breeding prediction
  wd_3 <- wd[wd$yday > 87 & wd$yday < 134, ] #  weather data for jday breeding prediction
  wd_4 <- wd[wd$yday > 88 & wd$yday < 135, ] #  weather data for jday breeding prediction
  wd_5 <- wd[wd$yday > 89 & wd$yday < 136, ] #  weather data for jday breeding prediction
  wd_6 <- wd[wd$yday > 90 & wd$yday < 137, ] #  weather data for jday breeding prediction
  wd_7 <- wd[wd$yday > 91 & wd$yday < 138, ] #  weather data for jday breeding prediction
  wd_8 <- wd[wd$yday > 92 & wd$yday < 139, ] #  weather data for jday breeding prediction
  wd_9 <- wd[wd$yday > 93 & wd$yday < 140, ] #  weather data for jday breeding prediction
  wd_10 <- wd[wd$yday > 94 & wd$yday < 141, ] #  weather data for jday breeding prediction
  wd_11 <- wd[wd$yday > 95 & wd$yday < 142, ] #  weather data for jday breeding prediction
  wd_12 <- wd[wd$yday > 96 & wd$yday < 143, ] #  weather data for jday breeding prediction
  wd_13 <- wd[wd$yday > 97 & wd$yday < 144, ] #  weather data for jday breeding prediction
  wd_14 <- wd[wd$yday > 98 & wd$yday < 145, ] #  weather data for jday breeding prediction
  wd_15 <- wd[wd$yday > 99 & wd$yday < 146, ] # weather data for jday breeding prediction
  wd_16 <- wd[wd$yday > 100 & wd$yday < 147, ] # weather data for jday breeding prediction
  wd_17 <- wd[wd$yday > 101 & wd$yday < 148, ] # weather data for jday breeding prediction
  wd_18 <- wd[wd$yday > 102 & wd$yday < 149, ] # weather data for jday breeding prediction
  wd_19 <- wd[wd$yday > 103 & wd$yday < 150, ] # weather data for jday breeding prediction
  wd_20 <- wd[wd$yday > 104 & wd$yday < 151, ] # weather data for jday breeding prediction
  wd_21 <- wd[wd$yday > 105 & wd$yday < 152, ] # weather data for jday breeding prediction
  wd_22 <- wd[wd$yday > 106 & wd$yday < 153, ] # weather data for jday breeding prediction
  wd_23 <- wd[wd$yday > 107 & wd$yday < 154, ] # weather data for jday breeding prediction
  wd_24 <- wd[wd$yday > 108 & wd$yday < 155, ] # weather data for jday breeding prediction
  wd_25 <- wd[wd$yday > 109 & wd$yday < 156, ] # weather data for jday breeding prediction
  wd_26 <- wd[wd$yday > 110 & wd$yday < 157, ] # weather data for jday breeding prediction
  wd_27 <- wd[wd$yday > 111 & wd$yday < 158, ] # weather data for jday breeding prediction
  wd_28 <- wd[wd$yday > 112 & wd$yday < 159, ] # weather data for jday breeding prediction
  wd_29 <- wd[wd$yday > 113 & wd$yday < 160, ] # weather data for jday breeding prediction
  wd_30 <- wd[wd$yday > 114 & wd$yday < 161, ] # weather data for jday breeding prediction
  wd_31 <- wd[wd$yday > 115 & wd$yday < 162, ] # weather data for jday breeding prediction
  wd_32 <- wd[wd$yday > 116 & wd$yday < 163, ] # weather data for jday breeding prediction
  wd_33 <- wd[wd$yday > 117 & wd$yday < 164, ] # weather data for jday breeding prediction
  wd_34 <- wd[wd$yday > 118 & wd$yday < 165, ] # weather data for jday breeding prediction
  wd_35 <- wd[wd$yday > 119 & wd$yday < 166, ] # weather data for jday breeding prediction
  wd_36 <- wd[wd$yday > 120 & wd$yday < 167, ] # weather data for jday breeding prediction
  wd_37 <- wd[wd$yday > 121 & wd$yday < 168, ] # weather data for jday breeding prediction
  wd_38 <- wd[wd$yday > 122 & wd$yday < 169, ] # weather data for jday breeding prediction
  wd_39 <- wd[wd$yday > 123 & wd$yday < 170, ] # weather data for jday breeding prediction
  wd_40 <- wd[wd$yday > 124 & wd$yday < 171, ] # weather data for jday breeding prediction
  wd_41 <- wd[wd$yday > 125 & wd$yday < 172, ] # weather data for jday breeding prediction
  wd_42 <- wd[wd$yday > 126 & wd$yday < 173, ] # weather data for jday breeding prediction
  wd_43 <- wd[wd$yday > 127 & wd$yday < 174, ] # weather data for jday breeding prediction
  wd_44 <- wd[wd$yday > 128 & wd$yday < 175, ] # weather data for jday breeding prediction
  wd_45 <- wd[wd$yday > 129 & wd$yday < 176, ] # weather data for jday breeding prediction
  wd_46 <- wd[wd$yday > 130 & wd$yday < 177, ] # weather data for jday breeding prediction
  wd_47 <- wd[wd$yday > 131 & wd$yday < 178, ] # weather data for jday breeding prediction
  wd_48 <- wd[wd$yday > 132 & wd$yday < 179, ] # weather data for jday breeding prediction
  wd_49 <- wd[wd$yday > 133 & wd$yday < 180, ] # weather data for jday breeding prediction
  wd_50 <- wd[wd$yday > 134 & wd$yday < 181, ] # weather data for jday breeding prediction
  wd_51 <- wd[wd$yday > 135 & wd$yday < 182, ] # weather data for jday breeding prediction
  wd_52 <- wd[wd$yday > 136 & wd$yday < 183, ] # weather data for jday breeding prediction
  wd_53 <- wd[wd$yday > 137 & wd$yday < 184, ] # weather data for jday breeding prediction
  wd_54 <- wd[wd$yday > 138 & wd$yday < 185, ] # weather data for jday breeding prediction
  wd_55 <- wd[wd$yday > 139 & wd$yday < 186, ] # weather data for jday breeding prediction
  wd_56 <- wd[wd$yday > 140 & wd$yday < 187, ] # weather data for jday breeding prediction
  wd_57 <- wd[wd$yday > 141 & wd$yday < 188, ] # weather data for jday breeding prediction
  wd_58 <- wd[wd$yday > 142 & wd$yday < 189, ] # weather data for jday breeding prediction
  wd_59 <- wd[wd$yday > 143 & wd$yday < 190, ] # weather data for jday breeding prediction
  wd_60 <- wd[wd$yday > 144 & wd$yday < 191, ] # weather data for jday breeding prediction
  wd_61 <- wd[wd$yday > 145 & wd$yday < 192, ] # weather data for jday breeding prediction
  wd_62 <- wd[wd$yday > 156 & wd$yday < 203, ] # weather data for jday breeding prediction
  wd_63 <- wd[wd$yday > 159 & wd$yday < 206, ] # weather data for jday breeding prediction
  wd_64 <- wd[wd$yday > 203 & wd$yday < 250, ] # weather data for jday breeding prediction
  
  for(j in 1:64){
    for(k in 1980:2016){
      eval(parse(text = paste0("yr_k <- wd_",j,"[which(wd_",j,"$year==",k,"),]")))
      eval(parse(text = paste0("df.list[[", k - 1979 ,"]]$breeding_avg_high_",j,"[",i,"] <- mean(yr_k$tmax..deg.c.)"))) #mean max
      eval(parse(text = paste0("df.list[[", k - 1979 ,"]]$breeding_max_high_",j,"[",i,"] <- max(yr_k$tmax..deg.c.)"))) #highest max
      eval(parse(text = paste0("df.list[[", k - 1979 ,"]]$breeding_avg_low_",j,"[",i,"] <- mean(yr_k$tmin..deg.c.)"))) #mean min
      eval(parse(text = paste0("df.list[[", k - 1979 ,"]]$breeding_min_low_",j,"[",i,"] <- min(yr_k$tmin..deg.c.)"))) #lowest min
      eval(parse(text = paste0("df.list[[", k - 1979 ,"]]$breeding_avg_precip_",j,"[",i,"] <- mean(yr_k$prcp..mm.day.)"))) #mean precip
      eval(parse(text = paste0("df.list[[", k - 1979 ,"]]$breeding_avg_swe_",j,"[",i,"] <- mean(yr_k$swe..kg.m.2)"))) #mean swe
    }
  }
}

# rBind the list into a single large data.frame that includes data for each predicted breeding date (64 different ones)
# for the specific year of the lag. It includes the means across all years as well as the data for that year specifically. 
survey_breeding_weather <- dplyr::bind_rows(df.list)
save(survey_breeding_weather, file = "survey_breeding_weather.Rdata")

#######################

## Combine this with the weather data with the lag data
load("survey_breeding_weather.Rdata")

# read in the lag data to combine it with the breeding weather data
lag_1 <- read.csv("./BBS_Data/lag1_80to16_data.csv")
lag_1 <- lag_1[,-1] #remove that first column of nothing
lag_1 <- lag_1[(lag_1$year_1 > 1979),] # Restrict to years 1980-2016

## Remove the bad birds (although this still includes passerines)
bird_species <- read.csv("BBS_Data/SpeciesList.csv")
good_birds <- bird_species[which(rowSums(bird_species[,c(8:13, 16, 19)], na.rm = T) == 0), ]
good_birds$aou <- good_birds$AOU

lag_1 <- lag_1[which(lag_1$aou %in% good_birds$AOU), ]
lag_1 <- dplyr::inner_join(lag_1, good_birds, by = "aou")


# Combine it with the survey_breeding_weather
# For most variables:
# The months July through December of year Y matter for lags with Y == year_1
# The months January through May of year Y matter for lags with Y == year_2
# June can matter in both years.
# For species whose breeding productivity is affected precipitation in the previous 
#   fall/winter/spring, the year that matters is Y == year_1 for January through May,
#   and Y == year_1 - 1 for July through December.

# So what we're going to do for now is add on rows to lag_1 giving the year_2 values for each 
# breeding date, the year_1 values for each breeding date, and the year_1 - 1 values for each breeding date

# For the cross-year averages, we don't need to keep track of year.
survey_breeding_weather_means_sds <- survey_breeding_weather[1:5651 , 6:518]
lag_breeding_weather <- dplyr::inner_join(lag_1, survey_breeding_weather_means_sds, by = "routeID")


# We append the year_0 data
lag_breeding_weather$year_0 <- lag_breeding_weather$year_1 - 1
survey_breeding_weather_0 <- survey_breeding_weather[ , c(6, 521:905)]
names(survey_breeding_weather_0)[which(names(survey_breeding_weather_0) == "year")] <- "year_0"
names(survey_breeding_weather_0)[2:385] <- paste0(names(survey_breeding_weather_0)[2:385], "_year_0")

lag_breeding_weather <- dplyr::inner_join(lag_breeding_weather, survey_breeding_weather_0, by = c("routeID", "year_0"))

# year_1 data
survey_breeding_weather_1 <- survey_breeding_weather[ , c(6, 521:905)]
names(survey_breeding_weather_1)[which(names(survey_breeding_weather_1) == "year")] <- "year_1"
names(survey_breeding_weather_1)[2:385] <- paste0(names(survey_breeding_weather_1)[2:385], "_year_1") 

lag_breeding_weather <- dplyr::inner_join(lag_breeding_weather, survey_breeding_weather_1, by = c("routeID", "year_1"))

# year_2 data
survey_breeding_weather_2 <- survey_breeding_weather[ , c(6, 521:905)] 
names(survey_breeding_weather_2)[which(names(survey_breeding_weather_2) == "year")] <- "year_2"
names(survey_breeding_weather_2)[2:385] <- paste0(names(survey_breeding_weather_2)[2:385], "_year_2") 

lag_breeding_weather <- dplyr::inner_join(lag_breeding_weather, survey_breeding_weather_2, by = c("routeID", "year_2"))

save(lag_breeding_weather, file = "lag_breeding_weather.Rdata")
write.csv(lag_breeding_weather, file = "lag_breeding_weather.csv")



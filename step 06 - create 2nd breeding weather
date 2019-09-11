#### Creating the weather variables based on average second breeding times 
## Created September 11, 2019
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
  wd_1 <- wd[wd$yday > 131 & wd$yday < 178, ] # weather data for jday 2nd breeding prediction
  wd_2 <- wd[wd$yday > 132 & wd$yday < 179, ] #  weather data for jday 2nd breeding prediction
  wd_3 <- wd[wd$yday > 133 & wd$yday < 180, ] #  weather data for jday 2nd breeding prediction
  wd_4 <- wd[wd$yday > 134 & wd$yday < 181, ] #  weather data for jday 2nd breeding prediction
  wd_5 <- wd[wd$yday > 135 & wd$yday < 182, ] #  weather data for jday 2nd breeding prediction
  wd_6 <- wd[wd$yday > 136 & wd$yday < 183, ] #  weather data for jday 2nd breeding prediction
  wd_7 <- wd[wd$yday > 137 & wd$yday < 184, ] #  weather data for jday 2nd breeding prediction
  wd_8 <- wd[wd$yday > 138 & wd$yday < 185, ] #  weather data for jday 2nd breeding prediction
  wd_9 <- wd[wd$yday > 139 & wd$yday < 186, ] #  weather data for jday 2nd breeding prediction
  wd_10 <- wd[wd$yday > 140 & wd$yday < 187, ] #  weather data for jday 2nd breeding prediction
  wd_11 <- wd[wd$yday > 141 & wd$yday < 188, ] #  weather data for jday 2nd breeding prediction
  wd_12 <- wd[wd$yday > 142 & wd$yday < 189, ] #  weather data for jday 2nd breeding prediction
  wd_13 <- wd[wd$yday > 143 & wd$yday < 190, ] #  weather data for jday 2nd breeding prediction
  wd_14 <- wd[wd$yday > 144 & wd$yday < 191, ] #  weather data for jday 2nd breeding prediction
  wd_15 <- wd[wd$yday > 145 & wd$yday < 192, ] # weather data for jday 2nd breeding prediction
  wd_16 <- wd[wd$yday > 146 & wd$yday < 193, ] # weather data for jday 2nd breeding prediction
  wd_17 <- wd[wd$yday > 147 & wd$yday < 194, ] # weather data for jday 2nd breeding prediction
  wd_18 <- wd[wd$yday > 148 & wd$yday < 195, ] # weather data for jday 2nd breeding prediction
  wd_19 <- wd[wd$yday > 149 & wd$yday < 196, ] # weather data for jday 2nd breeding prediction
  wd_20 <- wd[wd$yday > 150 & wd$yday < 197, ] # weather data for jday 2nd breeding prediction
  wd_21 <- wd[wd$yday > 151 & wd$yday < 198, ] # weather data for jday 2nd breeding prediction
  wd_22 <- wd[wd$yday > 152 & wd$yday < 199, ] # weather data for jday 2nd breeding prediction
  wd_23 <- wd[wd$yday > 153 & wd$yday < 200, ] # weather data for jday 2nd breeding prediction
  wd_24 <- wd[wd$yday > 154 & wd$yday < 201, ] # weather data for jday 2nd breeding prediction
  wd_25 <- wd[wd$yday > 155 & wd$yday < 202, ] # weather data for jday 2nd breeding prediction
  wd_26 <- wd[wd$yday > 156 & wd$yday < 203, ] # weather data for jday 2nd breeding prediction
  wd_27 <- wd[wd$yday > 157 & wd$yday < 204, ] # weather data for jday 2nd breeding prediction
  wd_28 <- wd[wd$yday > 158 & wd$yday < 205, ] # weather data for jday 2nd breeding prediction
  wd_29 <- wd[wd$yday > 159 & wd$yday < 206, ] # weather data for jday 2nd breeding prediction
  wd_30 <- wd[wd$yday > 160 & wd$yday < 207, ] # weather data for jday 2nd breeding prediction
  wd_31 <- wd[wd$yday > 161 & wd$yday < 208, ] # weather data for jday 2nd breeding prediction
  wd_32 <- wd[wd$yday > 162 & wd$yday < 209, ] # weather data for jday 2nd breeding prediction
  wd_33 <- wd[wd$yday > 163 & wd$yday < 210, ] # weather data for jday 2nd breeding prediction
  wd_34 <- wd[wd$yday > 164 & wd$yday < 211, ] # weather data for jday 2nd breeding prediction
  wd_35 <- wd[wd$yday > 165 & wd$yday < 212, ] # weather data for jday 2nd breeding prediction
  wd_36 <- wd[wd$yday > 166 & wd$yday < 213, ] # weather data for jday 2nd breeding prediction
  wd_37 <- wd[wd$yday > 167 & wd$yday < 214, ] # weather data for jday 2nd breeding prediction
  wd_38 <- wd[wd$yday > 168 & wd$yday < 215, ] # weather data for jday 2nd breeding prediction
  wd_39 <- wd[wd$yday > 169 & wd$yday < 216, ] # weather data for jday 2nd breeding prediction
  wd_40 <- wd[wd$yday > 170 & wd$yday < 217, ] # weather data for jday 2nd breeding prediction
  wd_41 <- wd[wd$yday > 171 & wd$yday < 218, ] # weather data for jday 2nd breeding prediction
  wd_42 <- wd[wd$yday > 172 & wd$yday < 219, ] # weather data for jday 2nd breeding prediction
  wd_43 <- wd[wd$yday > 173 & wd$yday < 220, ] # weather data for jday 2nd breeding prediction
  wd_44 <- wd[wd$yday > 174 & wd$yday < 221, ] # weather data for jday 2nd breeding prediction
  wd_45 <- wd[wd$yday > 175 & wd$yday < 222, ] # weather data for jday 2nd breeding prediction
  wd_46 <- wd[wd$yday > 176 & wd$yday < 223, ] # weather data for jday 2nd breeding prediction
  wd_47 <- wd[wd$yday > 177 & wd$yday < 224, ] # weather data for jday 2nd breeding prediction
  wd_48 <- wd[wd$yday > 178 & wd$yday < 225, ] # weather data for jday 2nd breeding prediction
  wd_49 <- wd[wd$yday > 179 & wd$yday < 226, ] # weather data for jday 2nd breeding prediction
  wd_50 <- wd[wd$yday > 180 & wd$yday < 227, ] # weather data for jday 2nd breeding prediction
  wd_51 <- wd[wd$yday > 181 & wd$yday < 228, ] # weather data for jday 2nd breeding prediction
  wd_52 <- wd[wd$yday > 182 & wd$yday < 229, ] # weather data for jday 2nd breeding prediction
  wd_53 <- wd[wd$yday > 183 & wd$yday < 230, ] # weather data for jday 2nd breeding prediction
  wd_54 <- wd[wd$yday > 184 & wd$yday < 231, ] # weather data for jday 2nd breeding prediction
  wd_55 <- wd[wd$yday > 185 & wd$yday < 232, ] # weather data for jday 2nd breeding prediction
  wd_56 <- wd[wd$yday > 186 & wd$yday < 233, ] # weather data for jday 2nd breeding prediction
  wd_57 <- wd[wd$yday > 187 & wd$yday < 234, ] # weather data for jday 2nd breeding prediction
  wd_58 <- wd[wd$yday > 188 & wd$yday < 235, ] # weather data for jday 2nd breeding prediction
  wd_59 <- wd[wd$yday > 189 & wd$yday < 236, ] # weather data for jday 2nd breeding prediction
  wd_60 <- wd[wd$yday > 190 & wd$yday < 237, ] # weather data for jday 2nd breeding prediction
  wd_61 <- wd[wd$yday > 191 & wd$yday < 238, ] # weather data for jday 2nd breeding prediction
  wd_62 <- wd[wd$yday > 202 & wd$yday < 249, ] # weather data for jday 2nd breeding prediction
  wd_63 <- wd[wd$yday > 205 & wd$yday < 252, ] # weather data for jday 2nd breeding prediction
  wd_64 <- wd[wd$yday > 249 & wd$yday < 296, ] # weather data for jday 2nd breeding prediction
  
  # Averages across years during the predicted 2nd breeding dates
  for(j in 1:64){
    eval(parse(text = paste0("BBS_routes_sp$mean_2nd_breeding_high_",j,"[",i,"] <- mean(wd_",j,"$tmax..deg.c)"))) #site mean high for breeding time
    eval(parse(text = paste0("BBS_routes_sp$sd_2nd_breeding_high_",j,"[",i,"] <- sd(wd_",j,"$tmax..deg.c)"))) #site sd high for breeding time
    
    eval(parse(text = paste0("BBS_routes_sp$mean_2nd_breeding_low_",j,"[",i,"] <- mean(wd_",j,"$tmin..deg.c)"))) #site mean min for breeding time
    eval(parse(text = paste0("BBS_routes_sp$sd_2nd_breeding_low_",j,"[",i,"] <- sd(wd_",j,"$tmin..deg.c)"))) #site sd min for breeding time
    
    eval(parse(text = paste0("BBS_routes_sp$mean_2nd_breeding_precip_",j,"[",i,"] <- mean(wd_",j,"$prcp..mm.day.)"))) #site mean precip for breeding time
    eval(parse(text = paste0("BBS_routes_sp$sd_2nd_breeding_precip_",j,"[",i,"] <- sd(wd_",j,"$prcp..mm.day.)"))) #site sd precip for breeding time
    
    eval(parse(text = paste0("BBS_routes_sp$mean_2nd_breeding_swe_",j,"[",i,"] <- mean(wd_",j,"$swe..kg.m.2)"))) #site mean swe for breeding time
    eval(parse(text = paste0("BBS_routes_sp$sd_2nd_breeding_swe_",j,"[",i,"] <- sd(wd_",j,"$swe..kg.m.2)"))) #site sd swe for breeding time
  }
  print(i)
}


# Create averages within each year for the predicted 2nd breeding dates
# Switch from spatialPointsDataFrame to DataFrame
BBS_routes_data <- BBS_routes_sp@data
BBS_routes_data$lat <- BBS_routes_sp@coords[ , 1]
BBS_routes_data$lon <- BBS_routes_sp@coords[ , 2]
BBS_weather <- BBS_routes_data

# Prepare columns for average within one year
for(j in 1:64){
  eval(parse(text = paste0("BBS_weather$second_breeding_avg_high_",j, "<- NA"))) #avg max
  eval(parse(text = paste0("BBS_weather$second_breeding_max_high_",j, "<- NA"))) #highest max
  eval(parse(text = paste0("BBS_weather$second_breeding_avg_low_",j, "<- NA"))) #avg min
  eval(parse(text = paste0("BBS_weather$second_breeding_min_low_",j, "<- NA"))) #lowest min
  eval(parse(text = paste0("BBS_weather$second_breeding_avg_precip_",j, "<- NA"))) #mean precip
  eval(parse(text = paste0("BBS_weather$second_breeding_avg_swe_",j, "<- NA"))) #mean swe
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
  wd_1 <- wd[wd$yday > 131 & wd$yday < 178, ] # weather data for jday 2nd breeding prediction
  wd_2 <- wd[wd$yday > 132 & wd$yday < 179, ] #  weather data for jday 2nd breeding prediction
  wd_3 <- wd[wd$yday > 133 & wd$yday < 180, ] #  weather data for jday 2nd breeding prediction
  wd_4 <- wd[wd$yday > 134 & wd$yday < 181, ] #  weather data for jday 2nd breeding prediction
  wd_5 <- wd[wd$yday > 135 & wd$yday < 182, ] #  weather data for jday 2nd breeding prediction
  wd_6 <- wd[wd$yday > 136 & wd$yday < 183, ] #  weather data for jday 2nd breeding prediction
  wd_7 <- wd[wd$yday > 137 & wd$yday < 184, ] #  weather data for jday 2nd breeding prediction
  wd_8 <- wd[wd$yday > 138 & wd$yday < 185, ] #  weather data for jday 2nd breeding prediction
  wd_9 <- wd[wd$yday > 139 & wd$yday < 186, ] #  weather data for jday 2nd breeding prediction
  wd_10 <- wd[wd$yday > 140 & wd$yday < 187, ] #  weather data for jday 2nd breeding prediction
  wd_11 <- wd[wd$yday > 141 & wd$yday < 188, ] #  weather data for jday 2nd breeding prediction
  wd_12 <- wd[wd$yday > 142 & wd$yday < 189, ] #  weather data for jday 2nd breeding prediction
  wd_13 <- wd[wd$yday > 143 & wd$yday < 190, ] #  weather data for jday 2nd breeding prediction
  wd_14 <- wd[wd$yday > 144 & wd$yday < 191, ] #  weather data for jday 2nd breeding prediction
  wd_15 <- wd[wd$yday > 145 & wd$yday < 192, ] # weather data for jday 2nd breeding prediction
  wd_16 <- wd[wd$yday > 146 & wd$yday < 193, ] # weather data for jday 2nd breeding prediction
  wd_17 <- wd[wd$yday > 147 & wd$yday < 194, ] # weather data for jday 2nd breeding prediction
  wd_18 <- wd[wd$yday > 148 & wd$yday < 195, ] # weather data for jday 2nd breeding prediction
  wd_19 <- wd[wd$yday > 149 & wd$yday < 196, ] # weather data for jday 2nd breeding prediction
  wd_20 <- wd[wd$yday > 150 & wd$yday < 197, ] # weather data for jday 2nd breeding prediction
  wd_21 <- wd[wd$yday > 151 & wd$yday < 198, ] # weather data for jday 2nd breeding prediction
  wd_22 <- wd[wd$yday > 152 & wd$yday < 199, ] # weather data for jday 2nd breeding prediction
  wd_23 <- wd[wd$yday > 153 & wd$yday < 200, ] # weather data for jday 2nd breeding prediction
  wd_24 <- wd[wd$yday > 154 & wd$yday < 201, ] # weather data for jday 2nd breeding prediction
  wd_25 <- wd[wd$yday > 155 & wd$yday < 202, ] # weather data for jday 2nd breeding prediction
  wd_26 <- wd[wd$yday > 156 & wd$yday < 203, ] # weather data for jday 2nd breeding prediction
  wd_27 <- wd[wd$yday > 157 & wd$yday < 204, ] # weather data for jday 2nd breeding prediction
  wd_28 <- wd[wd$yday > 158 & wd$yday < 205, ] # weather data for jday 2nd breeding prediction
  wd_29 <- wd[wd$yday > 159 & wd$yday < 206, ] # weather data for jday 2nd breeding prediction
  wd_30 <- wd[wd$yday > 160 & wd$yday < 207, ] # weather data for jday 2nd breeding prediction
  wd_31 <- wd[wd$yday > 161 & wd$yday < 208, ] # weather data for jday 2nd breeding prediction
  wd_32 <- wd[wd$yday > 162 & wd$yday < 209, ] # weather data for jday 2nd breeding prediction
  wd_33 <- wd[wd$yday > 163 & wd$yday < 210, ] # weather data for jday 2nd breeding prediction
  wd_34 <- wd[wd$yday > 164 & wd$yday < 211, ] # weather data for jday 2nd breeding prediction
  wd_35 <- wd[wd$yday > 165 & wd$yday < 212, ] # weather data for jday 2nd breeding prediction
  wd_36 <- wd[wd$yday > 166 & wd$yday < 213, ] # weather data for jday 2nd breeding prediction
  wd_37 <- wd[wd$yday > 167 & wd$yday < 214, ] # weather data for jday 2nd breeding prediction
  wd_38 <- wd[wd$yday > 168 & wd$yday < 215, ] # weather data for jday 2nd breeding prediction
  wd_39 <- wd[wd$yday > 169 & wd$yday < 216, ] # weather data for jday 2nd breeding prediction
  wd_40 <- wd[wd$yday > 170 & wd$yday < 217, ] # weather data for jday 2nd breeding prediction
  wd_41 <- wd[wd$yday > 171 & wd$yday < 218, ] # weather data for jday 2nd breeding prediction
  wd_42 <- wd[wd$yday > 172 & wd$yday < 219, ] # weather data for jday 2nd breeding prediction
  wd_43 <- wd[wd$yday > 173 & wd$yday < 220, ] # weather data for jday 2nd breeding prediction
  wd_44 <- wd[wd$yday > 174 & wd$yday < 221, ] # weather data for jday 2nd breeding prediction
  wd_45 <- wd[wd$yday > 175 & wd$yday < 222, ] # weather data for jday 2nd breeding prediction
  wd_46 <- wd[wd$yday > 176 & wd$yday < 223, ] # weather data for jday 2nd breeding prediction
  wd_47 <- wd[wd$yday > 177 & wd$yday < 224, ] # weather data for jday 2nd breeding prediction
  wd_48 <- wd[wd$yday > 178 & wd$yday < 225, ] # weather data for jday 2nd breeding prediction
  wd_49 <- wd[wd$yday > 179 & wd$yday < 226, ] # weather data for jday 2nd breeding prediction
  wd_50 <- wd[wd$yday > 180 & wd$yday < 227, ] # weather data for jday 2nd breeding prediction
  wd_51 <- wd[wd$yday > 181 & wd$yday < 228, ] # weather data for jday 2nd breeding prediction
  wd_52 <- wd[wd$yday > 182 & wd$yday < 229, ] # weather data for jday 2nd breeding prediction
  wd_53 <- wd[wd$yday > 183 & wd$yday < 230, ] # weather data for jday 2nd breeding prediction
  wd_54 <- wd[wd$yday > 184 & wd$yday < 231, ] # weather data for jday 2nd breeding prediction
  wd_55 <- wd[wd$yday > 185 & wd$yday < 232, ] # weather data for jday 2nd breeding prediction
  wd_56 <- wd[wd$yday > 186 & wd$yday < 233, ] # weather data for jday 2nd breeding prediction
  wd_57 <- wd[wd$yday > 187 & wd$yday < 234, ] # weather data for jday 2nd breeding prediction
  wd_58 <- wd[wd$yday > 188 & wd$yday < 235, ] # weather data for jday 2nd breeding prediction
  wd_59 <- wd[wd$yday > 189 & wd$yday < 236, ] # weather data for jday 2nd breeding prediction
  wd_60 <- wd[wd$yday > 190 & wd$yday < 237, ] # weather data for jday 2nd breeding prediction
  wd_61 <- wd[wd$yday > 191 & wd$yday < 238, ] # weather data for jday 2nd breeding prediction
  wd_62 <- wd[wd$yday > 202 & wd$yday < 249, ] # weather data for jday 2nd breeding prediction
  wd_63 <- wd[wd$yday > 205 & wd$yday < 252, ] # weather data for jday 2nd breeding prediction
  wd_64 <- wd[wd$yday > 249 & wd$yday < 296, ] # weather data for jday 2nd breeding prediction
  
  for(j in 1:64){
    for(k in 1980:2016){
      eval(parse(text = paste0("yr_k <- wd_",j,"[which(wd_",j,"$year==",k,"),]")))
      eval(parse(text = paste0("df.list[[", k - 1979 ,"]]$second_breeding_avg_high_",j,"[",i,"] <- mean(yr_k$tmax..deg.c.)"))) #mean max
      eval(parse(text = paste0("df.list[[", k - 1979 ,"]]$second_breeding_max_high_",j,"[",i,"] <- max(yr_k$tmax..deg.c.)"))) #highest max
      eval(parse(text = paste0("df.list[[", k - 1979 ,"]]$second_breeding_avg_low_",j,"[",i,"] <- mean(yr_k$tmin..deg.c.)"))) #mean min
      eval(parse(text = paste0("df.list[[", k - 1979 ,"]]$second_breeding_min_low_",j,"[",i,"] <- min(yr_k$tmin..deg.c.)"))) #lowest min
      eval(parse(text = paste0("df.list[[", k - 1979 ,"]]$second_breeding_avg_precip_",j,"[",i,"] <- mean(yr_k$prcp..mm.day.)"))) #mean precip
      eval(parse(text = paste0("df.list[[", k - 1979 ,"]]$second_breeding_avg_swe_",j,"[",i,"] <- mean(yr_k$swe..kg.m.2)"))) #mean swe
    }
  }
}

# rBind the list into a single large data.frame that includes data for each predicted breeding date (64 different ones)
# for the specific year of the lag. It includes the means across all years as well as the data for that year specifically. 
second_breeding_weather <- dplyr::bind_rows(df.list)
#save(second_breeding_weather, file = "second_breeding_weather.Rdata")

#######################

## Combine this with the weather data with the lag data
#load("second_breeding_weather.Rdata") # If starting from here, and saved second_breeding_weather.Rdata

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


# Combine it with the second_breeding_weather
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
second_breeding_weather_means_sds <- second_breeding_weather[1:5651 , 6:518]
lag_breeding_weather <- dplyr::inner_join(lag_1, second_breeding_weather_means_sds, by = "routeID")


# We append the year_0 data
lag_breeding_weather$year_0 <- lag_breeding_weather$year_1 - 1
second_breeding_weather_0 <- second_breeding_weather[ , c(6, 521:905)]
names(second_breeding_weather_0)[which(names(second_breeding_weather_0) == "year")] <- "year_0"
names(second_breeding_weather_0)[2:385] <- paste0(names(second_breeding_weather_0)[2:385], "_year_0")

lag_breeding_weather <- dplyr::inner_join(lag_breeding_weather, second_breeding_weather_0, by = c("routeID", "year_0"))

# year_1 data
second_breeding_weather_1 <- second_breeding_weather[ , c(6, 521:905)]
names(second_breeding_weather_1)[which(names(second_breeding_weather_1) == "year")] <- "year_1"
names(second_breeding_weather_1)[2:385] <- paste0(names(second_breeding_weather_1)[2:385], "_year_1") 

lag_breeding_weather <- dplyr::inner_join(lag_breeding_weather, second_breeding_weather_1, by = c("routeID", "year_1"))

# year_2 data
second_breeding_weather_2 <- second_breeding_weather[ , c(6, 521:905)] 
names(second_breeding_weather_2)[which(names(second_breeding_weather_2) == "year")] <- "year_2"
names(second_breeding_weather_2)[2:385] <- paste0(names(second_breeding_weather_2)[2:385], "_year_2") 

lag_breeding_weather <- dplyr::inner_join(lag_breeding_weather, second_breeding_weather_2, by = c("routeID", "year_2"))

lag_2nd_breeding_weather <- lag_breeding_weather

save(lag_2nd_breeding_weather, file = "lag_2nd_breeding_weather.Rdata")
#write.csv(lag_2nd_breeding_weather, file = "lag_2nd_breeding_weather.csv")

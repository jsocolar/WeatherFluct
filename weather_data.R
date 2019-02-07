# This script: 
# 1) Downloads and unzips the Route file distributed by the BBS ftp site
# 2) Downloads and imports Daymet weather data from 1980 to 2015 at each BBS route.

#### Directories and packages ####
setwd("/Users/Jacob/Dropbox/Work/BBS_Project/") #Jacob's laptop
#setwd("/Users/JacobSocolar/Dropbox/Work/BBS_Project/") #Jacob's Minnesota desktop
#setwd("C:/Users/Tingley Lab_2/Dropbox/BBS_Project") #Austin's lab computer

'%ni%' <- Negate('%in%')

#### 1) BBS route download ####
#download.file('ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Routes.zip', 
#              destfile = 'BBS_Data/BBS_routes.zip')   # Run on 20 March 2018
#unzip('BBS_Data/BBS_routes.zip')
#BBS_routes <- read.csv('BBS_Data/routes.csv')
## Create SpatialPoints structure
#BBS_routes_sp <- sp::SpatialPointsDataFrame(BBS_routes[,6:7], BBS_routes[,-c(6,7)], proj4string = sp::CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#### 2) Download and read in Daymet data ####
## Option 1, using package FedData, would give the full Daymet polygon as a giant rasterbrick. 
# FedData::get_daymet(BBS_routes_sp, BBS_area, elements = c("prcp", "swe", "tmax", "tmin"), raw.dir = "/Users/TingleyLab/Desktop/useful_datasets/Daymet/RAW/, extraction.dir = "/Users/TingleyLab/Desktop/useful_datasets/Daymet/EXTRACTIONS/")
## Option 2: because the rasterbrick would be so huge an unwieldy, we implement a workaround
## based on an online single-pixel extraction tool:
#npts <- dim(BBS_routes_sp)[1]
#for(i in 1:npts){
#  tryCatch(download.file(url=paste("https://daymet.ornl.gov/single-pixel/send/send/saveData?lat=",
#                                   BBS_routes_sp[i,]@coords[1],"&lon=",BBS_routes_sp[i,]@coords[2],
#                                   "&measuredParams=tmax,tmin,prcp,swe&year=1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016&daacid=39622",
#                                   sep=""),
#                         destfile = paste("/Users/TingleyLab/Dropbox/Work/BBS_Project/Daymet/pt",
#                                          i,".csv", sep="")), error=function(e){})
#  print(paste(i, "of", npts, sep=" "))
#}

## Some files don't exist because of georeferences that are just offshore according to the Daymet grid
## 10 points had problems downloading, and of these 9 were within 250 meters of a coastline, while one
## was ostensibly at 59, -139 (exactly) which is well offshore and appears to be a typo.
## We re-download data from the 9 sites by changing the coordinates by hand to very nearby onshore points.
## Then we remove the tenth point later.
#
#wdata <- list.files("/Users/Jacob/Dropbox/Work/BBS_Project/Daymet")
#good_pts <- as.numeric(gsub("\\D", "", wdata))  # \D is regex for characters that are not digits
#bad_pts <- which(c(1:5652) %ni% good_pts)
#BBS_routes_sp[bad_pts,]@coords
#BBS_routes_sp@coords[bad_pts[1],] <- c(30.691180, -88.065178)
#BBS_routes_sp@coords[bad_pts[2],] <- c(59.548006, -151.326056)
#BBS_routes_sp@coords[bad_pts[4],] <- c(24.673233, -81.245839)
#BBS_routes_sp@coords[bad_pts[5],] <- c(61.097428, -94.060951)
#BBS_routes_sp@coords[bad_pts[6],] <- c(38.418999, -76.544451)
#BBS_routes_sp@coords[bad_pts[7],] <- c(38.418999, -76.544451)
#BBS_routes_sp@coords[bad_pts[8],] <- c(61.097428, -94.060951)
#BBS_routes_sp@coords[bad_pts[9],] <- c(52.238510, -78.50438)
#BBS_routes_sp@coords[bad_pts[10],] <- c(46.285794, -124.066913)
#
#for(i in bad_pts){
#  tryCatch(download.file(url=paste("https://daymet.ornl.gov/single-pixel/send/send/saveData?lat=",
#                                   BBS_routes_sp[i,]@coords[1],"&lon=",BBS_routes_sp[i,]@coords[2],
#                                   "&measuredParams=tmax,tmin,prcp,swe&year=1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016&daacid=39622",
#                                   sep=""),
#                         destfile = paste("/Users/TingleyLab/Dropbox/Work/BBS_Project/Daymet/pt",
#                                          i,".csv", sep="")), error=function(e){})
#  print(paste(i, "of", npts, sep=" "))
#}
#
#wdata <- list.files("/Users/Jacob/Dropbox/Work/BBS_Project/Daymet")
#good_pts <- as.numeric(gsub("\\D", "", wdata))  # \D is regex for characters that are not digits

#BBS_routes_sp <- BBS_routes_sp[good_pts,]

#save(BBS_routes_sp, file = 'BBS_routes_sp.Rdata')

#### 3) Extract example temperature summaries for BBS locations ####
load('BBS_routes_sp.Rdata')

# Unique Route ID
BBS_routes_sp$routeID <- rep(NA, dim(BBS_routes_sp)[1])
BBS_routes_sp$routeID <- paste(BBS_routes_sp$countrynum, BBS_routes_sp$statenum, BBS_routes_sp$Route, sep=".")
BBS_routes_sp <- BBS_routes_sp[ , -c(1:5)]

# Now we will put in all of the site-level summaries, i.e. everything that does not take
# a different value in each year, i.e. elevation and weather averages.

# Elevation:
BBS_routes_sp$w_elev <- rep(NA, dim(BBS_routes_sp)[1])

# Getting weather data for each month
# Overall means for each weather point for each site across years
for(i in 1:12){
  eval(parse(text = paste0("BBS_routes_sp$mean_high_",i,"<- NA"))) # mean daily high
  eval(parse(text = paste0("BBS_routes_sp$mean_low_",i,"<- NA"))) # mean daily low
  eval(parse(text = paste0("BBS_routes_sp$mean_precip_",i, "<- NA"))) # mean daily precip
  eval(parse(text = paste0("BBS_routes_sp$mean_swe_",i, "<- NA"))) # mean daily swe
}

# Populate BBS_routes_sp with appropriate data:
wdata <- list.files("Daymet")
wd_list <- list() # Store all the imported weather csv files in a list, so we don't have to
                  # re-import later
for(i in 1:dim(BBS_routes_sp)[1]){
  # Get the elevation
  md <- read.csv(paste("Daymet/", wdata[i], sep=""), header=F) ## this reads in the files of the daymet (in alphabetical, not numeric, order)
  BBS_routes_sp$w_elev[i] <- as.numeric(gsub("\\D", "", md[4,])) # This pulls out the elevation (it is in the fourth cell). Remove the characters not digits, left with elevation
  
  # Get the weather data and store as list
  wd_list[[i]] <- read.csv(paste("Daymet/", wdata[i], sep=""), skip=7) # this skips the first 7 rows to read data
}
save(wd_list, file = "wd_list.Rdata")

load("wd_list.Rdata")
for(i in 1:dim(BBS_routes_sp)[1]){
  wd <- wd_list[[i]]
  wd_1 <- wd[wd$yday > 1 & wd$yday < 31, ] # january weather data
  wd_2 <- wd[wd$yday > 32 & wd$yday < 59, ] # february weather data
  wd_3 <- wd[wd$yday > 60 & wd$yday < 90, ] # march weather data
  wd_4 <- wd[wd$yday > 91 & wd$yday < 120, ] # april weather data
  wd_5 <- wd[wd$yday > 121 & wd$yday < 151, ] # may weather data
  wd_6 <- wd[wd$yday > 152 & wd$yday < 181, ] # june weather data
  wd_7 <- wd[wd$yday > 182 & wd$yday < 212, ] # july weather data
  wd_8 <- wd[wd$yday > 213 & wd$yday < 243, ] # august weather data
  wd_9 <- wd[wd$yday > 244 & wd$yday < 273, ] # september weather data
  wd_10 <- wd[wd$yday > 274 & wd$yday < 304, ] # october weather data
  wd_11 <- wd[wd$yday > 305 & wd$yday < 334, ] # november weather data
  wd_12 <- wd[wd$yday > 335 & wd$yday < 365, ] # december weather data

  # Monthly averages across years
  for(j in 1:12){
    eval(parse(text = paste0("BBS_routes_sp$mean_high_",j,"[",i,"] <- mean(wd_",j,"$tmax..deg.c)"))) #site mean high
    eval(parse(text = paste0("BBS_routes_sp$mean_low_",j,"[",i,"] <- mean(wd_",j,"$tmin..deg.c)"))) #site mean min
    eval(parse(text = paste0("BBS_routes_sp$mean_precip_",j,"[",i,"] <- mean(wd_",j,"$prcp..mm.day.)"))) #site mean precip
    eval(parse(text = paste0("BBS_routes_sp$mean_swe_",j,"[",i,"] <- mean(wd_",j,"$swe..kg.m.2)"))) #site mean swe
  }
  print(i)
}

# Monthly averages within each year
# Switch from spatialPointsDataFrame to DataFrame
BBS_routes_data <- BBS_routes_sp@data
BBS_routes_data$lat <- BBS_routes_sp@coords[ , 1]
BBS_routes_data$lon <- BBS_routes_sp@coords[ , 2]
BBS_weather <- BBS_routes_data

# Prepare columns
for(j in 1:12){
  eval(parse(text = paste0("BBS_weather$monthly_avg_high_",j, "<- NA"))) #mean max
  eval(parse(text = paste0("BBS_weather$monthly_max_high_",j, "<- NA"))) #highest max
  eval(parse(text = paste0("BBS_weather$monthly_avg_low_",j, "<- NA"))) #mean min
  eval(parse(text = paste0("BBS_weather$monthly_min_low_",j, "<- NA"))) #lowest min
  eval(parse(text = paste0("BBS_weather$monthly_avg_precip_",j, "<- NA"))) #mean precip
  eval(parse(text = paste0("BBS_weather$monthly_avg_swe_",j, "<- NA"))) #mean swe
}

# Create a list of replicated data.frames, one for each year
    # Next, we will populate the list as appropriate, and then rBind its elements together.
    # The row indexing is much simpler this way, compared to rBinding and then populating.
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
  wd_1 <- wd[wd$yday > 1 & wd$yday < 31, ] # january weather data
  wd_2 <- wd[wd$yday > 32 & wd$yday < 59, ] # february weather data
  wd_3 <- wd[wd$yday > 60 & wd$yday < 90, ] # march weather data
  wd_4 <- wd[wd$yday > 91 & wd$yday < 120, ] # april weather data
  wd_5 <- wd[wd$yday > 121 & wd$yday < 151, ] # may weather data
  wd_6 <- wd[wd$yday > 152 & wd$yday < 181, ] # june weather data
  wd_7 <- wd[wd$yday > 182 & wd$yday < 212, ] # july weather data
  wd_8 <- wd[wd$yday > 213 & wd$yday < 243, ] # august weather data
  wd_9 <- wd[wd$yday > 244 & wd$yday < 273, ] # september weather data
  wd_10 <- wd[wd$yday > 274 & wd$yday < 304, ] # october weather data
  wd_11 <- wd[wd$yday > 305 & wd$yday < 334, ] # november weather data
  wd_12 <- wd[wd$yday > 335 & wd$yday < 365, ] # december weather data
  
  for(j in 1:12){
    for(k in 1980:2016){
      eval(parse(text = paste0("yr_k <- wd_",j,"[which(wd_",j,"$year==",k,"),]")))
      eval(parse(text = paste0("df.list[[", k - 1979 ,"]]$monthly_avg_high_",j,"[",i,"] <- mean(yr_k$tmax..deg.c.)"))) #mean max
      eval(parse(text = paste0("df.list[[", k - 1979 ,"]]$monthly_max_high_",j,"[",i,"] <- max(yr_k$tmax..deg.c.)"))) #highest max
      eval(parse(text = paste0("df.list[[", k - 1979 ,"]]$monthly_avg_low_",j,"[",i,"] <- mean(yr_k$tmin..deg.c.)"))) #mean min
      eval(parse(text = paste0("df.list[[", k - 1979 ,"]]$monthly_min_low_",j,"[",i,"] <- min(yr_k$tmin..deg.c.)"))) #lowest min
      eval(parse(text = paste0("df.list[[", k - 1979 ,"]]$monthly_avg_precip_",j,"[",i,"] <- mean(yr_k$prcp..mm.day.)"))) #mean precip
      eval(parse(text = paste0("df.list[[", k - 1979 ,"]]$monthly_avg_swe_",j,"[",i,"] <- mean(yr_k$swe..kg.m.2)"))) #mean swe
    }
  }
}

# rBind the list into a single large data.frame
survey_weather <- dplyr::bind_rows(df.list)
save(survey_weather, file = "survey_weather.Rdata")

## Combine with the lag data and create a new data object
load("survey_weather.Rdata")

# read in the lag data
lag_1 <- read.csv("BBS_Data/lag1_80to16_data.csv")
lag_1 <- lag_1[,-1] #remove that first column of nothing
lag_1 <- lag_1[(lag_1$year_1 > 1979),] # Restrict to years 1980-2016

## Remove the bad birds
bird_species <- read.csv("BBS_Data/SpeciesList.csv")
good_birds <- bird_species[which(rowSums(bird_species[,c(8:13, 16, 19)], na.rm = T) == 0), ]
good_birds$aou <- good_birds$AOU

lag_1 <- lag_1[which(lag_1$aou %in% good_birds$AOU), ]
lag_1 <- dplyr::inner_join(lag_1, good_birds, by = "aou")

# Combine it with the survey_weather
# For most variables:
# The months July through December of year Y matter for lags with Y == year_1
# The months January through May of year Y matter for lags with Y == year_2
# June can matter in both years.
# For species whose breeding productivity is affected precipitation in the previous 
#   fall/winter/spring, the year that matters is Y == year_1 for January through May,
#   and Y == year_1 - 1 for July through December.

# So what we're going to do for now is add on rows to lag_1 giving the year_2 values for each 
# month, the year_1 values for each month, and the year_1 - 1 values for each month.

# For the cross-year averages, we don't need to keep track of year.
survey_weather_averages <- survey_weather[1:5651 , 6:55]
lag_weather <- dplyr::inner_join(lag_1, survey_weather_averages, by = "routeID")

# We append the year_0 data
lag_weather$year_0 <- lag_weather$year_1 - 1
survey_weather_0 <- survey_weather[ , c(6, 58:130)]
names(survey_weather_0)[which(names(survey_weather_0) == "year")] <- "year_0"
names(survey_weather_0)[2:73] <- paste0(names(survey_weather_0)[2:73], "_year_0")

lag_weather <- dplyr::inner_join(lag_weather, survey_weather_0, by = c("routeID", "year_0"))

# year_1 data
survey_weather_1 <- survey_weather[ , c(6, 58:130)]
names(survey_weather_1)[which(names(survey_weather_1) == "year")] <- "year_1"
names(survey_weather_1)[2:73] <- paste0(names(survey_weather_1)[2:73], "_year_1")

lag_weather <- dplyr::inner_join(lag_weather, survey_weather_1, by = c("routeID", "year_1"))

# year_2 data
survey_weather_2 <- survey_weather[ , c(6, 58:130)]
names(survey_weather_2)[which(names(survey_weather_2) == "year")] <- "year_2"
names(survey_weather_2)[2:73] <- paste0(names(survey_weather_2)[2:73], "_year_2")

lag_weather <- dplyr::inner_join(lag_weather, survey_weather_2, by = c("routeID", "year_2"))

save(lag_weather, file = "BBS_Data/lag_weather.Rdata")
write.csv(lag_weather, file = "BBS_Data/lag_weather.csv")

#### Z-score calculation:
# The z-scores represent whether a route is near the center or near the edge of a species range
# for a particular weather variable. They can be calculated only for species that are detected
# on a sufficient number of unique routes. Data exploration to determine a good minimum number of
# routes for a cut-off:
lag_weather <- read.csv("BBS_Data/lag_weather.csv", header = TRUE)

route_numbers <- doBy::summaryBy(routeID ~ AOU, FUN=length, data=lag_weather)
hist(route_numbers$routeID.length[which(route_numbers$routeID.length < 200)])
z_score_species <- route_numbers$AOU[which(route_numbers$routeID.length > 69)]

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

# Below is an example to compute z-scores for March average daily high temps. 
# WARNING: running this will take a while!
test <- zsc(lag_weather, "mean_high_3", z_score_species)
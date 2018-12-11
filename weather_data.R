# Written to run on Picoides
# This script: 
# 1) Downloads and unzips the Route file distributed by the BBS ftp site
# 2) Downloads and imports Daymet weather data from 1980 to 2015 at each BBS route.

#### Directories and packages ####
setwd("/Users/Jacob/Dropbox/Work/BBS_Project/")
#setwd("C:/Users/Tingley Lab_2/Dropbox/BBS_Project") #Austin's lab computer
'%ni%' <- Negate('%in%')

#library(FedData)

#### 1) BBS route download ####

#download.file('ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/Routes.zip', 
#              destfile = 'BBS_Data/BBS_routes.zip')   # Run on 20 March 2018
#unzip('BBS_Data/BBS_routes.zip')

#BBS_routes <- read.csv('BBS_Data/routes.csv')

## Create SpatialPoints structure
#BBS_routes_sp <- sp::SpatialPointsDataFrame(BBS_routes[,6:7], BBS_routes[,-c(6,7)], proj4string = sp::CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#### 2) Download and read in Daymet data ####

# FedData::get_daymet(BBS_routes_sp, BBS_area, elements = c("prcp", "swe", "tmax", "tmin"), raw.dir = "/Users/TingleyLab/Desktop/useful_datasets/Daymet/RAW/, extraction.dir = "/Users/TingleyLab/Desktop/useful_datasets/Daymet/EXTRACTIONS/")
## The above would give the full Daymet polygon as a rasterbrick.  Below-is a hard-coded work-around based
## on an online single-pixel extraction tool:

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

# Set up BBS_routes_sp to accept the elevations of the Daymet tiles and a some of temp. summaries:

# Elevation:
BBS_routes_sp$w_elev <- rep(NA, dim(BBS_routes_sp)[1])

# Unique Route ID
BBS_routes_sp$route_ID <- rep(NA, dim(BBS_routes_sp)[1])
BBS_routes_sp$route_ID <- paste(BBS_routes_sp$countrynum, BBS_routes_sp$statenum, BBS_routes_sp$Route, sep=".")

# Getting weather data for each month

# Overall means for each weather point for each site across years
for(i in 1:12){
eval(parse(text = paste0("BBS_routes_sp$site_meanmax_month_",i,"<- NA"))) #mean max
eval(parse(text = paste0("BBS_routes_sp$site_meanmin_month_",i,"<- NA"))) #mean min
eval(parse(text = paste0("BBS_routes_sp$site_meanPrecip_month_",i, "<- NA"))) #mean precip
eval(parse(text = paste0("BBS_routes_sp$site_meanSWE_month_",i, "<- NA"))) #mean swe
}

# mean daily high across years
# max daily high in this period across years
# mean daily low across years
# min daily low in this period across years
# mean precipitation across years
# mean swe across years

for(i in 1980:2016){
  for(j in 1:12){
  eval(parse(text = paste0("BBS_routes_sp$meanmax_",i,"_",j, "<- NA"))) #mean max
  eval(parse(text = paste0("BBS_routes_sp$max_",i,"_",j, "<- NA"))) #highest max
  eval(parse(text = paste0("BBS_routes_sp$meanmin_",i,"_",j, "<- NA"))) #mean min
  eval(parse(text = paste0("BBS_routes_sp$min_",i,"_",j, "<- NA"))) #lowest min
  eval(parse(text = paste0("BBS_routes_sp$meanPrecip_",i,"_",j, "<- NA"))) #mean precip
  eval(parse(text = paste0("BBS_routes_sp$meanSWE_",i,"_",j, "<- NA"))) #mean swe
}
}

# Need to get alternative 
for(i in 1980:2016){
    eval(parse(text = paste0("BBS_routes_sp$mean_june_aug_precip_",i,"<- NA"))) # mean jun-aug precip
}

for(i in 1981:2016){
  eval(parse(text = paste0("BBS_routes_sp$mean_july_april_",i,"<- NA"))) #mean jul(t-1) - april
}

# Populate BBS_routes_sp with appropriate data:

wdata <- list.files("/Users/Jacob/Dropbox/Work/BBS_Project/Daymet")
#wdata <- list.files("C:/Users/Tingley Lab_2/Dropbox/BBS_Project/Daymet") #Lab 2 computer

for(i in 1:dim(BBS_routes_sp)[1]){
  # Get out the elevation
  md <- read.csv(paste("Daymet/", wdata[i], sep=""), header=F) ## this reads in the files of the daymet (in alphabetical order!)
  #md <- read.csv(paste("C:/Users/Tingley Lab_2/Dropbox/BBS_Project/Daymet/", wdata[i], sep=""), header=F) # Lab 2 computer
  BBS_routes_sp$w_elev[i] <- as.numeric(gsub("\\D", "", md[4,])) # This pulls out the elevation (it is in the fourth cell). Remove the characters not digits, left with elevation
  print(i)
  
  # Get out only early summer dates
  wd <- read.csv(paste("Daymet/", wdata[i], sep=""), skip=7) # this skips the first 7 rows to read data, including the header (it can add this)
  #wd <- read.csv(paste("C:/Users/Tingley Lab_2/Dropbox/BBS_Project/Daymet/", wdata[i], sep=""), skip=7) #Lab 2 computer
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
  
  wd_june_aug <- wd[wd$yday > 152 & wd$yday < 243, ] # june through august weather data
  
  # Get the overall means from all months per site
  for(j in 1:12){
  
  eval(parse(text = paste0("BBS_routes_sp$site_meanmax_month_",j,"<- mean(wd_",j,"$tmax..deg.c)"))) #site mean max
  eval(parse(text = paste0("BBS_routes_sp$site_meanmin_month_",j,"<- mean(wd_",j,"$tmin..deg.c)"))) #site mean min
  eval(parse(text = paste0("BBS_routes_sp$site_meanPrecip_month_",j, "<- mean(wd_",j,"$prcp..mm.day.)"))) #site mean precip
  eval(parse(text = paste0("BBS_routes_sp$site_meanSWE_month_",j, "<- mean(wd_",j,"$swe..kg.m.2)"))) #site mean swe
  
  
  # Set the years apart
  for(k in 1980:2016){
    
    eval(parse(text = paste0("yr_",k, "<- wd_",j,"[which(wd_",j,"$year==",k,"),]")))

    eval(parse(text = paste0("BBS_routes_sp$meanmax_",k,"_", j, "[i] <- mean(yr_",k, "$tmax..deg.c.)"))) #mean max
    eval(parse(text = paste0("BBS_routes_sp$max_",k,"_", j,"[i] <- max(yr_",k,"$tmax..deg.c.)"))) #highest max
    eval(parse(text = paste0("BBS_routes_sp$meanmin_",k,"_", j,"[i] <- mean(yr_",k,"$tmin..deg.c.)"))) #mean min
    eval(parse(text = paste0("BBS_routes_sp$min_",k,"_", j,"[i] <- min(yr_",k,"$tmin..deg.c.)"))) #lowest min
    eval(parse(text = paste0("BBS_routes_sp$meanPrecip_",k,"_", j,"[i]<- mean(yr_",k,"$prcp..mm.day.)"))) #mean precip
    eval(parse(text = paste0("BBS_routes_sp$meanSWE_",k,"_", j,"[i] <- mean(yr_",k,"$swe..kg.m.2)"))) #mean swe
  }
  
  }
}


save(BBS_routes_sp, file = "BBS_routes_sp2.Rdata")

## Alternative precipitation measures
for(i in 1:dim(BBS_routes_sp)[1]){
  print(i)
  wd <- read.csv(paste("Daymet/", wdata[i], sep=""), skip=7) # this skips the first X number of rows to read data, including the header (it can add this)
  #wd <- read.csv(paste("C:/Users/Tingley Lab_2/Dropbox/BBS_Project/Daymet/", wdata[i], sep=""), skip=7) #Lab 2 computer

  wd_june_aug <- wd[wd$yday > 152 & wd$yday < 243, ] # june weather data
  wd_april <- wd[wd$yday < 120, ] # jan - April data
  wd_july <- wd[wd$yday > 182 , ] # july - dec weather data
  
  # Get the june through august means from per site
  for(j in 1980:2016){
      eval(parse(text = paste0("yr_",j, "<- wd_june_aug[which(wd_june_aug$year==",j,"),]")))
      eval(parse(text = paste0("BBS_routes_sp$mean_june_aug_precip_",j,"[i] <- mean(yr_",j, "$prcp..mm.day.)"))) #mean june - august precip

  }
  
  for(k in 1981:2016){
    eval(parse(text = paste0("yr2_",k, "<- rbind(wd_april[which(wd_april$year==",k,"),],wd_july[which(wd_july$year==",k-1,"),] )")))
    eval(parse(text = paste0("BBS_routes_sp$mean_july_april_",k,"[i] <- mean(yr2_",k, "$prcp..mm.day.)"))) #mean july(t-1) to april precip
  }
}

save(BBS_routes_sp, file = "BBS_routes_sp3.Rdata") # Changing the filename so it doesn't overwrite our earlier saves

## Combine with the lag data and create a new data document to move forward with

# read in the lag data
lag_1 <- read.csv("BBS_Data/lag1_80to16_data.csv")
lag_1 <- lag_1[,-1] #remove that first column of nothing
lag_1 <- lag_1[(lag_1$year_1 > 1979),] #Make it so it is only from 1980-2016

## Remove the bad birds
bird_species <- read.csv("BBS_Data/SpeciesList.csv")
good_birds <- bird_species[which(rowSums(bird_species[,c(8:13, 16, 19)], na.rm = T) == 0), ]
good_birds$aou <- good_birds$AOU

lag_1 <- lag_1[which(lag_1$aou %in% good_birds$AOU), ]
lag_1 <- dplyr::inner_join(lag_1, good_birds, by = "aou")

#Combine it with the BBS_routes_sp
# Have to rename BBS_routes_sp route_ID to routeID
BBS_routes_sp$routeID <- BBS_routes_sp$route_ID

## Save BBS_routes_sp as R.data file
save(BBS_routes_sp, file = "BBS_routes_sp4.Rdata") # Changing the filename so it doesn't overwrite our earlier saves

# Begin cutting down on bbs_routes_sp
BBS_routes_sp2 <- BBS_routes_sp[,-c(1:10)]
names(BBS_routes_sp2)[2] <- "routeID"
lag_weather <- dplyr::inner_join(lag_1, BBS_routes_sp2@data[,c(1:50)], by = "routeID")

BBS_routes_sp3 <- BBS_routes_sp2[, c(2, 51:2788)]

bbs3 <- reshape2::melt(BBS_routes_sp3@data, id.vars = c("routeID"))
bbs3 <- bbs3[which(bbs3$variable != "routeID.1"), ]

bbs3$year <- as.integer(as.vector(stringr::str_extract(bbs3$variable, stringr::regex("(?<![0-9])[0-9]{4}(?![0-9])"))))

dummy <- bbs3$variable
for(i in unique(bbs3$year)){
  dummy <- gsub(as.character(i), "", dummy)
}

bbs3$month <- as.integer(as.vector(stringr::str_extract_all(dummy, stringr::regex("[0-9]+"))))

bbs3$june_aug_precip <- 0
bbs3$june_aug_precip[grep("mean_june_aug_precip", bbs3$variable)] <- 1
bbs3$july_april_precip <- 0
bbs3$july_april_precip[grep("mean_july_april", bbs3$variable)] <- 1

#write.csv(lag_weather, file = "lag1_weather.csv")


#### Read in the weather data to transform it ----------------------------------------------------------------------------
setwd("C:/Users/Tingley Lab_2/Dropbox/BBS_Project") # Lab 2 Computer directory

## Load in the data

lag_and_weather <- read.csv("./BBS_Data/lag1_weather.csv", header = TRUE)

# Pull out just the count data
lag <- lag_and_weather[,c(2:10, 21:25)]
#colnames(lag)[11] <- "site_meanmaxESu"
#colnames(lag)[12] <- "site_meanminESu"
#colnames(lag)[13] <- "site_meanESuPrecip"
#colnames(lag)[14] <- "site_meanESuSWE"



#### Add in only the weather data from the year of year1

#### For loop getting the weather from Year 1 in one loop

n <- dim(lag)[1]

lag2 <- lag_and_weather

a <- Sys.time()
lag2$mean_high_ESu <- lag2[cbind(c(1:n),26+6*(lag2$year1-1980))]
lag2$max_high_ESu <- lag2[cbind(c(1:n),27+6*(lag2$year1-1980))]
lag2$mean_low_ESu <- lag2[cbind(c(1:n),28+6*(lag2$year1-1980))]
lag2$min_low_ESu <- lag2[cbind(c(1:n),29+6*(lag2$year1-1980))]
lag2$mean_ESu_Precip <- lag2[cbind(c(1:n),30+6*(lag2$year1-1980))]
lag2$mean_ESu_SWE <- lag2[cbind(c(1:n),31+6*(lag2$year1-1980))]
elapsed <- Sys.time() - a
lag <- lag2

## Get the mean temperature across all sites the bird is in 
species_list <- unique(lag$aou)
lag$SP_meanmaxESu <- rep(NA,n)
lag$SP_meanminESu <- rep(NA,n)
lag$SP_meanESuPrecip <- rep(NA,n)
lag$SP_meanESuSWE <- rep(NA,n)

lag$SP_sdmaxESu <- rep(NA,n)
lag$SP_sdminESu <- rep(NA,n)
lag$SP_sdESuPrecip <- rep(NA,n)
lag$SP_sdESuSWE <- rep(NA,n)

for(i in 1:length(species_list)){
  print(i)
  lag_SP <- lag[which(lag$aou == species_list[i]), ]
  one_route <- lag_SP[!duplicated(lag_SP$route_ID),]
  
  lag$SP_meanmaxESu[which(lag$aou == species_list[i])] <- mean(one_route$overall_meanmaxESu)
  lag$SP_meanminESu[which(lag$aou == species_list[i])] <- mean(one_route$overall_meanminESu)
  lag$SP_meanESuPrecip[which(lag$aou == species_list[i])] <- mean(one_route$overall_meanESuPrecip)
  lag$SP_meanESuSWE[which(lag$aou == species_list[i])] <- mean(one_route$overall_meanESuSWE)
  
  lag$SP_sdmaxESu[which(lag$aou == species_list[i])] <- sd(one_route$overall_meanmaxESu)
  lag$SP_sdminESu[which(lag$aou == species_list[i])] <- sd(one_route$overall_meanminESu)
  lag$SP_sdESuPrecip[which(lag$aou == species_list[i])] <- sd(one_route$overall_meanESuPrecip)
  lag$SP_sdESuSWE[which(lag$aou == species_list[i])] <- sd(one_route$overall_meanESuSWE)
}


#### Get a z score for the average site temp away from average sp temp
######### Just do the Z-score in January and June (not every month)
######### Precip and SWE in winter perhaps, although think about where this matters
######### Find precipitation measure that matters for productivity
lag$Z_meanmaxESu <- (lag$overall_meanmaxESu - lag$SP_meanmaxESu)/ lag$SP_sdmaxESu
lag$Z_meanminESu <- (lag$overall_meanminESu - lag$overall_meanminESu)/ lag$SP_sdminESu
lag$Z_meanESuPrecip <- (lag$overall_meanESuPrecip - lag$overall_meanESuPrecip)/ lag$SP_sdESuPrecip
lag$Z_meanESuSWE <- (lag$overall_meanESuSWE - lag$overall_meanESuSWE)/ lag$SP_sdESuSWE


write.csv(lag, file = "97_16_bbs_weather.csv")



# This script creates a model for avian lay-dates as a function of geographic location
# and elevation across North America. It then extracts predicted lay dates for an average
# species at the locations of each BBS route.

setwd("/Users/Jacob/Dropbox/Work/BBS_Project/")

# Load Nestwatch dataset
locations <- read.csv("/Users/Jacob/Dropbox/Work/Nestwatch/AllLocations.csv")

# Extract nests with known lay date, and prepare dataframe to hold jday and year data
nestdates <- locations[locations$FIRST_LAY_DT != locations$FIRST_LAY_DT[1], ]
nestdates$date <- as.Date(substr(nestdates$FIRST_LAY_DT,1,9),format="%d%B%Y")
nestdates$year <- NA
nestdates$jday <- NA

# get jday and year.  The below is fast simple code based on the lubridate package;
# however I don't have lubridate installed and I have no internet in Colombia, so below
# that is a much clunkier and slower base R solution.

nestdates$jday <- lubridate::yday(nestdates$date)
nestdates$year <- lubridate::year(nestdates$date)

# Trim to a reasonable geographic area, and remove some nests where the elevation is obviously
# erroneous
ndf <- nestdates[nestdates$LONGITUDE < -50  & nestdates$LATITUDE < 70 & nestdates$LATITUDE > 26 & nestdates$ELEVATION_M < 4000, ]
ndf <- ndf[-which(ndf$SUBNATIONAL1_CODE == "US-NY" & ndf$ELEVATION_M > 1500), ]

# Model nest date using a GAM with a 2D tensor smooth for lat/lon, and a 1D smooth for elevation.
# Default selection of smooth terms
# I don't fully understand the bs = "re" argument, but documentation assures me that this produces
# and IID Gaussian random effect of SPECIES_CODE
nest_model <- mgcv::gam(jday ~ s(ELEVATION_M) + t2(LONGITUDE, LATITUDE) + s(SPECIES_CODE, bs = "re"), data = ndf)

summary(nest_model)
plot(nest_model, scheme = 2)
plot(nest_model, scheme = 2, select = 2)
points(nestdates$LATITUDE ~ nestdates$LONGITUDE, pch = '.')

save(nest_model, file = "nest_model.Rdata")

# put predicted lay dates for each BBS route into the appropriate rows of lag_weather
load("BBS_Data/lag_weather.Rdata")
load("nest_model.Rdata")

# Create lat an lon columns in lag_weather (at some point in creating lag_weather we dropped
# the spatial data; this was an oversight)
load('BBS_routes_sp.Rdata')
BBS_routes_sp$routeID <- rep(NA, dim(BBS_routes_sp)[1])
BBS_routes_sp$routeID <- paste(BBS_routes_sp$countrynum, BBS_routes_sp$statenum, BBS_routes_sp$Route, sep=".")
BBS_routes_sp <- BBS_routes_sp[ , -c(1:5)]
routes_df <- cbind(BBS_routes_sp@data, BBS_routes_sp@coords)[, c("routeID", "Latitude", "Longitude")]
lag_weather <- dplyr::inner_join(lag_weather, routes_df)

# It turns out that lag_weather has a small number of really weird elevations greather than 
# 10,000 masl. All elevations over 4000 masl are also greater than 10000 masl. We can try to 
# understand this later, but for now we'll just drop them.
hist(lag_weather$w_elev)
length(which(lag_weather$w_elev > 10000))
length(which(lag_weather$w_elev > 4000))
lag_weather <- lag_weather[-which(lag_weather$w_elev > 4000), ]

# predict nest_model at locations of BBS routes, and store result in lag_weather
# Note that nest_model's intercept is for acadian flycatcher.  We want an intercept for the 
# average species in the dataset.

# We will do our prediction for Acadian Flycatcher, and then subtract the random effect estimate
# for Acadian Flycatcher from the resulting predictions, yielding a prediction for an "average"
# species. I don't know what the slick way to extract this estimate is, but the below should
# do the trick, taking advantage of the fact that the mean of the BLUPS should be 0.
# ***Need to double-confirm that this definitley works***

species <- unique(ndf$SPECIES_CODE)
species <- species[-which(is.na(species))]
avg_pred <- rep(NA, length(species))
for(i in 1:length(species)){
  p_data <- data.frame(SPECIES_CODE = species[i], ELEVATION_M = mean(ndf$ELEVATION_M, na.rm = T),
                       LATITUDE = mean(ndf$LATITUDE, na.rm = T),
                       LONGITUDE = mean(ndf$LONGITUDE, na.rm = T))
  avg_pred[i] <- mgcv::predict.gam(nest_model, newdata = p_data)
}

intercept.offset <- mean(avg_pred, ) - avg_pred[1]

# Now do the prediction
predict.data <- data.frame(SPECIES_CODE = "acafly", ELEVATION_M = lag_weather$w_elev,
                           LONGITUDE = lag_weather$Longitude, LATITUDE = lag_weather$Latitude)
lag_weather$pred_nest_jday <- as.vector(mgcv::predict.gam(nest_model, newdata = predict.data)) + intercept.offset

lag_weather_n <- lag_weather

save(lag_weather_n, file = "BBS_Data/lag_weather_n.Rdata")

#### Breeding Bird Survey Project
## Austin Spence
## February 12, 2018
## Last updated: October 8, 2018

# Packages 
library(tidyverse)
library(lubridate)

# Set wd
setwd("C:/Users/Tingley Lab_2/Dropbox/BBS_Project/BBS_Data/")

#Get all the state data and put into "states"
states <-
  list.files(path = "./states_80_16/",
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) 

## Begin to clean up states

#Only take runs with a standard operating procedure (RPID = 101) 
states <- states[which(states$RPID==101),]

# Create a unique route ID (combination of countrynum, statenum, and route)
states$RouteID <- paste(states$countrynum, states$statenum, states$Route, sep=".")

# Remove all columns except year, AOU, SpeciesTotal, and route_ID
states <- states[,c(5,6,13,14)]

# Remove unidentified birds and hybrids
states <- states[!states$Aou %in% c(01489, 01657, 01512, 02973,
                                    00012, 03875, 04205, 04206,
                                    04295, 04335, 04402, 02090,
                                    02429, 02327, 02555, 00315, 
                                    00534, 00634, 00077, 01206, 
                                    01205, 01207, 01945, 01861,
                                    03255, 03333, 03451, 04022,
                                    03920, 03827, 14035, 04665, 
                                    04689, 04642, 04679, 04462, 
                                    04447, 06296, 06295, 04810, 
                                    04881, 04882, 04865, 06122, 
                                    07352, 07353, 07354, 07351, 
                                    07332, 07315, 07220, 07539, 
                                    06185, 05195, 05225, 05275, 
                                    06556, 06685, 05871, 05740, 
                                    05677, 05935, 05012, 05135, 
                                    04955, 05077, 01326, 00495, 
                                    06686, 05986, 05078),]

# Combine subspecies to species level
states$Aou[states$Aou == 01740] <- 01730 #black brant to brant
states$Aou[states$Aou == 01691] <- 01690 #blue goose to snow goose
states$Aou[states$Aou == 03380] <- 01730 # harlan's hawk to redtail
states$Aou[states$Aou == 04120] <- 04123 #northern flicker together
states$Aou[states$Aou == 04130] <- 04123 #northern flicker together
states$Aou[states$Aou == 04125] <- 04123 #northern flicker together
states$Aou[states$Aou == 06556] <- 06550 #yellow rumped together
states$Aou[states$Aou == 06560] <- 06550 #yellow rumped together
states$Aou[states$Aou == 05677] <- 05670 #juncos together
states$Aou[states$Aou == 05690] <- 05670 #juncos together
states$Aou[states$Aou == 05660] <- 05670 #juncos together
states$Aou[states$Aou == 05680] <- 05670 #juncos together
states$Aou[states$Aou == 05671] <- 05670 #juncos together
states$Aou[states$Aou == 01331] <- 01320 #mexican mallard to mallard
states$Aou[states$Aou == 01920] <- 01940 #great white heron to great blue heron

# Make "Year" and "SpeciesTotal" numeric
states$Year <- as.numeric(states$Year)
states$SpeciesTotal <- as.numeric(states$SpeciesTotal)

# Check to make sure it is correct
str(states)


#########################################################################################
# Combine the route data with route observer, route temperature, and route day
setwd("C:/Users/Tingley Lab_2/Dropbox/BBS_Project/BBS_Data")
route_info <- read.csv("route_observer_info.csv")

# Create the same unique route ID (combination of countrynum, statenum, and route)
route_info$RouteID <- paste(route_info$CountryNum, route_info$StateNum, route_info$Route, sep=".")

# Change the month/day into julian day
route_info$date <- paste(route_info$Year, "-0", route_info$Month,"-", route_info$Day, sep = "")
route_info$julian_date <- yday(route_info$date)

# Change the incorrect temperature labels to "C" or "F" 
route_info[c(14362, 2879),]$TempScale <- "C"
route_info[c(38572, 21812, 32637, 11140, 31404,
             32991, 41543, 42126, 44108, 46424,
             48695, 53471, 57414, 59326, 62608,
             63152, 65412, 68112, 68433, 70467,
             72347, 79741, 84874, 86762, 90861,
             90983, 94938, 98452, 98859, 99550,
             99874, 101934, 108716, 109100, 
             114358, 117922, 92228, 116521, 120387),]$TempScale <- "F"

# Remove year/route combos with Null and " " for temperature data, then drop unused levels
route_info <- route_info[-c(which(route_info$TempScale == "NULL")),]
route_info<- route_info[-c(which(route_info$TempScale == " ")),]
route_info$TempScale <- factor(route_info$TempScale) 

# Make EndTemp numeric  create final_temp to be used
route_info$EndTemp <- as.numeric(route_info$EndTemp)

# Create final_temp column to be used for final dataframe
route_info$final_temp <- rep(NA, dim(route_info)[1])

# Transform only the fahrenheit temperatures to celsius
route_info <- transform(route_info, final_temp = ifelse(TempScale == "F", (EndTemp - 32)*(5/9), EndTemp))

###  Cut down route_info to be Year, Observer, StartTime, EndTime, RunType, RouteID, Julian_date, final_temp
route_info <- route_info[,c(6,9, 18,19, 22, 23,25,26)]


################################################################################################
# Add the Route Information with the Count Information
states_routes <- dplyr::inner_join(states, route_info, by = c("RouteID", "Year"))

# Remove all observations where the Run Type does not equal 1
states_routes <- states_routes[(states_routes$RunType == "1"),]

# Remove Run Type from the dataframe
states_routes <- states_routes[,-8]

################################################################################################
#### Create the new dataframe with the lags ----
lag_data <- list()

for(i in 1:1000){
  lag_data[[i]] <- as.data.frame(matrix(data=NA, nrow=1000, ncol=18))
  colnames(lag_data[[i]]) <- c("Year", "Aou", "SpeciesTotal", "RouteID", "ObsN", "StartTime", "EndTime", "julian_date", "final_temp", 
                               "Year", "Aou", "SpeciesTotal", "RouteID", "ObsN", "StartTime", "EndTime", "julian_date", "final_temp")
}


species_list <- unique(states_routes$Aou)
badys <- list()
counter <- 0
counter2 <- 1
breaks <- 1000*(0:4000)
for(i in 1:length(species_list)){
  states_routes_SP <- states_routes[which(states_routes$Aou == species_list[i]), ]
  route_list_SP <- unique(states_routes_SP$RouteID)
  for(j in 1:length(route_list_SP)){
    print(paste(species_list[i], i, "   ", "route", j, sep=" "))
    SP_R <- states_routes_SP[which(states_routes_SP$RouteID == route_list_SP[j]), ]
    SP_R <- SP_R[order(SP_R$Year),]
    years <- SP_R$Year
    year1s <- years[which((years+1) %in% years)]
    if(length(year1s > 0)){
      for(k in 1:length(year1s)){
        counter <- counter+1
        ind <- which(breaks == max(breaks[breaks<counter]))
        yearline0 <- which(SP_R$Year==year1s[k])
        yearline <- min(which(SP_R$Year==year1s[k]))
        yearline2 <- min(which(SP_R$Year==(year1s[k]+1)))
        if(length(yearline0) > 1){
          badys[[counter2]] <- c(i,j,k)
          counter2 <- counter2+1
        }
        lag_data[[ind]][counter-breaks[ind], ] <- cbind(SP_R[yearline,], SP_R[yearline2,])
      }
    }
  }
}


# Combine all the dataframes into 1 large one
lag_df <- do.call("rbind", lag_data)

# Remove the redundant information (AOU2, route_ID_2)
lag_df <- lag_df[,-c(11,13)]

# get rid of useless trailing NA rows
lag_df <- na.omit(lag_df)

# Rename the columns
colnames(lag_df) <- c("year_1", "aou", "n1_total", "routeID", "observer_1", "start_time_1", "end_time_1", "julian_date_1", "final_temp_1",
                      "year_2", "n2_total", "observer_2", "start_time_2", "end_time_2", "julian_date_2", "final_temp_2")

#Check to make sure it worked
str(lag_df)
head(lag_df) 

#### Create the lags - although this is now done in the big for-loop and makes only year 1 lags
lag_df$lag <- lag_df$year_2 - lag_df$year_1

# Create the r_star component
lag_df$r_star <- lag_df$n2_total/lag_df$n1_total

### Now create only lags with of 1 year, and cull different observers, far julian dates and times

# Make all lag years have the same observer
lag_df$observer_check <- lag_df$observer_1 - lag_df$observer_2
lag_df <- lag_df[(lag_df$observer_check == "0"),]

# Make all lag years start within 7 days of each other
lag_df$day_check <- abs(lag_df$julian_date_1 - lag_df$julian_date_2)
lag_df <- lag_df[(lag_df$day_check < 8),]

# Make all lag years start within 20 minutes of each other
lag_df$time_check <- abs(lag_df$start_time_1 - lag_df$start_time_2)
lag_df <- lag_df[(lag_df$time_check < 21),]

# Make all lag years end within 20 minutes of each other
lag_df$time_check2 <- abs(lag_df$end_time_1 - lag_df$end_time_2)
lag_df <- lag_df[(lag_df$time_check2 < 21),]

# Find the temperature differential between days
lag_df$temp_difference <- lag_df$final_temp_2 - lag_df$final_temp_1

## Write the csv for all lag years
write.csv(lag_df, file = "lag1_80to16_data.csv")



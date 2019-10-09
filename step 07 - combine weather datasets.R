#### Step 6 - combine breeding and monthly weather datasets to a full dataset

setwd("C:/Users/Tingley Lab_2/Dropbox/BBS_Project") #Austin's lab computer

load("lag_breeding_weather.RData") # this has the breeding weather data
load("./BBS_Data/lag_weather_n.RData") # This has the month weather data and the predicted jday


## DO A JANKY ADDITION OF THE JULY AUGUST WEATHER
# TO FIX: get data from jacob to run the pred.j.date stuff, run it, and the lag_weather_n.Rdata 
# will be up to date
# This janky fix simply takes the july/august columns and adds it to the lag_weather_n.Rdata,
# from the correct lag_weather.RData (pre-predicted breeding jdate)

load("./BBS_Data/lag_weather.RData") # this has the monthly weather weather data including july/august
length(which(lag_weather$w_elev > 4000))
lag_weather <- lag_weather[-which(lag_weather$w_elev > 4000), ]

lag_weather$pred_nest_jday <- lag_weather_n$pred_nest_jday
lag_weather_n <- lag_weather
rm(lag_weather)
### change the month names from 1-12 to jan-dec in the lag_weather_n file

colname <- names(lag_weather_n)
colname[44:147] <- c("mean_high_jan","mean_low_jan", "mean_precip_jan", "mean_swe_jan",
                     "mean_high_feb","mean_low_feb", "mean_precip_feb", "mean_swe_feb",
                     "mean_high_mar","mean_low_mar", "mean_precip_mar", "mean_swe_mar",
                     "mean_high_apr","mean_low_apr", "mean_precip_apr", "mean_swe_apr",
                     "mean_high_may","mean_low_may", "mean_precip_may", "mean_swe_may",
                     "mean_high_jun","mean_low_jun", "mean_precip_jun", "mean_swe_jun",
                     "mean_high_jul","mean_low_jul", "mean_precip_jul", "mean_swe_jul",
                     "mean_high_aug","mean_low_aug", "mean_precip_aug", "mean_swe_aug",
                     "mean_high_sep","mean_low_sep", "mean_precip_sep", "mean_swe_sep",
                     "mean_high_oct","mean_low_oct", "mean_precip_oct", "mean_swe_oct",
                     "mean_high_nov","mean_low_nov", "mean_precip_nov", "mean_swe_nov",
                     "mean_high_dec","mean_low_dec", "mean_precip_dec", "mean_swe_dec",
                     "mean_high_jul_aug","mean_low_jul_aug", "mean_precip_jul_aug", "mean_swe_jul_aug",
                     "sd_high_jan","sd_low_jan", "sd_precip_jan", "sd_swe_jan",
                     "sd_high_feb","sd_low_feb", "sd_precip_feb", "sd_swe_feb",
                     "sd_high_mar","sd_low_mar", "sd_precip_mar", "sd_swe_mar",
                     "sd_high_apr","sd_low_apr", "sd_precip_apr", "sd_swe_apr",
                     "sd_high_may","sd_low_may", "sd_precip_may", "sd_swe_may",
                     "sd_high_jun","sd_low_jun", "sd_precip_jun", "sd_swe_jun",
                     "sd_high_jul","sd_low_jul", "sd_precip_jul", "sd_swe_jul",
                     "sd_high_aug","sd_low_aug", "sd_precip_aug", "sd_swe_aug",
                     "sd_high_sep","sd_low_sep", "sd_precip_sep", "sd_swe_sep",
                     "sd_high_oct","sd_low_oct", "sd_precip_oct", "sd_swe_oct",
                     "sd_high_nov","sd_low_nov", "sd_precip_nov", "sd_swe_nov",
                     "sd_high_dec","sd_low_dec", "sd_precip_dec", "sd_swe_dec",
                     "sd_high_jul_aug","sd_low_jul_aug", "sd_precip_jul_aug", "sd_swe_jul_aug")

colnames(lag_weather_n) <- colname

### Round the pred_nest_jday to nearest day

lag_weather_n$pred_nest_jday <- round(lag_weather_n$pred_nest_jday)

# add in the ordered predicted lay dates (earliest day date gets 1, second earliest gets 2, etc)

pred_lay_dates <- lag_weather_n[,c(1,2,4,383)]

jdays <- data.frame(matrix(ncol = 2, nrow = 64))
names <- c("pred_nest_jday", "jday_order")
colnames(jdays) <- names
jdays$pred_nest_jday <- unique((pred_lay_dates$pred_nest_jday))
jdays$pred_nest_jday <- sort(jdays$pred_nest_jday)
jdays$jday_order <- c(1:64)

lag_weather_n <- dplyr::inner_join(lag_weather_n, jdays, by = "pred_nest_jday") # Jday order is the ordinal number for lay date (86 was the first lay date so it gets 1, 204 was last so it is 64)


# Figure out how to add them together without doubling up stuff (currently by doing it by all the other variables - clearly not the best way)

both_weather_datasets <- dplyr::inner_join(lag_breeding_weather, lag_weather_n, by = c("year_1", "aou", "n1_total", "routeID", 
                                                                                       "observer_1", "start_time_1", "end_time_1", "julian_date_1",
                                                                                       "final_temp_1", "year_2", "n2_total", "observer_2",
                                                                                       "start_time_2", "end_time_2", "julian_date_2", "final_temp_2", 
                                                                                       "lag", "r_star", "observer_check", "day_check", 
                                                                                       "time_check", "time_check2", "temp_difference", "Seq", 
                                                                                       "AOU", "English_Common_Name", "ORDER", "Family", 
                                                                                       "Genus", "Species", "Difficult.to.ID.", "Food.from.ocean.",
                                                                                       "Breed.on.beach.", "Breed.in.saltmarsh.", "Breed.on.tundra.", "Mexican.southern.range.", 
                                                                                       "Neotropical.migrant.", "Short.distance.migrant", "Alt..problem.", "Notes",
                                                                                       "year_0", "year_2"))



# Get the breeding and month data together alone, get rid of all the 1-64 and only leaving it for that sites specific
# predicted lay date. 

# First take out the the monthly means and year 0,1,2 for those (we want to keep them) 
breeding_and_month_weather <- both_weather_datasets[c(1:42, 555, 1710:2050)]

# Get the breeding specific means across all years for that specific lay date
ptm <- proc.time()
for(i in 1:length(breeding_and_month_weather[[1]])){
  
  breeding_and_month_weather[i,"breeding_mean_high"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'mean_breeding_high_",both_weather_datasets[i,2050],"']")))
  breeding_and_month_weather[i,"breeding_sd_high"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'sd_breeding_high_",both_weather_datasets[i,2050],"']")))
  
  breeding_and_month_weather[i,"breeding_mean_low"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'mean_breeding_low_",both_weather_datasets[i,2050],"']")))
  breeding_and_month_weather[i,"breeding_sd_low"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'sd_breeding_low_",both_weather_datasets[i,2050],"']")))
  
  breeding_and_month_weather[i,"breeding_mean_precip"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'mean_breeding_precip_",both_weather_datasets[i,2050],"']")))
  breeding_and_month_weather[i,"breeding_sd_precip"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'sd_breeding_precip_",both_weather_datasets[i,2050],"']")))
  
  breeding_and_month_weather[i,"breeding_mean_swe"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'mean_breeding_swe_",both_weather_datasets[i,2050],"']")))
  breeding_and_month_weather[i,"breeding_sd_swe"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'sd_breeding_swe_",both_weather_datasets[i,2050],"']")))
  
  # Get the breeding specific weather variables for the specific year for that specific lay date
  breeding_and_month_weather[i,"breeding_avg_high_year_0"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'breeding_avg_high_",both_weather_datasets[i,2050],"_year_0']")))
  breeding_and_month_weather[i,"breeding_max_high_year_0"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'breeding_max_high_",both_weather_datasets[i,2050],"_year_0']")))
  breeding_and_month_weather[i,"breeding_avg_low_year_0"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'breeding_avg_low_",both_weather_datasets[i,2050],"_year_0']")))
  breeding_and_month_weather[i,"breeding_min_low_year_0"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'breeding_min_low_",both_weather_datasets[i,2050],"_year_0']")))
  breeding_and_month_weather[i,"breeding_avg_precip_year_0"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'breeding_avg_precip_",both_weather_datasets[i,2050],"_year_0']")))
  breeding_and_month_weather[i,"breeding_avg_swe_year_0"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'breeding_avg_swe_",both_weather_datasets[i,2050],"_year_0']")))
  
  breeding_and_month_weather[i,"breeding_avg_high_year_1"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'breeding_avg_high_",both_weather_datasets[i,2050],"_year_1']")))
  breeding_and_month_weather[i,"breeding_max_high_year_1"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'breeding_max_high_",both_weather_datasets[i,2050],"_year_1']")))
  breeding_and_month_weather[i,"breeding_avg_low_year_1"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'breeding_avg_low_",both_weather_datasets[i,2050],"_year_1']")))
  breeding_and_month_weather[i,"breeding_min_low_year_1"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'breeding_min_low_",both_weather_datasets[i,2050],"_year_1']")))
  breeding_and_month_weather[i,"breeding_avg_precip_year_1"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'breeding_avg_precip_",both_weather_datasets[i,2050],"_year_1']")))
  breeding_and_month_weather[i,"breeding_avg_swe_year_1"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'breeding_avg_swe_",both_weather_datasets[i,2050],"_year_1']")))
  
  breeding_and_month_weather[i,"breeding_avg_high_year_2"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'breeding_avg_high_",both_weather_datasets[i,2050],"_year_2']")))
  breeding_and_month_weather[i,"breeding_max_high_year_2"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'breeding_max_high_",both_weather_datasets[i,2050],"_year_2']")))
  breeding_and_month_weather[i,"breeding_avg_low_year_2"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'breeding_avg_low_",both_weather_datasets[i,2050],"_year_2']")))
  breeding_and_month_weather[i,"breeding_min_low_year_2"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'breeding_min_low_",both_weather_datasets[i,2050],"_year_2']")))
  breeding_and_month_weather[i,"breeding_avg_precip_year_2"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'breeding_avg_precip_",both_weather_datasets[i,2050],"_year_2']")))
  breeding_and_month_weather[i,"breeding_avg_swe_year_2"] <- eval(parse(text = paste0("both_weather_datasets[",i,",'breeding_avg_swe_",both_weather_datasets[i,2050],"_year_2']")))
}
proc.time() - ptm # This takes about 2 hours


# Create the new data file with both month and breeding date information 

save(breeding_and_month_weather, file = "BBS_Data/breeding_and_month_weather.Rdata")
write.csv(breeding_and_month_weather, file = "BBS_Data/breeding_and_month_weather.csv")




#### Attach the Post-Breeding Weather onto the breeding_and_month_weather
load("lag_2nd_breeding_weather.Rdata")

# add them together without doubling up on the columns 

add_post_breeding <- dplyr::inner_join(breeding_and_month_weather, lag_2nd_breeding_weather, by = c("year_1", "aou", "n1_total", "routeID", 
                                                                                       "observer_1", "start_time_1", "end_time_1", "julian_date_1",
                                                                                       "final_temp_1", "year_2", "n2_total", "observer_2",
                                                                                       "start_time_2", "end_time_2", "julian_date_2", "final_temp_2", 
                                                                                       "lag", "r_star", "observer_check", "day_check", 
                                                                                       "time_check", "time_check2", "temp_difference", "Seq", 
                                                                                       "AOU", "English_Common_Name", "ORDER", "Family", 
                                                                                       "Genus", "Species", "Difficult.to.ID.", "Food.from.ocean.",
                                                                                       "Breed.on.beach.", "Breed.in.saltmarsh.", "Breed.on.tundra.", "Mexican.southern.range.", 
                                                                                       "Neotropical.migrant.", "Short.distance.migrant", "Alt..problem.", "Notes",
                                                                                       "year_0", "year_2"))



# Get the breeding and month data together alone, get rid of all the 1-64 and only leaving it for that sites specific
# predicted lay date. 

# first keep all of the breeding_and_month_weather
breed_postbreed_month_weather <- add_post_breeding[c(1:410)]

# Get the breeding specific means across all years for that specific lay date
ptm <- proc.time()
for(i in 1:length(breed_postbreed_month_weather[[1]])){
  
  breed_postbreed_month_weather[i,"post_breeding_mean_high"] <- eval(parse(text = paste0("add_post_breeding[",i,",'mean_2nd_breeding_high_",add_post_breeding[i,384],"']")))
  breed_postbreed_month_weather[i,"post_breeding_sd_high"] <- eval(parse(text = paste0("add_post_breeding[",i,",'sd_2nd_breeding_high_",add_post_breeding[i,384],"']")))
  
  breed_postbreed_month_weather[i,"post_breeding_mean_low"] <- eval(parse(text = paste0("add_post_breeding[",i,",'mean_2nd_breeding_low_",add_post_breeding[i,384],"']")))
  breed_postbreed_month_weather[i,"post_breeding_sd_low"] <- eval(parse(text = paste0("add_post_breeding[",i,",'sd_2nd_breeding_low_",add_post_breeding[i,384],"']")))
  
  breed_postbreed_month_weather[i,"post_breeding_mean_precip"] <- eval(parse(text = paste0("add_post_breeding[",i,",'mean_2nd_breeding_precip_",add_post_breeding[i,384],"']")))
  breed_postbreed_month_weather[i,"post_breeding_sd_precip"] <- eval(parse(text = paste0("add_post_breeding[",i,",'sd_2nd_breeding_precip_",add_post_breeding[i,384],"']")))
  
  breed_postbreed_month_weather[i,"post_breeding_mean_swe"] <- eval(parse(text = paste0("add_post_breeding[",i,",'mean_2nd_breeding_swe_",add_post_breeding[i,384],"']")))
  breed_postbreed_month_weather[i,"post_breeding_sd_swe"] <- eval(parse(text = paste0("add_post_breeding[",i,",'sd_2nd_breeding_swe_",add_post_breeding[i,384],"']")))
  
  # Get the breeding specific weather variables for the specific year for that specific lay date
  breed_postbreed_month_weather[i,"post_breeding_avg_high_year_0"] <- eval(parse(text = paste0("add_post_breeding[",i,",'second_breeding_avg_high_",add_post_breeding[i,384],"_year_0']")))
  breed_postbreed_month_weather[i,"post_breeding_max_high_year_0"] <- eval(parse(text = paste0("add_post_breeding[",i,",'second_breeding_max_high_",add_post_breeding[i,384],"_year_0']")))
  breed_postbreed_month_weather[i,"post_breeding_avg_low_year_0"] <- eval(parse(text = paste0("add_post_breeding[",i,",'second_breeding_avg_low_",add_post_breeding[i,384],"_year_0']")))
  breed_postbreed_month_weather[i,"post_breeding_min_low_year_0"] <- eval(parse(text = paste0("add_post_breeding[",i,",'second_breeding_min_low_",add_post_breeding[i,384],"_year_0']")))
  breed_postbreed_month_weather[i,"post_breeding_avg_precip_year_0"] <- eval(parse(text = paste0("add_post_breeding[",i,",'second_breeding_avg_precip_",add_post_breeding[i,384],"_year_0']")))
  breed_postbreed_month_weather[i,"post_breeding_avg_swe_year_0"] <- eval(parse(text = paste0("add_post_breeding[",i,",'second_breeding_avg_swe_",add_post_breeding[i,384],"_year_0']")))
  
  breed_postbreed_month_weather[i,"post_breeding_avg_high_year_1"] <- eval(parse(text = paste0("add_post_breeding[",i,",'second_breeding_avg_high_",add_post_breeding[i,384],"_year_1']")))
  breed_postbreed_month_weather[i,"post_breeding_max_high_year_1"] <- eval(parse(text = paste0("add_post_breeding[",i,",'second_breeding_max_high_",add_post_breeding[i,384],"_year_1']")))
  breed_postbreed_month_weather[i,"post_breeding_avg_low_year_1"] <- eval(parse(text = paste0("add_post_breeding[",i,",'second_breeding_avg_low_",add_post_breeding[i,384],"_year_1']")))
  breed_postbreed_month_weather[i,"post_breeding_min_low_year_1"] <- eval(parse(text = paste0("add_post_breeding[",i,",'second_breeding_min_low_",add_post_breeding[i,384],"_year_1']")))
  breed_postbreed_month_weather[i,"post_breeding_avg_precip_year_1"] <- eval(parse(text = paste0("add_post_breeding[",i,",'second_breeding_avg_precip_",add_post_breeding[i,384],"_year_1']")))
  breed_postbreed_month_weather[i,"post_breeding_avg_swe_year_1"] <- eval(parse(text = paste0("add_post_breeding[",i,",'second_breeding_avg_swe_",add_post_breeding[i,384],"_year_1']")))
  
  breed_postbreed_month_weather[i,"post_breeding_avg_high_year_2"] <- eval(parse(text = paste0("add_post_breeding[",i,",'second_breeding_avg_high_",add_post_breeding[i,384],"_year_2']")))
  breed_postbreed_month_weather[i,"post_breeding_max_high_year_2"] <- eval(parse(text = paste0("add_post_breeding[",i,",'second_breeding_max_high_",add_post_breeding[i,384],"_year_2']")))
  breed_postbreed_month_weather[i,"post_breeding_avg_low_year_2"] <- eval(parse(text = paste0("add_post_breeding[",i,",'second_breeding_avg_low_",add_post_breeding[i,384],"_year_2']")))
  breed_postbreed_month_weather[i,"post_breeding_min_low_year_2"] <- eval(parse(text = paste0("add_post_breeding[",i,",'second_breeding_min_low_",add_post_breeding[i,384],"_year_2']")))
  breed_postbreed_month_weather[i,"post_breeding_avg_precip_year_2"] <- eval(parse(text = paste0("add_post_breeding[",i,",'second_breeding_avg_precip_",add_post_breeding[i,384],"_year_2']")))
  breed_postbreed_month_weather[i,"post_breeding_avg_swe_year_2"] <- eval(parse(text = paste0("add_post_breeding[",i,",'second_breeding_avg_swe_",add_post_breeding[i,384],"_year_2']")))

  }
proc.time() - ptm # This takes about 2 hours

save(breed_postbreed_month_weather, file = "BBS_Data/breed_postbreed_month_weather.Rdata")

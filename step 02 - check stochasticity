#### Breeding Bird Survey Project
## Austin Spence
## October 8, 2018
## Last updated: October 8, 2018

## This code runs the lag data through to make sure it is 
## environmentally stochastic, not demographically stochastic. 
## While this was proven in the Kazhuniak (Spelling incorrect)
## paper, we want to do it on our data too. 

####################################################################################################
# Beyond here is checking for environmental stochasticity
lag_total <- lag_df

lag_total$total_demo <- (lag_total$n2_total - lag_total$n1_total)/sqrt(lag_total$n1_total)
lag_total$total_enviro <- log(lag_total$n2_total/lag_total$n1_total)

# Check to make sure it worked
head(lag_total)

#### Find out if it is demographic or environmental stochasticity  ----
### lag_total 
# Pool by initial population size
lag_total$pop_group <- ifelse(31>lag_total$n1_total, "low",
                              ifelse(61>lag_total$n1_total & lag_total$n1_total>30,"medium",
                                     ifelse(91>lag_total$n1_total & lag_total$n1_total>60, "high", "very_high")))

# Find the variance per lag number 
## I am doing this a very dragged out way - I'm sure there is an easier way with less code
low_group_total <- lag_total[ which(lag_total$pop_group == "low"), ]
medium_group_total <- lag_total[ which(lag_total$pop_group == "medium"), ]
high_group_total <- lag_total[ which(lag_total$pop_group == "high"), ]
very_high_group_total <- lag_total[ which(lag_total$pop_group == "very_high"), ]

low_var_total <- aggregate(low_group_total[, 16:17], list(low_group_total$lag), var) #Get the variance of enviro and demo stochasticity
medium_var_total <- aggregate(medium_group_total[, 16:17], list(medium_group_total$lag), var)
high_var_total <- aggregate(high_group_total[, 16:17], list(high_group_total$lag), var)
very_high_var_total <- aggregate(very_high_group_total[, 16:17], list(very_high_group_total$lag), var)

# Combine all the variances by group into one dataframe                  
### This is a terrible way to combine them. I do it better below but I already wrote some plot code so I'll deal with is later. 
total_lag_vars <- cbind(low_var_total, medium_var_total, high_var_total, very_high_var_total)
total_lag_vars <- total_lag_vars[,-c(4, 7, 10)]
colnames(total_lag_vars) <- c("lag", "low_demo", "low_env", "medium_demo", "medium_env", "high_demo", "high_env", "v_high_demo", "v_high_env")

# Plot to see the slopes
plot(total_enviro~Group.1, low_var_total, ylim = c(0, 1.2), col = "black", main = "Total Route Lag Variation", xlab = "Lag",
     ylab = "V(E)")
abline(lm(total_enviro~Group.1, low_var_total))
abline(lm(total_enviro~Group.1, medium_var_total), col = "red")
abline(lm(total_enviro~Group.1, high_var_total), col = "blue")
abline(lm(total_enviro~Group.1, very_high_var_total), col = "pink")
legend(1, 1.2, legend=c("Low", "Medium", "High", "Very High"), col=c("black", "red", "blue", "pink"), lty=1, cex=0.8)

plot(total_demo~Group.1, low_var_total, ylim = c(10, 70), col = "black", main = "Total Route Lag Variation", xlab = "Lag",
     ylab = "V(E)")
abline(lm(total_demo~Group.1, low_var_total))
abline(lm(total_demo~Group.1, medium_var_total), col = "red")
abline(lm(total_demo~Group.1, high_var_total), col = "blue")
abline(lm(total_demo~Group.1, very_high_var_total), col = "pink")
legend(1, 70, legend=c("Low", "Medium", "High", "Very High"), col=c("black", "red", "blue", "pink"), lty=1, cex=0.8)

### Make a Better Linear Regression with the data set up correctly (4 columns, not like above)
# Reorder the data
# We have low:v_high vars, just don't c bind them, r bind them....
# create a new factor column that explains the population size
low_var$pop <- "low"
medium_var$pop <- "medium"
high_var$pop <- "high"
very_high_var$pop <- "v_high"

# combine the dataframes together
tot_lag_var <- rbind(low_var, medium_var, high_var, very_high_var)
tot_lag_var$pop <- as.factor(tot_lag_var$pop)
colnames(tot_lag_var) <- c("lag","dem","env","pop")

# Now do a linear regression with the interaction to test if the pop size affects things
# super simple anova - we will need to figure out how we want to do this. The paper doesn't explicitly say how they did it besides
# that the slopes were independent of population size
env_mod <- aov(env ~ lag*pop, tot_lag_var)
dem_mod <- aov(dem ~ lag*pop, tot_lag_var)

#Analyze the summaries
summary(env_mod) ## Lag and pop don't have an interaction
summary(dem_mod)

# Test to see if no lag/pop interaction is better
env_mod2 <- aov(env ~ lag + pop, tot_lag_var)
summary(env_mod2) ## Check it out
anova(env_mod, env_mod2) # More parsimonious without interaction





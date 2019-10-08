#### -------------------------------------------------------------------------------- ####
#### Preliminary Bayesian multi-species models
#### -------------------------------------------------------------------------------- ####

# Created by Andrew, worked through by Andrew/Austin

library(R2jags)
library(dclone)      # built-in functionality for parallel MCMC with jags via jags.parfit()


#setwd("~/Dropbox/____Projects____/BBS_Project") # Andrew
#setwd("C:/Users/Tingley Lab_2/Dropbox/BBS_Project") #Tingley lab computer
setwd("./BBS_Data")

## Load and organize data
load("passeriformes.Rdata")

## Change the name 
birds <- passeriformes

## Scale appropriate data

## Scale raw temperature and precipitation values
birds$breeding_avg_high_year_1_scaled <- (birds$breeding_avg_high_year_1 - mean(birds$breeding_avg_high_year_1))/sd(birds$breeding_avg_high_year_1)
birds$breeding_avg_precip_year_1_scaled <- (birds$breeding_avg_precip_year_1 - mean(birds$breeding_avg_precip_year_1))/sd(birds$breeding_avg_precip_year_1)
birds$monthly_avg_high_13_year_1_zscore <- birds$monthly_avg_high_13_year_1 - mean(birds$monthly_avg_high_13_year_1)/sd(birds$monthly_avg_high_13_year_1)
birds$monthly_avg_precip_13_year_1_zscore <- birds$monthly_avg_precip_13_year_1 - mean(birds$monthly_avg_precip_13_year_1)/sd(birds$monthly_avg_precip_13_year_1)

## Scale raw temperature and precipitation values
birds$breeding_avg_high_year_1_scaled <- (birds$breeding_avg_high_year_1 - mean(birds$breeding_avg_high_year_1))/sd(birds$breeding_avg_high_year_1)
birds$breeding_avg_precip_year_1_scaled <- (birds$breeding_avg_precip_year_1 - mean(birds$breeding_avg_precip_year_1))/sd(birds$breeding_avg_precip_year_1)
birds$monthly_avg_high_13_year_1_scaled <- birds$monthly_avg_high_13_year_1 - mean(birds$monthly_avg_high_13_year_1)/sd(birds$monthly_avg_high_13_year_1)
birds$monthly_avg_precip_13_year_1_scaled <- birds$monthly_avg_precip_13_year_1 - mean(birds$monthly_avg_precip_13_year_1)/sd(birds$monthly_avg_precip_13_year_1)

####--------------------------------- Breeding High Temp Model ----------------------------- ####

b.hightemp.function <- function(){
  
  ## Prior distributions on parameters
  b.0 ~ dnorm(0,0.001)
  b.relative_niche_high ~ dnorm(0,0.001)       # Is the site in the warm part of the thermal niche?
  b.year1 ~ dnorm(0,0.001)                        # What was the temperature of that year?
 
  b.niche_year1 ~ dnorm(0,0.001)                # Interaction between actual temperature and thermal niche
  
  tau ~ dgamma(0.001,0.001)
  
  tau.species ~ dgamma(0.001, 0.001)  # could change to simag~unif()
  sigma.species <- 1/sqrt(tau.species)
  
  ## Process model
  for(i in 1:n.obs){
    mu[i] <- b.0 + 
      b.relative_niche_high*relative_niche_high[i] +                    # effect of relative niche temp
      b.year1*year1[i] +                                                # effect of year 1 temp
      b.niche_year1*relative_niche_high[i]*year1[i] +                   # effect of niche and year 1 temp interaction
      b.species[species[i]]                                             # random spp intercept
    
    y[i] ~ dlnorm(mu[i], tau)                                           # log normal response
  }
  
  ## Random intercept for each species 
  for(j in 1:n.species){
    b.species[j] ~ dnorm(0, tau.species)
  }
}


## Set data for the model
jags.list.b.hightemp <- list(n.obs=nrow(birds), 
                           n.species=length(unique(birds$aou)),
                           relative_niche_high = birds$breeding_mean_high_zscore,
                           year1 = birds$breeding_avg_high_year_1_scaled,
                           species = birds$aou,
                           y = birds$r_star)


params.save <- c("b.0", "b.year1", "b.relative_niche_high", "b.niche_year1",  "sigma.species")

ptm <- proc.time()



cl <- makePSOCKcluster(3)   # Make your socket cluster and name it 'cl'.  nc is the number of cores you want your cluster to have--generally the number of chains you want to run.
tmp <- clusterEvalQ(cl, library(dclone))  # Check that `dclone` is loaded on each of cl's workers. dclone includes JAGS functionality
parLoadModule(cl, "glm")  # load the JAGS module 'glm' on each worker
parListModules(cl)  # make sure previous line worked.
## Run the model here:
b.hightemp.mod <- jags.parfit(cl=cl, data=jags.list.b.hightemp, 
                            params=params.save, model=b.hightemp.function, 
                            n.chains=3, n.adapt=1000, n.update=5000, thin=10, n.iter=20000)
stopCluster(cl)    # close out the cluster.
proc.time() - ptm

#save(b.hightemp.mod, file = "b.hightemp_mod.Rdata")



####--------------------------------- Breeding Anomalous High Temp Model ----------------------------- ####

b.a.hightemp.function <- function(){
  
  ## Prior distributions on parameters
  b.0 ~ dnorm(0,0.001)
  b.relative_niche_high ~ dnorm(0,0.001)       # Is the site in the warm part of the thermal niche?
  b.relative_year1 ~ dnorm(0,0.001)               # Was this site particularly warm relative to the mean of this site?
  
  b.niche_relative_year1 ~ dnorm(0,0.001)         # Interaction between relative temperature and thermal niche
  
  tau ~ dgamma(0.001,0.001)
  
  tau.species ~ dgamma(0.001, 0.001)  # could change to simag~unif()
  sigma.species <- 1/sqrt(tau.species)
  
  ## Process model
  for(i in 1:n.obs){
    mu[i] <- b.0 + b.relative_niche_high*relative_niche_high[i] +                     # effect of relative niche temp
      b.relative_year1*relative_year1[i] +                                     # effect of relative year 1 temp
      b.niche_relative_year1*relative_niche_high[i]*relative_year1[i] +     # effect of nich and relative year 1 temp interaction
      b.species[species[i]]                                                    # random intercept
    
    y[i] ~ dlnorm(mu[i], tau)                                                               # log normal response
  }
  
  ## Random intercept for each species 
  for(j in 1:n.species){
    b.species[j] ~ dnorm(0, tau.species)
  }
}


## Set data for the model
jags.list.b.a.hightemp <- list(n.obs=nrow(birds), 
                           n.species=length(unique(birds$aou)),
                           relative_niche_high = birds$breeding_mean_high_zscore,
                           relative_year1 = birds$breeding_avg_high_year_1_zscore,
                           species = birds$aou,
                           y = birds$r_star)


params.save <- c("b.0",  "b.relative_niche_high", " b.relative_year1", "b.niche_relative_year1",  "sigma.species")

ptm <- proc.time()



cl <- makePSOCKcluster(3)   # Make your socket cluster and name it 'cl'.  nc is the number of cores you want your cluster to have--generally the number of chains you want to run.
tmp <- clusterEvalQ(cl, library(dclone))  # Check that `dclone` is loaded on each of cl's workers. dclone includes JAGS functionality
parLoadModule(cl, "glm")  # load the JAGS module 'glm' on each worker
parListModules(cl)  # make sure previous line worked.
## Run the model here:
b.a.hightemp.mod <- jags.parfit(cl=cl, data=jags.list.b.a.hightemp, 
                            params=params.save, model=b.a.hightemp.function, 
                            n.chains=3, n.adapt=1000, n.update=5000, thin=10, n.iter=20000)
stopCluster(cl)    # close out the cluster.
proc.time() - ptm

#save(b.a.hightemp.mod, file = "b.a.hightemp_mod.Rdata")




####----------------------------- Breeding Precipitation Model --------------------------- ####

b.precip.function <- function(){
  
  ## Prior distributions on parameters
  b.0 ~ dnorm(0,0.001)
  b.relative_niche_precip ~ dnorm(0,0.001)        # Is the site in the warm part of the thermal niche?
  b.year1 ~ dnorm(0,0.001)                        # What was the temperature of that year?

  b.niche_year1 ~ dnorm(0,0.001)                  # Interaction between actual temperature and thermal niche

  tau ~ dgamma(0.001,0.001)
  
  tau.species ~ dgamma(0.001, 0.001)  # could change to simag~unif()
  sigma.species <- 1/sqrt(tau.species)
  
  ## Process model
  for(i in 1:n.obs){
    mu[i] <- b.0 + b.relative_niche_precip*relative_niche_precip[i] +                       # effect of relative niche precip
      b.year1*year1[i] +                                                       # effect of year 1 precip
      b.niche_year1*relative_niche_precip[i]*year1[i] +                        # effect of niche and year 1 precip interaction
      b.species[species[i]]                                                    # random intercept
    
    y[i] ~ dlnorm(mu[i], tau)                                                               # log normal response
  }
  
  ## Random intercept for each species 
  for(j in 1:n.species){
    b.species[j] ~ dnorm(0, tau.species)
  }
}


## Set data for the model
jags.list.b.precip <- list(n.obs=nrow(birds), 
                         n.species=length(unique(birds$aou)),
                         relative_niche_precip = birds$breeding_mean_precip_zscore,
                         year1 = birds$breeding_avg_precip_year_1_scaled,
                         species = birds$aou,
                         y = birds$r_star)


params.save <- c("b.0", "b.year1", "b.relative_niche_precip", "b.niche_year1", "sigma.species")


ptm <- proc.time()
cl <- makePSOCKcluster(3)   # Make your socket cluster and name it 'cl'.  nc is the number of cores you want your cluster to have--generally the number of chains you want to run.
tmp <- clusterEvalQ(cl, library(dclone))  # Check that `dclone` is loaded on each of cl's workers. dclone includes JAGS functionality
parLoadModule(cl, "glm")  # load the JAGS module 'glm' on each worker
parListModules(cl)  # make sure previous line worked.
## Run the model here:
b.precip.mod <- jags.parfit(cl=cl, data=jags.list.b.precip, 
                          params=params.save, model=b.precip.function, 
                          n.chains=3, n.adapt=1000, n.update=5000, thin=10, n.iter=20000)


stopCluster(cl)    # close out the cluster.
proc.time() - ptm

#save(b.precip.mod, file = "b.precip_mod.Rdata")



####----------------------------- Full Breeding Precipitation Model --------------------------- ####

b.a.precip.function <- function(){
  
  ## Prior distributions on parameters
  b.0 ~ dnorm(0,0.001)
  b.relative_niche_precip ~ dnorm(0,0.001)        # Is the site in the wet part of the thermal niche?
  b.relative_year1 ~ dnorm(0,0.001)               # Was this site particularly wet relative to the mean of this site?
  
  b.niche_relative_year1 ~ dnorm(0,0.001)         # Interaction between relative temperature and thermal niche
  
  tau ~ dgamma(0.001,0.001)
  
  tau.species ~ dgamma(0.001, 0.001)  # could change to simag~unif()
  sigma.species <- 1/sqrt(tau.species)
  
  ## Process model
  for(i in 1:n.obs){
    mu[i] <- b.0 + b.relative_niche_precip*relative_niche_precip[i] +                       # effect of relative niche precip
      b.relative_year1*relative_year1[i] +                                     # effect of relative year 1 precip
      b.niche_relative_year1*relative_niche_precip[i]*relative_year1[i] +      # effect of nich and relative year 1 precip interaction
      b.species[species[i]]                                                    # random intercept
    
    y[i] ~ dlnorm(mu[i], tau)                                                               # log normal response
  }
  
  ## Random intercept for each species 
  for(j in 1:n.species){
    b.species[j] ~ dnorm(0, tau.species)
  }
}


## Set data for the model
jags.list.b.a.precip <- list(n.obs=nrow(birds), 
                         n.species=length(unique(birds$aou)),
                         relative_niche_precip = birds$breeding_mean_precip_zscore,
                         relative_year1 = birds$breeding_avg_precip_year_1_zscore,
                         species = birds$aou,
                         y = birds$r_star)


params.save <- c("b.0", "b.relative_niche_precip", " b.relative_year1",  "b.niche_relative_year1",  "sigma.species")


ptm <- proc.time()
cl <- makePSOCKcluster(3)   # Make your socket cluster and name it 'cl'.  nc is the number of cores you want your cluster to have--generally the number of chains you want to run.
tmp <- clusterEvalQ(cl, library(dclone))  # Check that `dclone` is loaded on each of cl's workers. dclone includes JAGS functionality
parLoadModule(cl, "glm")  # load the JAGS module 'glm' on each worker
parListModules(cl)  # make sure previous line worked.
## Run the model here:
b.a.precip.mod <- jags.parfit(cl=cl, data=jags.list.b.a.precip, 
                          params=params.save, model=b.a.precip.function, 
                          n.chains=3, n.adapt=1000, n.update=5000, thin=10, n.iter=20000)


stopCluster(cl)    # close out the cluster.
proc.time() - ptm

#save(b.a.precip.mod, file = "b.a.precip_mod.Rdata")

#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################



####----------------------------- Full July/August High Temp Model ----------------------------- ####

ja.hightemp.function <- function(){
  
  ## Prior distributions on parameters
  b.0 ~ dnorm(0,0.001)
  b.relative_niche_high ~ dnorm(0,0.001)       # Is the site in the warm part of the thermal niche?
  b.year1 ~ dnorm(0,0.001)                        # What was the temperature of that year?

  b.niche_year1 ~ dnorm(0,0.001)                # Interaction between actual temperature and thermal niche

  tau ~ dgamma(0.001,0.001)
  
  tau.species ~ dgamma(0.001, 0.001)  # could change to simag~unif()
  sigma.species <- 1/sqrt(tau.species)
  
  ## Process model
  for(i in 1:n.obs){
    mu[i] <- b.0 + b.relative_niche_high*relative_niche_high[i] +                     # effect of relative niche temp
      b.year1*year1[i] +                                                       # effect of year 1 temp
      b.niche_year1*relative_niche_high[i]*year1[i] +                        # effect of niche and year 1 temp interaction
      b.species[species[i]]                                                    # random intercept
    
    y[i] ~ dlnorm(mu[i], tau)                                                               # log normal response
  }
  
  ## Random intercept for each species 
  for(j in 1:n.species){
    b.species[j] ~ dnorm(0, tau.species)
  }
}


## Set data for the model
jags.list.ja.hightemp <- list(n.obs=nrow(birds), 
                              n.species=length(unique(birds$aou)),
                              relative_niche_high = birds$mean_high_jul_aug_zscore,
                              year1 = birds$monthly_avg_high_13_year_1_scaled,
                              species = birds$aou,
                              y = birds$r_star)


params.save <- c("b.0", "b.year1", "b.relative_niche_high",  "b.niche_year1",  "sigma.species")

ptm <- proc.time()



cl <- makePSOCKcluster(3)   # Make your socket cluster and name it 'cl'.  nc is the number of cores you want your cluster to have--generally the number of chains you want to run.
tmp <- clusterEvalQ(cl, library(dclone))  # Check that `dclone` is loaded on each of cl's workers. dclone includes JAGS functionality
parLoadModule(cl, "glm")  # load the JAGS module 'glm' on each worker
parListModules(cl)  # make sure previous line worked.
## Run the model here:
ja.hightemp.mod <- jags.parfit(cl=cl, data=jags.list.ja.hightemp, 
                               params=params.save, model=ja.hightemp.function, 
                               n.chains=3, n.adapt=1000, n.update=5000, thin=10, n.iter=20000)
stopCluster(cl)    # close out the cluster.
proc.time() - ptm

#save(ja.hightemp.mod, file = "ja.hightemp_mod.Rdata")




####----------------------------- Full July/August High Temp Model ----------------------------- ####

ja.a.hightemp.function <- function(){
  
  ## Prior distributions on parameters
  b.0 ~ dnorm(0,0.001)
  b.relative_niche_high ~ dnorm(0,0.001)       # Is the site in the warm part of the thermal niche?
  b.relative_year1 ~ dnorm(0,0.001)               # Was this site particularly warm relative to the mean of this site?
  
  b.niche_relative_year1 ~ dnorm(0,0.001)         # Interaction between relative temperature and thermal niche
  
  tau ~ dgamma(0.001,0.001)
  
  tau.species ~ dgamma(0.001, 0.001)  # could change to simag~unif()
  sigma.species <- 1/sqrt(tau.species)
  
  ## Process model
  for(i in 1:n.obs){
    mu[i] <- b.0 + b.relative_niche_high*relative_niche_high[i] +                     # effect of relative niche temp
      b.relative_year1*relative_year1[i] +                                     # effect of relative year 1 temp
      b.niche_relative_year1*relative_niche_high[i]*relative_year1[i] +     # effect of nich and relative year 1 temp interaction
      b.species[species[i]]                                                    # random intercept
    
    y[i] ~ dlnorm(mu[i], tau)                                                               # log normal response
  }
  
  ## Random intercept for each species 
  for(j in 1:n.species){
    b.species[j] ~ dnorm(0, tau.species)
  }
}


## Set data for the model
jags.list.ja.a.hightemp <- list(n.obs=nrow(birds), 
                              n.species=length(unique(birds$aou)),
                              relative_niche_high = birds$mean_high_jul_aug_zscore,
                              relative_year1 = birds$monthly_avg_high_13_year_1_zscore,
                              species = birds$aou,
                              y = birds$r_star)


params.save <- c("b.0",  "b.relative_niche_high", "b.relative_year1", "b.niche_relative_year1", "sigma.species")

ptm <- proc.time()



cl <- makePSOCKcluster(3)   # Make your socket cluster and name it 'cl'.  nc is the number of cores you want your cluster to have--generally the number of chains you want to run.
tmp <- clusterEvalQ(cl, library(dclone))  # Check that `dclone` is loaded on each of cl's workers. dclone includes JAGS functionality
parLoadModule(cl, "glm")  # load the JAGS module 'glm' on each worker
parListModules(cl)  # make sure previous line worked.
## Run the model here:
ja.a.hightemp.mod <- jags.parfit(cl=cl, data=jags.list.ja.a.hightemp, 
                               params=params.save, model=ja.a.hightemp.function, 
                               n.chains=3, n.adapt=1000, n.update=5000, thin=10, n.iter=20000)
stopCluster(cl)    # close out the cluster.
proc.time() - ptm

save(ja.a.hightemp.mod, file = "ja.a.hightemp_mod.Rdata")



####----------------------------- Full July/August Precipitation Model --------------------------- ####

ja.precip.function <- function(){
  
  ## Prior distributions on parameters
  b.0 ~ dnorm(0,0.001)
  b.relative_niche_precip ~ dnorm(0,0.001)        # Is the site in the warm part of the thermal niche?
  b.year1 ~ dnorm(0,0.001)                        # What was the temperature of that year?

  b.niche_year1 ~ dnorm(0,0.001)                  # Interaction between actual temperature and thermal niche

  tau ~ dgamma(0.001,0.001)
  
  tau.species ~ dgamma(0.001, 0.001)  # could change to simag~unif()
  sigma.species <- 1/sqrt(tau.species)
  
  ## Process model
  for(i in 1:n.obs){
    mu[i] <- b.0 + b.relative_niche_precip*relative_niche_precip[i] +                       # effect of relative niche precip
      b.year1*year1[i] +                                                       # effect of year 1 precip
      b.niche_year1*relative_niche_precip[i]*year1[i] +                        # effect of niche and year 1 precip interaction
      b.species[species[i]]                                                    # random intercept
    
    y[i] ~ dlnorm(mu[i], tau)                                                               # log normal response
  }
  
  ## Random intercept for each species 
  for(j in 1:n.species){
    b.species[j] ~ dnorm(0, tau.species)
  }
}


## Set data for the model
jags.list.ja.precip <- list(n.obs=nrow(birds), 
                            n.species=length(unique(birds$aou)),
                            relative_niche_precip = birds$mean_precip_jul_aug_zscore,
                            year1 = birds$monthly_avg_precip_13_year_1_scaled,
                            species = birds$aou,
                            y = birds$r_star)


params.save <- c("b.0", "b.year1", "b.relative_niche_precip", "b.niche_year1", "sigma.species")


ptm <- proc.time()
cl <- makePSOCKcluster(3)   # Make your socket cluster and name it 'cl'.  nc is the number of cores you want your cluster to have--generally the number of chains you want to run.
tmp <- clusterEvalQ(cl, library(dclone))  # Check that `dclone` is loaded on each of cl's workers. dclone includes JAGS functionality
parLoadModule(cl, "glm")  # load the JAGS module 'glm' on each worker
parListModules(cl)  # make sure previous line worked.
## Run the model here:
ja.precip.mod <- jags.parfit(cl=cl, data=jags.list.ja.precip, 
                             params=params.save, model=ja.precip.function, 
                             n.chains=3, n.adapt=1000, n.update=5000, thin=10, n.iter=20000)


stopCluster(cl)    # close out the cluster.
proc.time() - ptm

save(ja.precip.mod, file = "ja.precip_mod.Rdata")




####----------------------------- Full July/August Precipitation Model --------------------------- ####

ja.a.precip.function <- function(){
  
  ## Prior distributions on parameters
  b.0 ~ dnorm(0,0.001)
  b.relative_niche_precip ~ dnorm(0,0.001)        # Is the site in the warm part of the thermal niche?
  b.relative_year1 ~ dnorm(0,0.001)               # Was this site particularly warm relative to the mean of this site?
  
  b.niche_relative_year1 ~ dnorm(0,0.001)         # Interaction between relative temperature and thermal niche
  
  tau ~ dgamma(0.001,0.001)
  
  tau.species ~ dgamma(0.001, 0.001)  # could change to simag~unif()
  sigma.species <- 1/sqrt(tau.species)
  
  ## Process model
  for(i in 1:n.obs){
    mu[i] <- b.0 + b.relative_niche_precip*relative_niche_precip[i] +                       # effect of relative niche precip
      b.relative_year1*relative_year1[i] +                                     # effect of relative year 1 precip
      b.niche_relative_year1*relative_niche_precip[i]*relative_year1[i] +      # effect of nich and relative year 1 precip interaction
      b.species[species[i]]                                                    # random intercept
    
    y[i] ~ dlnorm(mu[i], tau)                                                               # log normal response
  }
  
  ## Random intercept for each species 
  for(j in 1:n.species){
    b.species[j] ~ dnorm(0, tau.species)
  }
}


## Set data for the model
jags.list.ja.a.precip <- list(n.obs=nrow(birds), 
                            n.species=length(unique(birds$aou)),
                            relative_niche_precip = birds$mean_precip_jul_aug_zscore,
                            relative_year1 = birds$monthly_avg_precip_13_year_1_zscore,
                            species = birds$aou,
                            y = birds$r_star)


params.save <- c("b.0","b.relative_niche_precip", " b.relative_year1", "b.niche_relative_year1", "sigma.species")


ptm <- proc.time()
cl <- makePSOCKcluster(3)   # Make your socket cluster and name it 'cl'.  nc is the number of cores you want your cluster to have--generally the number of chains you want to run.
tmp <- clusterEvalQ(cl, library(dclone))  # Check that `dclone` is loaded on each of cl's workers. dclone includes JAGS functionality
parLoadModule(cl, "glm")  # load the JAGS module 'glm' on each worker
parListModules(cl)  # make sure previous line worked.
## Run the model here:
ja.a.precip.mod <- jags.parfit(cl=cl, data=jags.list.ja.a.precip, 
                             params=params.save, model=ja.a.precip.function, 
                             n.chains=3, n.adapt=1000, n.update=5000, thin=10, n.iter=20000)


stopCluster(cl)    # close out the cluster.
proc.time() - ptm

#save(ja.a.precip.mod, file = "ja.a.precip_mod.Rdata")
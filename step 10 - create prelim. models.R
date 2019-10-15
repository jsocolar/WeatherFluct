#### -------------------------------------------------------------------------------- ####
#### Updated Bayesian multi-species models
#### -------------------------------------------------------------------------------- ####

# Created: Oct. 9 by Austin and Andrew


library(R2jags)
library(dclone)      # built-in functionality for parallel MCMC with jags via jags.parfit()
library(MCMCvis)

#setwd("~/Dropbox/____Projects____/BBS_Project") # Andrew
#setwd("C:/Users/Tingley Lab_2/Dropbox/BBS_Project") #Tingley lab computer
setwd("./Dropbox/BBS_Project")

## Load and organize data
load("passeriformes.Rdata")

## Change the name 
birds <- passeriformes

## Scale appropriate data

## Scale raw temperature and precipitation values
# Breeding
birds$breeding_avg_high_year_1_scaled <- (birds$breeding_avg_high_year_1 - mean(birds$breeding_avg_high_year_1))/sd(birds$breeding_avg_high_year_1)
birds$breeding_avg_precip_year_1_scaled <- (birds$breeding_avg_precip_year_1 - mean(birds$breeding_avg_precip_year_1))/sd(birds$breeding_avg_precip_year_1)

# Post-breeding
birds$post_breeding_avg_high_year_1_scaled <- (birds$post_breeding_avg_high_year_1 - mean(birds$post_breeding_avg_high_year_1))/sd(birds$post_breeding_avg_high_year_1)
birds$post_breeding_avg_precip_year_1_scaled <- (birds$post_breeding_avg_precip_year_1 - mean(birds$post_breeding_avg_precip_year_1))/sd(birds$post_breeding_avg_precip_year_1)

# July/August
birds$monthly_avg_high_13_year_1_scaled <- birds$monthly_avg_high_13_year_1 - mean(birds$monthly_avg_high_13_year_1)/sd(birds$monthly_avg_high_13_year_1)
birds$monthly_avg_precip_13_year_1_scaled <- birds$monthly_avg_precip_13_year_1 - mean(birds$monthly_avg_precip_13_year_1)/sd(birds$monthly_avg_precip_13_year_1)


#################################################################################################################
#################################################################################################################
#### 12 models below ####

## Breeding weather ##
# Breeding raw high
# Breeding relative high
# Breeding raw precip
# Breeding relative precip

## Post-breeding weather ##
# Post-breeding raw high
# Post-breeding relative high
# Post-breeding raw precip
# Post-breeding relative precip

## July/August weather ##
# July/August raw high
# July/August relative high
# July/August raw precip
# July/August relative precip

#### abbreviations for below
# b = breeding (raw values)
# b.a = breeding anomaly 

# pb = post-breeding (raw values)
# pb.a = post-breeding anomaly

# ja = july/august (raw values)
# ja.a = july/august anomaly

#################################################################################################################
####--------------------------------- Breeding High Temp Model ----------------------------- ####
#################################################################################################################
b.hightemp.function <- function(){
  
  ## Prior distributions on parameters
  b.0 ~ dnorm(0,0.001)
  b.niche ~ dnorm(0,0.001)                      # Is the site in the warm part of the thermal niche?
  b.year1 ~ dnorm(0,0.001)                      # What was the temperature of that year?
  b.year1.niche ~ dnorm(0,0.001)                  # Interaction between actual temperature and thermal niche along the cold niche space
  
  tau ~ dgamma(0.001,0.001)
  
  tau.species ~ dgamma(0.001, 0.001)            # could change to simag~unif()
  sigma.species <- 1/sqrt(tau.species)
  
  ## Process model
  for(i in 1:n.obs){
    mu[i] <- b.0 + 
      niche[i] * b.niche  +
      year1[i] * b.year1 +
      year1[i] * niche[i] * b.year1.niche + 
      b.species[species[i]]  
    
    y[i] ~ dlnorm(mu[i], tau)       # log normal response
  }
  
  ## Random intercept for each species 
  for(j in 1:n.species){
    b.species[j] ~ dnorm(0, tau.species)
  }
}


## Set data for the model
b.hightemp.jags.list <- list(n.obs=nrow(birds), 
                             n.species=length(unique(birds$aou)),
                             niche = birds$breeding_mean_high_zscore,
                             year1 = birds$breeding_avg_high_year_1_scaled,
                             species = birds$aou,
                             y = birds$r_star)


params.save <- c("b.0", "b.year1", "b.niche", "b.year1.niche", "sigma.species")

ptm <- proc.time()



cl <- makePSOCKcluster(3)                     # Make your socket cluster and name it 'cl'.  nc is the number of cores you want your cluster to have--generally the number of chains you want to run.
tmp <- clusterEvalQ(cl, library(dclone))      # Check that `dclone` is loaded on each of cl's workers. dclone includes JAGS functionality
parLoadModule(cl, "glm")                      # load the JAGS module 'glm' on each worker
parListModules(cl)                            # make sure previous line worked.

## Run the model here:
b.hightemp.mod <- jags.parfit(cl=cl, data=b.hightemp.jags.list, 
                              params=params.save, model=b.hightemp.function, 
                              n.chains=3, n.adapt=500, n.update=2500, thin=10, n.iter=10000)

stopCluster(cl)                               # close out the cluster.
proc.time() - ptm

#save(b.hightemp.mod, file = "./new_model_framework/b.hightemp_mod.Rdata")




#################################################################################################################
####--------------------------------- Breeding Anomalous High Temp Model ----------------------------- ####
#################################################################################################################

b.a.hightemp.function <- function(){
  
  ## Prior distributions on parameters
  b.0 ~ dnorm(0,0.001)
  b.niche ~ dnorm(0,0.001)                        # Is the site in the warm part of the thermal niche?
  b.relative.year1 ~ dnorm(0,0.001)               # Was that year hot for the site?
  b.relative.year1.niche ~ dnorm(0,0.001)           # Interaction between actual temperature and thermal niche along the cold niche space
  
  tau ~ dgamma(0.001,0.001)
  
  tau.species ~ dgamma(0.001, 0.001)              # could change to simag~unif()
  sigma.species <- 1/sqrt(tau.species)
  
  ## Process model
  for(i in 1:n.obs){
    mu[i] <- b.0 + 
      niche[i] * b.niche  +
      relative.year1[i] * b.relative.year1 +
      relative.year1[i] * niche[i] * b.relative.year1.niche +
      b.species[species[i]]  
    
    y[i] ~ dlnorm(mu[i], tau)       # log normal response
  }
  
  ## Random intercept for each species 
  for(j in 1:n.species){
    b.species[j] ~ dnorm(0, tau.species)
  }
}


## Set data for the model
b.a.hightemp.jags.list <- list(n.obs=nrow(birds), 
                               n.species=length(unique(birds$aou)),
                               niche = birds$breeding_mean_high_zscore,
                               relative.year1 = birds$breeding_avg_high_year_1_zscore,
                               species = birds$aou,
                               y = birds$r_star)


params.save <- c("b.0", "b.relative.year1", "b.niche", "b.relative.year1.niche", "sigma.species")

ptm <- proc.time()



cl <- makePSOCKcluster(3)                     # Make your socket cluster and name it 'cl'.  nc is the number of cores you want your cluster to have--generally the number of chains you want to run.
tmp <- clusterEvalQ(cl, library(dclone))      # Check that `dclone` is loaded on each of cl's workers. dclone includes JAGS functionality
parLoadModule(cl, "glm")                      # load the JAGS module 'glm' on each worker
parListModules(cl)                            # make sure previous line worked.

## Run the model here:
b.a.hightemp.mod <- jags.parfit(cl=cl, data=b.a.hightemp.jags.list, 
                                params=params.save, model=b.a.hightemp.function, 
                                n.chains=3, n.adapt=500, n.update=2500, thin=10, n.iter=10000)

stopCluster(cl)                               # close out the cluster.
proc.time() - ptm

#save(b.a.hightemp.mod, file = "./new_model_framework/b.a.hightemp_mod.Rdata")


#################################################################################################################
####--------------------------------- Breeding Precipitation Model ----------------------------- ####
#################################################################################################################
b.precip.function <- function(){
  
  ## Prior distributions on parameters
  b.0 ~ dnorm(0,0.001)
  b.niche ~ dnorm(0,0.001)                      # Is the site in the wet part of the precip niche?
  b.year1 ~ dnorm(0,0.001)                      # What was the precipitation of that year?
  b.year1.niche ~ dnorm(0, 0.001)                 # Added interaction between precip and precip niche along the wet niche space
  
  tau ~ dgamma(0.001,0.001)
  
  tau.species ~ dgamma(0.001, 0.001)            # could change to simag~unif()
  sigma.species <- 1/sqrt(tau.species)
  
  ## Process model
  for(i in 1:n.obs){
    mu[i] <- b.0 + 
      niche[i] * b.niche  +
      year1[i] * b.year1 +
      year1[i] * niche[i] * b.year1.niche + 
      b.species[species[i]]  
    
    y[i] ~ dlnorm(mu[i], tau)       # log normal response
  }
  
  ## Random intercept for each species 
  for(j in 1:n.species){
    b.species[j] ~ dnorm(0, tau.species)
  }
}


## Set data for the model
b.precip.jags.list <- list(n.obs=nrow(birds), 
                           n.species=length(unique(birds$aou)),
                           niche = birds$breeding_mean_precip_zscore,
                           year1 = birds$breeding_avg_precip_year_1_scaled,
                           species = birds$aou,
                           y = birds$r_star)


params.save <- c("b.0", "b.year1", "b.niche", "b.year1.niche", "sigma.species")

ptm <- proc.time()



cl <- makePSOCKcluster(3)                     # Make your socket cluster and name it 'cl'.  nc is the number of cores you want your cluster to have--generally the number of chains you want to run.
tmp <- clusterEvalQ(cl, library(dclone))      # Check that `dclone` is loaded on each of cl's workers. dclone includes JAGS functionality
parLoadModule(cl, "glm")                      # load the JAGS module 'glm' on each worker
parListModules(cl)                            # make sure previous line worked.

## Run the model here:
b.precip.mod <- jags.parfit(cl=cl, data=b.precip.jags.list, 
                            params=params.save, model=b.precip.function, 
                            n.chains=3, n.adapt=500, n.update=2500, thin=10, n.iter=10000)

stopCluster(cl)                               # close out the cluster.
proc.time() - ptm

#save(b.precip.mod, file = "./new_model_framework/b.precip_mod.Rdata")




#################################################################################################################
####--------------------------------- Breeding Anomalous Precip Model ----------------------------- ####
#################################################################################################################

b.a.precip.function <- function(){
  
  ## Prior distributions on parameters
  b.0 ~ dnorm(0,0.001)
  b.niche ~ dnorm(0,0.001)                                # Is the site in the wet part of the precip niche?
  b.relative.year1 ~ dnorm(0,0.001)                       # Was that year wet for the site?
  b.relative.year1.niche ~ dnorm(0, 0.001)                  # Interaction of precip niche and precipitation
  
  tau ~ dgamma(0.001,0.001)
  
  tau.species ~ dgamma(0.001, 0.001)                      # could change to simag~unif()
  sigma.species <- 1/sqrt(tau.species)
  
  ## Process model
  for(i in 1:n.obs){
    mu[i] <- b.0 + 
      niche[i] * b.niche  +
      relative.year1[i] * b.relative.year1 +
      relative.year1[i] * niche[i] * b.relative.year1.niche + 
      b.species[species[i]]  
    
    y[i] ~ dlnorm(mu[i], tau)       # log normal response
  }
  
  ## Random intercept for each species 
  for(j in 1:n.species){
    b.species[j] ~ dnorm(0, tau.species)
  }
}


## Set data for the model
b.a.precip.jags.list <- list(n.obs=nrow(birds), 
                             n.species=length(unique(birds$aou)),
                             niche = birds$breeding_mean_precip_zscore,
                             relative.year1 = birds$breeding_avg_precip_year_1_zscore,
                             species = birds$aou,
                             y = birds$r_star)


params.save <- c("b.0", "b.relative.year1", "b.niche", "b.relative.year1.niche", "sigma.species")

ptm <- proc.time()



cl <- makePSOCKcluster(3)                     # Make your socket cluster and name it 'cl'.  nc is the number of cores you want your cluster to have--generally the number of chains you want to run.
tmp <- clusterEvalQ(cl, library(dclone))      # Check that `dclone` is loaded on each of cl's workers. dclone includes JAGS functionality
parLoadModule(cl, "glm")                      # load the JAGS module 'glm' on each worker
parListModules(cl)                            # make sure previous line worked.

## Run the model here:
b.a.precip.mod <- jags.parfit(cl=cl, data=b.a.precip.jags.list, 
                              params=params.save, model=b.a.precip.function, 
                              n.chains=3, n.adapt=500, n.update=2500, thin=10, n.iter=10000)

stopCluster(cl)                               # close out the cluster.
proc.time() - ptm

#save(b.a.precip.mod, file = "./new_model_framework/b.a.precip_mod.Rdata")





#################################################################################################################
####--------------------------------- Post-breeding High Temp Model ----------------------------- ####
#################################################################################################################
pb.hightemp.function <- function(){
  
  ## Prior distributions on parameters
  b.0 ~ dnorm(0,0.001)
  b.niche ~ dnorm(0,0.001)                        # Is the site in the warm part of the thermal niche?
  b.year1 ~ dnorm(0,0.001)                        # What was the temperature of that year?
  b.year1.niche ~ dnorm(0,0.001)                  # Interaction between actual temperature and thermal niche
  
  tau ~ dgamma(0.001,0.001)
  
  tau.species ~ dgamma(0.001, 0.001)              # could change to simag~unif()
  sigma.species <- 1/sqrt(tau.species)
  
  ## Process model
  for(i in 1:n.obs){
    mu[i] <- b.0 + 
      niche[i] * b.niche  +
      year1[i] * b.year1 +
      year1[i] * niche[i] * b.year1.niche + 
      b.species[species[i]]  
    
    y[i] ~ dlnorm(mu[i], tau)       # log normal response
  }
  
  ## Random intercept for each species 
  for(j in 1:n.species){
    b.species[j] ~ dnorm(0, tau.species)
  }
}


## Set data for the model
pb.hightemp.jags.list <- list(n.obs=nrow(birds), 
                              n.species=length(unique(birds$aou)),
                              niche = birds$post_breeding_mean_high_zscore,
                              year1 = birds$post_breeding_avg_high_year_1_scaled,
                              species = birds$aou,
                              y = birds$r_star)


params.save <- c("b.0", "b.year1", "b.niche", "b.year1.niche", "sigma.species")

ptm <- proc.time()



cl <- makePSOCKcluster(3)                     # Make your socket cluster and name it 'cl'.  nc is the number of cores you want your cluster to have--generally the number of chains you want to run.
tmp <- clusterEvalQ(cl, library(dclone))      # Check that `dclone` is loaded on each of cl's workers. dclone includes JAGS functionality
parLoadModule(cl, "glm")                      # load the JAGS module 'glm' on each worker
parListModules(cl)                            # make sure previous line worked.

## Run the model here:
pb.hightemp.mod <- jags.parfit(cl=cl, data=pb.hightemp.jags.list, 
                               params=params.save, model=pb.hightemp.function, 
                               n.chains=3, n.adapt=500, n.update=2500, thin=10, n.iter=10000)

stopCluster(cl)                               # close out the cluster.
proc.time() - ptm

save(pb.hightemp.mod, file = "./new_model_framework/pb.hightemp_mod.Rdata")




#################################################################################################################
####--------------------------------- Post-breeding Anomalous High Temp Model ----------------------------- ####
#################################################################################################################

pb.a.hightemp.function <- function(){
  
  ## Prior distributions on parameters
  b.0 ~ dnorm(0,0.001)
  b.niche ~ dnorm(0,0.001)                              # Is the site in the warm part of the thermal niche?
  b.relative.year1 ~ dnorm(0,0.001)                     # Was that year hot for the site?
  b.relative.year1.niche ~ dnorm(0,0.001)                 # Interaction between actual temperature and thermal niche
  
  tau ~ dgamma(0.001,0.001)
  
  tau.species ~ dgamma(0.001, 0.001)                    # could change to simag~unif()
  sigma.species <- 1/sqrt(tau.species)
  
  ## Process model
  for(i in 1:n.obs){
    mu[i] <- b.0 + 
      niche[i] * b.niche  +
      relative.year1[i] * b.relative.year1 +
      relative.year1[i] * niche[i] * b.relative.year1.niche + 
      b.species[species[i]]  
    
    y[i] ~ dlnorm(mu[i], tau)       # log normal response
  }
  
  ## Random intercept for each species 
  for(j in 1:n.species){
    b.species[j] ~ dnorm(0, tau.species)
  }
}


## Set data for the model
pb.a.hightemp.jags.list <- list(n.obs=nrow(birds), 
                                n.species=length(unique(birds$aou)),
                                niche = birds$post_breeding_mean_high_zscore,
                                relative.year1 = birds$post_breeding_avg_high_year_1_zscore,
                                species = birds$aou,
                                y = birds$r_star)


params.save <- c("b.0", "b.relative.year1", "b.niche", "b.relative.year1.niche", "sigma.species")

ptm <- proc.time()



cl <- makePSOCKcluster(3)                     # Make your socket cluster and name it 'cl'.  nc is the number of cores you want your cluster to have--generally the number of chains you want to run.
tmp <- clusterEvalQ(cl, library(dclone))      # Check that `dclone` is loaded on each of cl's workers. dclone includes JAGS functionality
parLoadModule(cl, "glm")                      # load the JAGS module 'glm' on each worker
parListModules(cl)                            # make sure previous line worked.

## Run the model here:
pb.a.hightemp.mod <- jags.parfit(cl=cl, data=pb.a.hightemp.jags.list, 
                                 params=params.save, model=pb.a.hightemp.function, 
                                 n.chains=3, n.adapt=500, n.update=2500, thin=10, n.iter=10000)

stopCluster(cl)                               # close out the cluster.
proc.time() - ptm

save(pb.a.hightemp.mod, file = "./new_model_framework/pb.a.hightemp_mod.Rdata")


#################################################################################################################
####--------------------------------- Post-breeding Precipitation Model ----------------------------- ####
#################################################################################################################
pb.precip.function <- function(){
  
  ## Prior distributions on parameters
  b.0 ~ dnorm(0,0.001)
  b.niche ~ dnorm(0,0.001)                        # Is the site in the wet part of the precip niche?
  b.year1 ~ dnorm(0,0.001)                        # What was the precipitation of that year?
  b.year1.niche ~ dnorm(0, 0.001)                   # Interaction between precip and niche
  
  tau ~ dgamma(0.001,0.001)
  
  tau.species ~ dgamma(0.001, 0.001)              # could change to simag~unif()
  sigma.species <- 1/sqrt(tau.species)
  
  ## Process model
  for(i in 1:n.obs){
    mu[i] <- b.0 + 
      niche[i] * b.niche  +
      year1[i] * b.year1 +
      year1[i] * niche[i] * b.year1.niche + 
      b.species[species[i]]  
    
    y[i] ~ dlnorm(mu[i], tau)       # log normal response
  }
  
  ## Random intercept for each species 
  for(j in 1:n.species){
    b.species[j] ~ dnorm(0, tau.species)
  }
}


## Set data for the model
pb.precip.jags.list <- list(n.obs=nrow(birds), 
                            n.species=length(unique(birds$aou)),
                            niche = birds$post_breeding_mean_precip_zscore,
                            year1 = birds$post_breeding_avg_precip_year_1_scaled,
                            species = birds$aou,
                            y = birds$r_star)


params.save <- c("b.0", "b.year1", "b.niche", "b.year1.niche", "sigma.species")

ptm <- proc.time()



cl <- makePSOCKcluster(3)                     # Make your socket cluster and name it 'cl'.  nc is the number of cores you want your cluster to have--generally the number of chains you want to run.
tmp <- clusterEvalQ(cl, library(dclone))      # Check that `dclone` is loaded on each of cl's workers. dclone includes JAGS functionality
parLoadModule(cl, "glm")                      # load the JAGS module 'glm' on each worker
parListModules(cl)                            # make sure previous line worked.

## Run the model here:
pb.precip.mod <- jags.parfit(cl=cl, data=pb.precip.jags.list, 
                             params=params.save, model=pb.precip.function, 
                             n.chains=3, n.adapt=500, n.update=2500, thin=10, n.iter=10000)

stopCluster(cl)                               # close out the cluster.
proc.time() - ptm

save(pb.precip.mod, file = "./new_model_framework/pb.precip_mod.Rdata")




#################################################################################################################
####--------------------------------- Post-breeding Anomalous Precip Model ----------------------------- ####
#################################################################################################################

pb.a.precip.function <- function(){
  
  ## Prior distributions on parameters
  b.0 ~ dnorm(0,0.001)
  b.niche ~ dnorm(0,0.001)                                  # Is the site in the wet part of the precip niche?
  b.relative.year1 ~ dnorm(0,0.001)                         # Was that year wet for the site?
  b.relative.year1.niche ~ dnorm(0, 0.001)                    # Added interaction between preip and precip niche along the wet niche space
  
  tau ~ dgamma(0.001,0.001)
  
  tau.species ~ dgamma(0.001, 0.001)                        # could change to simag~unif()
  sigma.species <- 1/sqrt(tau.species)
  
  ## Process model
  for(i in 1:n.obs){
    mu[i] <- b.0 + 
      niche[i] * b.niche  +
      relative.year1[i] * b.relative.year1 +
      relative.year1[i] * niche[i] * b.relative.year1.niche +
      b.species[species[i]]  
    
    y[i] ~ dlnorm(mu[i], tau)       # log normal response
  }
  
  ## Random intercept for each species 
  for(j in 1:n.species){
    b.species[j] ~ dnorm(0, tau.species)
  }
}


## Set data for the model
pb.a.precip.jags.list <- list(n.obs=nrow(birds), 
                              n.species=length(unique(birds$aou)),
                              niche = birds$post_breeding_mean_precip_zscore,
                              relative.year1 = birds$post_breeding_avg_precip_year_1_zscore,
                              species = birds$aou,
                              y = birds$r_star)


params.save <- c("b.0", "b.relative.year1", "b.niche", "b.relative.year1.niche", "sigma.species")

ptm <- proc.time()



cl <- makePSOCKcluster(3)                     # Make your socket cluster and name it 'cl'.  nc is the number of cores you want your cluster to have--generally the number of chains you want to run.
tmp <- clusterEvalQ(cl, library(dclone))      # Check that `dclone` is loaded on each of cl's workers. dclone includes JAGS functionality
parLoadModule(cl, "glm")                      # load the JAGS module 'glm' on each worker
parListModules(cl)                            # make sure previous line worked.

## Run the model here:
pb.a.precip.mod <- jags.parfit(cl=cl, data=pb.a.precip.jags.list, 
                               params=params.save, model=pb.a.precip.function, 
                               n.chains=3, n.adapt=500, n.update=2500, thin=10, n.iter=10000)

stopCluster(cl)                               # close out the cluster.
proc.time() - ptm

save(pb.a.precip.mod, file = "./new_model_framework/pb.a.precip_mod.Rdata")




#################################################################################################################
####--------------------------------- July/August High Temp Model ----------------------------- ####
#################################################################################################################
ja.hightemp.function <- function(){
  
  ## Prior distributions on parameters
  b.0 ~ dnorm(0,0.001)
  b.niche ~ dnorm(0,0.001)                        # Is the site in the warm part of the thermal niche?
  b.year1 ~ dnorm(0,0.001)                        # What was the temperature of that year?
  b.year1.niche ~ dnorm(0,0.001)                    # Interaction between actual temperature and thermal niche 
  
  tau ~ dgamma(0.001,0.001)
  
  tau.species ~ dgamma(0.001, 0.001)              # could change to simag~unif()
  sigma.species <- 1/sqrt(tau.species)
  
  ## Process model
  for(i in 1:n.obs){
    mu[i] <- b.0 + 
      niche[i] * b.niche  +
      year1[i] * b.year1 +
      year1[i] * niche[i] * b.year1.niche + 
      b.species[species[i]]  
    
    y[i] ~ dlnorm(mu[i], tau)       # log normal response
  }
  
  ## Random intercept for each species 
  for(j in 1:n.species){
    b.species[j] ~ dnorm(0, tau.species)
  }
}


## Set data for the model
ja.hightemp.jags.list <- list(n.obs=nrow(birds), 
                              n.species=length(unique(birds$aou)),
                              niche = birds$mean_high_jul_aug_zscore,
                              year1 = birds$monthly_avg_high_13_year_1_scaled,
                              species = birds$aou,
                              y = birds$r_star)


params.save <- c("b.0", "b.year1", "b.niche", "b.year1.niche", "sigma.species")

ptm <- proc.time()



cl <- makePSOCKcluster(3)                     # Make your socket cluster and name it 'cl'.  nc is the number of cores you want your cluster to have--generally the number of chains you want to run.
tmp <- clusterEvalQ(cl, library(dclone))      # Check that `dclone` is loaded on each of cl's workers. dclone includes JAGS functionality
parLoadModule(cl, "glm")                      # load the JAGS module 'glm' on each worker
parListModules(cl)                            # make sure previous line worked.

## Run the model here:
ja.hightemp.mod <- jags.parfit(cl=cl, data=ja.hightemp.jags.list, 
                               params=params.save, model=ja.hightemp.function, 
                               n.chains=3, n.adapt=500, n.update=2500, thin=10, n.iter=10000)

stopCluster(cl)                               # close out the cluster.
proc.time() - ptm

save(ja.hightemp.mod, file = "./new_model_framework/ja.hightemp_mod.Rdata")




#################################################################################################################
####--------------------------------- July/August Anomalous High Temp Model ----------------------------- ####
#################################################################################################################

ja.a.hightemp.function <- function(){
  
  ## Prior distributions on parameters
  b.0 ~ dnorm(0,0.001)
  b.niche ~ dnorm(0,0.001)                                  # Is the site in the warm part of the thermal niche?
  b.relative.year1 ~ dnorm(0,0.001)                         # Was that year hot for the site?
  b.relative.year1.niche ~ dnorm(0,0.001)                     # Interaction between  temperature and thermal niche
  
  tau ~ dgamma(0.001,0.001)
  
  tau.species ~ dgamma(0.001, 0.001)              # could change to simag~unif()
  sigma.species <- 1/sqrt(tau.species)
  
  ## Process model
  for(i in 1:n.obs){
    mu[i] <- b.0 + 
      niche[i] * b.niche  +
      relative.year1[i] * b.relative.year1 +
      relative.year1[i] * niche[i] * b.relative.year1.niche + 
      b.species[species[i]]  
    
    y[i] ~ dlnorm(mu[i], tau)       # log normal response
  }
  
  ## Random intercept for each species 
  for(j in 1:n.species){
    b.species[j] ~ dnorm(0, tau.species)
  }
}


## Set data for the model
ja.a.hightemp.jags.list <- list(n.obs=nrow(birds), 
                                n.species=length(unique(birds$aou)),
                                niche = birds$mean_high_jul_aug_zscore,
                                relative.year1 = birds$monthly_avg_high_13_year_1_zscore,
                                species = birds$aou,
                                y = birds$r_star)


params.save <- c("b.0", "b.relative.year1", "b.niche", "b.relative.year1.niche", "sigma.species")

ptm <- proc.time()



cl <- makePSOCKcluster(3)                     # Make your socket cluster and name it 'cl'.  nc is the number of cores you want your cluster to have--generally the number of chains you want to run.
tmp <- clusterEvalQ(cl, library(dclone))      # Check that `dclone` is loaded on each of cl's workers. dclone includes JAGS functionality
parLoadModule(cl, "glm")                      # load the JAGS module 'glm' on each worker
parListModules(cl)                            # make sure previous line worked.

## Run the model here:
ja.a.hightemp.mod <- jags.parfit(cl=cl, data=ja.a.hightemp.jags.list, 
                                 params=params.save, model=ja.a.hightemp.function, 
                                 n.chains=3, n.adapt=500, n.update=2500, thin=10, n.iter=10000)

stopCluster(cl)                               # close out the cluster.
proc.time() - ptm

save(ja.a.hightemp.mod, file = "./new_model_framework/ja.a.hightemp_mod.Rdata")


#################################################################################################################
####--------------------------------- July/August Precipitation Model ----------------------------- ####
#################################################################################################################
ja.precip.function <- function(){
  
  ## Prior distributions on parameters
  b.0 ~ dnorm(0,0.001)
  b.niche ~ dnorm(0,0.001)                        # Is the site in the wet part of the precip niche?
  b.year1 ~ dnorm(0,0.001)                        # What was the precipitation of that year?
  b.year1.niche ~ dnorm(0, 0.001)                   # Added interaction between precip and precip niche 
  
  tau ~ dgamma(0.001,0.001)
  
  tau.species ~ dgamma(0.001, 0.001)              # could change to simag~unif()
  sigma.species <- 1/sqrt(tau.species)
  
  ## Process model
  for(i in 1:n.obs){
    mu[i] <- b.0 + 
      niche[i] * b.niche  +
      year1[i] * b.year1 +
      year1[i] * niche[i] * b.year1.niche + 
      b.species[species[i]]  
    
    y[i] ~ dlnorm(mu[i], tau)       # log normal response
  }
  
  ## Random intercept for each species 
  for(j in 1:n.species){
    b.species[j] ~ dnorm(0, tau.species)
  }
}


## Set data for the model
ja.precip.jags.list <- list(n.obs=nrow(birds), 
                            n.species=length(unique(birds$aou)),
                            niche = birds$mean_precip_jul_aug_zscore,
                            year1 = birds$monthly_avg_precip_13_year_1_scaled,
                            species = birds$aou,
                            y = birds$r_star)


params.save <- c("b.0", "b.year1", "b.niche", "b.year1.niche", "sigma.species")

ptm <- proc.time()



cl <- makePSOCKcluster(3)                     # Make your socket cluster and name it 'cl'.  nc is the number of cores you want your cluster to have--generally the number of chains you want to run.
tmp <- clusterEvalQ(cl, library(dclone))      # Check that `dclone` is loaded on each of cl's workers. dclone includes JAGS functionality
parLoadModule(cl, "glm")                      # load the JAGS module 'glm' on each worker
parListModules(cl)                            # make sure previous line worked.

## Run the model here:
ja.precip.mod <- jags.parfit(cl=cl, data=ja.precip.jags.list, 
                             params=params.save, model=ja.precip.function, 
                             n.chains=3, n.adapt=500, n.update=2500, thin=10, n.iter=10000)

stopCluster(cl)                               # close out the cluster.
proc.time() - ptm

save(ja.precip.mod, file = "./new_model_framework/ja.precip_mod.Rdata")




#################################################################################################################
####--------------------------------- July/August Anomalous Precip Model ----------------------------- ####
#################################################################################################################

ja.a.precip.function <- function(){
  
  ## Prior distributions on parameters
  b.0 ~ dnorm(0,0.001)
  b.niche ~ dnorm(0,0.001)                                  # Is the site in the wet part of the precip niche?
  b.relative.year1 ~ dnorm(0,0.001)                         # Was that year wet for the site?
  b.relative.year1.niche ~ dnorm(0, 0.001)                  # Added interaction between preip and precip niche along the wet niche space
  
  tau ~ dgamma(0.001,0.001)
  
  tau.species ~ dgamma(0.001, 0.001)              # could change to simag~unif()
  sigma.species <- 1/sqrt(tau.species)
  
  ## Process model
  for(i in 1:n.obs){
    mu[i] <- b.0 + 
      niche[i] * b.niche  +
      relative.year1[i] * b.relative.year1 +
      relative.year1[i] * niche[i] * b.relative.year1.niche + 
      b.species[species[i]]  
    
    y[i] ~ dlnorm(mu[i], tau)       # log normal response
  }
  
  ## Random intercept for each species 
  for(j in 1:n.species){
    b.species[j] ~ dnorm(0, tau.species)
  }
}


## Set data for the model
ja.a.precip.jags.list <- list(n.obs=nrow(birds), 
                              n.species=length(unique(birds$aou)),
                              niche = birds$mean_precip_jul_aug_zscore,
                              relative.year1 = birds$monthly_avg_precip_13_year_1_zscore,
                              species = birds$aou,
                              y = birds$r_star)


params.save <- c("b.0", "b.relative.year1", "b.niche", "b.relative.year1.niche", "sigma.species")

ptm <- proc.time()



cl <- makePSOCKcluster(3)                     # Make your socket cluster and name it 'cl'.  nc is the number of cores you want your cluster to have--generally the number of chains you want to run.
tmp <- clusterEvalQ(cl, library(dclone))      # Check that `dclone` is loaded on each of cl's workers. dclone includes JAGS functionality
parLoadModule(cl, "glm")                      # load the JAGS module 'glm' on each worker
parListModules(cl)                            # make sure previous line worked.

## Run the model here:
ja.a.precip.mod <- jags.parfit(cl=cl, data=ja.a.precip.jags.list, 
                               params=params.save, model=ja.a.precip.function, 
                               n.chains=3, n.adapt=500, n.update=2500, thin=10, n.iter=10000)

stopCluster(cl)                               # close out the cluster.
proc.time() - ptm

save(ja.a.precip.mod, file = "./new_model_framework/ja.a.precip_mod.Rdata")



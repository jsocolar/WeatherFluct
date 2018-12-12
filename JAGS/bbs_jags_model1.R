model{
  for(i in 1:bbs_obs){
    r_star[i] ~ dnorm(mu[i], tau)
    mu[i] <- b.0 + b.max*max[i] + b.min*min[i] + b.precip*precip[i] +
      b.swe*swe[i] + b.mean_max*mean_max[i] + b.mean_min*mean_min[i] +
      b.aou[aou[i]] ## Add in year/interaction
  }

  for(j in 1:aou_obs){
    b.aou[j] ~ dnorm(0, tau.aou)
  }
 
  
  b.0 ~ dnorm(0, 0.001)
  b.max ~ dnorm(0, 0.001)
  b.min ~ dnorm(0, 0.001)
  b.precip ~ dnorm(0, 0.001)
  b.swe ~ dnorm(0, 0.001)
  b.mean_max ~ dnorm(0, 0.001)
  b.mean_min ~ dnorm(0, 0.001)
  tau ~ dgamma(0.001, 0.001)
  tau.aou ~ dgamma(0.001, 0.001)
}
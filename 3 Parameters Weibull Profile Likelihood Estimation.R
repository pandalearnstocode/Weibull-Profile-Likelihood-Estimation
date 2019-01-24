library(FAdist)
# Simulated data from three parameters weibull distribution. 
# sample size : 104
# shape -> symmetry
# scale -> eta_inv
# thres -> volumn1 (location parameter)


simulated_data <- rweibull3(104,shape = 2,scale = 3, thres = 100)

weibull_loglik <- function(parm){
  n <- length(simulated_data)
  eta_hat <- parm[1]
  loglik <- sum(dweibull3(simulated_data,shape=2,scale=eta_hat,thres=100,log=TRUE))
  return(-loglik)
}
# Here we are assuming that we know the volumn1 which is lift over core or decomp by base sales. 
# We can say that we know that what will be the shape parameters because we fix that in the rapid. 
# say, in this case from calculation we get that the life over core is 100. (hypothetical, it should be less than 1)
# and share is 2 from rapid. 

# Now, weibull_loglik is the profile log-likelihood function to estimate the only unknown parameter, which is driver1
# or eta_inv in this context. (not sure either the original value will be eta_inv or 1/eta_inv). 

# Using non-linear minimization we can obtain the value for scale parameter. Originally for the data it was 3.
# We can see that the estimate is 3.03 which is quite close to the original one. 

weibull <- nlm(weibull_loglik, p = c(1.5), hessian=TRUE)
weibull$estimate

###########################

library(bbmle)

# Same thing we are doing here but using bootstrap parameters profile likelihood estimation. 

res <- mle2(minuslogl = weibull_loglik, start = list(parm = 1))
res

# Bootstaped parameters estimation for the unknown parameters.

confint(profile(res)) # confint w.r.t. the likelihood profile
confint(res, method="uniroot") 


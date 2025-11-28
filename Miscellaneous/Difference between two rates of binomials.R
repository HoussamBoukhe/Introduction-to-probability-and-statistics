#############################################
# Difference between two rates of binomials #
#############################################

library(rjags)
library(ggplot2)

n1 <- n2 <- 10
k1 <-  5
k2 <- 8
list_param <- list(n1 = n1, n2 = n2, k1 = k1, k2 = k2)

model <- "
model {
  # Likelihood
  k1 ~ dbin(theta1, n1)
  k2 ~ dbin(theta2, n2)
  
  # Prior
  theta1 ~ dbeta(1,1)
  theta2 ~ dbeta(1,1)
  
  delta <- theta1 - theta2
}
"

jags <- jags.model(textConnection(model), data=list_param, n.chains=3)
update(jags, 1000)
params <- c("theta1", "theta2", "delta")
samp <- coda.samples(jags, params, n.iter=5000)

# Summary
summary(samp)
plot(samp)
##############################
# Repeated measurement of IQ #
##############################

library(rjags)

# Each row is a subject. Each column is a repeated measure for IQ
data <- matrix(
  c(90, 95, 100,
    105, 110, 115,
    150, 155, 160),
  nrow = 3, byrow = TRUE
)

n <- nrow(data) # Number of subjects 
m <- ncol(data) # Number of repeated tests 

model_string <- "
model {
    # Likelihood
    for (i in 1:n){
        for (j in 1:m){
            x[i,j] ~ dnorm(mu[i], lambda)
            }
        }
    
    # Prio 
    for (i in 1:n){
        mu[i] ~ dunif(0, 300)
        }
    sigma ~ dunif(0, 100)
    lambda <- 1/(sigma*sigma)
}
"

data_list <- list(x = data, n = n, m = m)

jags <- jags.model(textConnection(model_string), data = data_list, n.chains = 5)
update(jags, 1000) # burn-in
params <- c("mu", "sigma")
samp <- coda.samples(jags, params, n.iter = 5000)
summary(samp)

# Estimation of the IQ of each subject from the posterior dist ??
post_mat <- as.matrix(samp)
mu_hat_post <- apply(post_mat[, 1:3], 2, mean)
print(mu_hat_post)


### Using a Gaussian prior

model_string2 <- "
model {
    # Likelihood
    for (i in 1:n){
        for (j in 1:m){
            x[i,j] ~ dnorm(mu[i], lambda)
            }
        }
    
    # Prior 
    for (i in 1:n){
        mu[i] ~ dnorm(100, 0.0044) # mean = 100, sd = 15
        }
    sigma ~ dunif(0, 100)
    lambda <- 1/(sigma*sigma)
}
"

jags2 <- jags.model(textConnection(model_string2), data = data_list, n.chains = 5)
update(jags2, 1000) # burn-in
params <- c("mu", "sigma")
samp2 <- coda.samples(jags2, params, n.iter = 5000)
summary(samp2)
post_mat2 <- as.matrix(samp2)
mu_hat_post2 <- apply(post_mat2[, 1:3], 2, mean)
print(mu_hat_post2)

### Comparison between two diffrent priors
# Uniform prior 
summary(samp)[1]
# Gaussian prior 
summary(samp2)[1]
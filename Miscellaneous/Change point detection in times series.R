##########################################
# Change point detection in times series #
##########################################

library(rjags)

true_mu1 <- 1
true_mu2 <- 2 
sigma <- 0.5

# A time series of length 500, with a change point at 300 
set.seed(12)
X <- c(rnorm(300, true_mu1, sigma), rnorm(200, true_mu2, sigma))
n <- length(X)

model_string <- "
model {
    mu1 ~ dnorm(0, 0.0001)
    mu2 ~ dnorm(0, 0.0001)
    lambda ~ dgamma(0.001, 0.001)
    sigma <- 1/sqrt(lambda)
    tau ~ dunif(1, n)

    for (i in 1:n){
        # You can't put ifelse condition here !
        mean[i] <- mu1 * step(tau - i) + mu2 * (1 - step(tau - i))
        X[i] ~ dnorm(mean[i], lambda)
    }
}
"

data_list <- list(X = X, n = n)  

jags <- jags.model(textConnection(model_string), data = data_list, n.chains = 3)
update(jags, 1000)

params <- c("mu1", "mu2", "sigma", "tau")
samp <- coda.samples(jags, params, n.iter = 5000)

summary(samp)

### Displaying the results 

library(coda)

# Extract posterior samples
tau_samples <- as.matrix(samp[, "tau"])
mu1_samples <- as.matrix(samp[, "mu1"])
mu2_samples <- as.matrix(samp[, "mu2"])

# Posterior estimates
tau_est <- mean(tau_samples)
tau_est_int <- quantile(tau_samples, probs = c(0.025, 0.975))
mu1_est <- mean(mu1_samples)
mu2_est <- mean(mu2_samples)

# Plot the time series
plot(X, type = "l", col = "blue", lwd = 2,
     xlab = "Time / Index", ylab = "Value",
     main = "Time Series with Estimated Change Point and Means")

# Change point
abline(v = tau_est, col = "red", lwd = 2, lty = 2)         # posterior mean
abline(v = tau_est_int, col = "red", lwd = 1, lty = 3)     # 95% CI

# Estimated means as horizontal lines
lines(1:floor(tau_est), rep(mu1_est, floor(tau_est)), col = "green", lwd = 2)
lines(ceiling(tau_est):length(X), rep(mu2_est, length(X) - ceiling(tau_est) + 1), col = "green", lwd = 2)

# Legend
legend("topleft",
       legend = c("X", "Estimated tau", "95% CI tau", "Estimated means"),
       col = c("blue", "red", "red", "green"),
       lty = c(1,2,3,1), lwd = c(2,2,1,2))

### What happens if there are more change points ? Adjust the code 
# for this situation.


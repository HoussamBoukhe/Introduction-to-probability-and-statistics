#############################################
# Binomial model - continuous uniform prior #
#############################################

#install rjags from https://sourceforge.net/projects/mcmc-jags/files
library(rjags)
library(ggplot2)

# Data: In an exam with 10 questions, you answered 9 correctly and 1 incorrectly. 
# Before the exam, we assumed that your success rate (proportion of correct answers) 
# could be any value between 0 and 1, which corresponds to a uniform prior on (0,1). 
# After observing the data, we updated our estimate of this parameter using the 
# prior and the likelihood to get the posterior, which represents our updated 
# belief about the probability of answering a question correctly.

n <- 10
k <- 9
data_list <- list(k = k, n = n)

# Model as a string
model_string <- "
model {
  # Likelihood
  k ~ dbin(theta, n)
  
  # Prior
  theta ~  dbeta(1,1)
}
"

# Initialize model 
jags_model <- jags.model(textConnection(model_string),
                         data = data_list,
                         n.chains = 3) 

# Burn-in
update(jags_model, 1000)

# Posterior sampling
samples <- coda.samples(jags_model,
                        variable.names = c("theta"),
                        n.iter = 5000) # try different values of n.iter, and discuss the results

plot(samples)
summary(samples)

theta_samples <- as.numeric(unlist(samples))

# Plot histogram + analytical Beta(10,2) density
df <- data.frame(theta = theta_samples)
ggplot(df, aes(x = theta)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black", alpha = 0.6) +
  stat_function(fun = dbeta, args = list(shape1 = k + 1, shape2 = n - k + 1),
                color = "red", size = 1) +
  labs(title = "Posterior of theta: MCMC vs Analytical Beta(10,2)",
       x = expression(theta),
       y = "Density") +
  theme_minimal()


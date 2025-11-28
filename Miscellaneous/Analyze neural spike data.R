# How to get the times of spikes 
spikes <- as.numeric(CAL1V[["neuron 1"]][["stim. 1"]])
cum_ss <- c(0)
for (t in T){
  tmp <- sum(spikes < t)
  cum_ss <- c(cum_ss, tmp)
}
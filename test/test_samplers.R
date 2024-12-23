
library(ggplot2)

# Define the negative log-probability function for a 2D Gaussian
neg_logp_gaussian <- function(q) {
  # Multivariate normal distribution with mean = (0, 0) and identity covariance
  return(0.5 * sum(q^2))
}

# Create an instance of the custom `SamplerMH`
custom_sampler <- SamplerMH$new(
  neg_logp = neg_logp_gaussian,
  initial_position = c(0, 0),
  proposal_sd = 1
)

# Typical Metropolis-Hastings Sampler
mh_sampler <- function(neg_logp, initial_position, n_samples, proposal_sd) {
  samples <- matrix(NA, ncol = length(initial_position), nrow = n_samples)
  samples[1, ] <- initial_position
  for (i in 2:n_samples) {
    current <- samples[i - 1, ]
    proposal <- current + rnorm(length(current), mean = 0, sd = proposal_sd)
    log_acceptance_ratio <- -neg_logp(proposal) + neg_logp(current)
    if (log(runif(1)) < log_acceptance_ratio) {
      samples[i, ] <- proposal
    } else {
      samples[i, ] <- current
    }
  }
  return(samples)
}

# Sampling parameters
n_samples <- 5000
proposal_sd <- 1

# Generate samples using the custom sampler
custom_samples <- do.call(rbind, lapply(1:n_samples, function(i) {
  custom_sampler$generate_sample()$sample
}))

# Generate samples using the typical MH sampler
typical_samples <- mh_sampler(
  neg_logp = neg_logp_gaussian,
  initial_position = c(0, 0),
  n_samples = n_samples,
  proposal_sd = proposal_sd
)

# Convert to data frames for plotting
df_custom <- as.data.frame(custom_samples)
colnames(df_custom) <- c("x1", "x2")
df_custom$sampler <- "Custom Sampler"

df_typical <- as.data.frame(typical_samples)
colnames(df_typical) <- c("x1", "x2")
df_typical$sampler <- "Typical MH Sampler"

# Combine for visualization
df_combined <- rbind(df_custom, df_typical)

# Plotting results
ggplot(df_combined, aes(x = x1, y = x2, color = sampler)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~sampler, ncol = 2) +
  labs(title = "Comparison of Custom and Typical MH Samplers",
       x = "x1", y = "x2") +
  theme_minimal()

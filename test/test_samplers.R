library(ggplot2)
library(mcmc)

SamplerMH <- R6::R6Class(
  "SamplerMH",
  public = list(
    neg_logp = NULL,
    proposal_sd = 1,
    proposal_type = "normal",  # Default proposal distribution
    initial_position = c(1.6, -0.6),
    generated_samples = list(),
    proposal_params = list(),  # Initialize as an empty list
    initialize = function(
      neg_logp, 
      initial_position = NULL, 
      proposal_sd = NULL, 
      proposal_type = "normal"
    ) { 
      if (!is.null(initial_position)) {
        self$initial_position <- initial_position
      }
      if (!is.null(proposal_sd)) {
        self$proposal_sd <- proposal_sd
      }
      self$proposal_type <- proposal_type
      self$neg_logp <- neg_logp
      self$proposal_params <- list(df = 5, cov_matrix = diag(length(self$initial_position)))
      self$generated_samples <- list(self$initial_position)
    },

    set_proposal_sd = function(sd) {
      stopifnot(sd > 0)
      self$proposal_sd <- sd
    },

    set_proposal_type = function(type, params = list()) {
      stopifnot(type %in% c("normal", "t", "multivariate_normal"))
      self$proposal_type <- type
      self$proposal_params <- params
    },

    generate_proposal = function(current_position) {
      if (self$proposal_type == "normal") {
        return(rnorm(length(current_position), mean = current_position, sd = self$proposal_sd))
      } else if (self$proposal_type == "t") {
        df <- self$proposal_params$df
        stopifnot(!is.null(df) && df > 0)
        return(current_position + rt(length(current_position), df = df) * self$proposal_sd)
      } else if (self$proposal_type == "multivariate_normal") {
        cov_matrix <- self$proposal_params$cov_matrix
        stopifnot(!is.null(cov_matrix) && is.matrix(cov_matrix))
        return(MASS::mvrnorm(1, mu = current_position, Sigma = cov_matrix))
      }
    },

    generate_sample = function() {
      q_current <- tail(self$generated_samples, 1)[[1]]
      q_proposal <- self$generate_proposal(q_current)

      # Calculate acceptance probability
      log_acceptance_ratio <- -self$neg_logp(q_proposal) + self$neg_logp(q_current)

      # Accept or reject
      u <- log(runif(1))
      accepted <- u < log_acceptance_ratio
      sample <- if (isTRUE(accepted)) q_proposal else q_current
      self$generated_samples <- append(self$generated_samples, list(sample))

      return(
        list(
          sample = matrix(sample, nrow = 1),
          accepted = accepted,
          q_current = q_current,
          q_proposal = q_proposal,
          log_acceptance_ratio = log_acceptance_ratio
        )
      )
    },

    generate_xyz = function() {
      initial_point <- tail(self$generated_samples, 1)[[1]]
      sample <- self$generate_sample()
      data <- rbind(initial_point, sample$sample)

      return(
        list(
          segments = list(
            x = data[, 1],
            y = data[, 2],
            z = apply(data, 1, self$neg_logp)
          ),
          point = list(
            x = tail(data[, 1], 1),
            y = tail(data[, 2], 1),
            z = self$neg_logp(tail(data, 1))
          ),
          accepted = isTRUE(sample$accepted),
          log_acceptance_ratio = sample$log_acceptance_ratio
        )
      )
    }
  )
)





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

# Raw plot
ggplot(df_combined, aes(x = x1, y = x2, color = sampler)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~sampler, ncol = 2) +
  labs(title = "Comparison of Custom and Typical MH Samplers",
       x = "x1", y = "x2") +
  theme_minimal()



# Marginal Density Plot
ggplot(df_combined, aes(x = x1, fill = sampler)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~sampler) +
  labs(title = "Marginal Distributions of x1",
       x = "x1", y = "Density") +
  theme_minimal()



##############################################################
# Using MCMC cran library
##############################################################


# Define the negative log-probability function for a 2D Gaussian
neg_logp_gaussian <- function(q) {
  # Multivariate normal distribution with mean = (0, 0) and identity covariance
  return(0.5 * sum(q^2))
}

# Custom Metropolis-Hastings Sampler (from the original code)
custom_sampler <- SamplerMH$new(
  neg_logp = neg_logp_gaussian,
  initial_position = c(0, 0),
  proposal_sd = 1
)

# Sampling parameters
n_samples <- 5000
proposal_sd <- 1

# Generate samples using the custom sampler
custom_samples <- do.call(rbind, lapply(1:n_samples, function(i) {
  custom_sampler$generate_sample()$sample
}))

# MCMC sampling using the 'mcmc' package (Typical MH sampler)
mcmc_sampler <- function(neg_logp, initial_position, n_samples, proposal_sd) {
  # Define the log-probability function for the MCMC package
  log_prob_func <- function(x) {
    -neg_logp(x)  # mcmc uses the log-probability, so we negate the log-potential
  }

  # Define the initial value for the sampler
  start_values <- matrix(initial_position, nrow = 1)

  # Perform the Metropolis-Hastings sampling
  samples_mcmc <- metrop(log_prob_func, start = start_values, niter = n_samples, scale = proposal_sd)
  
  # Return the samples from MCMC
  return(samples_mcmc$batch)
}

# Generate samples using the typical MCMC sampler
typical_samples_mcmc <- mcmc_sampler(
  neg_logp = neg_logp_gaussian,
  initial_position = c(0, 0),
  n_samples = n_samples,
  proposal_sd = proposal_sd
)


df_custom <- as.data.frame(custom_samples)
colnames(df_custom) <- c("x1", "x2")
df_custom$sampler <- "Custom Sampler"

df_typical <- as.data.frame(typical_samples_mcmc)
colnames(df_typical) <- c("x1", "x2")
df_typical$sampler <- "Typical MCMC Sampler"

df_combined <- rbind(df_custom, df_typical)

# Plotting results
ggplot(df_combined, aes(x = x1, y = x2, color = sampler)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~sampler, ncol = 2) +
  labs(title = "Comparison of Custom and Typical MCMC Samplers",
       x = "x1", y = "x2") +
  theme_minimal()

# Marginal Density Plot
ggplot(df_combined, aes(x = x1, fill = sampler)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~sampler) +
  labs(title = "Marginal Distributions of x1",
       x = "x1", y = "Density") +
  theme_minimal()

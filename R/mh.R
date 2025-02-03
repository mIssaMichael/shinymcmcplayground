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
          current = q_current,
          proposal = q_proposal,
          log_acceptance_ratio = log_acceptance_ratio
        )
      )
    },

    generate_xyz = function() {
      initial_point <- tail(self$generated_samples, 1)[[1]]
      sample <- self$generate_sample()
      
      # Create return data structure matching what server.R expects
      data <- rbind(
        c(sample$current[1], sample$current[2]),
        c(sample$sample[1], sample$sample[2])
      )
      
      z_values <- apply(data, 1, self$neg_logp)
      
      return(
        list(
          segments = list(
            x = data[, 1],
            y = data[, 2],
            z = z_values
          ),
          point = list(
            x = tail(data[, 1], 1),
            y = tail(data[, 2], 1),
            z = tail(z_values, 1)
          ),
          accepted = isTRUE(sample$accepted),
          current = sample$current,
          proposal = sample$proposal,
          log_acceptance_ratio = sample$log_acceptance_ratio
        )
      )
    }
  )
)
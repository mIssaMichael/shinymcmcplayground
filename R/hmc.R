lappend <- function(l, object) {
  l[[length(l) + 1]] <- object
  l
}

ltail <- function(l) {
  l[[length(l)]]
}

leapfrog <- function(p, q, neg_dlogp, path_length, step_size) {
  leap_q <- list()
  leap_p <- list()
  p <- p - step_size * neg_dlogp(q) / 2

  for (i in seq_len(round(path_length / step_size) - 1)) {
    q <- q + step_size * p
    p <- p - step_size * neg_dlogp(q)
    leap_q[[i]] <- q
    leap_p[[i]] <- p
  }
  q <- q + step_size * p
  p <- p - step_size * neg_dlogp(q) / 2

  # Flip del momentum
  return(list(q = q, p = -p, leap_q = leap_q, leap_p = leap_p))
}

SamplerHMC <- R6::R6Class(
  "SamplerHMC",
  public = list(
    neg_logp = NULL,
    neg_dlogp = NULL,
    initial_position = c(1.6, -0.6),
    path_length = 1,
    step_size = 0.01,
    generated_samples = list(),
    initialize = function(
      neg_logp,
      neg_dlogp,
      initial_position = NULL,
      path_length = NULL,
      step_size = NULL
    ) {
      if (!is.null(initial_position)) {
        self$initial_position = initial_position
      }
      if (!is.null(path_length)) {
        self$set_path_length(path_length)
      }
      if (!is.null(step_size)) {
        self$set_step_size(step_size)
      }
      self$neg_logp = neg_logp
      self$neg_dlogp = neg_dlogp
      self$generated_samples <- lappend(
        self$generated_samples, self$initial_position
      )
    },

    set_path_length = function(x) {
      stopifnot(x > 0)
      self$path_length <- x
    },

    set_step_size = function(x) {
      stopifnot(x > 0, x < self$path_length)
      self$step_size <- x
    },

    generate_sample = function(momentum = NULL) {
      if (is.null(momentum)) {
        p_current <- self$generate_momentum()
      } else {
        p_current <- momentum
      }

      q_current <- ltail(self$generated_samples)

      integration <- leapfrog(
        p_current, q_current, self$neg_dlogp, self$path_length, self$step_size
      )

      p_new <- integration$p
      q_new <- integration$q
      leap_p <- integration$leap_p
      leap_q <- integration$leap_q

      # Metropolis acceptance criteria
      H_current <- self$neg_logp(q_current) - mvtnorm::dmvnorm(p_current, log = TRUE)
      H_new <- self$neg_logp(q_new) - mvtnorm::dmvnorm(p_new, log = TRUE)

      u <- log(runif(1))
      accepted <- u < H_current - H_new
      sample <- if (isTRUE(accepted)) q_new else q_current
      self$generated_samples <- lappend(self$generated_samples, sample)

      # Compare energies and determine if it's a divergence
      energy_difference <- abs(H_new - H_current)
      is_divergent <- energy_difference > 1000 # heuristic taken from PyMC


      return(
        list(
          sample = matrix(sample, nrow = 1),
          leap_q = leap_q,
          momentum = matrix(p_current, nrow = 1),
          accepted = accepted,
          new_sample = q_new,
          h_current = H_current,
          h_proposal = H_new,
          divergent = is_divergent
        )
      )
    },

    generate_momentum = function() {
      n <- length(self$initial_position)
      mvtnorm::rmvnorm(1, rep(0, n), diag(n))
    },

    generate_xyz = function(momentum = NULL) {
      initial_point <- ltail(self$generated_samples)
      sample <- self$generate_sample(momentum)
      xy <- do.call("rbind", sample$leap_q)

      # Prepend the initial point so trajectories don't have a gap
      xy <- rbind(initial_point, xy)
      data <- cbind(xy, self$neg_logp(xy))
      data <- rbind(
        data,
        cbind(sample$sample, self$neg_logp(sample$sample))
      )

      x <- data[, 1]
      y <- data[, 2]
      z <- data[, 3]

      return(
        list(
          segments = list(x = x, y = y, z = z),
          point = list(x = tail(x, 1), y = tail(y, 1), z = tail(z, 1)),
          momentum = list(x = sample$momentum[, 1], y = sample$momentum[, 2]),
          accepted = isTRUE(sample$accepted),
          h_current = sample$h_current,
          h_proposal = sample$h_proposal,
          divergent = isTRUE(sample$divergent),
          new_sample = c(sample$q_new, self$neg_logp(sample$new_sample))
        )
      )
    }
  )
)




#Note: Modifiable options should include changing the proposal distribution, 
#      change the acceptance ratio
SamplerMH <- R6::R6Class(
  "SamplerMH",
  public = list(
    neg_logp = NULL,
    proposal_sd = 1,
    proposal_type = "normal",  # Default proposal distribution
    initial_position = c(1.6, -0.6),
    generated_samples = list(),
    proposal_params = list(df = 5, cov_matrix = diag(length(self$initial_position))),  # Additional params for t and multivariate normal
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

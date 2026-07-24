make_vasstra_data <- function(seed = 2026L) {
  stopifnot(is.numeric(seed), length(seed) == 1L)
  set.seed(seed)
  data <- expand.grid(
    student = seq_len(36L),
    course = seq_len(6L)
  )
  trajectory <- ceiling(data$student / 12L)
  patterns <- rbind(
    c(1, 1, 2, 2, 3, 3),
    c(2, 2, 3, 3, 1, 1),
    c(3, 3, 1, 1, 2, 2)
  )
  latent_state <- patterns[cbind(trajectory, data$course)]
  data$views <- latent_state * 6 + data$course * 0.2 +
    rnorm(nrow(data), sd = 0.08)
  data$sessions <- latent_state * 3 - data$course * 0.1 +
    rnorm(nrow(data), sd = 0.08)
  data$duration <- latent_state * 12 + data$course * 0.3 +
    rnorm(nrow(data), sd = 0.08)
  data$true_state <- c("Low", "Average", "High")[latent_state]
  data$true_trajectory <- paste("Trajectory", trajectory)
  data
}

# A small complete fit for tests that only need a finished pipeline.
vasstra_test_fit <- function(seed = 2026L) {
  data <- make_vasstra_data(seed = seed)
  suppressMessages(vasstra(
    data,
    variables = c("views", "sessions", "duration"),
    n_states = 3L,
    n_trajectories = 3L
  ))
}

same_partition <- function(first, second) {
  stopifnot(length(first) == length(second))
  identical(
    outer(as.character(first), as.character(first), "=="),
    outer(as.character(second), as.character(second), "==")
  )
}

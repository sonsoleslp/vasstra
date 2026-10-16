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

# The synthetic indicators have a within-state spread of 0.08, so a random EM
# start placed between the states can make every component density vanish.
# latents reports such starts with a "k of n starts failed" warning while the
# other starts reach and replicate the solution. Tests that are not about
# starts allow exactly that warning and nothing else.
allow_failed_starts <- function(expr) {
  withCallingHandlers(expr, warning = function(warning) {
    if (grepl("starts failed", conditionMessage(warning), fixed = TRUE)) {
      invokeRestart("muffleWarning")
    }
  })
}

# Subjects whose trajectory groups differ in how often they are in each state
# (mostly low, mostly average, mostly high), not in the order of the states.
# `make_vasstra_data()` is the opposite case: every group visits each state
# equally often, in a different order.
make_composition_data <- function(seed = 2026L) {
  stopifnot(is.numeric(seed), length(seed) == 1L)
  set.seed(seed)
  data <- expand.grid(student = seq_len(45L), course = seq_len(6L))
  group <- ceiling(data$student / 15L)
  mixes <- rbind(c(0.8, 0.15, 0.05), c(0.1, 0.8, 0.1), c(0.05, 0.15, 0.8))
  latent_state <- vapply(group, function(g) {
    sample.int(3L, 1L, prob = mixes[g, ])
  }, integer(1L))
  data$views <- latent_state * 6 + stats::rnorm(nrow(data), sd = 0.6)
  data$sessions <- latent_state * 3 + stats::rnorm(nrow(data), sd = 0.6)
  data$duration <- latent_state * 12 + stats::rnorm(nrow(data), sd = 0.6)
  data$true_group <- group
  data
}

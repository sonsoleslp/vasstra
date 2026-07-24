make_lpa_states <- function() {
  data <- make_vasstra_data()
  suppressMessages(step1_states(
    data,
    id = "student",
    time = "course",
    variables = c("views", "sessions", "duration"),
    n_states = 3,
    labels = c("Low", "Average", "High")
  ))
}

test_that("fit_indices returns the complete LPA index family", {
  states <- make_lpa_states()
  indices <- fit_indices(states)
  expect_s3_class(indices, "vasstra_fit_indices")
  expect_s3_class(indices, "data.frame")
  expect_identical(nrow(indices), 1L)
  expect_true(all(c(
    "n_states", "method", "lpa_model", "log_likelihood", "n_parameters",
    "aic", "bic", "sabic", "caic", "awe", "clc", "kic", "icl",
    "entropy", "prob_min", "prob_max", "silhouette",
    "min_size", "max_size", "size_ratio"
  ) %in% names(indices)))

  log_likelihood <- indices$log_likelihood
  n_parameters <- indices$n_parameters
  n_observations <- states$diagnostics$n_rows
  expect_equal(indices$aic, -2 * log_likelihood + 2 * n_parameters,
               tolerance = 1e-8)
  expect_equal(
    indices$bic,
    -2 * log_likelihood + n_parameters * log(n_observations),
    tolerance = 1e-8
  )
  expect_equal(
    indices$sabic,
    -2 * log_likelihood + n_parameters * log((n_observations + 2) / 24),
    tolerance = 1e-8
  )
  expect_equal(
    indices$caic,
    -2 * log_likelihood + n_parameters * (log(n_observations) + 1),
    tolerance = 1e-8
  )
  expect_equal(
    indices$kic,
    -2 * log_likelihood + 3 * (n_parameters + 1),
    tolerance = 1e-8
  )
  expect_true(indices$icl >= indices$bic)
  expect_true(indices$prob_min > 0 && indices$prob_max <= 1)
  expect_true(indices$entropy > 0 && indices$entropy <= 1)
})

test_that("fit_indices compares candidates and adapts to hard methods", {
  states <- make_lpa_states()
  comparison <- fit_indices(states, compare = TRUE)
  expect_true(nrow(comparison) >= 5L)
  expect_identical(sum(comparison$fitted), 1L)
  expect_true(any(comparison$best))
  expect_true(all(c("sabic", "awe", "prob_min") %in% names(comparison)))

  data <- make_vasstra_data()
  kmeans_states <- suppressMessages(step1_states(
    data,
    id = "student",
    time = "course",
    variables = c("views", "sessions", "duration"),
    n_states = 3,
    method = "kmeans"
  ))
  kmeans_indices <- fit_indices(kmeans_states)
  expect_false("aic" %in% names(kmeans_indices))
  expect_true("total_withinss" %in% names(kmeans_indices))
  expect_true("silhouette" %in% names(kmeans_indices))

  trajectories <- step3_trajectories(
    step2_sequences(kmeans_states),
    n_trajectories = 3
  )
  trajectory_indices <- fit_indices(trajectories)
  expect_identical(nrow(trajectory_indices), 1L)
  expect_true(all(c(
    "n_trajectories", "dissimilarity", "method", "silhouette",
    "mean_within_distance"
  ) %in% names(trajectory_indices)))
  trajectory_comparison <- fit_indices(trajectories, compare = TRUE)
  expect_identical(sum(trajectory_comparison$fitted), 1L)

  fit <- suppressMessages(vasstra(
    data[setdiff(names(data), c("true_state", "true_trajectory"))],
    n_states = 3,
    n_trajectories = 3
  ))
  expect_identical(fit_indices(fit)$n_states, 3L)
  expect_identical(
    fit_indices(fit, step = "trajectories")$n_trajectories,
    3L
  )
  printed <- capture.output(print(fit_indices(fit)))
  expect_true(any(grepl("VaSStra fit indices: states", printed)))
})

test_that("set_labels renames states and trajectories everywhere", {
  states <- make_lpa_states()
  renamed <- set_labels(states, c("Low" = "Struggling"))
  expect_identical(
    renamed$settings$labels,
    c("Struggling", "Average", "High")
  )
  expect_identical(
    levels(renamed$data$state),
    c("Struggling", "Average", "High")
  )
  expect_true("Struggling" %in% renamed$profiles$state)
  expect_identical(
    names(renamed$diagnostics$state_sizes)[[1L]],
    "Struggling"
  )
  expect_identical(
    as.integer(renamed$data$state),
    as.integer(states$data$state)
  )
  expect_error(set_labels(states, c("Nope" = "X")), "Unknown labels label")
  expect_error(set_labels(states, c("A", "B")), "must contain 3 labels")
  expect_error(set_labels(states, c("A", "A", "B")), "must be unique")

  fit <- suppressMessages(vasstra(
    make_vasstra_data()[c("student", "course", "views", "sessions",
                          "duration")],
    n_states = 3,
    n_trajectories = 3,
    state_labels = c("Low", "Average", "High"),
    positive_states = "High",
    negative_states = "Low"
  ))
  relabeled <- set_labels(
    fit,
    states = c("Low" = "Struggling"),
    trajectories = c("Up", "Flat", "Down")
  )
  expect_identical(
    relabeled$states$settings$labels,
    c("Struggling", "Average", "High")
  )
  expect_identical(
    relabeled$sequences$states,
    c("Struggling", "Average", "High")
  )
  expect_true(all(
    unlist(relabeled$sequences$data) %in%
      c("Struggling", "Average", "High")
  ))
  expect_identical(
    relabeled$description$settings$negative_states,
    "Struggling"
  )
  expect_identical(
    relabeled$description$settings$positive_states,
    "High"
  )
  expect_identical(
    levels(relabeled$trajectories$assignments),
    c("Up", "Flat", "Down")
  )
  expect_identical(
    as.integer(relabeled$trajectories$assignments),
    as.integer(fit$trajectories$assignments)
  )
  expect_identical(
    levels(relabeled$description$transitions$trajectory),
    c("Up", "Flat", "Down")
  )
  expect_setequal(
    unique(relabeled$description$transitions$from),
    c("Struggling", "Average", "High")
  )
  expect_identical(
    as.data.frame(relabeled, unit = "trajectory")$n,
    as.data.frame(fit, unit = "trajectory")$n
  )
  expect_error(
    set_labels(fit),
    "Supply `states`, `trajectories`, or both."
  )
})

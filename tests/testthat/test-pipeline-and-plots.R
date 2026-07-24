test_that("vasstra runs the full indicators-to-trajectories pipeline", {
  data <- make_vasstra_data()
  fit <- vasstra(
    data,
    id = "student",
    time = "course",
    variables = c("views", "sessions", "duration"),
    n_states = 3,
    n_trajectories = 3,
    state_labels = c("Low", "Average", "High"),
    positive_states = "High",
    negative_states = "Low",
    backend = "base",
    seed = 51L
  )

  expect_s3_class(fit, "vasstra")
  expect_s3_class(fit$states, "vasstra_states")
  expect_s3_class(fit$sequences, "vasstra_sequences")
  expect_s3_class(fit$trajectories, "vasstra_trajectories")
  expect_s3_class(fit$description, "vasstra_description")
  expect_equal(fit$diagnostics$n_subjects, 36L)
  expect_equal(fit$diagnostics$n_times, 6L)
  expect_equal(fit$diagnostics$n_states, 3L)
  expect_equal(fit$diagnostics$n_trajectories, 3L)
  expect_s3_class(summary(fit), "data.frame")
  expect_equal(nrow(as.data.frame(fit)), 36L)
})

test_that("vasstra accepts precomputed states", {
  data <- make_vasstra_data()
  fit <- vasstra(
    data,
    id = "student",
    time = "course",
    state = "true_state",
    n_trajectories = 3,
    positive_states = "High",
    negative_states = "Low",
    backend = "base"
  )

  expect_null(fit$states)
  expect_equal(fit$diagnostics$n_states, 3L)
  truth <- unique(data[c("student", "true_trajectory")])
  expect_true(same_partition(
    fit$trajectories$assignments,
    truth$true_trajectory
  ))
  expect_error(
    vasstra(
      data,
      "student",
      "course",
      variables = c("views", "sessions"),
      state = "true_state"
    ),
    "at most one"
  )
})

test_that("plot methods render and return Nestimate plot data", {
  data <- make_vasstra_data()
  fit <- vasstra(
    data,
    id = "student",
    time = "course",
    variables = c("views", "sessions", "duration"),
    state_labels = c("Low", "Average", "High"),
    backend = "base"
  )
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(grDevices::dev.off(), add = TRUE)

  states_plot <- plot(fit, which = "states")
  sequences_plot <- plot(fit, which = "sequences")
  trajectories_plot <- plot(fit, which = "trajectories")

  expect_true(is.matrix(states_plot))
  expect_named(
    sequences_plot,
    c("counts", "proportions", "levels", "palette", "groups")
  )
  expect_equal(dim(sequences_plot$counts$all), c(3L, 6L))
  expect_named(
    trajectories_plot,
    c("codes", "palette", "levels", "orders", "groups")
  )
  expect_equal(dim(trajectories_plot$codes), c(36L, 6L))
  expect_equal(length(trajectories_plot$orders), 3L)
  expect_equal(
    trajectories_plot$groups,
    levels(fit$trajectories$assignments)
  )
})

test_that("sequence plot methods expose every Nestimate plot type", {
  data <- make_vasstra_data()
  sequences <- step2_sequences(
    data,
    id = "student",
    time = "course",
    state = "true_state"
  )
  trajectories <- step3_trajectories(
    sequences,
    n_trajectories = 3,
    backend = "base"
  )
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(grDevices::dev.off(), add = TRUE)

  sequence_index <- plot(sequences, type = "index", legend = "none")
  sequence_heatmap <- plot(sequences, type = "heatmap", legend = "none")
  trajectory_distribution <- plot(
    trajectories,
    type = "distribution",
    legend = "none"
  )
  trajectory_heatmap <- plot(
    trajectories,
    type = "heatmap",
    legend = "none"
  )

  expect_equal(dim(sequence_index$codes), c(36L, 6L))
  expect_equal(dim(sequence_heatmap$codes), c(36L, 6L))
  expect_equal(sequence_heatmap$sort_used, "lcs")
  expect_equal(length(trajectory_distribution$groups), 3L)
  expect_equal(dim(trajectory_heatmap$codes), c(36L, 6L))
})

test_that("Nestimate trajectory index plots handle singleton groups", {
  data <- make_vasstra_data()
  trajectories <- data |>
    step2_sequences("student", "course", "true_state") |>
    step3_trajectories(n_trajectories = 3, backend = "base")
  singleton_labels <- c("Singleton", "Other")
  trajectories$assignments <- factor(
    c("Singleton", rep("Other", nrow(trajectories$data) - 1L)),
    levels = singleton_labels,
    ordered = TRUE
  )
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(grDevices::dev.off(), add = TRUE)

  result <- plot(
    trajectories,
    type = "index",
    legend = "none"
  )

  expect_equal(result$groups, singleton_labels)
  expect_equal(length(result$orders[[1L]]), 1L)
})

test_that("Nestimate distribution plots show missingness only when present", {
  data <- data.frame(
    id = c(1, 1, 1, 2, 2),
    time = c(1, 2, 3, 1, 3),
    state = c("A", "B", "A", "B", "A")
  )
  sequences <- step2_sequences(
    data,
    "id",
    "time",
    "state",
    missing = "keep"
  )
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(grDevices::dev.off(), add = TRUE)

  automatic <- plot(
    sequences,
    type = "distribution",
    legend = "none"
  )
  hidden <- plot(
    sequences,
    type = "distribution",
    na = FALSE,
    legend = "none"
  )

  expect_equal(automatic$levels, c("A", "B", "NA"))
  expect_equal(hidden$levels, c("A", "B"))
  expect_equal(dim(automatic$counts$all), c(3L, 3L))
  expect_equal(dim(hidden$counts$all), c(2L, 3L))
})

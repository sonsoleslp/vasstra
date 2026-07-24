test_that("vasstra runs fully automatically on plain data", {
  data <- make_vasstra_data()
  messages <- capture_messages(
    fit <- vasstra(data[setdiff(
      names(data),
      c("true_state", "true_trajectory")
    )])
  )
  expect_true(any(grepl("Detected id = \"student\"", messages)))
  expect_true(any(grepl("Selected n_states = 3", messages)))
  expect_true(any(grepl("Selected n_trajectories = 3", messages)))
  expect_identical(fit$settings$id, "student")
  expect_identical(fit$settings$time, "course")
  expect_identical(fit$settings$n_states, 3L)
  expect_identical(fit$settings$n_trajectories, 3L)
  expect_s3_class(
    fit$states$diagnostics$selection$candidates,
    "data.frame"
  )
  expect_s3_class(
    fit$trajectories$diagnostics$selection$candidates,
    "data.frame"
  )
  truth <- unique(data[c("student", "true_trajectory")])
  expect_true(same_partition(
    fit$trajectories$assignments,
    truth$true_trajectory
  ))
})

test_that("automatic counts match the explicit choice workflow", {
  data <- make_vasstra_data()
  automatic <- suppressMessages(step1_states(
    data,
    id = "student",
    time = "course",
    variables = c("views", "sessions", "duration")
  ))
  choices <- state_choices(
    data,
    id = "student",
    time = "course",
    variables = c("views", "sessions", "duration"),
    n_states = 2:6,
    method = "kmeans"
  )
  manual <- fit_state_choice(
    choices,
    candidate_id = choices$recommendations$candidate_id[[1L]]
  )
  expect_identical(
    automatic$settings$n_states,
    manual$settings$n_states
  )
  expect_identical(automatic$data$state, manual$data$state)
})

test_that("state detection refuses ambiguous categorical columns", {
  data <- make_vasstra_data()
  expect_error(
    suppressMessages(step2_sequences(data)),
    "several categorical columns qualify"
  )
  single <- data[setdiff(names(data), "true_trajectory")]
  sequences <- suppressMessages(step2_sequences(single))
  expect_identical(sequences$settings$state, "true_state")
})

test_that("labels imply the count and vectors define the candidates", {
  data <- make_vasstra_data()
  expect_message(
    states <- step1_states(
      data,
      id = "student",
      time = "course",
      variables = c("views", "sessions", "duration"),
      labels = c("Low", "Rising", "High", "Peak")
    ),
    "Using n_states = 4 to match the supplied labels"
  )
  expect_identical(states$settings$n_states, 4L)

  ranged <- suppressMessages(step1_states(
    data,
    id = "student",
    time = "course",
    variables = c("views", "sessions", "duration"),
    n_states = 2:4
  ))
  expect_identical(ranged$settings$n_states, 3L)
  expect_setequal(
    ranged$diagnostics$selection$candidates$n_states,
    2:4
  )

  sequences <- step2_sequences(suppressMessages(step1_states(
    data,
    id = "student",
    time = "course",
    variables = c("views", "sessions", "duration"),
    n_states = 3
  )))
  expect_message(
    trajectories <- step3_trajectories(
      sequences,
      labels = c("One", "Two", "Three")
    ),
    "Using n_trajectories = 3 to match the supplied labels"
  )
  expect_identical(trajectories$settings$n_trajectories, 3L)
})

test_that("choice fits resolve by recommendation and by filters", {
  data <- make_vasstra_data()
  choices <- state_choices(
    data,
    id = "student",
    time = "course",
    variables = c("views", "sessions", "duration"),
    n_states = 2:4,
    method = c("kmeans", "pam")
  )
  expect_message(
    recommended <- fit_state_choice(choices),
    "Fitting recommended candidate"
  )
  expect_true(
    recommended$settings$n_states %in% choices$recommendations$n_states
  )
  filtered <- fit_state_choice(choices, n_states = 3, method = "pam")
  expect_identical(filtered$settings$n_states, 3L)
  expect_identical(filtered$settings$method, "pam")
  expect_error(
    fit_state_choice(choices, n_states = 99),
    "No candidate matches"
  )
  expect_error(
    suppressMessages(fit_state_choice(choices, n_states = 4)),
    "Several candidates match"
  )

  sequences <- step2_sequences(suppressMessages(step1_states(
    data,
    id = "student",
    time = "course",
    variables = c("views", "sessions", "duration"),
    n_states = 3
  )))
  trajectory_options <- trajectory_choices(
    sequences,
    n_trajectories = 2:3,
    dissimilarity = "hamming",
    method = c("pam", "ward.D2")
  )
  expect_message(
    fit_trajectory_choice(trajectory_options),
    "Fitting recommended candidate"
  )
  selected <- fit_trajectory_choice(
    trajectory_options,
    n_trajectories = 3,
    method = "ward.D2"
  )
  expect_identical(selected$settings$n_trajectories, 3L)
  expect_identical(selected$settings$method, "ward.D2")
})

test_that("evaluate summarizes states, trajectories, and complete fits", {
  data <- make_vasstra_data()
  fit <- suppressMessages(vasstra(
    data,
    id = "student",
    time = "course",
    variables = c("views", "sessions", "duration"),
    state_labels = c("Low", "Average", "High")
  ))

  state_evaluation <- evaluate(fit$states)
  expect_s3_class(state_evaluation, "vasstra_evaluation")
  expect_identical(state_evaluation$unit, "states")
  expect_identical(sum(state_evaluation$candidates$fitted), 1L)
  expect_true(any(state_evaluation$candidates$best))
  expect_identical(nrow(state_evaluation$clusters), 3L)
  expect_identical(
    as.data.frame(state_evaluation),
    state_evaluation$candidates
  )
  expect_identical(summary(state_evaluation), state_evaluation$clusters)
  expect_true(all(is.finite(state_evaluation$clusters$silhouette)))

  trajectory_evaluation <- evaluate(fit$trajectories)
  expect_identical(trajectory_evaluation$unit, "trajectories")
  expect_identical(sum(trajectory_evaluation$candidates$fitted), 1L)
  expect_identical(
    names(trajectory_evaluation$clusters),
    c("trajectory", "n", "proportion", "mean_within_distance", "silhouette")
  )

  both <- evaluate(fit)
  expect_s3_class(both, "vasstra_evaluations")
  expect_named(both, c("states", "trajectories"))
  combined <- as.data.frame(both)
  expect_identical(
    names(combined),
    c("unit", "k", "silhouette", "min_size", "max_size", "size_ratio",
      "eligible", "best", "fitted")
  )
  expect_setequal(unique(combined$unit), c("states", "trajectories"))
})

test_that("evaluate accepts explicit ranges and keeps the fitted count", {
  data <- make_vasstra_data()
  states <- suppressMessages(step1_states(
    data,
    id = "student",
    time = "course",
    variables = c("views", "sessions", "duration"),
    n_states = 4
  ))
  evaluation <- evaluate(states, n_states = 2:3)
  expect_setequal(evaluation$candidates$n_states, 2:4)
  expect_identical(
    evaluation$candidates$n_states[evaluation$candidates$fitted],
    4L
  )
})

test_that("evaluation plots render and return the drawn data", {
  data <- make_vasstra_data()
  fit <- suppressMessages(vasstra(
    data,
    id = "student",
    time = "course",
    variables = c("views", "sessions", "duration")
  ))
  evaluation <- evaluate(fit$trajectories)
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(grDevices::dev.off(), add = TRUE)

  summary_result <- withVisible(plot(evaluation))
  expect_false(summary_result$visible)
  expect_named(summary_result$value, c("candidates", "clusters"))
  curve_result <- withVisible(plot(evaluation, type = "curve"))
  expect_false(curve_result$visible)
  expect_identical(curve_result$value, evaluation$candidates)
  silhouette_result <- withVisible(plot(evaluation, type = "silhouette"))
  expect_identical(silhouette_result$value, evaluation$clusters)
  sizes_result <- withVisible(plot(evaluation, type = "sizes"))
  expect_identical(sizes_result$value, evaluation$clusters)
  expect_error(
    plot(evaluation, colors = "red"),
    "one color per fitted cluster"
  )

  both_result <- withVisible(plot(evaluate(fit)))
  expect_false(both_result$visible)
  expect_named(both_result$value, c("states", "trajectories"))
})

test_that("state bar and overview plots return the profile matrix", {
  data <- make_vasstra_data()
  states <- suppressMessages(step1_states(
    data,
    id = "student",
    time = "course",
    variables = c("views", "sessions", "duration"),
    n_states = 3,
    labels = c("Low", "Average", "High")
  ))
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(grDevices::dev.off(), add = TRUE)

  bars <- withVisible(plot(states, type = "bars"))
  expect_false(bars$visible)
  profile <- withVisible(plot(states, type = "profile"))
  expect_equal(
    as.numeric(bars$value),
    as.numeric(profile$value),
    tolerance = 1e-12
  )
  overview <- withVisible(plot(states, type = "all"))
  expect_false(overview$visible)
  expect_equal(
    as.numeric(overview$value),
    as.numeric(profile$value),
    tolerance = 1e-12
  )
  expect_error(
    plot(states, colors = "red", type = "bars"),
    "one color per state"
  )
})

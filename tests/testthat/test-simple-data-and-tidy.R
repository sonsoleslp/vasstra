test_that("engagement data are named, subsetted, and analysis-ready", {
  data_environment <- new.env(parent = emptyenv())
  utils::data(
    "engagement",
    package = "VaSStra",
    envir = data_environment
  )
  engagement_data <- data_environment$engagement
  raw_names <- c(
    "course_view_count",
    "forum_consume_count",
    "forum_contribute_count",
    "lecture_view_count",
    "course_view_regularity",
    "session_count",
    "total_duration",
    "active_days"
  )
  standardized_names <- paste0(raw_names, "_z")
  expected_names <- c(
    "user_id",
    "course_id",
    "sequence_position",
    raw_names,
    standardized_names
  )

  expect_s3_class(engagement_data, "data.frame")
  expect_identical(dim(engagement_data), c(1136L, 19L))
  expect_identical(names(engagement_data), expected_names)
  expect_identical(
    vapply(
      engagement_data[c("user_id", "course_id")],
      class,
      character(1L)
    ),
    c(user_id = "character", course_id = "character")
  )
  expect_identical(class(engagement_data$sequence_position), "integer")
  expect_true(all(vapply(
    engagement_data[c(raw_names, standardized_names)],
    is.numeric,
    logical(1L)
  )))
  expect_equal(length(unique(engagement_data$user_id)), 142L)
  expect_equal(length(unique(engagement_data$course_id)), 38L)
  expect_identical(
    sort(unique(engagement_data$sequence_position)),
    seq_len(8L)
  )
  expect_identical(
    anyDuplicated(
      engagement_data[c("user_id", "sequence_position")]
    ),
    0L
  )
  expect_true(all(table(engagement_data$user_id) == 8L))
  expect_false(anyNA(engagement_data[setdiff(
    names(engagement_data),
    standardized_names
  )]))
  expect_identical(
    sum(is.na(engagement_data[standardized_names])),
    24L
  )
  expect_true(all(
    rowSums(is.na(engagement_data[standardized_names])) %in% c(0L, 8L)
  ))

  invisible(Map(function(raw_name, standardized_name) {
    raw_values <- engagement_data[[raw_name]]
    course_means <- stats::ave(
      raw_values,
      engagement_data$course_id,
      FUN = mean
    )
    course_deviations <- stats::ave(
      raw_values,
      engagement_data$course_id,
      FUN = stats::sd
    )
    expected <- (raw_values - course_means) / course_deviations
    expect_equal(
      engagement_data[[standardized_name]],
      expected,
      tolerance = 1e-12,
      info = standardized_name
    )
  }, raw_names, standardized_names))

  expect_identical(
    attr(engagement_data, "vasstra"),
    list(
      id = "user_id",
      time = "sequence_position",
      variables = standardized_names,
      standardize = "none",
      missing = "median",
      standardized_by = "course_id"
    )
  )
})

test_that("role metadata makes state and complete workflow calls concise", {
  data <- make_vasstra_data()
  variables <- c("views", "sessions", "duration")
  attr(data, "vasstra") <- list(
    id = "student",
    time = "course",
    variables = variables,
    standardize = "time",
    missing = "error"
  )

  automatic_states <- step1_states(
    data,
    n_states = 3,
    labels = c("Low", "Average", "High"),
    seed = 241L
  )
  explicit_states <- step1_states(
    data,
    id = "student",
    time = "course",
    variables = variables,
    n_states = 3,
    labels = c("Low", "Average", "High"),
    standardize = "time",
    missing = "error",
    seed = 241L
  )
  expect_identical(
    automatic_states$data$state,
    explicit_states$data$state
  )
  expect_equal(
    automatic_states$standardized,
    explicit_states$standardized,
    tolerance = 1e-12
  )
  expect_identical(
    automatic_states$settings[c(
      "id", "time", "variables", "standardize", "missing"
    )],
    explicit_states$settings[c(
      "id", "time", "variables", "standardize", "missing"
    )]
  )

  automatic_choices <- state_choices(
    data,
    n_states = 2:3,
    method = "kmeans",
    seed = 243L
  )
  explicit_choices <- state_choices(
    data,
    id = "student",
    time = "course",
    variables = variables,
    n_states = 2:3,
    method = "kmeans",
    standardize = "time",
    missing = "error",
    seed = 243L
  )
  expect_equal(
    automatic_choices$candidates,
    explicit_choices$candidates,
    tolerance = 1e-12
  )

  fit <- vasstra(
    data,
    n_states = 3,
    n_trajectories = 3,
    state_labels = c("Low", "Average", "High"),
    positive_states = "High",
    negative_states = "Low",
    seed = 245L
  )
  expect_s3_class(fit, "vasstra")
  expect_identical(fit$settings$id, "student")
  expect_identical(fit$settings$time, "course")
  expect_identical(fit$settings$variables, variables)
})

test_that("complete fits expose tidy tables at four analysis units", {
  data <- make_vasstra_data()
  attr(data, "vasstra") <- list(
    id = "student",
    time = "course",
    variables = c("views", "sessions", "duration"),
    standardize = "time",
    missing = "error"
  )
  fit <- vasstra(
    data,
    n_states = 3,
    n_trajectories = 3,
    state_labels = c("Low", "Average", "High"),
    positive_states = "High",
    negative_states = "Low",
    seed = 247L
  )

  subject <- as.data.frame(fit)
  observation <- as.data.frame(fit, unit = "observation")
  state_profile <- as.data.frame(fit, unit = "state_profile")
  trajectory <- as.data.frame(fit, unit = "trajectory")

  expect_identical(subject, fit$description$indices)
  expect_identical(state_profile, fit$states$profiles)
  expect_identical(trajectory, fit$description$trajectory_summary)
  expect_identical(
    names(observation),
    c("student", "course", "state", "position", "trajectory")
  )
  expect_identical(nrow(observation), nrow(data))
  expect_false(anyNA(observation$trajectory))
  expect_identical(
    as.character(observation$trajectory),
    as.character(fit$trajectories$membership$trajectory[
      match(
        observation$student,
        fit$trajectories$membership$student
      )
    ])
  )
  expect_error(
    as.data.frame(fit, unit = "unknown"),
    "should be one of"
  )

  existing_state_fit <- vasstra(
    fit$states$data,
    id = "student",
    time = "course",
    state = "state",
    n_trajectories = 3,
    positive_states = "High",
    negative_states = "Low",
    seed = 249L
  )
  expect_error(
    as.data.frame(existing_state_fit, unit = "state_profile"),
    "State profiles are unavailable"
  )
})

test_that("missing state roles are detected transparently or refused", {
  data <- make_vasstra_data()
  expect_message(
    states <- step1_states(data, n_states = 3),
    "Detected id = \"student\", time = \"course\""
  )
  expect_identical(states$settings$id, "student")
  expect_identical(states$settings$time, "course")
  expect_identical(
    states$settings$variables,
    c("views", "sessions", "duration")
  )

  opaque <- data
  names(opaque)[names(opaque) == "student"] <- "alpha"
  expect_error(
    suppressMessages(step1_states(opaque, n_states = 3)),
    "Supply `id` explicitly"
  )
  no_time <- data
  names(no_time)[names(no_time) == "course"] <- "beta"
  expect_error(
    suppressMessages(step1_states(no_time, n_states = 3)),
    "Supply `time` explicitly"
  )

  malformed <- data
  attr(malformed, "vasstra") <- "not a list"
  expect_error(
    step1_states(malformed, n_states = 3),
    "must be a named list"
  )
})

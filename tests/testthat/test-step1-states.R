test_that("step1_states returns tidy reproducible state results", {
  data <- make_vasstra_data()
  set.seed(901L)
  random_state <- .Random.seed

  first <- step1_states(
    data,
    id = "student",
    time = "course",
    variables = c("views", "sessions", "duration"),
    n_states = 3,
    labels = c("Low", "Average", "High"),
    seed = 44L
  )
  second <- step1_states(
    data,
    id = "student",
    time = "course",
    variables = c("views", "sessions", "duration"),
    n_states = 3,
    labels = c("Low", "Average", "High"),
    seed = 44L
  )

  expect_s3_class(first, "vasstra_states")
  expect_named(
    first,
    c(
      "data", "profiles", "state_colors", "standardized", "model",
      "settings", "diagnostics"
    )
  )
  expect_identical(first$data$state, second$data$state)
  expect_identical(.Random.seed, random_state)
  expect_equal(dim(first$data), c(nrow(data), ncol(data) + 1L))
  expect_equal(dim(first$profiles), c(9L, 5L))
  expect_false(anyNA(first$standardized))
  expect_equal(first$diagnostics$missing_imputed, 0L)
  expect_equal(
    levels(first$data$state),
    c("Low", "Average", "High")
  )
  expect_equal(
    mean(first$data$state == data$true_state),
    1,
    tolerance = 1e-12
  )
  expect_s3_class(summary(first), "data.frame")
  expect_s3_class(as.data.frame(first), "data.frame")
})

test_that("step1_states validates structure and indicator types", {
  data <- make_vasstra_data()
  duplicated <- rbind(data, data[1L, ])
  nonnumeric <- data
  nonnumeric$views <- as.character(nonnumeric$views)

  expect_error(
    step1_states(
      duplicated,
      "student",
      "course",
      c("views", "sessions")
    ),
    "subject-time"
  )
  expect_error(
    step1_states(
      nonnumeric,
      "student",
      "course",
      c("views", "sessions")
    ),
    "numeric"
  )
  expect_error(
    step1_states(data, "student", "course", "views"),
    "at least two"
  )
  expect_error(
    step1_states(
      data,
      "student",
      "course",
      c("views", "sessions"),
      n_states = 3,
      labels = c("Low", "High")
    ),
    "one unique"
  )
  implied <- suppressMessages(step1_states(
    data,
    "student",
    "course",
    c("views", "sessions"),
    labels = c("Low", "High")
  ))
  expect_identical(implied$settings$n_states, 2L)
})

test_that("step1_states handles missing and zero-variance indicators explicitly", {
  data <- make_vasstra_data()
  data$views[c(1L, 20L)] <- NA_real_
  data$constant <- 1

  expect_error(
    step1_states(
      data,
      "student",
      "course",
      c("views", "sessions")
    ),
    "Missing indicator"
  )
  imputed <- step1_states(
    data,
    "student",
    "course",
    c("views", "sessions"),
    missing = "median"
  )
  expect_equal(imputed$diagnostics$missing_imputed, 2L)
  expect_false(anyNA(imputed$standardized))
  expect_warning(
    constant <- suppressMessages(step1_states(
      data,
      "student",
      "course",
      c("sessions", "constant"),
      missing = "median",
      method = "kmeans"
    )),
    "Zero-variance"
  )
  expect_true(all(constant$standardized$constant == 0))
})

test_that("standardize supports time, subject, global, and none", {
  set.seed(3)
  data <- expand.grid(person = 1:20, t = 1:5)
  baseline <- data$person
  data$a <- baseline * 4 + (data$t %% 2) + rnorm(nrow(data), sd = 0.1)
  data$b <- baseline * 2 + (data$t %% 2) + rnorm(nrow(data), sd = 0.1)

  for (s in c("time", "subject", "global", "none")) {
    states <- step1_states(
      data, "person", "t", c("a", "b"),
      n_states = 3, method = "kmeans", standardize = s
    )
    expect_equal(states$settings$standardize, s)
    expect_false(anyNA(states$standardized))
  }

  # "none" keeps the raw scale; the standardizing options shrink it.
  raw <- step1_states(data, "person", "t", c("a", "b"), n_states = 3,
                      method = "kmeans", standardize = "none")
  subject <- step1_states(data, "person", "t", c("a", "b"), n_states = 3,
                          method = "kmeans", standardize = "subject")
  expect_gt(stats::sd(raw$standardized$a), stats::sd(subject$standardized$a))
})

test_that("clara, fanny, and diana produce valid state clusterings", {
  data <- make_vasstra_data()
  variables <- c("views", "sessions", "duration")

  for (m in c("clara", "fanny", "diana")) {
    states <- step1_states(
      data, "student", "course", variables,
      n_states = 3, method = m,
      labels = c("Low", "Average", "High")
    )
    expect_s3_class(states, "vasstra_states")
    # Every observation is assigned to exactly one of the three states.
    assigned <- states$data[[states$settings$state]]
    expect_setequal(levels(assigned), c("Low", "Average", "High"))
    expect_equal(sum(states$diagnostics$state_sizes), nrow(data))
    # Silhouette-based diagnostics are populated for the non-LPA methods.
    expect_true(is.finite(states$diagnostics$silhouette))
    expect_equal(states$settings$method, m)
  }
})

test_that("character time requires explicit chronology", {
  data <- make_vasstra_data()
  data$course <- paste0("Wave ", data$course)

  expect_error(
    step1_states(
      data,
      "student",
      "course",
      c("views", "sessions")
    ),
    "time_levels"
  )
  result <- step1_states(
    data,
    "student",
    "course",
    c("views", "sessions"),
    time_levels = paste0("Wave ", 1:6)
  )
  expect_equal(result$settings$time_levels, paste0("Wave ", 1:6))
})

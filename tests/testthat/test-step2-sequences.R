test_that("step2_sequences aligns subjects and computes transitions", {
  data <- data.frame(
    id = rep(1:3, each = 3),
    time = rep(1:3, 3),
    state = c("A", "A", "B", "B", "B", "C", "A", "B", "B")
  )
  result <- step2_sequences(
    data,
    id = "id",
    time = "time",
    state = "state"
  )

  expect_s3_class(result, "vasstra_sequences")
  expect_equal(dim(result$data), c(3L, 3L))
  expect_equal(dim(result$long_data), c(9L, 4L))
  expect_equal(result$diagnostics$n_transitions, 6L)
  expect_equal(
    result$transitions$count[
      result$transitions$from == "A" &
        result$transitions$to == "B"
    ],
    2L
  )
  expect_equal(
    result$transitions$probability[
      result$transitions$from == "A" &
        result$transitions$to == "B"
    ],
    2 / 3
  )
  expect_false(anyNA(result$data))
  expect_equal(sum(result$distribution$proportion), 3)
  expect_s3_class(summary(result), "data.frame")
  expect_s3_class(as.data.frame(result), "data.frame")
})

test_that("step2_sequences accepts step 1 directly", {
  data <- make_vasstra_data()
  states <- step1_states(
    data,
    "student",
    "course",
    c("views", "sessions", "duration")
  )
  result <- step2_sequences(states)

  expect_equal(result$settings$id, "student")
  expect_equal(result$settings$time, "course")
  expect_equal(result$settings$state, "state")
  expect_equal(dim(result$data), c(36L, 6L))
  expect_equal(result$diagnostics$n_structural_missing, 0L)
})

test_that("step2_sequences makes structural missingness explicit", {
  data <- data.frame(
    id = c(1, 1, 1, 2, 2),
    time = c(1, 2, 3, 1, 3),
    state = c("A", "B", "A", "B", "A")
  )

  expect_error(
    step2_sequences(data, "id", "time", "state"),
    "missing state cell"
  )
  explicit <- step2_sequences(
    data,
    "id",
    "time",
    "state",
    missing = "explicit",
    missing_label = "Gap"
  )
  kept <- step2_sequences(
    data,
    "id",
    "time",
    "state",
    missing = "keep"
  )

  expect_true("Gap" %in% explicit$states)
  expect_equal(explicit$data$T2[[2L]], "Gap")
  expect_equal(explicit$diagnostics$n_missing, 0L)
  expect_true(is.na(kept$data$T2[[2L]]))
  expect_equal(kept$diagnostics$n_missing, 1L)
})

test_that("step2_sequences rejects duplicates and guessed character time", {
  data <- data.frame(
    id = c(1, 1, 2, 2),
    time = c("First", "Second", "First", "Second"),
    state = c("A", "B", "B", "A")
  )

  expect_error(
    step2_sequences(data, "id", "time", "state"),
    "time_levels"
  )
  result <- step2_sequences(
    data,
    "id",
    "time",
    "state",
    time_levels = c("First", "Second")
  )
  expect_equal(result$settings$time_levels, c("First", "Second"))

  duplicate <- rbind(data, data[1L, ])
  expect_error(
    step2_sequences(
      duplicate,
      "id",
      "time",
      "state",
      time_levels = c("First", "Second")
    ),
    "subject-time"
  )
})

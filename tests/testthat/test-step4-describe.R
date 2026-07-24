test_that("step4_describe computes interpretable sequence indices", {
  data <- data.frame(
    id = rep(1:6, each = 4),
    time = rep(1:4, 6),
    state = c(
      "A", "A", "A", "A",
      "A", "A", "A", "A",
      "A", "A", "A", "A",
      "A", "B", "A", "B",
      "A", "B", "A", "B",
      "A", "B", "A", "B"
    )
  )
  trajectories <- data |>
    step2_sequences("id", "time", "state") |>
    step3_trajectories(
      n_trajectories = 2,
      backend = "base"
    )
  result <- step4_describe(
    trajectories,
    positive_states = "A",
    negative_states = "B"
  )
  constant <- result$indices[result$indices$id == 1L, ]
  alternating <- result$indices[result$indices$id == 4L, ]

  expect_s3_class(result, "vasstra_description")
  expect_equal(constant$transitions, 0)
  expect_equal(constant$entropy, 0)
  expect_equal(constant$complexity, 0)
  expect_equal(constant$volatility, 0.25)
  expect_equal(constant$integrative_potential, 1)
  expect_equal(constant$negative_exposure, 0)
  expect_equal(alternating$transitions, 3)
  expect_equal(alternating$entropy, 1)
  expect_equal(alternating$complexity, 1)
  expect_equal(alternating$volatility, 1)
  expect_equal(alternating$integrative_potential, 0.4)
  expect_equal(alternating$negative_exposure, 0.6)
  expect_equal(nrow(result$mean_time), 4L)
  expect_equal(nrow(result$transitions), 8L)
  expect_s3_class(summary(result), "data.frame")
  expect_s3_class(as.data.frame(result), "data.frame")
})

test_that("step4 rejects unknown state semantics", {
  data <- make_vasstra_data()
  trajectories <- data |>
    step2_sequences("student", "course", "true_state") |>
    step3_trajectories(n_trajectories = 3, backend = "base")

  expect_error(
    step4_describe(trajectories, positive_states = "Unknown"),
    "Unknown positive"
  )
  expect_error(
    step4_describe(trajectories, negative_states = "Unknown"),
    "Unknown negative"
  )
  expect_error(
    step4_describe(trajectories, omega = 0),
    "positive"
  )
})

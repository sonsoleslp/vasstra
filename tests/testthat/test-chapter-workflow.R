test_that("chapter data runs through the complete Nestimate workflow", {
  data_environment <- new.env(parent = emptyenv())
  utils::data(
    "engagement",
    package = "VaSSTra",
    envir = data_environment
  )
  data <- data_environment$engagement

  states <- step1_states(
    data,
    n_states = 3,
    labels = c("Disengaged", "Average", "Active"),
    seed = 22294L
  )
  sequences <- step2_sequences(states)
  trajectories <- step3_trajectories(
    sequences,
    n_trajectories = 3,
    dissimilarity = "lcs",
    method = "ward.D2",
    backend = "Nestimate",
    labels = c(
      "Mostly active",
      "Mostly average",
      "Mostly disengaged"
    ),
    seed = 22295L
  )
  description <- step4_describe(
    trajectories,
    positive_states = "Active",
    negative_states = "Disengaged"
  )

  expect_equal(dim(data), c(1136L, 19L))
  expect_equal(length(unique(data$user_id)), 142L)
  expect_equal(sort(unique(data$sequence_position)), seq_len(8L))
  expect_equal(states$diagnostics$missing_imputed, 24L)
  expect_equal(sum(states$diagnostics$state_sizes), 1136L)
  expect_equal(dim(sequences$data), c(142L, 8L))
  expect_equal(sequences$diagnostics$n_missing, 0L)
  expect_equal(sequences$diagnostics$n_structural_missing, 0L)
  expect_s3_class(trajectories$model, "net_clustering")
  expect_equal(trajectories$settings$backend, "Nestimate")
  expect_equal(trajectories$settings$dissimilarity, "lcs")
  expect_equal(trajectories$settings$method, "ward.D2")
  expect_equal(
    levels(trajectories$assignments),
    c("Mostly active", "Mostly average", "Mostly disengaged")
  )
  expect_equal(sum(trajectories$diagnostics$sizes), 142L)
  expect_true(is.finite(trajectories$silhouette))
  expect_equal(dim(description$indices), c(142L, 10L))
  expect_false(anyNA(description$indices))
  expect_equal(
    anyDuplicated(sequences$long_data[c(
      "user_id",
      "sequence_position"
    )]),
    0L
  )
})

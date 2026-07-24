test_that("local distances reproduce known Hamming and LCS values", {
  data <- data.frame(
    id = rep(1:6, each = 4),
    time = rep(1:4, 6),
    state = c(
      "A", "A", "A", "A",
      "A", "A", "A", "B",
      "B", "B", "B", "B",
      "B", "B", "B", "A",
      "C", "C", "C", "C",
      "C", "C", "C", "B"
    )
  )
  sequences <- step2_sequences(data, "id", "time", "state")
  hamming <- step3_trajectories(
    sequences,
    n_trajectories = 3,
    dissimilarity = "hamming",
    method = "ward.D2",
    backend = "base"
  )
  lcs <- step3_trajectories(
    sequences,
    n_trajectories = 3,
    dissimilarity = "lcs",
    method = "ward.D2",
    backend = "base"
  )

  expect_equal(as.matrix(hamming$distance)[1L, 2L], 1)
  expect_equal(as.matrix(lcs$distance)[1L, 2L], 2)
  expect_true(same_partition(
    hamming$assignments,
    rep(1:3, each = 2)
  ))
  expect_true(same_partition(
    lcs$assignments,
    rep(1:3, each = 2)
  ))
  expect_equal(sum(hamming$diagnostics$sizes), 6L)
  expect_true(is.finite(hamming$silhouette))
  expect_s3_class(summary(hamming), "data.frame")
  expect_s3_class(as.data.frame(hamming), "data.frame")
})

test_that("PAM groups clearly similar sequences", {
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
    backend = "base",
    seed = 8L
  )
  truth <- unique(data[c("student", "true_trajectory")])

  expect_s3_class(trajectories, "vasstra_trajectories")
  expect_equal(dim(trajectories$membership), c(36L, 2L))
  expect_true(same_partition(
    trajectories$membership$trajectory,
    truth$true_trajectory
  ))
  expect_gt(trajectories$silhouette, 0.9)
})

test_that("step3 validates trajectory counts", {
  data <- make_vasstra_data()
  sequences <- step2_sequences(
    data,
    id = "student",
    time = "course",
    state = "true_state"
  )

  expect_error(
    step3_trajectories(sequences, n_trajectories = 1),
    "at least 2"
  )
  expect_error(
    step3_trajectories(sequences, n_trajectories = 36),
    "n - 1"
  )
})

test_that("Nestimate backend is numerically equivalent", {
  data <- make_vasstra_data()
  sequences <- step2_sequences(
    data,
    id = "student",
    time = "course",
    state = "true_state"
  )
  set.seed(801L)
  random_state <- .Random.seed
  result <- step3_trajectories(
    sequences,
    n_trajectories = 3,
    dissimilarity = "lcs",
    method = "ward.D2",
    backend = "Nestimate",
    seed = 22L
  )
  expect_identical(.Random.seed, random_state)
  direct <- Nestimate::build_clusters(
    sequences$data,
    k = 3,
    dissimilarity = "lcs",
    method = "ward.D2",
    seed = 22L
  )

  expect_equal(
    as.matrix(result$distance),
    as.matrix(direct$distance),
    tolerance = 1e-12
  )
  expect_equal(result$silhouette, direct$silhouette, tolerance = 1e-12)
  expect_true(same_partition(result$assignments, direct$assignments))
})

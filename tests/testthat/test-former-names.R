# The step functions' former names still call the renamed functions.

test_that("the former step names return what the new names return", {
  data <- make_vasstra_data()
  variables <- c("views", "sessions", "duration")
  old <- step1_states(data, "student", "course", variables, n_states = 3,
                      engine = "kmeans")
  new <- states(data, "student", "course", variables, n_states = 3,
                engine = "kmeans")
  expect_identical(old$data$state, new$data$state)
  old_sequences <- step2_sequences(old)
  expect_identical(old_sequences$data, sequences(new)$data)
  old_trajectories <- step3_trajectories(old_sequences, n_trajectories = 3)
  new_trajectories <- trajectories(sequences(new), n_trajectories = 3)
  expect_identical(old_trajectories$membership, new_trajectories$membership)
  expect_identical(step4_describe(old_trajectories)$indices,
                   describe(new_trajectories)$indices)
})

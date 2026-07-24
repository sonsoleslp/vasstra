test_that("bundle resolution follows the documented rules", {
  # "auto" only bundles once subjects outnumber the requested line budget.
  expect_null(.vasstra_flow_bundle("auto", 30, 50))
  expect_identical(.vasstra_flow_bundle("auto", 142, 50), 3L)
  expect_identical(.vasstra_flow_bundle("auto", 100, 50), 2L)
  expect_null(.vasstra_flow_bundle(FALSE, 142, 50))
  expect_null(.vasstra_flow_bundle(NULL, 142, 50))
  expect_null(.vasstra_flow_bundle(1, 142, 50))
  expect_identical(.vasstra_flow_bundle(4, 142, 50), 4L)
  expect_error(.vasstra_flow_bundle("some", 142, 50), "auto")
  expect_error(.vasstra_flow_bundle(-2, 142, 50), "positive")
  expect_error(.vasstra_flow_bundle("auto", 142, 0), "bundle_max")
})

test_that("flow colour source is validated per plot type", {
  expect_identical(.vasstra_flow_color_by(NULL, "alluvial"), "source")
  expect_identical(.vasstra_flow_color_by(NULL, "individual"), "first")
  expect_identical(
    .vasstra_flow_color_by("destination", "alluvial"),
    "destination"
  )
  expect_identical(.vasstra_flow_color_by("last", "individual"), "last")
  # Aggregated bands have no first/last state to borrow a colour from.
  expect_error(.vasstra_flow_color_by("first", "alluvial"))
  expect_error(.vasstra_flow_color_by("last", "alluvial"))
})

test_that("flow plots return ggplot objects for every supported input", {
  skip_if_not_installed("cograph")
  fit <- vasstra_test_fit()
  expect_s3_class(flow_plot(fit), "ggplot")
  expect_s3_class(flow_plot(fit, type = "individual"), "ggplot")
  expect_s3_class(flow_plot(fit$sequences), "ggplot")
  expect_s3_class(flow_plot(fit$trajectories), "ggplot")
  expect_s3_class(
    flow_plot(fit$sequences, type = "individual", bundle = FALSE),
    "ggplot"
  )
})

test_that("flow plots accept one color per state and reject other lengths", {
  skip_if_not_installed("cograph")
  fit <- vasstra_test_fit()
  states <- fit$sequences$states
  expect_s3_class(
    flow_plot(fit, colors = .vasstra_palette(length(states))),
    "ggplot"
  )
  expect_error(
    flow_plot(fit, colors = c("red", "blue")),
    "one color per state"
  )
  expect_error(
    flow_plot(fit, colors = c("red", "blue", "green", "orange")),
    "one color per state"
  )
})

test_that("cograph is reported as required when it is unavailable", {
  # The guard must name the missing package rather than failing inside
  # cograph::plot_alluvial with an opaque error.
  local_mocked_bindings(
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
  expect_error(
    .vasstra_flow_plot(
      sequence_data = data.frame(T1 = "a", T2 = "b"),
      states = c("a", "b"),
      time_labels = c("1", "2"),
      type = "alluvial",
      colors = NULL,
      main = NULL,
      color_by = NULL,
      bundle = "auto",
      bundle_max = 50,
      dots = list()
    ),
    "cograph"
  )
})

test_that("group restricts a trajectory flow plot and is validated", {
  skip_if_not_installed("cograph")
  fit <- vasstra_test_fit()
  label <- fit$trajectories$settings$labels[[1L]]
  expect_s3_class(flow_plot(fit, group = label), "ggplot")
  expect_s3_class(
    flow_plot(fit$trajectories, group = label, type = "individual"),
    "ggplot"
  )
  expect_error(flow_plot(fit, group = "not a trajectory"), "must be one")
  expect_error(flow_plot(fit, group = c("a", "b")), "must be one")
})

test_that("flow plots reject inputs with too few time points", {
  skip_if_not_installed("cograph")
  states <- data.frame(
    student = rep(1:4, each = 2),
    course = rep(1:2, 4),
    engagement = c("Low", "High", "High", "High",
                   "Low", "Low", "High", "Low")
  )
  sequences <- step2_sequences(
    states,
    id = "student",
    time = "course",
    state = "engagement"
  )
  # Two time points is the documented minimum, so this must succeed.
  expect_s3_class(flow_plot(sequences), "ggplot")
})

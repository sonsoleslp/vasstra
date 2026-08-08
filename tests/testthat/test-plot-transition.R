# Draw to a throwaway device so the tidy return can be inspected without
# leaving a plot device open.
draw_transition_plot <- function(x, ...) {
  path <- tempfile(fileext = ".png")
  grDevices::png(path)
  on.exit({
    grDevices::dev.off()
    unlink(path)
  }, add = TRUE)
  transition_plot(x, ...)
}

test_that("node sizes map a centrality onto the range, anchored at zero", {
  local_mocked_bindings(
    net_centrality = function(x, measures, loops, ...) {
      data.frame(
        state = c("Low", "Mid", "High"),
        InStrength = c(0, 0.25, 0.5),
        stringsAsFactors = FALSE
      )
    },
    .package = "Nestimate"
  )
  sizes <- .vasstra_transition_node_size(
    network = NULL,
    states = c("Low", "Mid", "High"),
    size = "InStrength",
    loops = FALSE,
    size_range = c(8, 18)
  )
  # Anchored at zero: zero in-strength sits at the floor and half the
  # maximum draws halfway up the range, so sizes compare across plots.
  expect_identical(sizes$node_size, c(8, 13, 18))
  expect_identical(sizes$state, c("Low", "Mid", "High"))
  expect_identical(unique(sizes$measure), "InStrength")
})

test_that("node sizing handles degenerate and unavailable measures", {
  local_mocked_bindings(
    net_centrality = function(x, measures, loops, ...) {
      data.frame(
        state = c("A", "B"),
        InStrength = c(0, 0),
        stringsAsFactors = FALSE
      )
    },
    .package = "Nestimate"
  )
  # An all-zero measure must not divide by zero.
  flat <- .vasstra_transition_node_size(
    NULL, c("A", "B"), "InStrength", FALSE, c(8, 18)
  )
  expect_identical(flat$node_size, c(13, 13))

  expect_error(
    .vasstra_transition_node_size(
      NULL, c("A", "B"), "Betweenness", FALSE, c(8, 18)
    ),
    "available measures"
  )
})

test_that("size = 'none' draws one uniform node size", {
  uniform <- .vasstra_transition_node_size(
    network = NULL,
    states = c("A", "B", "C"),
    size = "none",
    loops = FALSE,
    size_range = c(8, 18)
  )
  expect_identical(uniform$node_size, rep(13, 3L))
  expect_true(all(is.na(uniform$value)))
})

test_that("sequence colors are realigned to Nestimate's state order", {
  # Nestimate sorts states alphabetically and assigns state_colors
  # positionally, ignoring factor levels, so colors supplied in VaSSTra's
  # state order must be remapped by name.
  states <- c("Zeta", "Mid", "Alpha")
  colors <- c("#FF0000", "#00FF00", "#0000FF")
  data <- data.frame(
    T1 = c("Zeta", "Alpha", "Mid"),
    T2 = c("Zeta", "Alpha", "Mid"),
    stringsAsFactors = FALSE
  )
  expect_identical(
    .vasstra_sequence_colors(colors, states, data),
    c("#0000FF", "#00FF00", "#FF0000")
  )
  # NULL means "let Nestimate choose", and must stay NULL.
  expect_null(.vasstra_sequence_colors(NULL, states, data))
  # A subset that never visits a state drops that state's color only.
  partial <- data.frame(T1 = c("Zeta", "Mid"), T2 = c("Mid", "Mid"),
                        stringsAsFactors = FALSE)
  expect_identical(
    .vasstra_sequence_colors(colors, states, partial),
    c("#00FF00", "#FF0000")
  )
})

test_that("the sequence panel argument resolves to a plot type", {
  expect_null(.vasstra_sequence_panel(FALSE))
  expect_null(.vasstra_sequence_panel(NULL))
  expect_identical(.vasstra_sequence_panel(TRUE), "index")
  expect_identical(.vasstra_sequence_panel("heatmap"), "heatmap")
  expect_identical(.vasstra_sequence_panel("distribution"), "distribution")
  expect_error(.vasstra_sequence_panel(3), "TRUE, FALSE")
  expect_error(.vasstra_sequence_panel("nope"))
})

test_that("group labels are validated against the fitted trajectories", {
  expect_identical(.vasstra_check_group("B", c("A", "B")), "B")
  expect_error(.vasstra_check_group("Z", c("A", "B")), "must be one")
  expect_error(.vasstra_check_group(c("A", "B"), c("A", "B")), "must be one")
  expect_error(.vasstra_check_group(NA, c("A", "B")), "must be one")
})

test_that("transition_centrality returns a tidy row per state", {
  fit <- vasstra_test_fit()
  tidy <- transition_centrality(fit)

  expect_s3_class(tidy, "data.frame")
  expect_identical(names(tidy), c("state", "InStrength", "OutStrength"))
  expect_setequal(tidy$state, fit$sequences$states)
  expect_true(all(vapply(
    tidy[c("InStrength", "OutStrength")], is.numeric, logical(1L)
  )))
  # Row names must not leak the network's node names into the tidy table.
  expect_identical(row.names(tidy), as.character(seq_len(nrow(tidy))))
})

test_that("transition_centrality honours measures, weights, and loops", {
  fit <- vasstra_test_fit()

  # Raw counts are whole transitions; probabilities are not.
  counts <- transition_centrality(fit, weights = "count")
  expect_true(all(counts$InStrength == round(counts$InStrength)))

  every <- transition_centrality(fit, measures = "all")
  expect_true(all(
    c("InStrength", "OutStrength", "Betweenness") %in% names(every)
  ))

  plain <- transition_centrality(fit, measures = "InStrength")
  looped <- transition_centrality(fit, measures = "InStrength", loops = TRUE)
  # Self-transitions can only add incoming weight.
  expect_true(all(looped$InStrength >= plain$InStrength))
  expect_error(transition_centrality(fit, loops = NA), "TRUE or FALSE")
})

test_that("transition_centrality reaches every supported input", {
  fit <- vasstra_test_fit()
  label <- fit$trajectories$settings$labels[[1L]]

  from_sequences <- transition_centrality(fit$sequences)
  from_trajectories <- transition_centrality(fit$trajectories)
  # A complete fit and its sequences describe the same transition network.
  expect_equal(from_sequences$InStrength, from_trajectories$InStrength)

  grouped <- transition_centrality(fit, group = label)
  # One trajectory need not visit every state.
  expect_true(nrow(grouped) <= nrow(from_sequences))
  expect_error(transition_centrality(fit, group = "nope"), "must be one")
})

test_that("transition_plot draws and returns the tidy node table", {
  skip_if_not_installed("cograph")
  fit <- vasstra_test_fit()
  drawn <- draw_transition_plot(fit)

  expect_s3_class(drawn, "data.frame")
  expect_identical(names(drawn), c("state", "measure", "value", "node_size"))
  expect_setequal(drawn$state, fit$sequences$states)
  expect_identical(unique(drawn$measure), "InStrength")
  expect_true(all(drawn$node_size >= 8 & drawn$node_size <= 18))
  # The largest in-strength is drawn at the top of the range.
  expect_equal(max(drawn$node_size), 18)
  # The plotted values must agree with the standalone accessor.
  expect_equal(
    drawn$value[order(drawn$state)],
    transition_centrality(fit, measures = "InStrength")$InStrength[
      order(transition_centrality(fit, measures = "InStrength")$state)
    ]
  )
})

test_that("transition_plot honours weights, measure, loops, and range", {
  skip_if_not_installed("cograph")
  fit <- vasstra_test_fit()

  counts <- draw_transition_plot(fit, weights = "count",
                                 size = "OutStrength")
  expect_identical(unique(counts$measure), "OutStrength")
  expect_true(all(counts$value == round(counts$value)))

  ranged <- draw_transition_plot(fit, size_range = c(5, 10))
  expect_true(all(ranged$node_size >= 5 & ranged$node_size <= 10))

  uniform <- draw_transition_plot(fit, size = "none")
  expect_identical(length(unique(uniform$node_size)), 1L)
})

test_that("transition_plot draws sequences beside the network", {
  skip_if_not_installed("cograph")
  fit <- vasstra_test_fit()
  before <- graphics::par("mfrow")

  paired <- draw_transition_plot(fit, sequences = TRUE)
  expect_s3_class(paired, "data.frame")
  # The side-by-side layout must not leak out of the call.
  expect_identical(graphics::par("mfrow"), before)

  # Every sequence view, and a group subset, must compose the same way.
  expect_s3_class(draw_transition_plot(fit, sequences = "heatmap"),
                  "data.frame")
  expect_s3_class(
    draw_transition_plot(
      fit,
      sequences = TRUE,
      group = fit$trajectories$settings$labels[[1L]]
    ),
    "data.frame"
  )
  # Pairing changes only the layout, never the numbers.
  expect_equal(paired$value, draw_transition_plot(fit)$value)
  expect_error(draw_transition_plot(fit, sequences = 3), "TRUE, FALSE")
})

test_that("transition_plot validates its arguments", {
  skip_if_not_installed("cograph")
  fit <- vasstra_test_fit()
  expect_error(draw_transition_plot(fit, loops = NA), "TRUE or FALSE")
  expect_error(draw_transition_plot(fit, size_range = c(20, 5)),
               "increasing")
  expect_error(draw_transition_plot(fit, size_range = 8), "increasing")
  expect_error(draw_transition_plot(fit, group = "no such group"),
               "must be one")
  expect_error(draw_transition_plot(fit, size = 42), "centrality measure")
  expect_error(draw_transition_plot(fit, colors = c("red", "blue")),
               "one color per state")
})

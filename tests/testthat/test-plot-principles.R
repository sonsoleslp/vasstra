test_that("state plots preserve positional arguments and return exact data", {
  data <- make_vasstra_data()
  variables <- c("views", "sessions", "duration")
  labels <- c("Low", "Average", "High")
  states <- step1_states(
    data,
    id = "student",
    time = "course",
    variables = variables,
    n_states = 3,
    labels = labels,
    seed = 211L
  )
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(grDevices::dev.off(), add = TRUE)

  state_plot_method <- utils::getS3method("plot", "vasstra_states")
  expect_identical(
    names(formals(state_plot_method)),
    c("x", "colors", "main", "...", "type", "scale")
  )

  positional <- withVisible(plot(
    states,
    c("#1b9e77", "#d95f02", "#7570b3"),
    "Positional state title"
  ))
  standardized_profile <- withVisible(plot(
    states,
    type = "profile",
    scale = "standardized",
    cex = 0.8,
    xlab = "Custom indicator axis",
    ylab = "Custom profile axis"
  ))
  original_profile <- withVisible(plot(
    states,
    type = "profile",
    scale = "original"
  ))
  standardized_heatmap <- withVisible(plot(
    states,
    type = "heatmap",
    scale = "standardized"
  ))
  original_heatmap <- withVisible(plot(
    states,
    type = "heatmap",
    scale = "original",
    xlab = "Custom heatmap x",
    ylab = "Custom heatmap y"
  ))
  sizes <- withVisible(plot(
    states,
    type = "sizes",
    scale = "original",
    xlab = "Custom size x",
    ylab = "Custom size y"
  ))

  expect_false(positional$visible)
  expect_false(standardized_profile$visible)
  expect_false(original_profile$visible)
  expect_false(standardized_heatmap$visible)
  expect_false(original_heatmap$visible)
  expect_false(sizes$visible)

  expected_standardized <- matrix(
    states$profiles$mean,
    nrow = length(variables),
    ncol = length(labels),
    byrow = TRUE,
    dimnames = list(variables, labels)
  )
  state_values <- as.character(states$data$state)
  expected_original <- vapply(labels, function(label) {
    colMeans(
      states$data[state_values == label, variables, drop = FALSE]
    )
  }, numeric(length(variables)))
  rownames(expected_original) <- variables
  expected_sizes <- data.frame(
    state = factor(labels, levels = labels, ordered = TRUE),
    n = as.integer(states$diagnostics$state_sizes),
    proportion = as.integer(states$diagnostics$state_sizes) / nrow(data),
    stringsAsFactors = FALSE
  )

  expect_equal(
    as.numeric(positional$value),
    as.numeric(expected_standardized),
    tolerance = 1e-12
  )
  expect_equal(
    as.numeric(standardized_profile$value),
    as.numeric(expected_standardized),
    tolerance = 1e-12
  )
  expect_equal(
    dimnames(standardized_profile$value),
    list(variable = variables, state = labels)
  )
  expect_equal(
    original_profile$value,
    expected_original,
    tolerance = 1e-12
  )
  expect_equal(
    as.numeric(standardized_heatmap$value),
    as.numeric(expected_standardized),
    tolerance = 1e-12
  )
  expect_equal(
    original_heatmap$value,
    expected_original,
    tolerance = 1e-12
  )
  expect_identical(sizes$value, expected_sizes)

  expect_error(
    plot(states, colors = "red", type = "profile"),
    "one color per state"
  )
  expect_error(
    plot(states, colors = "red", type = "sizes"),
    "one color per state"
  )
  expect_silent(
    plot(states, colors = "red", type = "heatmap")
  )
  expect_error(
    plot(states, type = "unknown"),
    "should be one of"
  )
  expect_error(
    plot(states, scale = "unknown"),
    "should be one of"
  )
})

test_that("state choice plots return candidates and recompute metric optima", {
  skip_if_not_installed("mclust")
  data <- make_vasstra_data()
  choices <- state_choices(
    data,
    id = "student",
    time = "course",
    variables = c("views", "sessions", "duration"),
    n_states = 2:4,
    method = c("kmeans", "pam", "lpa"),
    lpa_model = "EEI",
    lpa_criterion = "bic",
    seed = 223L,
    minimum_size = 40L
  )
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(grDevices::dev.off(), add = TRUE)

  state_choice_plot_method <- utils::getS3method(
    "plot",
    "vasstra_state_choices"
  )
  expect_identical(
    names(formals(state_choice_plot_method)),
    c(
      "x", "metric", "colors", "main",
      "groups", "show_legend", "..."
    )
  )

  positional <- withVisible(plot(
    choices,
    "silhouette",
    c("#1b9e77", "#d95f02", "#7570b3"),
    "Positional state-choice title",
    xlab = "Custom candidate count",
    ylab = "Custom state metric"
  ))
  metrics <- c(
    "silhouette", "bic", "aic", "classification_entropy",
    "mean_uncertainty", "min_size", "size_ratio",
    "bic_native", "icl_native"
  )
  plotted <- lapply(metrics, function(metric_name) {
    withVisible(plot(
      choices,
      metric = metric_name,
      main = paste("State metric:", metric_name)
    ))
  })

  expect_false(positional$visible)
  expect_true(all(vapply(
    plotted,
    function(plot_result) !plot_result$visible,
    logical(1L)
  )))
  expect_named(
    positional$value,
    c(
      "candidate_id", "size", "group", "value",
      "eligible", "stored_recommended", "is_metric_optimal"
    )
  )

  group <- ifelse(
    choices$candidates$method == "lpa",
    paste0("lpa/", choices$candidates$lpa_model),
    choices$candidates$method
  )
  higher_is_better <- c(
    "silhouette", "classification_entropy", "min_size",
    "bic_native", "icl_native"
  )
  invisible(Map(function(plot_result, metric_name) {
    keep <- choices$candidates$status == "ok" &
      is.finite(choices$candidates[[metric_name]]) &
      !is.na(group)
    expected <- data.frame(
      candidate_id = choices$candidates$candidate_id[keep],
      size = choices$candidates$n_states[keep],
      group = group[keep],
      value = choices$candidates[[metric_name]][keep],
      eligible = choices$candidates$eligible[keep],
      stored_recommended = choices$candidates$is_recommended[keep],
      stringsAsFactors = FALSE
    )
    best_ids <- unlist(lapply(
      split(seq_len(nrow(expected)), expected$group),
      function(indices) {
        eligible_indices <- indices[expected$eligible[indices]]
        scores <- expected$value[eligible_indices]
        ordered_scores <- if (metric_name %in% higher_is_better) {
          -scores
        } else {
          scores
        }
        expected$candidate_id[eligible_indices][order(
          ordered_scores,
          expected$size[eligible_indices],
          expected$candidate_id[eligible_indices]
        )][[1L]]
      }
    ), use.names = FALSE)
    expected$is_metric_optimal <- expected$candidate_id %in% best_ids

    expect_identical(plot_result$value, expected, info = metric_name)
    expect_true(all(
      plot_result$value$eligible[
        plot_result$value$is_metric_optimal
      ]
    ), info = metric_name)
  }, plotted, metrics))

  aic_plot <- plotted[[match("aic", metrics)]]$value
  lpa_aic <- aic_plot[aic_plot$group == "lpa/EEI", , drop = FALSE]
  expect_equal(
    lpa_aic$value[lpa_aic$is_metric_optimal],
    min(lpa_aic$value[lpa_aic$eligible]),
    tolerance = 1e-12
  )
  min_size_plot <- plotted[[match("min_size", metrics)]]$value
  expect_true(all(
    min_size_plot$size[min_size_plot$is_metric_optimal] == 2L
  ))
  expect_true(all(choices$recommendations$n_states == 3L))

  filtered <- withVisible(plot(
    choices,
    metric = "silhouette",
    groups = "lpa/EEI",
    colors = "#7570b3",
    show_legend = FALSE,
    xlab = "Filtered state count",
    ylab = "Filtered score"
  ))
  expect_false(filtered$visible)
  expect_identical(unique(filtered$value$group), "lpa/EEI")
  expect_identical(
    filtered$value$candidate_id,
    choices$candidates$candidate_id[
      choices$candidates$method == "lpa"
    ]
  )

  classification_info <- getFromNamespace(
    ".vasstra_metric_info",
    "VaSStra"
  )("classification_entropy")
  expect_identical(
    classification_info$label,
    "Normalized classification certainty"
  )
  expect_identical(classification_info$direction, "higher is better")

  expect_error(
    plot(choices, metric = "silhouette", colors = "red"),
    "one color per displayed choice group"
  )
  expect_error(
    plot(choices, metric = "unknown"),
    "should be one of"
  )
  expect_error(
    plot(choices, groups = "not a group"),
    "Unknown plot group"
  )
  expect_error(
    plot(choices, groups = character(0L)),
    "one or more non-empty"
  )
  hard_choices <- choices
  hard_choices$candidates <- hard_choices$candidates[
    hard_choices$candidates$method != "lpa",
    ,
    drop = FALSE
  ]
  expect_error(
    plot(hard_choices, metric = "bic"),
    "No successful candidates"
  )
})

test_that("trajectory choice plots return exact metric-specific optima", {
  data <- make_vasstra_data()
  sequences <- step2_sequences(
    data,
    id = "student",
    time = "course",
    state = "true_state"
  )
  choices <- trajectory_choices(
    sequences,
    n_trajectories = 2:4,
    dissimilarity = c("hamming", "lcs"),
    method = c("pam", "ward.D2"),
    seed = 227L,
    minimum_size = 10L
  )
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(grDevices::dev.off(), add = TRUE)

  trajectory_choice_plot_method <- utils::getS3method(
    "plot",
    "vasstra_trajectory_choices"
  )
  expect_identical(
    names(formals(trajectory_choice_plot_method)),
    c(
      "x", "metric", "colors", "main",
      "groups", "show_legend", "..."
    )
  )

  positional <- withVisible(plot(
    choices,
    "silhouette",
    c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"),
    "Positional trajectory-choice title",
    xlab = "Custom trajectory count",
    ylab = "Custom trajectory metric"
  ))
  metrics <- c(
    "silhouette", "mean_within_distance", "min_size", "size_ratio"
  )
  plotted <- lapply(metrics, function(metric_name) {
    if (identical(metric_name, "mean_within_distance")) {
      plot_result <- NULL
      expect_warning(
        plot_result <- withVisible(plot(
          choices,
          metric = metric_name,
          main = paste("Trajectory metric:", metric_name)
        )),
        "comparable only within the same dissimilarity"
      )
      plot_result
    } else {
      withVisible(plot(
        choices,
        metric = metric_name,
        main = paste("Trajectory metric:", metric_name)
      ))
    }
  })

  expect_false(positional$visible)
  expect_true(all(vapply(
    plotted,
    function(plot_result) !plot_result$visible,
    logical(1L)
  )))
  expect_named(
    positional$value,
    c(
      "candidate_id", "size", "group", "value",
      "eligible", "stored_recommended", "is_metric_optimal"
    )
  )

  group <- paste(
    choices$candidates$dissimilarity,
    choices$candidates$method,
    sep = " + "
  )
  invisible(Map(function(plot_result, metric_name) {
    keep <- choices$candidates$status == "ok" &
      is.finite(choices$candidates[[metric_name]])
    expected <- data.frame(
      candidate_id = choices$candidates$candidate_id[keep],
      size = choices$candidates$n_trajectories[keep],
      group = group[keep],
      value = choices$candidates[[metric_name]][keep],
      eligible = choices$candidates$eligible[keep],
      stored_recommended = choices$candidates$is_recommended[keep],
      stringsAsFactors = FALSE
    )
    best_ids <- unlist(lapply(
      split(seq_len(nrow(expected)), expected$group),
      function(indices) {
        eligible_indices <- indices[expected$eligible[indices]]
        scores <- expected$value[eligible_indices]
        ordered_scores <- if (metric_name %in%
                              c("silhouette", "min_size")) {
          -scores
        } else {
          scores
        }
        expected$candidate_id[eligible_indices][order(
          ordered_scores,
          expected$size[eligible_indices],
          expected$candidate_id[eligible_indices]
        )][[1L]]
      }
    ), use.names = FALSE)
    expected$is_metric_optimal <- expected$candidate_id %in% best_ids

    expect_identical(plot_result$value, expected, info = metric_name)
    expect_true(all(
      plot_result$value$eligible[
        plot_result$value$is_metric_optimal
      ]
    ), info = metric_name)
  }, plotted, metrics))

  min_size_plot <- plotted[[match("min_size", metrics)]]$value
  expect_true(all(
    min_size_plot$size[min_size_plot$is_metric_optimal] == 2L
  ))
  expect_true(all(
    choices$recommendations$n_trajectories == 3L
  ))

  filtered <- NULL
  expect_silent(
    filtered <- withVisible(plot(
      choices,
      metric = "mean_within_distance",
      groups = c("lcs + pam", "lcs + ward.D2"),
      show_legend = FALSE,
      xlab = "Filtered trajectory count",
      ylab = "Filtered distance"
    ))
  )
  expect_false(filtered$visible)
  expect_setequal(
    unique(filtered$value$group),
    c("lcs + pam", "lcs + ward.D2")
  )

  expect_error(
    plot(choices, metric = "silhouette", colors = "red"),
    "one color per displayed choice group"
  )
  expect_error(
    plot(choices, metric = "unknown"),
    "should be one of"
  )
  expect_error(
    plot(choices, groups = "not a group"),
    "Unknown plot group"
  )

  many_choices <- trajectory_choices(
    sequences,
    n_trajectories = 3L,
    dissimilarity = c("hamming", "lcs", "osa", "lv", "dl"),
    method = c("pam", "ward.D2", "complete"),
    seed = 229L
  )
  many_result <- NULL
  expect_warning(
    many_result <- withVisible(plot(
      many_choices,
      metric = "silhouette"
    )),
    "15 choice groups.*legend was suppressed"
  )
  expect_false(many_result$visible)
  expect_equal(length(unique(many_result$value$group)), 15L)
  expect_silent(plot(
    many_choices,
    metric = "silhouette",
    show_legend = FALSE
  ))
})

#' Plot Estimated VaSStra States
#'
#' @param x A `vasstra_states` object.
#' @param colors Optional colors, one per state. Used by profile, bar, and
#'   size plots; heatmaps use a value-based sequential or diverging palette.
#' @param main Optional plot title. A type-specific title is used by default.
#' @param type One of `"profile"` (default), `"heatmap"`, `"bars"`,
#'   `"sizes"`, or `"all"` for a two-by-two overview of the four types.
#' @param scale Show state profiles on the `"standardized"` (default) or
#'   `"original"` indicator scale. Ignored by `type = "sizes"`.
#' @param ... Additional graphical arguments passed to [graphics::matplot()],
#'   [graphics::image()], or [graphics::barplot()].
#'
#' @return For profile, bar, heatmap, and overview plots, the plotted
#'   indicator-by-state matrix, invisibly. For a size plot, a tidy
#'   state-size data frame.
#' @export
plot.vasstra_states <- function(
    x,
    colors = NULL,
    main = NULL,
    ...,
    type = c("profile", "heatmap", "bars", "sizes", "all"),
    scale = c("standardized", "original")) {
  stopifnot(inherits(x, "vasstra_states"))
  dots <- list(...)
  type <- match.arg(type)
  scale <- match.arg(scale)
  labels <- x$settings$labels
  variables <- x$settings$variables
  if (is.null(main)) {
    main <- switch(
      type,
      profile = if (identical(scale, "standardized")) {
        "State profiles"
      } else {
        "State profiles (original scale)"
      },
      heatmap = sprintf("State profile heatmap (%s scale)", scale),
      bars = sprintf("State profile bars (%s scale)", scale),
      sizes = "State sizes",
      all = NULL
    )
  }
  if (is.null(colors) && !identical(type, "heatmap")) {
    colors <- .vasstra_palette(length(labels))
  }
  if (!identical(type, "heatmap") && length(colors) != length(labels)) {
    stop("`colors` must contain one color per state.", call. = FALSE)
  }
  old_par <- .vasstra_style_par()
  on.exit(graphics::par(old_par), add = TRUE)
  if (identical(type, "all")) {
    old_layout <- graphics::par(mfrow = c(2L, 2L))
    on.exit(graphics::par(old_layout), add = TRUE)
    for (panel in c("profile", "bars", "heatmap", "sizes")) {
      plot(x, colors = colors, type = panel, scale = scale, ...)
    }
    return(invisible(.vasstra_state_profile_matrix(x, scale)))
  }
  if (identical(type, "bars")) {
    old_margins <- graphics::par(mar = c(7.6, 4.3, 2.6, 1.1))
    on.exit(graphics::par(old_margins), add = TRUE)
    profile_matrix <- .vasstra_state_profile_matrix(x, scale)
    heights <- t(profile_matrix)
    y_range <- range(c(0, heights))
    do.call(
      graphics::barplot,
      .vasstra_graphics_arguments(
        list(
          height = heights,
          beside = TRUE,
          names.arg = variables,
          col = colors,
          border = NA,
          las = 2,
          cex.names = 0.8,
          ylim = y_range + diff(y_range) * c(0, 0.18),
          xlab = "",
          ylab = sprintf("Mean value (%s scale)", scale),
          main = main
        ),
        dots
      )
    )
    graphics::abline(h = 0, col = "grey70", lwd = 0.8)
    graphics::legend(
      "topleft",
      legend = labels,
      fill = colors,
      border = NA,
      bty = "n",
      cex = 0.85
    )
    return(invisible(profile_matrix))
  }
  if (identical(type, "sizes")) {
    sizes <- as.integer(x$diagnostics$state_sizes)
    size_data <- data.frame(
      state = factor(labels, levels = labels, ordered = TRUE),
      n = sizes,
      proportion = sizes / sum(sizes),
      stringsAsFactors = FALSE
    )
    positions <- do.call(
      graphics::barplot,
      .vasstra_graphics_arguments(
        list(
          height = sizes,
          names.arg = labels,
          col = colors,
          border = NA,
          ylim = c(0, max(sizes) * 1.2),
          xlab = "State",
          ylab = "Number of observations",
          main = main
        ),
        dots
      )
    )
    graphics::text(
      x = positions,
      y = sizes,
      labels = sprintf("%d\n(%.1f%%)", sizes, 100 * size_data$proportion),
      pos = 3,
      xpd = TRUE
    )
    return(invisible(size_data))
  }
  profile_matrix <- .vasstra_state_profile_matrix(x, scale)
  if (identical(type, "profile")) {
    label_margin <- min(9, 1.2 + 0.5 * max(nchar(labels)))
    old_margins <- graphics::par(mar = c(7.6, 4.3, 2.6, label_margin))
    on.exit(graphics::par(old_margins), add = TRUE)
    value_range <- range(profile_matrix)
    profile_arguments <- .vasstra_graphics_arguments(
      list(
        x = seq_along(variables),
        y = profile_matrix,
        type = "b",
        lty = 1,
        lwd = 2.2,
        pch = 19,
        cex = 0.9,
        col = colors,
        xaxt = "n",
        ylim = value_range + diff(value_range) * c(-0.06, 0.06),
        xlab = "",
        ylab = sprintf("Mean value (%s scale)", scale),
        main = main
      ),
      dots
    )
    frame_arguments <- profile_arguments
    frame_arguments$type <- "n"
    do.call(graphics::matplot, frame_arguments)
    .vasstra_grid_horizontal()
    line_arguments <- profile_arguments[
      names(profile_arguments) %in%
        c("x", "y", "type", "lty", "lwd", "pch", "col", "cex", "bg")
    ]
    do.call(graphics::matplot, c(line_arguments, list(add = TRUE)))
    graphics::axis(
      1,
      at = seq_along(variables),
      labels = variables,
      las = 2,
      cex.axis = 0.75
    )
    end_values <- profile_matrix[length(variables), ]
    label_positions <- .vasstra_spread_positions(
      as.numeric(end_values),
      gap = 0.05 * diff(graphics::par("usr")[3:4])
    )
    graphics::text(
      x = length(variables) + 0.12,
      y = label_positions,
      labels = labels,
      adj = 0,
      cex = 0.85,
      col = rep_len(profile_arguments$col, length(labels)),
      xpd = NA
    )
    return(invisible(profile_matrix))
  }
  color_scale <- .vasstra_heatmap_colors(profile_matrix)
  old_margins <- graphics::par(mar = c(7.6, 6.1, 2.6, 1.1))
  on.exit(graphics::par(old_margins), add = TRUE)
  do.call(
    graphics::image,
    .vasstra_graphics_arguments(
      list(
        x = seq_along(variables),
        y = seq_along(labels),
        z = profile_matrix,
        col = color_scale$colors,
        zlim = color_scale$limits,
        axes = FALSE,
        xlab = "",
        ylab = "",
        main = main
      ),
      dots
    )
  )
  graphics::axis(
    1,
    at = seq_along(variables),
    labels = variables,
    las = 2,
    cex.axis = 0.8
  )
  graphics::axis(2, at = seq_along(labels), labels = labels, las = 1)
  graphics::box(bty = "o")
  graphics::text(
    x = rep(seq_along(variables), times = length(labels)),
    y = rep(seq_along(labels), each = length(variables)),
    labels = formatC(
      as.vector(profile_matrix),
      format = "f",
      digits = 2
    ),
    cex = 0.75,
    col = .vasstra_heatmap_text_colors(
      as.vector(profile_matrix),
      color_scale$limits
    )
  )
  invisible(profile_matrix)
}

.vasstra_graphics_arguments <- function(defaults, supplied) {
  stopifnot(is.list(defaults), is.list(supplied))
  if (length(supplied) == 0L) {
    return(defaults)
  }
  supplied_names <- names(supplied)
  if (is.null(supplied_names)) {
    return(c(defaults, supplied))
  }
  named <- nzchar(supplied_names)
  if (any(named)) {
    defaults[supplied_names[named]] <- supplied[named]
  }
  c(defaults, supplied[!named])
}

.vasstra_state_profile_matrix <- function(
    x,
    scale = c("standardized", "original")) {
  stopifnot(inherits(x, "vasstra_states"))
  scale <- match.arg(scale)
  labels <- x$settings$labels
  variables <- x$settings$variables
  if (identical(scale, "standardized")) {
    profile_data <- x$profiles
    profile_data$variable <- factor(
      profile_data$variable,
      levels = variables
    )
    profile_data$state <- factor(
      profile_data$state,
      levels = labels
    )
    return(stats::xtabs(
      mean ~ variable + state,
      data = profile_data
    ))
  }
  state_values <- as.character(x$data[[x$settings$state]])
  means_by_variable <- vapply(variables, function(variable) {
    vapply(labels, function(label) {
      mean(x$data[[variable]][state_values == label], na.rm = TRUE)
    }, numeric(1L))
  }, numeric(length(labels)))
  profile_matrix <- t(means_by_variable)
  rownames(profile_matrix) <- variables
  colnames(profile_matrix) <- labels
  profile_matrix
}

.vasstra_heatmap_colors <- function(values) {
  stopifnot(is.numeric(values), length(values) > 0L)
  value_minimum <- min(values, na.rm = TRUE)
  value_maximum <- max(values, na.rm = TRUE)
  if (!is.finite(value_minimum) || !is.finite(value_maximum)) {
    stop("Heatmap values must be finite.", call. = FALSE)
  }
  if (value_minimum >= 0 && value_maximum > 0) {
    return(list(
      colors = grDevices::colorRampPalette(
        c("white", "#2166ac")
      )(101L),
      limits = c(0, value_maximum)
    ))
  }
  if (value_maximum <= 0 && value_minimum < 0) {
    return(list(
      colors = grDevices::colorRampPalette(
        c("#b2182b", "white")
      )(101L),
      limits = c(value_minimum, 0)
    ))
  }
  maximum_absolute <- max(abs(c(value_minimum, value_maximum, 1e-12)))
  list(
    colors = grDevices::colorRampPalette(
      c("#b2182b", "white", "#2166ac")
    )(101L),
    limits = c(-maximum_absolute, maximum_absolute)
  )
}

.vasstra_heatmap_text_colors <- function(values, limits) {
  stopifnot(
    is.numeric(values),
    is.numeric(limits),
    length(limits) == 2L,
    all(is.finite(limits))
  )
  maximum_absolute <- max(abs(limits))
  darkness <- if (limits[[1L]] < 0 && limits[[2L]] > 0) {
    abs(values) / maximum_absolute
  } else if (limits[[1L]] >= 0) {
    values / max(limits[[2L]], .Machine$double.eps)
  } else {
    abs(values) / max(abs(limits[[1L]]), .Machine$double.eps)
  }
  ifelse(!is.na(darkness) & darkness >= 0.58, "white", "black")
}

#' Plot State-Clustering Choices
#'
#' Draws a compact model-selection plot from [state_choices()]. Each line is
#' one clustering method (and LPA covariance model). Stars mark the optimal
#' eligible value of the plotted metric within each group; crosses mark
#' candidates that fail the requested size constraints.
#'
#' @param x A `vasstra_state_choices` object.
#' @param metric Candidate metric to plot.
#' @param colors Optional colors, one per displayed method/model.
#' @param main Optional plot title.
#' @param groups Optional character vector of method/model labels to display,
#'   such as `"kmeans"` or `"lpa/EEI"`.
#' @param show_legend Show the group and marker legends.
#' @param ... Additional arguments passed to [graphics::plot()].
#'
#' @return The tidy candidate data used in the plot, invisibly.
#' @export
plot.vasstra_state_choices <- function(
    x,
    metric = c(
      "silhouette", "bic", "aic", "classification_entropy",
      "mean_uncertainty", "min_size", "size_ratio",
      "bic_native", "icl_native"
    ),
    colors = NULL,
    main = NULL,
    groups = NULL,
    show_legend = TRUE,
    ...) {
  stopifnot(inherits(x, "vasstra_state_choices"))
  metric <- match.arg(metric)
  group <- ifelse(
    x$candidates$method == "lpa",
    paste0("lpa/", x$candidates$lpa_model),
    x$candidates$method
  )
  filtered <- .vasstra_filter_plot_groups(
    x$candidates,
    group,
    groups
  )
  metric_info <- .vasstra_metric_info(metric)
  .vasstra_plot_choices(
    candidates = filtered$candidates,
    size_column = "n_states",
    group = filtered$group,
    metric = metric,
    metric_label = metric_info$label,
    direction = metric_info$direction,
    note = metric_info$note,
    colors = colors,
    main = if (is.null(main)) "State-clustering choices" else main,
    show_legend = show_legend,
    dots = list(...)
  )
}

#' Plot Trajectory-Clustering Choices
#'
#' Draws a compact Nestimate model-selection plot from
#' [trajectory_choices()]. Each line is one distance-method combination.
#' Stars mark the optimal eligible value of the plotted metric within each
#' group; crosses mark candidates that fail the requested size constraints.
#'
#' @param x A `vasstra_trajectory_choices` object.
#' @param metric One of `"silhouette"`, `"mean_within_distance"`,
#'   `"min_size"`, or `"size_ratio"`.
#' @param colors Optional colors, one per displayed distance-method pair.
#' @param main Optional plot title.
#' @param groups Optional character vector of distance-method labels to
#'   display, such as `"lcs + ward.D2"`.
#' @param show_legend Show the group and marker legends.
#' @param ... Additional arguments passed to [graphics::plot()].
#'
#' @return The tidy candidate data used in the plot, invisibly.
#' @export
plot.vasstra_trajectory_choices <- function(
    x,
    metric = c(
      "silhouette", "mean_within_distance", "min_size", "size_ratio"
    ),
    colors = NULL,
    main = NULL,
    groups = NULL,
    show_legend = TRUE,
    ...) {
  stopifnot(inherits(x, "vasstra_trajectory_choices"))
  metric <- match.arg(metric)
  group <- paste(
    x$candidates$dissimilarity,
    x$candidates$method,
    sep = " + "
  )
  filtered <- .vasstra_filter_plot_groups(
    x$candidates,
    group,
    groups
  )
  if (identical(metric, "mean_within_distance") &&
      length(unique(filtered$candidates$dissimilarity)) > 1L) {
    warning(
      paste0(
        "Mean within-group distances are comparable only within the same ",
        "dissimilarity; use `groups` to focus the plot."
      ),
      call. = FALSE
    )
  }
  metric_info <- .vasstra_metric_info(metric)
  .vasstra_plot_choices(
    candidates = filtered$candidates,
    size_column = "n_trajectories",
    group = filtered$group,
    metric = metric,
    metric_label = metric_info$label,
    direction = metric_info$direction,
    note = metric_info$note,
    colors = colors,
    main = if (is.null(main)) "Trajectory-clustering choices" else main,
    show_legend = show_legend,
    dots = list(...)
  )
}

.vasstra_filter_plot_groups <- function(candidates, group, groups = NULL) {
  stopifnot(
    is.data.frame(candidates),
    is.character(group),
    length(group) == nrow(candidates)
  )
  available <- unique(group[!is.na(group)])
  if (is.null(groups)) {
    return(list(candidates = candidates, group = group))
  }
  if (!is.character(groups) || length(groups) == 0L ||
      anyNA(groups) || any(!nzchar(groups))) {
    stop("`groups` must contain one or more non-empty group labels.",
         call. = FALSE)
  }
  missing_groups <- setdiff(groups, available)
  if (length(missing_groups) > 0L) {
    stop(sprintf(
      "Unknown plot group%s: %s. Available groups: %s.",
      if (length(missing_groups) == 1L) "" else "s",
      paste(missing_groups, collapse = ", "),
      paste(available, collapse = ", ")
    ), call. = FALSE)
  }
  keep <- group %in% unique(groups)
  list(
    candidates = candidates[keep, , drop = FALSE],
    group = group[keep]
  )
}

.vasstra_metric_info <- function(metric) {
  stopifnot(is.character(metric), length(metric) == 1L)
  information <- list(
    silhouette = list(
      label = "Average silhouette",
      direction = "higher is better",
      note = ""
    ),
    bic = list(
      label = "Conventional BIC",
      direction = "lower is better",
      note = "LPA candidates only"
    ),
    aic = list(
      label = "Conventional AIC",
      direction = "lower is better",
      note = "LPA candidates only"
    ),
    classification_entropy = list(
      label = "Normalized classification certainty",
      direction = "higher is better",
      note = "LPA candidates only"
    ),
    mean_uncertainty = list(
      label = "Mean classification uncertainty",
      direction = "lower is better",
      note = "LPA candidates only"
    ),
    min_size = list(
      label = "Smallest group size",
      direction = "higher is better",
      note = "size diagnostic, not model fit"
    ),
    size_ratio = list(
      label = "Largest-to-smallest size ratio",
      direction = "lower is better",
      note = "balance diagnostic, not model fit"
    ),
    bic_native = list(
      label = "Native mclust BIC",
      direction = "higher is better",
      note = "LPA candidates only"
    ),
    icl_native = list(
      label = "Native mclust ICL",
      direction = "higher is better",
      note = "LPA candidates only"
    ),
    mean_within_distance = list(
      label = "Mean within-group distance",
      direction = "lower is better",
      note = "compare only within one dissimilarity"
    )
  )
  if (!metric %in% names(information)) {
    stop("Unsupported plotting metric.", call. = FALSE)
  }
  information[[metric]]
}

.vasstra_plot_choices <- function(
    candidates,
    size_column,
    group,
    metric,
    metric_label,
    direction,
    note,
    colors,
    main,
    show_legend,
    dots) {
  stopifnot(
    is.data.frame(candidates),
    is.character(size_column),
    length(size_column) == 1L,
    is.character(group),
    length(group) == nrow(candidates),
    is.character(metric),
    length(metric) == 1L,
    is.character(metric_label),
    length(metric_label) == 1L,
    is.character(direction),
    length(direction) == 1L,
    is.character(note),
    length(note) == 1L,
    is.character(main),
    length(main) == 1L,
    is.logical(show_legend),
    length(show_legend) == 1L,
    !is.na(show_legend),
    is.list(dots)
  )
  keep <- candidates$status == "ok" &
    is.finite(candidates[[metric]]) &
    !is.na(group)
  if (!any(keep)) {
    stop(sprintf(
      "No successful candidates have finite `%s` values.",
      metric
    ), call. = FALSE)
  }
  recommendation_candidates <- candidates
  recommendation_candidates$.plot_group <- group
  plot_recommendations <- .vasstra_recommendation_flags(
    recommendation_candidates,
    group_columns = ".plot_group",
    size_column = size_column,
    score_column = metric,
    direction = if (identical(direction, "higher is better")) {
      "max"
    } else {
      "min"
    }
  )
  plot_data <- data.frame(
    candidate_id = candidates$candidate_id[keep],
    size = candidates[[size_column]][keep],
    group = group[keep],
    value = candidates[[metric]][keep],
    eligible = candidates$eligible[keep],
    stored_recommended = candidates$is_recommended[keep],
    is_metric_optimal = plot_recommendations[keep],
    stringsAsFactors = FALSE
  )
  displayed_groups <- unique(plot_data$group)
  if (is.null(colors)) {
    colors <- .vasstra_palette(length(displayed_groups))
  }
  if (length(colors) != length(displayed_groups)) {
    stop("`colors` must contain one color per displayed choice group.",
         call. = FALSE)
  }
  if (length(displayed_groups) > 12L && show_legend) {
    warning(
      sprintf(
        paste0(
          "%d choice groups are displayed, so the legend was suppressed; ",
          "use `groups` to focus the plot."
        ),
        length(displayed_groups)
      ),
      call. = FALSE
    )
    show_legend <- FALSE
  }
  y_limits <- range(plot_data$value)
  if (diff(y_limits) == 0) {
    padding <- max(abs(y_limits[[1L]]) * 0.05, 0.05)
    y_limits <- y_limits + c(-padding, padding)
  }
  x_values <- sort(unique(plot_data$size))
  old_par <- .vasstra_style_par()
  on.exit(graphics::par(old_par), add = TRUE)
  do.call(
    graphics::plot,
    .vasstra_graphics_arguments(
      list(
        x = NA_real_,
        y = NA_real_,
        xlim = range(x_values),
        ylim = y_limits,
        xaxt = "n",
        xlab = if (identical(size_column, "n_states")) {
          "Number of states"
        } else {
          "Number of trajectories"
        },
        ylab = metric_label,
        main = main
      ),
      dots
    )
  )
  .vasstra_grid_horizontal()
  graphics::mtext(
    paste0(direction, if (nzchar(note)) paste0(" | ", note) else ""),
    side = 3,
    line = 0.35,
    cex = 0.75,
    col = "grey40"
  )
  graphics::axis(1, at = x_values, labels = x_values)
  invisible(Map(function(group_name, color) {
    rows <- plot_data$group == group_name
    group_data <- plot_data[rows, , drop = FALSE]
    group_data <- group_data[order(group_data$size), , drop = FALSE]
    graphics::lines(
      group_data$size,
      group_data$value,
      lwd = 2.2,
      col = color
    )
    graphics::points(
      group_data$size,
      group_data$value,
      pch = 19,
      cex = 0.9,
      col = color
    )
    ineligible <- !group_data$eligible
    if (any(ineligible)) {
      graphics::points(
        group_data$size[ineligible],
        group_data$value[ineligible],
        pch = 21,
        bg = "white",
        lwd = 1.6,
        cex = 1,
        col = color
      )
    }
    optimal <- group_data$is_metric_optimal
    if (any(optimal)) {
      graphics::points(
        group_data$size[optimal],
        group_data$value[optimal],
        pch = 21,
        bg = NA,
        cex = 1.8,
        lwd = 1.8,
        col = color
      )
    }
    invisible(NULL)
  }, displayed_groups, colors))
  if (show_legend) {
    graphics::legend(
      "topright",
      legend = displayed_groups,
      col = colors,
      lty = 1,
      lwd = 2.2,
      pch = 19,
      bty = "n",
      cex = 0.8
    )
    graphics::legend(
      "bottomleft",
      legend = c("Metric optimum", "Ineligible"),
      pch = 21,
      pt.cex = c(1.6, 1),
      pt.bg = c(NA, "white"),
      col = "grey30",
      bty = "n",
      cex = 0.8
    )
  }
  invisible(plot_data)
}

#' Plot VaSStra State Sequences with Nestimate
#'
#' Delegates sequence index, distribution, and heatmap rendering to
#' [Nestimate::sequence_plot()].
#'
#' @param x A `vasstra_sequences` object.
#' @param type One of `"heatmap"` (default), `"distribution"`, or
#'   `"index"`. The heatmap shows every aligned sequence at full
#'   resolution and is the most informative single view of the complete
#'   cohort.
#' @param colors Optional colors, one per state, passed to Nestimate as
#'   `state_colors`.
#' @param main Plot title. A type-specific title is used by default.
#' @param sort Sequence ordering passed to [Nestimate::sequence_plot()].
#' @param na Show missing cells as a separate distribution band. By default,
#'   this is `TRUE` only when the sequence data contain `NA`.
#' @param ... Additional arguments passed to [Nestimate::sequence_plot()].
#'
#' @return The value returned by [Nestimate::sequence_plot()].
#' @export
plot.vasstra_sequences <- function(
    x,
    type = c("heatmap", "distribution", "index"),
    colors = NULL,
    main = NULL,
    sort = "lcs",
    na = x$diagnostics$n_missing > 0L,
    ...) {
  stopifnot(inherits(x, "vasstra_sequences"))
  type <- match.arg(type)
  if (is.null(main)) {
    main <- switch(
      type,
      heatmap = "State sequences (all subjects)",
      distribution = "State distribution over time",
      index = "State sequences"
    )
  }
  sort <- match.arg(
    sort,
    c(
      "lcs", "frequency", "start", "end", "hamming", "osa", "lv", "dl",
      "qgram", "cosine", "jaccard", "jw"
    )
  )
  if (!is.logical(na) || length(na) != 1L || is.na(na)) {
    stop("`na` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(colors) && length(colors) != length(x$states)) {
    stop("`colors` must contain one color per state.", call. = FALSE)
  }
  arguments <- c(
    list(
      x = x$data,
      type = type,
      sort = sort,
      na = na,
      state_colors = colors,
      main = main,
      time_label = x$settings$time
    ),
    list(...)
  )
  result <- do.call(
    Nestimate::sequence_plot,
    arguments
  )
  invisible(result)
}

#' Plot VaSStra Trajectories with Nestimate
#'
#' Delegates grouped sequence index, distribution, and heatmap rendering to
#' [Nestimate::sequence_plot()].
#'
#' @param x A `vasstra_trajectories` object.
#' @param type One of `"index"` (default), `"distribution"`, or `"heatmap"`.
#' @param colors Optional colors, one per state, passed to Nestimate as
#'   `state_colors`.
#' @param main Plot title.
#' @param sort Sequence ordering passed to [Nestimate::sequence_plot()].
#'   The default is `"start"` for robust within-trajectory index plots,
#'   including trajectories containing one sequence.
#' @param na Show missing cells as a separate distribution band. By default,
#'   this is `TRUE` only when the sequence data contain `NA`.
#' @param ... Additional arguments passed to [Nestimate::sequence_plot()].
#'
#' @return The value returned by [Nestimate::sequence_plot()].
#' @export
plot.vasstra_trajectories <- function(
    x,
    type = c("index", "distribution", "heatmap"),
    colors = NULL,
    main = "Sequences grouped by trajectory",
    sort = "start",
    na = x$source$diagnostics$n_missing > 0L,
    ...) {
  stopifnot(inherits(x, "vasstra_trajectories"))
  type <- match.arg(type)
  sort <- match.arg(
    sort,
    c(
      "lcs", "frequency", "start", "end", "hamming", "osa", "lv", "dl",
      "qgram", "cosine", "jaccard", "jw"
    )
  )
  if (!is.logical(na) || length(na) != 1L || is.na(na)) {
    stop("`na` must be TRUE or FALSE.", call. = FALSE)
  }
  states <- x$source$states
  if (!is.null(colors) && length(colors) != length(states)) {
    stop("`colors` must contain one color per state.", call. = FALSE)
  }
  arguments <- c(
    list(
      x = x$data,
      type = type,
      sort = sort,
      group = if (type == "heatmap") NULL else x$assignments,
      na = na,
      state_colors = colors,
      main = main,
      time_label = x$source$settings$time
    ),
    list(...)
  )
  result <- do.call(
    Nestimate::sequence_plot,
    arguments
  )
  invisible(result)
}

#' Plot a Clustering Evaluation
#'
#' Draws the selection curve, per-cluster silhouette widths, and cluster
#' sizes for one [evaluate()] result. The default `"summary"` layout places
#' the three panels side by side.
#'
#' @param x A `vasstra_evaluation` object from [evaluate()].
#' @param type One of `"summary"` (default), `"curve"`, `"silhouette"`, or
#'   `"sizes"`.
#' @param colors Optional colors, one per fitted cluster. The selection curve
#'   uses the first color.
#' @param main Optional plot title. A panel-specific title is used by
#'   default.
#' @param ... Additional graphical arguments passed to the panel functions.
#'
#' @return The tidy data drawn by the requested panel, invisibly. The
#'   `"summary"` layout returns both the candidate and cluster tables.
#' @export
plot.vasstra_evaluation <- function(
    x,
    type = c("summary", "curve", "silhouette", "sizes"),
    colors = NULL,
    main = NULL,
    ...) {
  stopifnot(inherits(x, "vasstra_evaluation"))
  type <- match.arg(type)
  n_clusters <- nrow(x$clusters)
  if (is.null(colors)) {
    colors <- .vasstra_palette(n_clusters)
  }
  if (length(colors) != n_clusters) {
    stop("`colors` must contain one color per fitted cluster.",
         call. = FALSE)
  }
  old_par <- .vasstra_style_par()
  on.exit(graphics::par(old_par), add = TRUE)
  unit_title <- sprintf(
    "%s%s",
    toupper(substring(x$unit, 1L, 1L)),
    substring(x$unit, 2L)
  )
  titles <- list(
    curve = sprintf("%s: selection curve (%s)", unit_title,
                    x$fitted$method),
    silhouette = sprintf("%s: silhouette by group", unit_title),
    sizes = sprintf("%s: group sizes", unit_title)
  )
  if (type == "summary") {
    old_layout <- graphics::par(mfrow = c(1L, 3L))
    on.exit(graphics::par(old_layout), add = TRUE)
    .vasstra_evaluation_curve(x, colors[[1L]], titles$curve, ...)
    .vasstra_evaluation_silhouette(x, colors, titles$silhouette, ...)
    .vasstra_evaluation_sizes(x, colors, titles$sizes, ...)
    return(invisible(list(candidates = x$candidates, clusters = x$clusters)))
  }
  panel_title <- if (is.null(main)) titles[[type]] else main
  switch(
    type,
    curve = .vasstra_evaluation_curve(x, colors[[1L]], panel_title, ...),
    silhouette = .vasstra_evaluation_silhouette(
      x, colors, panel_title, ...
    ),
    sizes = .vasstra_evaluation_sizes(x, colors, panel_title, ...)
  )
}

#' Plot the Evaluations of a Complete Fit
#'
#' Draws one row of evaluation panels per clustering step: the state
#' clustering on top and the trajectory clustering below.
#'
#' @param x A `vasstra_evaluations` object from [evaluate()] on a complete
#'   `vasstra` fit.
#' @param colors Optional colors recycled within each evaluation row.
#' @param ... Additional graphical arguments passed to the panel functions.
#'
#' @return The evaluated candidate and cluster tables, invisibly.
#' @export
plot.vasstra_evaluations <- function(x, colors = NULL, ...) {
  stopifnot(inherits(x, "vasstra_evaluations"))
  old_par <- .vasstra_style_par()
  on.exit(graphics::par(old_par), add = TRUE)
  old_layout <- graphics::par(mfrow = c(length(x), 3L))
  on.exit(graphics::par(old_layout), add = TRUE)
  drawn <- lapply(x, function(evaluation) {
    n_clusters <- nrow(evaluation$clusters)
    panel_colors <- if (is.null(colors)) {
      .vasstra_palette(n_clusters)
    } else {
      rep_len(colors, n_clusters)
    }
    unit_title <- sprintf(
      "%s%s",
      toupper(substring(evaluation$unit, 1L, 1L)),
      substring(evaluation$unit, 2L)
    )
    .vasstra_evaluation_curve(
      evaluation,
      panel_colors[[1L]],
      sprintf("%s: selection curve (%s)", unit_title,
              evaluation$fitted$method),
      ...
    )
    .vasstra_evaluation_silhouette(
      evaluation,
      panel_colors,
      sprintf("%s: silhouette by group", unit_title),
      ...
    )
    .vasstra_evaluation_sizes(
      evaluation,
      panel_colors,
      sprintf("%s: group sizes", unit_title),
      ...
    )
    list(
      candidates = evaluation$candidates,
      clusters = evaluation$clusters
    )
  })
  invisible(drawn)
}

#' Plot a Complete VaSStra Analysis
#'
#' @param x A `vasstra` object.
#' @param which One of `"trajectories"`, `"states"`, or `"sequences"`.
#' @param ... Passed to the selected step's plot method. Sequence and
#'   trajectory plots are rendered by Nestimate.
#'
#' @return The selected plot method's invisible result.
#' @export
plot.vasstra <- function(
    x,
    which = c("trajectories", "states", "sequences"),
    ...) {
  stopifnot(inherits(x, "vasstra"))
  which <- match.arg(which)
  if (which == "states") {
    if (is.null(x$states)) {
      stop(
        "This fit used precomputed states and has no step-1 profile plot.",
        call. = FALSE
      )
    }
    return(plot(x$states, ...))
  }
  if (which == "sequences") {
    return(plot(x$sequences, ...))
  }
  plot(x$trajectories, ...)
}

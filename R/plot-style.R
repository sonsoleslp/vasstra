# Shared palette and panel styling for VaSStra base-graphics plots.

.vasstra_palette <- function(n) {
  stopifnot(is.numeric(n), length(n) == 1L, n >= 1L)
  base_colors <- c(
    "#0072B2", "#D55E00", "#009E73", "#CC79A7",
    "#E69F00", "#56B4E9", "#999999", "#F0E442"
  )
  if (n <= length(base_colors)) {
    return(base_colors[seq_len(n)])
  }
  grDevices::hcl.colors(n, "Dark 3")
}

.vasstra_accent <- "#0072B2"
.vasstra_accent_secondary <- "#D55E00"

# Nestimate's sequence_plot() sorts states alphabetically and assigns
# `state_colors` positionally to that order, ignoring factor levels. VaSStra
# takes colors in its own state order, so they must be re-aligned by name or
# every state is drawn in another state's color.
.vasstra_sequence_colors <- function(colors, states, sequence_data) {
  if (is.null(colors)) {
    return(NULL)
  }
  stopifnot(is.character(states), length(colors) == length(states))
  named <- stats::setNames(as.character(colors), states)
  observed <- sort(unique(as.character(unlist(
    sequence_data,
    use.names = FALSE
  ))))
  observed <- observed[!is.na(observed)]
  unname(named[observed])
}

# Apply the shared axis and title style; callers restore the returned value.
.vasstra_style_par <- function() {
  graphics::par(
    mgp = c(2.4, 0.6, 0),
    tcl = -0.25,
    las = 1,
    bty = "L",
    cex.axis = 0.9,
    col.axis = "grey25",
    col.lab = "grey15",
    font.main = 1,
    cex.main = 1.1,
    col.main = "grey10"
  )
}

.vasstra_grid_horizontal <- function() {
  limits <- graphics::par("usr")
  graphics::abline(
    h = pretty(limits[3:4], n = 5L),
    col = "grey92",
    lwd = 0.8
  )
}

# Nudge overlapping label positions apart while preserving their order.
.vasstra_spread_positions <- function(values, gap) {
  stopifnot(is.numeric(values), is.numeric(gap), length(gap) == 1L)
  if (length(values) <= 1L) {
    return(values)
  }
  ordered_index <- order(values)
  sorted <- values[ordered_index]
  spread <- Reduce(
    function(done, value) c(done, max(value, done[[length(done)]] + gap)),
    sorted[-1L],
    init = sorted[[1L]]
  )
  result <- numeric(length(values))
  result[ordered_index] <- spread
  result
}

# One evaluation panel: the selection curve of the compared metric.
.vasstra_evaluation_curve <- function(x, color, main, ...) {
  stopifnot(inherits(x, "vasstra_evaluation"))
  candidates <- x$candidates
  counts <- candidates[[x$size_column]]
  values <- candidates$silhouette
  finite <- is.finite(values)
  y_limits <- range(values[finite])
  if (diff(y_limits) == 0) {
    y_limits <- y_limits + c(-0.05, 0.05)
  } else {
    y_limits <- y_limits + diff(y_limits) * c(-0.1, 0.18)
  }
  graphics::plot(
    NA_real_,
    NA_real_,
    xlim = range(counts),
    ylim = y_limits,
    xaxt = "n",
    xlab = sprintf("Number of %s", x$unit),
    ylab = "Average silhouette",
    main = main,
    ...
  )
  .vasstra_grid_horizontal()
  graphics::axis(1, at = sort(unique(counts)))
  ordered <- order(counts)
  graphics::lines(
    counts[ordered],
    values[ordered],
    col = color,
    lwd = 2.2
  )
  graphics::points(
    counts[ordered],
    values[ordered],
    pch = 19,
    col = color,
    cex = 0.95
  )
  best <- candidates$best & finite
  if (any(best)) {
    graphics::points(
      counts[best],
      values[best],
      pch = 21,
      bg = NA,
      col = color,
      lwd = 1.8,
      cex = 1.8
    )
  }
  fitted_rows <- candidates$fitted & finite
  if (any(fitted_rows)) {
    graphics::points(
      counts[fitted_rows],
      values[fitted_rows],
      pch = 19,
      col = .vasstra_accent_secondary,
      cex = 1.15
    )
    graphics::points(
      counts[fitted_rows],
      values[fitted_rows],
      pch = 21,
      bg = NA,
      col = .vasstra_accent_secondary,
      lwd = 1.8,
      cex = 1.8
    )
    graphics::text(
      counts[fitted_rows],
      values[fitted_rows],
      labels = "fitted",
      pos = 3,
      offset = 0.75,
      cex = 0.8,
      col = .vasstra_accent_secondary
    )
  }
  invisible(candidates)
}

# One evaluation panel: mean silhouette width per fitted cluster.
.vasstra_evaluation_silhouette <- function(x, colors, main, ...) {
  stopifnot(inherits(x, "vasstra_evaluation"))
  clusters <- x$clusters
  labels <- as.character(clusters[[1L]])
  values <- clusters$silhouette
  x_limits <- range(c(0, values, x$fitted$silhouette))
  x_limits <- x_limits + diff(x_limits) * c(0, 0.22)
  positions <- graphics::barplot(
    rev(values),
    names.arg = rev(labels),
    horiz = TRUE,
    col = rev(colors),
    border = NA,
    xlim = x_limits,
    xlab = "Mean silhouette width",
    main = main,
    ...
  )
  graphics::abline(
    v = x$fitted$silhouette,
    col = "grey40",
    lty = 3,
    lwd = 1.2
  )
  inside <- rev(values) > 0.3 * max(x_limits)
  graphics::text(
    x = rev(values),
    y = positions,
    labels = formatC(rev(values), format = "f", digits = 2),
    pos = ifelse(inside, 2L, 4L),
    cex = 0.8,
    col = ifelse(inside, "white", "grey25"),
    xpd = TRUE
  )
  invisible(clusters)
}

# One evaluation panel: fitted cluster sizes with counts and percentages.
.vasstra_evaluation_sizes <- function(x, colors, main, ...) {
  stopifnot(inherits(x, "vasstra_evaluation"))
  clusters <- x$clusters
  labels <- as.character(clusters[[1L]])
  counts <- clusters$n
  positions <- graphics::barplot(
    counts,
    names.arg = labels,
    col = colors,
    border = NA,
    ylim = c(0, max(counts) * 1.22),
    ylab = "Group size",
    main = main,
    ...
  )
  graphics::text(
    x = positions,
    y = counts,
    labels = sprintf("%d (%.0f%%)", counts, 100 * clusters$proportion),
    pos = 3,
    cex = 0.8,
    col = "grey25",
    xpd = TRUE
  )
  invisible(clusters)
}

# Flow visualizations of state sequences, delegated to cograph.
#
# Ownership boundary: Nestimate renders index, distribution, and heatmap
# views of sequences; cograph renders every flow view (alluvial bands and
# individual tracked lines). VaSSTra supplies only what cograph cannot
# infer: the shared state palette, the substantive state order, and the
# real time labels.

#' Plot State Flows Between Consecutive Time Points
#'
#' Draws how subjects move between states from one time point to the next,
#' using [cograph::plot_alluvial()] for aggregated bands and
#' [cograph::plot_trajectories()] for individually tracked lines. Sequence
#' index, distribution, and heatmap views remain with [plot()] and
#' Nestimate; `flow_plot()` answers the complementary question of *where
#' movement goes*, which a sequence plot ordered by similarity cannot show.
#'
#' State colors, state order, and time labels are taken from the fitted
#' object, so a flow plot is directly comparable with the sequence heatmap
#' and the state profiles.
#'
#' @param x A `vasstra_sequences`, `vasstra_trajectories`, or `vasstra`
#'   object.
#' @param type `"alluvial"` (default) for aggregated flow bands whose width
#'   is the number of subjects making that move, or `"individual"` for one
#'   line per subject.
#' @param colors Optional colors, one per state, in state order. Defaults to
#'   the shared VaSSTra palette.
#' @param main Optional plot title. A type-specific title is used by default.
#' @param color_by State that gives a flow its color: `"source"` (default for
#'   `"alluvial"`) or `"destination"`. Individual lines also accept
#'   `"first"` (the default, coloring each subject by the state they start
#'   in) and `"last"`.
#' @param bundle Line bundling for `type = "individual"`. `"auto"` (default)
#'   bundles only when subjects outnumber `bundle_max` lines, `FALSE` draws
#'   every subject, and a number sets the subjects represented by one line.
#'   Ignored by `type = "alluvial"`.
#' @param bundle_max Largest number of lines drawn before `"auto"` bundling
#'   starts. Default 50.
#' @param ... Additional arguments passed to [cograph::plot_alluvial()] or
#'   [cograph::plot_trajectories()].
#'
#' @return A `ggplot` object. Unlike the base-graphics [plot()] methods,
#'   which draw immediately and return their tidy data invisibly, this is
#'   returned visibly so that it prints at the console.
#'
#' @seealso [plot.vasstra_sequences()] for the Nestimate sequence views.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("cograph", quietly = TRUE)) {
#'   data("engagement", package = "VaSSTra")
#'   fit <- vasstra(
#'     engagement,
#'     state_labels = c("Disengaged", "Average", "Active")
#'   )
#'   flow_plot(fit)
#'   flow_plot(fit, type = "individual")
#'   flow_plot(fit, group = "Trajectory 1")
#' }
#' }
#' @export
flow_plot <- function(x, ...) {
  UseMethod("flow_plot")
}

#' @rdname flow_plot
#' @export
flow_plot.vasstra_sequences <- function(
    x,
    type = c("alluvial", "individual"),
    colors = NULL,
    main = NULL,
    color_by = NULL,
    bundle = "auto",
    bundle_max = 50,
    ...) {
  stopifnot(inherits(x, "vasstra_sequences"))
  .vasstra_flow_plot(
    sequence_data = x$data,
    states = x$states,
    time_labels = as.character(x$settings$time_levels),
    type = match.arg(type),
    colors = .vasstra_resolve_palette(colors, x$state_colors, x$states),
    main = main,
    color_by = color_by,
    bundle = bundle,
    bundle_max = bundle_max,
    dots = list(...)
  )
}

#' @rdname flow_plot
#' @param group Optional trajectory label. Restricts the plot to the
#'   subjects of one trajectory, which is how a single group's movement is
#'   inspected; flow plots draw one panel and cannot be faceted.
#' @export
flow_plot.vasstra_trajectories <- function(
    x,
    type = c("alluvial", "individual"),
    colors = NULL,
    main = NULL,
    color_by = NULL,
    bundle = "auto",
    bundle_max = 50,
    group = NULL,
    ...) {
  stopifnot(inherits(x, "vasstra_trajectories"))
  type <- match.arg(type)
  sequence_data <- x$data
  labels <- x$settings$labels
  if (!is.null(group)) {
    group <- .vasstra_check_group(group, labels)
    sequence_data <- sequence_data[
      as.character(x$assignments) == group, , drop = FALSE
    ]
    if (is.null(main)) {
      main <- sprintf(
        "%s: state flows (%d subjects)",
        group,
        nrow(sequence_data)
      )
    }
  }
  .vasstra_flow_plot(
    sequence_data = sequence_data,
    states = x$source$states,
    time_labels = as.character(x$source$settings$time_levels),
    type = type,
    colors = .vasstra_resolve_palette(
      colors, x$source$state_colors, x$source$states
    ),
    main = main,
    color_by = color_by,
    bundle = bundle,
    bundle_max = bundle_max,
    dots = list(...)
  )
}

#' @rdname flow_plot
#' @export
flow_plot.vasstra <- function(x, ...) {
  stopifnot(inherits(x, "vasstra"))
  flow_plot(x$trajectories, ...)
}

# Shared builder: enrich the wide state grid with the package palette, the
# substantive state order, and real time labels, then delegate to cograph.
.vasstra_flow_plot <- function(
    sequence_data,
    states,
    time_labels,
    type,
    colors,
    main,
    color_by,
    bundle,
    bundle_max,
    dots) {
  stopifnot(is.data.frame(sequence_data), is.character(states), is.list(dots))
  if (!requireNamespace("cograph", quietly = TRUE)) {
    stop(
      paste0(
        "Flow plots require the 'cograph' package.\n",
        "Install it with install.packages(\"cograph\")."
      ),
      call. = FALSE
    )
  }
  if (nrow(sequence_data) < 1L) {
    stop("No sequences are available to plot.", call. = FALSE)
  }
  if (ncol(sequence_data) < 2L) {
    stop("Flow plots require at least two time points.", call. = FALSE)
  }
  if (is.null(colors)) {
    colors <- .vasstra_palette(length(states))
  }
  if (length(colors) != length(states)) {
    stop("`colors` must contain one color per state.", call. = FALSE)
  }
  colors <- stats::setNames(as.character(colors), states)
  color_by <- .vasstra_flow_color_by(color_by, type)
  # Factor levels drive node order; without them cograph orders states
  # alphabetically and loses the substantive low-to-high reading.
  sequence_data[] <- lapply(
    sequence_data,
    factor,
    levels = states
  )
  if (length(time_labels) != ncol(sequence_data)) {
    time_labels <- names(sequence_data)
  }
  if (is.null(main)) {
    main <- switch(
      type,
      alluvial = "State flows between time points",
      individual = "Individual state flows"
    )
  }
  arguments <- list(
    x = sequence_data,
    from_title = time_labels,
    title = main,
    from_colors = colors,
    flow_color_by = color_by,
    node_width = 0.05,
    label_position = "beside",
    label_size = 2.7,
    label_halo = TRUE,
    title_size = 3.6
  )
  if (identical(type, "alluvial")) {
    arguments$flow_alpha <- 0.55
    return(do.call(
      cograph::plot_alluvial,
      .vasstra_graphics_arguments(arguments, dots)
    ))
  }
  arguments$line_alpha <- 0.5
  arguments$line_width <- 0.5
  # Label the states once, beside the end columns (black), as the alluvial
  # view does. `mid_label_position = "inside"` repeated a label inside every
  # node, where cograph auto-contrasts it to white -- inconsistent with the
  # black beside-labels and cluttered.
  arguments$bundle_size <- .vasstra_flow_bundle(
    bundle,
    nrow(sequence_data),
    bundle_max
  )
  do.call(
    cograph::plot_trajectories,
    .vasstra_graphics_arguments(arguments, dots)
  )
}

# Aggregated bands can only be colored by their endpoints; individual lines
# can also carry the colour of the subject's first or last state.
.vasstra_flow_color_by <- function(color_by, type) {
  allowed <- if (identical(type, "alluvial")) {
    c("source", "destination")
  } else {
    c("first", "source", "destination", "last")
  }
  if (is.null(color_by)) {
    return(allowed[[1L]])
  }
  match.arg(color_by, allowed)
}

# Resolve the number of subjects drawn as one line. NULL disables bundling.
.vasstra_flow_bundle <- function(bundle, n_subjects, bundle_max) {
  stopifnot(is.numeric(n_subjects), length(n_subjects) == 1L)
  if (isFALSE(bundle) || is.null(bundle)) {
    return(NULL)
  }
  if (identical(bundle, "auto")) {
    if (!is.numeric(bundle_max) || length(bundle_max) != 1L ||
        is.na(bundle_max) || bundle_max < 1L) {
      stop("`bundle_max` must be one positive number.", call. = FALSE)
    }
    if (n_subjects <= bundle_max) {
      return(NULL)
    }
    return(as.integer(ceiling(n_subjects / bundle_max)))
  }
  if (!is.numeric(bundle) || length(bundle) != 1L || is.na(bundle) ||
      bundle < 1L) {
    stop(
      "`bundle` must be \"auto\", FALSE, or one positive number.",
      call. = FALSE
    )
  }
  if (bundle <= 1L) {
    return(NULL)
  }
  as.integer(bundle)
}

# Transition networks: Nestimate builds the network and computes the
# centralities, cograph draws it. `splot()` recognises a Nestimate
# netobject and applies TNA styling, labels, initial-probability donuts,
# and integer weight formatting on its own, so VaSSTra only sizes the
# nodes.

#' State Transition Network Centralities
#'
#' Tidy centralities of the state transition network, one row per state.
#' The network is built by [Nestimate::build_tna()] and the measures come
#' from [Nestimate::net_centrality()], so they match `tna::centralities()`.
#'
#' In-strength is the total incoming transition weight, so the state with
#' the largest in-strength is the one the cohort most often moves *into*.
#' Self-transitions are excluded by default: with `loops = TRUE` a
#' persistent state scores highly merely because its members stay put,
#' which is a different claim from attracting movement.
#'
#' @param x A `vasstra_sequences`, `vasstra_trajectories`, or `vasstra`
#'   object.
#' @param measures Centrality measures to compute, passed to
#'   [Nestimate::net_centrality()]. Defaults to `c("InStrength",
#'   "OutStrength")`; `"all"` returns every built-in measure.
#' @param weights `"probability"` (default) uses row-normalized transition
#'   probabilities from [Nestimate::build_tna()]; `"count"` uses raw
#'   transition counts from [Nestimate::build_ftna()].
#' @param loops Include self-transitions in the computation. Default
#'   `FALSE`, matching [Nestimate::net_centrality()].
#' @param ... Not used.
#'
#' @return A tidy data frame with one row per state and one column per
#'   requested measure.
#'
#' @seealso [transition_plot()] to draw the network.
#'
#' @examples
#' data("engagement", package = "VaSSTra")
#' fit <- vasstra(engagement, n_states = 3, n_trajectories = 3)
#' transition_centrality(fit)
#' transition_centrality(fit, weights = "count")
#' @export
transition_centrality <- function(x, ...) {
  UseMethod("transition_centrality")
}

#' @rdname transition_centrality
#' @export
transition_centrality.vasstra_sequences <- function(
    x,
    measures = c("InStrength", "OutStrength"),
    weights = c("probability", "count"),
    loops = FALSE,
    ...) {
  stopifnot(inherits(x, "vasstra_sequences"))
  .vasstra_transition_centrality(
    network = .vasstra_transition_network(x$data, match.arg(weights)),
    measures = measures,
    loops = loops
  )
}

#' @rdname transition_centrality
#' @param group Optional trajectory label restricting the network to one
#'   trajectory's subjects.
#' @export
transition_centrality.vasstra_trajectories <- function(
    x,
    measures = c("InStrength", "OutStrength"),
    weights = c("probability", "count"),
    loops = FALSE,
    group = NULL,
    ...) {
  stopifnot(inherits(x, "vasstra_trajectories"))
  .vasstra_transition_centrality(
    network = .vasstra_transition_network(
      .vasstra_group_sequences(x, group)$data,
      match.arg(weights)
    ),
    measures = measures,
    loops = loops
  )
}

#' @rdname transition_centrality
#' @export
transition_centrality.vasstra <- function(x, ...) {
  stopifnot(inherits(x, "vasstra"))
  transition_centrality(x$trajectories, ...)
}

#' Plot the State Transition Network
#'
#' Draws states as nodes and transitions as directed edges, with node size
#' encoding a centrality of the transition network. The network comes from
#' [Nestimate::build_tna()] and the centralities from
#' [Nestimate::net_centrality()]; [cograph::splot()] draws it, recognising
#' the Nestimate object and applying TNA styling, node labels, and
#' initial-probability donuts automatically.
#'
#' Where [flow_plot()] shows movement resolved over time,
#' `transition_plot()` collapses every time step into one network and asks
#' which states attract movement overall.
#'
#' @inheritParams transition_centrality
#' @param size Centrality measure that sets node size, or `"none"` to draw
#'   every node at one size. Default `"InStrength"`.
#' @param size_range Smallest and largest node size. The measure is mapped
#'   onto this range anchored at zero, so a state with half the in-strength
#'   of the largest draws halfway up the range and sizes stay comparable
#'   between plots.
#' @param sequences Draw the state sequences beside the network, the
#'   conventional pairing in which the sequences show the raw data and the
#'   network summarises its movement. `FALSE` (default) draws the network
#'   alone; `TRUE` adds an index plot; `"index"`, `"heatmap"`, or
#'   `"distribution"` choose the sequence view. Both panels are drawn on
#'   one device, so a state has the same colour in each.
#' @param colors Optional colors, one per state, in state order. Defaults
#'   to the shared VaSSTra palette, so the network matches the sequence and
#'   state plots. Colors are matched to states by name, so a trajectory
#'   that never reaches a state still colours the rest correctly.
#' @param main Optional plot title.
#' @param ... Additional arguments passed to [cograph::splot()], such as
#'   `layout` or `threshold`.
#'
#' @return A tidy data frame of the plotted centrality and node size, one
#'   row per state, invisibly.
#'
#' @seealso [transition_centrality()] for the numbers without a plot, and
#'   [flow_plot()] for time-resolved movement.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("cograph", quietly = TRUE)) {
#'   data("engagement", package = "VaSSTra")
#'   fit <- vasstra(engagement, n_states = 3, n_trajectories = 3)
#'   transition_plot(fit)
#'   transition_plot(fit, weights = "count", size = "OutStrength")
#'   transition_plot(fit, sequences = TRUE)
#' }
#' }
#' @export
transition_plot <- function(x, ...) {
  UseMethod("transition_plot")
}

#' @rdname transition_plot
#' @export
transition_plot.vasstra_sequences <- function(
    x,
    size = "InStrength",
    weights = c("probability", "count"),
    loops = FALSE,
    size_range = c(8, 18),
    sequences = FALSE,
    colors = NULL,
    main = NULL,
    ...) {
  stopifnot(inherits(x, "vasstra_sequences"))
  .vasstra_transition_plot(
    network = .vasstra_transition_network(x$data, match.arg(weights)),
    sequence_data = x$data,
    states = x$states,
    size = size,
    loops = loops,
    size_range = size_range,
    sequences = sequences,
    colors = .vasstra_resolve_palette(colors, x$state_colors, x$states),
    main = main,
    dots = list(...)
  )
}

#' @rdname transition_plot
#' @export
transition_plot.vasstra_trajectories <- function(
    x,
    size = "InStrength",
    weights = c("probability", "count"),
    loops = FALSE,
    size_range = c(8, 18),
    sequences = FALSE,
    colors = NULL,
    main = NULL,
    group = NULL,
    ...) {
  stopifnot(inherits(x, "vasstra_trajectories"))
  subset <- .vasstra_group_sequences(x, group)
  .vasstra_transition_plot(
    network = .vasstra_transition_network(subset$data, match.arg(weights)),
    sequence_data = subset$data,
    states = x$source$states,
    size = size,
    loops = loops,
    size_range = size_range,
    sequences = sequences,
    colors = .vasstra_resolve_palette(
      colors, x$source$state_colors, x$source$states
    ),
    main = if (is.null(main)) subset$main else main,
    dots = list(...)
  )
}

#' @rdname transition_plot
#' @export
transition_plot.vasstra <- function(x, ...) {
  stopifnot(inherits(x, "vasstra"))
  transition_plot(x$trajectories, ...)
}

# Build the transition network with Nestimate.
.vasstra_transition_network <- function(sequence_data, weights) {
  stopifnot(is.data.frame(sequence_data))
  if (nrow(sequence_data) < 1L) {
    stop("No sequences are available.", call. = FALSE)
  }
  if (ncol(sequence_data) < 2L) {
    stop("A transition network requires at least two time points.",
         call. = FALSE)
  }
  builder <- if (identical(weights, "count")) {
    Nestimate::build_ftna
  } else {
    Nestimate::build_tna
  }
  builder(as.data.frame(sequence_data))
}

# Optionally restrict a trajectories object to one group's sequences.
.vasstra_group_sequences <- function(x, group) {
  if (is.null(group)) {
    return(list(data = x$data, main = NULL))
  }
  group <- .vasstra_check_group(group, x$settings$labels)
  data <- x$data[as.character(x$assignments) == group, , drop = FALSE]
  list(
    data = data,
    main = sprintf("%s (%d subjects)", group, nrow(data))
  )
}

# Tidy centralities in the network's own node order.
.vasstra_transition_centrality <- function(network, measures, loops) {
  if (!is.logical(loops) || length(loops) != 1L || is.na(loops)) {
    stop("`loops` must be TRUE or FALSE.", call. = FALSE)
  }
  centrality <- tryCatch(
    suppressMessages(
      Nestimate::net_centrality(network, measures = measures, loops = loops)
    ),
    error = function(error) {
      stop(sprintf(
        "Could not compute centralities: %s",
        conditionMessage(error)
      ), call. = FALSE)
    }
  )
  tidy <- as.data.frame(centrality, stringsAsFactors = FALSE)
  row.names(tidy) <- NULL
  tidy
}

# Size the nodes by one centrality and hand the netobject to cograph,
# optionally beside the sequences the network summarises.
.vasstra_transition_plot <- function(
    network,
    sequence_data,
    states,
    size,
    loops,
    size_range,
    sequences,
    colors,
    main,
    dots) {
  stopifnot(is.list(dots), is.character(states))
  if (!requireNamespace("cograph", quietly = TRUE)) {
    stop(
      paste0(
        "Transition network plots require the 'cograph' package.\n",
        "Install it with install.packages(\"cograph\")."
      ),
      call. = FALSE
    )
  }
  if (!is.numeric(size_range) || length(size_range) != 2L ||
      anyNA(size_range) || any(size_range <= 0) ||
      size_range[[1L]] > size_range[[2L]]) {
    stop(
      "`size_range` must be two increasing positive node sizes.",
      call. = FALSE
    )
  }
  sequence_type <- .vasstra_sequence_panel(sequences)
  if (is.null(colors)) {
    colors <- .vasstra_palette(length(states))
  }
  if (length(colors) != length(states)) {
    stop("`colors` must contain one color per state.", call. = FALSE)
  }
  palette <- stats::setNames(as.character(colors), states)
  node_states <- rownames(network$weights)
  node_size <- .vasstra_transition_node_size(
    network = network,
    states = node_states,
    size = size,
    loops = loops,
    size_range = size_range
  )
  if (!is.null(sequence_type)) {
    old_par <- graphics::par(mfrow = c(1L, 2L))
    on.exit(graphics::par(old_par), add = TRUE)
    Nestimate::sequence_plot(
      sequence_data,
      type = sequence_type,
      sort = if (identical(sequence_type, "index")) "start" else "lcs",
      state_colors = .vasstra_sequence_colors(
        colors, states, sequence_data
      ),
      main = main,
      legend = "none"
    )
    # sequence_plot() restores the par it saved, which rewinds the panel
    # counter, so the network would overdraw the sequences without this.
    graphics::par(mfg = c(1L, 2L))
  }
  arguments <- .vasstra_graphics_arguments(
    list(
      x = network,
      node_size = node_size$node_size,
      # Match by name: the network orders states alphabetically, and a
      # single trajectory need not visit every state.
      node_fill = unname(palette[node_states])
    ),
    dots
  )
  if (!is.null(main) && is.null(sequence_type)) {
    arguments$title <- main
  }
  do.call(cograph::splot, arguments)
  invisible(node_size)
}

# Resolve the `sequences` argument to a sequence-plot type, or NULL for
# the network on its own.
.vasstra_sequence_panel <- function(sequences) {
  if (isFALSE(sequences) || is.null(sequences)) {
    return(NULL)
  }
  if (isTRUE(sequences)) {
    return("index")
  }
  if (!is.character(sequences) || length(sequences) != 1L) {
    stop(
      paste0(
        "`sequences` must be TRUE, FALSE, or one of ",
        "\"index\", \"heatmap\", \"distribution\"."
      ),
      call. = FALSE
    )
  }
  match.arg(sequences, c("index", "heatmap", "distribution"))
}

# Map a Nestimate centrality onto a node-size range, anchored at zero so
# that equal measures draw equal nodes across separate plots.
.vasstra_transition_node_size <- function(
    network,
    states,
    size,
    loops,
    size_range) {
  stopifnot(is.character(states), is.numeric(size_range))
  if (identical(size, "none")) {
    return(data.frame(
      state = states,
      measure = "none",
      value = NA_real_,
      node_size = rep(mean(size_range), length(states)),
      stringsAsFactors = FALSE
    ))
  }
  if (!is.character(size) || length(size) != 1L || is.na(size)) {
    stop("`size` must be one centrality measure name, or \"none\".",
         call. = FALSE)
  }
  centrality <- .vasstra_transition_centrality(network, size, loops)
  if (!size %in% names(centrality)) {
    stop(sprintf(
      "Nestimate did not return a '%s' column; available measures: %s.",
      size,
      paste(setdiff(names(centrality), "state"), collapse = ", ")
    ), call. = FALSE)
  }
  values <- stats::setNames(centrality[[size]], centrality$state)[states]
  values[is.na(values)] <- 0
  largest <- max(values)
  scaled <- if (largest <= 0) {
    rep(mean(size_range), length(values))
  } else {
    size_range[[1L]] + values / largest * diff(size_range)
  }
  data.frame(
    state = states,
    measure = size,
    value = as.numeric(values),
    node_size = as.numeric(scaled),
    stringsAsFactors = FALSE
  )
}

# Shared trajectory-label check for the group argument of flow and
# transition plots.
.vasstra_check_group <- function(group, labels) {
  if (length(group) != 1L || is.na(group) ||
      !as.character(group) %in% labels) {
    stop(sprintf(
      "`group` must be one trajectory label: %s.",
      paste(labels, collapse = ", ")
    ), call. = FALSE)
  }
  as.character(group)
}

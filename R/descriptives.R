#' Descriptive Statistics per State
#'
#' Summarises each clustering indicator within each state, the numeric
#' companion to `plot(x, which = "states", type = "variables")`. By default the
#' indicators are pooled over time; `by_time = TRUE` reports one block per time
#' point.
#'
#' @param x A `vasstra` or `vasstra_states` object.
#' @param scale Indicator scale: `"standardized"` (default, the scale the
#'   states were clustered on) or `"original"` (the raw indicators, where a
#'   trailing `"_z"` resolves to its untransformed column).
#' @param by_time Break the summary down per time point (`TRUE`) or pool over
#'   time (`FALSE`, default).
#' @param ... Not used.
#'
#' @return A tidy data frame with one row per state and indicator (and time
#'   point when `by_time = TRUE`): the grouping columns followed by `n`,
#'   `mean`, `sd`, `median`, `min`, and `max`. States are ordered by the fit's
#'   state order and indicators by their clustering order.
#'
#' @seealso [plot.vasstra_states()] with `type = "variables"` for the matching
#'   boxplots.
#'
#' @examples
#' data("engagement", package = "VaSSTra")
#' fit <- vasstra(engagement, n_states = 3)
#' state_descriptives(fit)
#' state_descriptives(fit, scale = "original")
#' state_descriptives(fit, by_time = TRUE)
#' @export
state_descriptives <- function(x, ...) {
  UseMethod("state_descriptives")
}

#' @rdname state_descriptives
#' @export
state_descriptives.vasstra_states <- function(
    x,
    scale = c("standardized", "original"),
    by_time = FALSE,
    ...) {
  stopifnot(inherits(x, "vasstra_states"))
  scale <- match.arg(scale)
  .vasstra_state_descriptives(x, scale, by_time)
}

#' @rdname state_descriptives
#' @export
state_descriptives.vasstra <- function(x, ...) {
  stopifnot(inherits(x, "vasstra"))
  if (is.null(x$states)) {
    stop(
      "This fit used precomputed states and has no indicators to summarise.",
      call. = FALSE
    )
  }
  state_descriptives(x$states, ...)
}

# Per-state (optionally per-time) descriptive statistics for each indicator.
.vasstra_state_descriptives <- function(x, scale, by_time) {
  data <- x$data
  time_col <- x$settings$time
  state_col <- x$settings$state
  if (is.null(state_col) || !state_col %in% names(data)) {
    state_col <- "state"
  }
  variables <- .vasstra_indicator_columns(x, scale)
  labels <- x$settings$labels
  long <- do.call(rbind, lapply(variables, function(v) {
    data.frame(
      time = data[[time_col]],
      variable = factor(v, levels = variables),
      state = factor(as.character(data[[state_col]]), levels = labels),
      value = data[[v]],
      stringsAsFactors = FALSE
    )
  }))
  formula <- if (isTRUE(by_time)) {
    value ~ state + variable + time
  } else {
    value ~ state + variable
  }
  summarise <- function(z) {
    z <- z[is.finite(z)]
    c(
      n = length(z),
      mean = mean(z),
      sd = stats::sd(z),
      median = stats::median(z),
      min = min(z),
      max = max(z)
    )
  }
  aggregated <- stats::aggregate(formula, data = long, FUN = summarise)
  # aggregate() returns the statistics as a matrix column; expand it to plain
  # numeric columns and drop the "value." prefix.
  result <- do.call(data.frame, aggregated)
  names(result) <- sub("^value\\.", "", names(result))
  result$n <- as.integer(result$n)
  order_columns <- if (isTRUE(by_time)) {
    c("state", "variable", "time")
  } else {
    c("state", "variable")
  }
  result <- result[do.call(order, result[order_columns]), , drop = FALSE]
  result$state <- as.character(result$state)
  result$variable <- as.character(result$variable)
  rownames(result) <- NULL
  result
}

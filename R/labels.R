# Tidy relabeling of fitted states and trajectories.

# Resolve new labels from either a complete replacement vector or a named
# partial rename such as c("State 1" = "Low").
.vasstra_resolve_labels <- function(old, new, what) {
  stopifnot(is.character(old), is.character(what), length(what) == 1L)
  if (!is.character(new) || length(new) == 0L || anyNA(new) ||
      any(!nzchar(new))) {
    stop(sprintf("`%s` must contain non-empty labels.", what),
         call. = FALSE)
  }
  supplied_names <- names(new)
  if (!is.null(supplied_names) && all(nzchar(supplied_names))) {
    unknown <- setdiff(supplied_names, old)
    if (length(unknown) > 0L) {
      stop(sprintf(
        "Unknown %s label%s: %s. Current labels: %s.",
        what,
        if (length(unknown) == 1L) "" else "s",
        paste(unknown, collapse = ", "),
        paste(old, collapse = ", ")
      ), call. = FALSE)
    }
    resolved <- old
    resolved[match(supplied_names, old)] <- as.character(new)
  } else {
    if (length(new) != length(old)) {
      stop(sprintf(
        "`%s` must contain %d labels (or use named renames such as c(\"%s\" = \"...\")).",
        what,
        length(old),
        old[[1L]]
      ), call. = FALSE)
    }
    resolved <- as.character(new)
  }
  if (anyDuplicated(resolved)) {
    stop(sprintf("`%s` must be unique after renaming.", what),
         call. = FALSE)
  }
  resolved
}

#' Relabel Fitted States or Trajectories
#'
#' Renames the groups of an already-fitted object and propagates the new
#' labels through every derived table. Supply either a complete label
#' vector ordered from the first to the last group, or named renames such
#' as `c("State 1" = "Disengaged")` to change only some labels.
#'
#' @param x A `vasstra_states`, `vasstra_trajectories`, or complete
#'   `vasstra` object.
#' @param ... Method-specific arguments.
#'
#' @return The relabeled object, with the same class as `x`.
#'
#' @examples
#' set.seed(1)
#' data <- expand.grid(student = 1:12, course = 1:3)
#' level <- rep(c(2, 8, 16), length.out = nrow(data))
#' data$views <- level + rnorm(nrow(data), sd = 0.4)
#' data$duration <- level * 3 + rnorm(nrow(data), sd = 0.4)
#' states <- step1_states(data, n_states = 3)
#' states <- set_labels(states, c("Low", "Average", "High"))
#' summary(states)
#' @export
set_labels <- function(x, ...) {
  UseMethod("set_labels")
}

#' @rdname set_labels
#' @param labels New labels: a complete vector or named partial renames.
#' @export
set_labels.vasstra_states <- function(x, labels, ...) {
  stopifnot(inherits(x, "vasstra_states"))
  old <- x$settings$labels
  new <- .vasstra_resolve_labels(old, labels, "labels")
  x$settings$labels <- new
  levels(x$data[[x$settings$state]]) <- new
  x$profiles$state <- new[match(x$profiles$state, old)]
  names(x$diagnostics$state_sizes) <- new
  if (!is.null(x$state_colors)) {
    names(x$state_colors) <- new[match(names(x$state_colors), old)]
  }
  x
}

#' @rdname set_labels
#' @export
set_labels.vasstra_trajectories <- function(x, labels, ...) {
  stopifnot(inherits(x, "vasstra_trajectories"))
  old <- x$settings$labels
  new <- .vasstra_resolve_labels(old, labels, "labels")
  x$settings$labels <- new
  levels(x$assignments) <- new
  levels(x$membership$trajectory) <- new
  levels(x$cluster_summary$trajectory) <- new
  names(x$diagnostics$sizes) <- new
  x
}

# Rename label values inside a factor or character column, leaving values
# outside the rename map (for example an explicit missing label) untouched.
.vasstra_apply_labels <- function(values, old, new) {
  stopifnot(is.character(old), is.character(new))
  if (is.factor(values)) {
    current <- levels(values)
    matched <- match(current, old)
    levels(values) <- ifelse(is.na(matched), current, new[matched])
    return(values)
  }
  renamed <- new[match(as.character(values), old)]
  ifelse(is.na(renamed) & !is.na(values), as.character(values), renamed)
}

#' @rdname set_labels
#' @param states New state labels for a complete fit (complete vector or
#'   named renames). The rename is propagated in place through the fitted
#'   states, sequences, trajectories, and description — including the
#'   recorded positive and negative states — without changing any fitted
#'   value.
#' @param trajectories New trajectory labels for a complete fit.
#' @export
set_labels.vasstra <- function(x, states = NULL, trajectories = NULL, ...) {
  stopifnot(inherits(x, "vasstra"))
  if (is.null(states) && is.null(trajectories)) {
    stop("Supply `states`, `trajectories`, or both.", call. = FALSE)
  }
  if (!is.null(states)) {
    if (is.null(x$states)) {
      stop(
        paste0(
          "This fit used precomputed states; rename the state column in ",
          "the source data instead."
        ),
        call. = FALSE
      )
    }
    old <- x$states$settings$labels
    x$states <- set_labels(x$states, states)
    new <- x$states$settings$labels
    state_column <- x$sequences$settings$state
    x$sequences$data[] <- lapply(
      x$sequences$data,
      .vasstra_apply_labels,
      old = old,
      new = new
    )
    x$sequences$long_data[[state_column]] <- .vasstra_apply_labels(
      x$sequences$long_data[[state_column]],
      old,
      new
    )
    x$sequences$states <- .vasstra_apply_labels(x$sequences$states, old, new)
    if (!is.null(x$sequences$state_colors)) {
      names(x$sequences$state_colors) <-
        new[match(names(x$sequences$state_colors), old)]
    }
    x$sequences$distribution$state <- .vasstra_apply_labels(
      x$sequences$distribution$state,
      old,
      new
    )
    x$sequences$transitions$from <- .vasstra_apply_labels(
      x$sequences$transitions$from,
      old,
      new
    )
    x$sequences$transitions$to <- .vasstra_apply_labels(
      x$sequences$transitions$to,
      old,
      new
    )
    x$sequences$source <- x$states
    x$trajectories$data[] <- lapply(
      x$trajectories$data,
      .vasstra_apply_labels,
      old = old,
      new = new
    )
    x$trajectories$source <- x$sequences
    x$description$mean_time$state <- .vasstra_apply_labels(
      x$description$mean_time$state,
      old,
      new
    )
    x$description$distribution$state <- .vasstra_apply_labels(
      x$description$distribution$state,
      old,
      new
    )
    x$description$transitions$from <- .vasstra_apply_labels(
      x$description$transitions$from,
      old,
      new
    )
    x$description$transitions$to <- .vasstra_apply_labels(
      x$description$transitions$to,
      old,
      new
    )
    x$description$settings$positive_states <- as.character(
      .vasstra_apply_labels(
        x$description$settings$positive_states,
        old,
        new
      )
    )
    x$description$settings$negative_states <- as.character(
      .vasstra_apply_labels(
        x$description$settings$negative_states,
        old,
        new
      )
    )
  }
  if (!is.null(trajectories)) {
    old <- x$trajectories$settings$labels
    x$trajectories <- set_labels(x$trajectories, trajectories)
    new <- x$trajectories$settings$labels
    x$description$indices$trajectory <- .vasstra_apply_labels(
      x$description$indices$trajectory,
      old,
      new
    )
    x$description$data$trajectory <- x$description$indices$trajectory
    x$description$trajectory_summary$trajectory <- .vasstra_apply_labels(
      x$description$trajectory_summary$trajectory,
      old,
      new
    )
    x$description$mean_time$trajectory <- .vasstra_apply_labels(
      x$description$mean_time$trajectory,
      old,
      new
    )
    x$description$distribution$trajectory <- .vasstra_apply_labels(
      x$description$distribution$trajectory,
      old,
      new
    )
    x$description$transitions$trajectory <- .vasstra_apply_labels(
      x$description$transitions$trajectory,
      old,
      new
    )
  }
  x$description$source <- x$trajectories
  x
}

#' Step 2: Turn States into Sequences
#'
#' Aligns every subject on the same observed time axis and returns wide
#' sequences plus tidy long data, state distributions, and transitions.
#'
#' @param data A `vasstra_states` object or a data frame containing states.
#' @param id Subject identifier column. Inferred from `vasstra_states`,
#'   attached role metadata, or a common identifier name.
#' @param time Time or ordering column. Inferred from `vasstra_states`,
#'   attached role metadata, or a common time name.
#' @param state State column. Inferred from `vasstra_states`, or detected as
#'   the single categorical non-role column of a plain data frame.
#' @param time_levels Explicit chronological values. Required for character or
#'   unordered-factor time when `data` is a plain data frame.
#' @param missing Structural-gap policy: `"error"` (default), `"explicit"` to
#'   create a state for missing subject-time cells, or `"keep"` to preserve
#'   `NA` cells for advanced use.
#' @param missing_label Label used when `missing = "explicit"`.
#'
#' @return A `vasstra_sequences` object with `data` (wide state sequences),
#'   `long_data`, `meta_data`, `distribution`, and `transitions`.
#'
#' @examples
#' states <- data.frame(
#'   student = rep(1:3, each = 3),
#'   course = rep(1:3, 3),
#'   engagement = c("Low", "Average", "High",
#'                  "Average", "Average", "High",
#'                  "Low", "Low", "Average")
#' )
#' sequences <- step2_sequences(
#'   states,
#'   id = "student",
#'   time = "course",
#'   state = "engagement"
#' )
#' sequences
#' @export
step2_sequences <- function(
    data,
    id = NULL,
    time = NULL,
    state = NULL,
    time_levels = NULL,
    missing = c("error", "explicit", "keep"),
    missing_label = "Missing") {
  stopifnot(is.data.frame(data) || inherits(data, "vasstra_states"))
  missing <- match.arg(missing)
  source_states <- inherits(data, "vasstra_states")
  if (source_states) {
    state_object <- data
    data <- state_object$data
    if (is.null(id)) id <- state_object$settings$id
    if (is.null(time)) time <- state_object$settings$time
    if (is.null(state)) state <- state_object$settings$state
    if (is.null(time_levels)) {
      time_levels <- state_object$settings$time_levels
    }
  } else if (is.null(id) || is.null(time) || is.null(state)) {
    resolved <- .vasstra_auto_roles(
      data,
      id = id,
      time = time,
      variables = NULL,
      need_variables = FALSE
    )
    id <- resolved$id
    time <- resolved$time
    if (is.null(state)) {
      state <- .vasstra_detect_state(data, id, time)
    }
  }
  roles <- .vasstra_validate_roles(data, id, time)
  id <- roles$id
  time <- roles$time
  state <- .vasstra_column(data, state, "state")
  if (state %in% c(id, time)) {
    stop("`state` must differ from the `id` and `time` columns.",
         call. = FALSE)
  }
  chronology <- .vasstra_time_levels(data[[time]], time_levels)
  if (!is.character(missing_label) || length(missing_label) != 1L ||
      is.na(missing_label) || !nzchar(missing_label)) {
    stop("`missing_label` must be one non-empty string.", call. = FALSE)
  }
  observed_states <- as.character(data[[state]])
  states <- if (is.factor(data[[state]])) {
    levels(data[[state]])[levels(data[[state]]) %in% observed_states]
  } else {
    sort(unique(observed_states[!is.na(observed_states)]))
  }
  if (length(states) < 1L) {
    stop("No observed states were found.", call. = FALSE)
  }
  if (length(unique(data[[id]])) < 2L) {
    stop("At least two subjects are required.", call. = FALSE)
  }
  wide <- .vasstra_wide_matrix(data, id, time, state, chronology)
  sequence_matrix <- wide$matrix
  structural_missing <- sum(is.na(sequence_matrix))
  if (missing == "error" && structural_missing > 0L) {
    stop(sprintf(
      paste0(
        "The aligned subject-time grid has %d missing state cell%s. ",
        "Use `missing = \"explicit\"` or `missing = \"keep\"`."
      ),
      structural_missing,
      if (structural_missing == 1L) "" else "s"
    ), call. = FALSE)
  }
  if (missing == "explicit") {
    if (missing_label %in% states) {
      stop("`missing_label` must not duplicate an observed state.",
           call. = FALSE)
    }
    sequence_matrix[is.na(sequence_matrix)] <- missing_label
    states <- c(states, missing_label)
  }
  sequence_data <- as.data.frame(sequence_matrix, stringsAsFactors = FALSE)
  meta_data <- as.data.frame(
    stats::setNames(list(wide$ids), id),
    stringsAsFactors = FALSE
  )
  completed_long <- as.data.frame(
    stats::setNames(
      list(
        rep(wide$ids, each = length(chronology)),
        rep(chronology, times = length(wide$ids)),
        as.vector(t(sequence_matrix))
      ),
      c(id, time, state)
    ),
    stringsAsFactors = FALSE
  )
  completed_long$position <- rep(
    seq_along(chronology),
    times = length(wide$ids)
  )
  transitions <- .vasstra_transition_table(sequence_matrix, states)
  distribution <- .vasstra_distribution_table(
    sequence_matrix,
    states,
    chronology
  )
  structure(
    list(
      data = sequence_data,
      long_data = completed_long,
      meta_data = meta_data,
      time_map = data.frame(
        position = seq_along(chronology),
        time = chronology,
        stringsAsFactors = FALSE
      ),
      states = states,
      distribution = distribution,
      transitions = transitions,
      settings = list(
        id = id,
        time = time,
        state = state,
        time_levels = chronology,
        missing = missing,
        missing_label = if (missing == "explicit") missing_label else NULL
      ),
      diagnostics = list(
        n_subjects = nrow(sequence_data),
        n_times = ncol(sequence_data),
        n_states = length(states),
        n_observed = sum(!is.na(sequence_matrix)),
        n_missing = sum(is.na(sequence_matrix)),
        n_structural_missing = structural_missing,
        n_transitions = sum(transitions$count)
      ),
      source = if (source_states) state_object else NULL
    ),
    class = c("vasstra_sequences", "vasstra_step", "list")
  )
}

#' @export
print.vasstra_sequences <- function(x, ...) {
  stopifnot(inherits(x, "vasstra_sequences"))
  cat("VaSStra Step 2: States -> Sequences\n")
  cat(sprintf(
    "  %d subjects | %d times | %d states | %d transitions\n",
    x$diagnostics$n_subjects,
    x$diagnostics$n_times,
    x$diagnostics$n_states,
    x$diagnostics$n_transitions
  ))
  if (x$diagnostics$n_missing > 0L) {
    cat(sprintf("  Missing sequence cells retained: %d\n",
                x$diagnostics$n_missing))
  }
  invisible(x)
}

#' @export
summary.vasstra_sequences <- function(object, ...) {
  stopifnot(inherits(object, "vasstra_sequences"))
  object$distribution
}

#' @export
as.data.frame.vasstra_sequences <- function(
    x,
    row.names = NULL,
    optional = FALSE,
    ...) {
  stopifnot(inherits(x, "vasstra_sequences"))
  as.data.frame(
    x$long_data,
    row.names = row.names,
    optional = optional,
    ...
  )
}

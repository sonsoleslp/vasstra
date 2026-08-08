#' Step 4: Describe the Trajectories
#'
#' Produces tidy per-subject sequence indices and trajectory-level state,
#' distribution, and transition summaries. Entropy is normalized Shannon
#' entropy. Complexity is the geometric mean of entropy and transition rate,
#' matching the common sequence complexity formulation. Integrative potential
#' and negative exposure are time-weighted state proportions.
#'
#' @param data A `vasstra_trajectories` object.
#' @param positive_states Optional states considered positive.
#' @param negative_states Optional states considered negative.
#' @param omega Positive exponent controlling how strongly later time points
#'   are weighted for integrative potential and negative exposure.
#'
#' @return A `vasstra_description` object with tidy `indices`,
#'   `trajectory_summary`, `mean_time`, `distribution`, and `transitions`.
#'
#' @examples
#' long <- data.frame(
#'   id = rep(1:6, each = 3),
#'   time = rep(1:3, 6),
#'   state = c(
#'     "Low", "Low", "Low", "Low", "Low", "Average",
#'     "Average", "Average", "Average", "Average", "Average", "Low",
#'     "High", "High", "High", "High", "High", "Average"
#'   )
#' )
#' trajectories <- long |>
#'   step2_sequences(id = "id", time = "time", state = "state") |>
#'   step3_trajectories(n_trajectories = 3)
#' description <- step4_describe(
#'   trajectories,
#'   positive_states = "High",
#'   negative_states = "Low"
#' )
#' description
#' @export
step4_describe <- function(
    data,
    positive_states = NULL,
    negative_states = NULL,
    omega = 1) {
  stopifnot(inherits(data, "vasstra_trajectories"))
  positive_states <- if (is.null(positive_states)) {
    character(0L)
  } else {
    positive_states
  }
  negative_states <- if (is.null(negative_states)) {
    character(0L)
  } else {
    negative_states
  }
  if (!is.character(positive_states) || anyNA(positive_states) ||
      !is.character(negative_states) || anyNA(negative_states)) {
    stop("Positive and negative states must be character vectors.",
         call. = FALSE)
  }
  if (!is.numeric(omega) || length(omega) != 1L ||
      !is.finite(omega) || omega <= 0) {
    stop("`omega` must be one positive finite number.", call. = FALSE)
  }
  sequences <- data$source
  alphabet <- sequences$states
  unknown_positive <- setdiff(positive_states, alphabet)
  unknown_negative <- setdiff(negative_states, alphabet)
  if (length(unknown_positive) > 0L) {
    stop(sprintf(
      "Unknown positive state%s: %s.",
      if (length(unknown_positive) == 1L) "" else "s",
      paste(unknown_positive, collapse = ", ")
    ), call. = FALSE)
  }
  if (length(unknown_negative) > 0L) {
    stop(sprintf(
      "Unknown negative state%s: %s.",
      if (length(unknown_negative) == 1L) "" else "s",
      paste(unknown_negative, collapse = ", ")
    ), call. = FALSE)
  }
  sequence_matrix <- as.matrix(sequences$data)
  metric_matrix <- t(vapply(seq_len(nrow(sequence_matrix)), function(index) {
    .vasstra_sequence_metrics(
      as.character(sequence_matrix[index, ]),
      alphabet,
      positive_states,
      negative_states,
      omega
    )
  }, numeric(8L)))
  id <- sequences$settings$id
  indices <- as.data.frame(
    stats::setNames(
      c(
        list(
          sequences$meta_data[[id]],
          data$assignments
        ),
        as.data.frame(metric_matrix, stringsAsFactors = FALSE)
      ),
      c(id, "trajectory", colnames(metric_matrix))
    ),
    stringsAsFactors = FALSE
  )
  trajectory_labels <- data$settings$labels
  index_summary <- stats::aggregate(
    indices[setdiff(names(indices), c(id, "trajectory"))],
    by = list(trajectory = indices$trajectory),
    FUN = function(values) mean(values, na.rm = TRUE)
  )
  trajectory_summary <- merge(
    data$cluster_summary,
    index_summary,
    by = "trajectory",
    sort = FALSE
  )
  trajectory_factor <- factor(
    rep(data$assignments, each = ncol(sequence_matrix)),
    levels = trajectory_labels,
    ordered = TRUE
  )
  state_factor <- factor(
    as.vector(t(sequence_matrix)),
    levels = alphabet
  )
  mean_counts <- table(trajectory_factor, state_factor)
  mean_time <- as.data.frame(mean_counts, stringsAsFactors = FALSE)
  names(mean_time) <- c("trajectory", "state", "total_time")
  trajectory_sizes <- data$diagnostics$sizes[
    match(as.character(mean_time$trajectory), names(data$diagnostics$sizes))
  ]
  mean_time$mean_time <- mean_time$total_time / trajectory_sizes
  position_factor <- factor(
    rep(seq_len(ncol(sequence_matrix)), times = nrow(sequence_matrix)),
    levels = seq_len(ncol(sequence_matrix))
  )
  distribution_counts <- table(
    trajectory_factor,
    position_factor,
    state_factor
  )
  distribution <- as.data.frame(
    distribution_counts,
    stringsAsFactors = FALSE
  )
  names(distribution) <- c("trajectory", "position", "state", "count")
  distribution$position <- as.integer(as.character(distribution$position))
  distribution$time <- sequences$settings$time_levels[
    distribution$position
  ]
  distribution_totals <- stats::ave(
    distribution$count,
    distribution$trajectory,
    distribution$position,
    FUN = sum
  )
  distribution$proportion <- ifelse(
    distribution_totals == 0,
    0,
    distribution$count / distribution_totals
  )
  distribution <- distribution[
    c("trajectory", "position", "time", "state", "count", "proportion")
  ]
  transition_parts <- lapply(
    seq_along(trajectory_labels),
    function(cluster_id) {
      rows <- data$assignments == trajectory_labels[[cluster_id]]
      result <- .vasstra_transition_table(
        sequence_matrix[rows, , drop = FALSE],
        alphabet
      )
      result$trajectory <- factor(
        trajectory_labels[[cluster_id]],
        levels = trajectory_labels,
        ordered = TRUE
      )
      result[c("trajectory", "from", "to", "count", "probability")]
    }
  )
  transitions <- do.call(rbind, transition_parts)
  rownames(transitions) <- NULL
  structure(
    list(
      data = indices,
      indices = indices,
      trajectory_summary = trajectory_summary,
      mean_time = mean_time,
      distribution = distribution,
      transitions = transitions,
      settings = list(
        positive_states = positive_states,
        negative_states = negative_states,
        omega = omega
      ),
      diagnostics = list(
        n_subjects = nrow(indices),
        n_trajectories = length(trajectory_labels),
        n_states = length(alphabet)
      ),
      source = data
    ),
    class = c("vasstra_description", "vasstra_step", "list")
  )
}

#' @export
print.vasstra_description <- function(x, ...) {
  stopifnot(inherits(x, "vasstra_description"))
  cat("VaSSTra Step 4: Describe Trajectories\n")
  cat(sprintf(
    "  %d subjects | %d trajectories | %d states\n",
    x$diagnostics$n_subjects,
    x$diagnostics$n_trajectories,
    x$diagnostics$n_states
  ))
  invisible(x)
}

#' @export
summary.vasstra_description <- function(object, ...) {
  stopifnot(inherits(object, "vasstra_description"))
  object$trajectory_summary
}

#' @export
as.data.frame.vasstra_description <- function(
    x,
    row.names = NULL,
    optional = FALSE,
    ...) {
  stopifnot(inherits(x, "vasstra_description"))
  as.data.frame(
    x$indices,
    row.names = row.names,
    optional = optional,
    ...
  )
}

#' Evaluate a Fitted Clustering
#'
#' Compares the fitted number of states or trajectories against the
#' alternative counts for the same clustering method and reports per-cluster
#' quality. The result contains one tidy candidate row per compared count,
#' with the fitted and recommended solutions marked, and one tidy row per
#' fitted cluster with its size and mean silhouette width.
#'
#' States from a model engine, and the trajectories of a two-level engine,
#' are evaluated the same way and also carry latents' own evaluation: the
#' enumeration of the compared counts ([latents::enumerate_lpa()],
#' [latents::enumerate_lca()] or [latents::enumerate_classes()]) and the
#' fitted model's classification diagnostics ([latents::diagnostics()]). The
#' result is then a `vasstra_model_evaluation`: `as.data.frame(x, what =
#' "enumeration")` and `what = "classification"` return latents' tables, and
#' `plot(x, type = "enumeration")` or `type = "diagnostics"` draws latents'
#' plots (see [plot.vasstra_model_evaluation()]).
#'
#' @param x A `vasstra_states`, `vasstra_trajectories`, or complete
#'   `vasstra` object.
#' @param ... Method-specific arguments.
#'
#' @return A `vasstra_evaluation` object, or a `vasstra_model_evaluation`
#'   (which is also one) for a model engine, or a `vasstra_evaluations` pair
#'   for a complete fit. `as.data.frame()` returns the candidate comparison;
#'   `summary()` returns the per-cluster quality table.
#'
#' @examples
#' set.seed(1)
#' data <- expand.grid(student = 1:12, course = 1:3)
#' level <- rep(c(2, 8, 16), length.out = nrow(data))
#' data$views <- level + rnorm(nrow(data), sd = 0.4)
#' data$duration <- level * 3 + rnorm(nrow(data), sd = 0.4)
#' states <- states(data, n_states = 3)
#' evaluate(states)
#' @export
evaluate <- function(x, ...) {
  UseMethod("evaluate")
}

.vasstra_drop_empty_columns <- function(candidates, keep) {
  stopifnot(is.data.frame(candidates), is.character(keep))
  informative <- vapply(candidates, function(values) !all(is.na(values)),
                        logical(1L))
  candidates[informative | names(candidates) %in% keep]
}

.vasstra_evaluation <- function(
    candidates,
    clusters,
    unit,
    size_column,
    fitted_size,
    fitted_silhouette,
    method_label,
    group_colors = NULL) {
  stopifnot(
    is.data.frame(candidates),
    is.data.frame(clusters),
    is.character(unit),
    is.character(size_column)
  )
  structure(
    list(
      candidates = candidates,
      clusters = clusters,
      unit = unit,
      size_column = size_column,
      # A per-group palette in cluster order, when the caller has one to
      # supply (states reuse the fit's `state_colors`); NULL lets the plot
      # fall back to the distinct evaluation palette.
      group_colors = group_colors,
      fitted = list(
        size = as.integer(fitted_size),
        silhouette = as.numeric(fitted_silhouette),
        method = method_label
      )
    ),
    class = c("vasstra_evaluation", "list")
  )
}

# One tidy candidate row per compared state count for the fitted method,
# with `best` and `fitted` markers. Reuses the stored automatic comparison
# when one exists and no explicit counts are requested.
.vasstra_state_candidate_table <- function(x, n_states = NULL) {
  stopifnot(inherits(x, "vasstra_states"))
  settings <- x$settings
  stored <- x$diagnostics$selection
  two_level <- isTRUE(settings$engine %in% .vasstra_two_level_engines())
  # A model engine's candidates come from one latents enumeration, kept so
  # the evaluation can also offer latents' own tables and plots.
  enumeration <- NULL
  candidates <- if (isTRUE(settings$supplied_fit)) {
    .vasstra_fitted_state_row(x)
  } else if (is.null(n_states) && !is.null(stored)) {
    enumeration <- stored$enumeration
    stored <- stored$candidates
    if (two_level) {
      stored <- stored[stored$n_trajectories == settings$n_trajectories, ,
                       drop = FALSE]
    }
    stored
  } else {
    counts <- if (is.null(n_states)) {
      .vasstra_auto_range(x$diagnostics$n_rows)
    } else {
      n_states
    }
    sweep <- suppressWarnings(state_choices(
      data = x$data[setdiff(names(x$data), settings$state)],
      id = settings$id,
      time = settings$time,
      variables = settings$variables,
      n_states = sort(unique(c(counts, settings$n_states))),
      n_trajectories = if (two_level) settings$n_trajectories else NULL,
      method = settings$method,
      lpa_model = if (is.na(settings$lpa_model)) "EEI" else settings$lpa_model,
      standardize = settings$standardize,
      missing = settings$missing,
      time_levels = settings$time_levels,
      n_start = settings$n_start,
      seed = settings$seed,
      minimum_proportion = 0.05
    ))
    if (length(sweep$enumerations) > 0L) {
      enumeration <- sweep$enumerations[[1L]]
    }
    sweep$candidates
  }
  candidates$best <- candidates$is_recommended
  candidates$fitted <- candidates$n_states == settings$n_states
  if (two_level) {
    candidates$fitted <- candidates$fitted &
      candidates$n_trajectories == settings$n_trajectories
  }
  attr(candidates, "enumeration") <- enumeration
  candidates
}

# The fitted states as their only candidate: a fitted latents model passed as
# the engine records no specification to refit other counts from.
.vasstra_fitted_state_row <- function(x) {
  stopifnot(inherits(x, "vasstra_states"))
  diagnostics <- x$diagnostics
  sizes <- as.integer(diagnostics$state_sizes)
  measure <- function(name) {
    value <- diagnostics[[name]]
    if (is.null(value)) NA_real_ else as.numeric(value)
  }
  data.frame(
    candidate_id = 1L,
    n_states = x$settings$n_states,
    n_trajectories = x$settings$n_trajectories,
    method = x$settings$method,
    lpa_model = x$settings$lpa_model,
    status = "ok",
    silhouette = measure("silhouette"),
    total_withinss = NA_real_,
    pam_objective = NA_real_,
    bic = measure("bic"),
    aic = measure("aic"),
    classification_entropy = measure("classification_entropy"),
    min_size = min(sizes),
    max_size = max(sizes),
    size_ratio = max(sizes) / min(sizes),
    eligible = TRUE,
    is_recommended = TRUE,
    stringsAsFactors = FALSE
  )
}

# One tidy candidate row per compared trajectory count for the fitted
# distance-method pair, with `best` and `fitted` markers.
.vasstra_trajectory_candidate_table <- function(x, n_trajectories = NULL) {
  stopifnot(inherits(x, "vasstra_trajectories"))
  settings <- x$settings
  stored <- x$diagnostics$selection
  candidates <- if (is.null(n_trajectories) && !is.null(stored)) {
    stored$candidates
  } else {
    counts <- if (is.null(n_trajectories)) {
      .vasstra_auto_range(x$diagnostics$n_sequences)
    } else {
      n_trajectories
    }
    sweep <- trajectory_choices(
      x$source,
      n_trajectories = sort(unique(c(counts, settings$n_trajectories))),
      dissimilarity = settings$dissimilarity,
      method = settings$method,
      seed = settings$seed,
      minimum_proportion = 0.05
    )
    sweep$candidates
  }
  candidates$best <- candidates$is_recommended
  candidates$fitted <- candidates$n_trajectories == settings$n_trajectories
  candidates
}

#' @rdname evaluate
#' @param n_states Candidate state counts to compare. Defaults to the stored
#'   automatic comparison when one exists, otherwise 2 through 6.
#' @export
evaluate.vasstra_states <- function(x, n_states = NULL, ...) {
  stopifnot(inherits(x, "vasstra_states"))
  settings <- x$settings
  candidates <- .vasstra_state_candidate_table(x, n_states)
  enumeration <- attr(candidates, "enumeration")
  candidates <- .vasstra_drop_empty_columns(
    candidates[c(
      "n_states", "method", "lpa_model", "silhouette", "total_withinss",
      "pam_objective", "bic", "aic", "classification_entropy",
      "min_size", "max_size", "size_ratio", "eligible", "best", "fitted"
    )],
    keep = c("n_states", "method", "silhouette", "min_size", "max_size",
             "size_ratio", "eligible", "best", "fitted")
  )
  assignments <- as.numeric(x$model$vasstra_assignments)
  # Silhouette widths need distances, which only complete numeric
  # indicators define (categorical or FIML-missing indicators have none).
  numeric_values <- length(settings$categorical) == 0L &&
    !anyNA(x$standardized)
  widths <- if (numeric_values) {
    .vasstra_silhouette_widths(
      assignments,
      stats::dist(as.matrix(x$standardized))
    )
  } else {
    rep(NA_real_, length(assignments))
  }
  sizes <- as.integer(x$diagnostics$state_sizes)
  clusters <- data.frame(
    state = factor(settings$labels, levels = settings$labels, ordered = TRUE),
    n = sizes,
    proportion = sizes / sum(sizes),
    silhouette = vapply(seq_along(settings$labels), function(state_id) {
      mean(widths[assignments == state_id])
    }, numeric(1L)),
    stringsAsFactors = FALSE
  )
  evaluation <- .vasstra_evaluation(
    candidates = candidates,
    clusters = clusters,
    unit = "states",
    size_column = "n_states",
    fitted_size = settings$n_states,
    fitted_silhouette = x$diagnostics$silhouette,
    method_label = settings$method,
    # Colour the state bars with the palette pinned on the fit, so the
    # evaluation matches every other state plot; NULL `state_colors` falls
    # back to the default VaSSTra palette.
    group_colors = .vasstra_resolve_palette(NULL, x$state_colors,
                                            settings$labels)
  )
  if (isTRUE(settings$method %in% .vasstra_model_engines())) {
    evaluation <- .vasstra_attach_latents(evaluation, x, enumeration)
  }
  evaluation
}

#' @rdname evaluate
#' @param n_trajectories Candidate trajectory counts to compare. Defaults to
#'   the stored automatic comparison when one exists, otherwise 2 through 6.
#' @export
evaluate.vasstra_trajectories <- function(x, n_trajectories = NULL, ...) {
  stopifnot(inherits(x, "vasstra_trajectories"))
  settings <- x$settings
  if (identical(settings$method, "model")) {
    return(.vasstra_model_trajectory_evaluation(x, n_trajectories))
  }
  candidates <- .vasstra_trajectory_candidate_table(x, n_trajectories)
  candidates <- candidates[c(
    "n_trajectories", "dissimilarity", "method", "silhouette",
    "mean_within_distance", "min_size", "max_size", "size_ratio",
    "eligible", "best", "fitted"
  )]
  assignments <- as.numeric(as.integer(x$assignments))
  widths <- .vasstra_silhouette_widths(assignments, x$distance)
  clusters <- x$cluster_summary
  clusters$silhouette <- vapply(
    seq_along(settings$labels),
    function(cluster_id) mean(widths[assignments == cluster_id]),
    numeric(1L)
  )
  .vasstra_evaluation(
    candidates = candidates,
    clusters = clusters,
    unit = "trajectories",
    size_column = "n_trajectories",
    fitted_size = settings$n_trajectories,
    fitted_silhouette = x$silhouette,
    method_label = paste(settings$dissimilarity, settings$method, sep = " + ")
  )
}

#' @rdname evaluate
#' @export
evaluate.vasstra <- function(x, ...) {
  stopifnot(inherits(x, "vasstra"))
  result <- list(
    states = if (is.null(x$states)) NULL else evaluate(x$states),
    trajectories = evaluate(x$trajectories)
  )
  structure(
    result[!vapply(result, is.null, logical(1L))],
    class = c("vasstra_evaluations", "list")
  )
}

#' @export
print.vasstra_evaluation <- function(x, digits = 3L, ...) {
  stopifnot(inherits(x, "vasstra_evaluation"))
  cat(sprintf("VaSSTra clustering evaluation: %s (%s)\n",
              x$unit, x$fitted$method))
  cat(sprintf(
    "  Fitted: %d %s | silhouette %.3f\n",
    x$fitted$size, x$unit, x$fitted$silhouette
  ))
  cat("  Candidates:\n")
  shown <- x$candidates
  numeric_columns <- vapply(shown, is.numeric, logical(1L))
  shown[numeric_columns] <- lapply(shown[numeric_columns], round,
                                   digits = digits)
  print.data.frame(shown, row.names = FALSE)
  cat(sprintf("  Fitted %s:\n", x$unit))
  clusters <- x$clusters
  numeric_columns <- vapply(clusters, is.numeric, logical(1L))
  clusters[numeric_columns] <- lapply(clusters[numeric_columns], round,
                                      digits = digits)
  print.data.frame(clusters, row.names = FALSE)
  invisible(x)
}

#' @export
summary.vasstra_evaluation <- function(object, ...) {
  stopifnot(inherits(object, "vasstra_evaluation"))
  object$clusters
}

#' @export
as.data.frame.vasstra_evaluation <- function(
    x,
    row.names = NULL,
    optional = FALSE,
    ...) {
  stopifnot(inherits(x, "vasstra_evaluation"))
  as.data.frame(
    x$candidates,
    row.names = row.names,
    optional = optional,
    ...
  )
}

#' @export
print.vasstra_evaluations <- function(x, ...) {
  stopifnot(inherits(x, "vasstra_evaluations"))
  for (evaluation in x) {
    print(evaluation, ...)
    cat("\n")
  }
  invisible(x)
}

#' @export
as.data.frame.vasstra_evaluations <- function(
    x,
    row.names = NULL,
    optional = FALSE,
    ...) {
  stopifnot(inherits(x, "vasstra_evaluations"))
  parts <- lapply(x, function(evaluation) {
    candidates <- evaluation$candidates
    candidates$unit <- evaluation$unit
    names(candidates)[names(candidates) == evaluation$size_column] <- "k"
    candidates[c(
      "unit", "k", "silhouette", "min_size", "max_size", "size_ratio",
      "eligible", "best", "fitted"
    )]
  })
  combined <- do.call(rbind, parts)
  rownames(combined) <- NULL
  as.data.frame(
    combined,
    row.names = row.names,
    optional = optional,
    ...
  )
}

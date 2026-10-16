#' Tidy Fit Indices for a Fitted Clustering
#'
#' Returns the fit statistics of the selected clustering as one tidy row,
#' or one row per compared candidate with `compare = TRUE`. Hard clustering
#' methods report their own objectives (total within-cluster sum of
#' squares, PAM objective), silhouette, and group sizes; columns that do not
#' apply to the fitted method are dropped.
#'
#' States from a model engine, and the trajectories of a two-level engine,
#' report latents' tables unchanged: the fitted model's information criteria
#' (`latents::get_results(fit, "information_criteria")`: log-likelihood,
#' AIC, KIC, and BIC, SABIC, CAIC, AWE, ICL and CLC with the groups and the
#' individuals as sample size), or with `compare = TRUE` the candidate table
#' of latents' enumeration of other counts.
#'
#' @param x A `vasstra_states`, `vasstra_trajectories`, or complete
#'   `vasstra` object.
#' @param ... Method-specific arguments.
#'
#' @return A `vasstra_fit_indices` data frame: one row for the fitted
#'   solution, or one row per candidate (with `best` and `fitted` markers)
#'   when `compare = TRUE`.
#'
#' @examples
#' set.seed(1)
#' data <- expand.grid(student = 1:12, course = 1:3)
#' level <- rep(c(2, 8, 16), length.out = nrow(data))
#' data$views <- level + rnorm(nrow(data), sd = 0.4)
#' data$duration <- level * 3 + rnorm(nrow(data), sd = 0.4)
#' states <- states(data, n_states = 3)
#' fit_indices(states)
#' fit_indices(states, compare = TRUE)
#' @export
fit_indices <- function(x, ...) {
  UseMethod("fit_indices")
}

.vasstra_state_index_columns <- c(
  "n_states", "method", "lpa_model", "log_likelihood", "n_parameters",
  "aic", "bic", "sabic", "caic", "awe", "clc", "kic", "icl",
  "classification_entropy", "prob_min", "prob_max",
  "total_withinss", "pam_objective", "silhouette",
  "min_size", "max_size", "size_ratio"
)

.vasstra_fit_indices <- function(table, unit, method_label) {
  stopifnot(is.data.frame(table))
  names(table)[names(table) == "classification_entropy"] <- "entropy"
  rownames(table) <- NULL
  structure(
    table,
    unit = unit,
    method_label = method_label,
    class = c("vasstra_fit_indices", "data.frame")
  )
}

#' @rdname fit_indices
#' @param compare Return one row per compared candidate instead of only the
#'   fitted solution.
#' @param n_states Candidate state counts used when `compare = TRUE`.
#'   Defaults to the stored automatic comparison when one exists, otherwise
#'   2 through 6.
#' @export
fit_indices.vasstra_states <- function(
    x,
    compare = FALSE,
    n_states = NULL,
    ...) {
  stopifnot(
    inherits(x, "vasstra_states"),
    is.logical(compare),
    length(compare) == 1L,
    !is.na(compare)
  )
  if (isTRUE(x$settings$method %in% .vasstra_model_engines())) {
    return(.vasstra_model_fit_indices(x, "states", compare, n_states))
  }
  if (compare) {
    candidates <- .vasstra_state_candidate_table(x, n_states)
    table <- candidates[c(
      .vasstra_state_index_columns,
      "eligible", "best", "fitted"
    )]
  } else {
    diagnostics <- x$diagnostics
    sizes <- as.integer(diagnostics$state_sizes)
    table <- data.frame(
      n_states = x$settings$n_states,
      method = x$settings$method,
      lpa_model = x$settings$lpa_model,
      log_likelihood = diagnostics$log_likelihood,
      n_parameters = diagnostics$n_parameters,
      aic = diagnostics$aic,
      bic = diagnostics$bic,
      sabic = diagnostics$sabic,
      caic = diagnostics$caic,
      awe = diagnostics$awe,
      clc = diagnostics$clc,
      kic = diagnostics$kic,
      icl = diagnostics$icl,
      classification_entropy = diagnostics$classification_entropy,
      prob_min = diagnostics$prob_min,
      prob_max = diagnostics$prob_max,
      total_withinss = diagnostics$total_withinss,
      pam_objective = diagnostics$pam_objective,
      silhouette = diagnostics$silhouette,
      min_size = min(sizes),
      max_size = max(sizes),
      size_ratio = max(sizes) / min(sizes),
      stringsAsFactors = FALSE
    )
  }
  table <- .vasstra_drop_empty_columns(
    table,
    keep = c("n_states", "method", "silhouette", "min_size", "max_size",
             "size_ratio")
  )
  .vasstra_fit_indices(table, "states", x$settings$method)
}

#' @rdname fit_indices
#' @param n_trajectories Candidate trajectory counts used when
#'   `compare = TRUE`. Defaults to the stored automatic comparison when one
#'   exists, otherwise 2 through 6.
#' @export
fit_indices.vasstra_trajectories <- function(
    x,
    compare = FALSE,
    n_trajectories = NULL,
    ...) {
  stopifnot(
    inherits(x, "vasstra_trajectories"),
    is.logical(compare),
    length(compare) == 1L,
    !is.na(compare)
  )
  if (identical(x$settings$method, "model")) {
    return(.vasstra_model_fit_indices(x$source$source, "trajectories",
                                      compare, n_trajectories))
  }
  method_label <- paste(
    x$settings$dissimilarity,
    x$settings$method,
    sep = " + "
  )
  if (compare) {
    candidates <- .vasstra_trajectory_candidate_table(x, n_trajectories)
    table <- candidates[c(
      "n_trajectories", "dissimilarity", "method", "silhouette",
      "mean_within_distance", "min_size", "max_size", "size_ratio",
      "eligible", "best", "fitted"
    )]
    return(.vasstra_fit_indices(table, "trajectories", method_label))
  }
  sizes <- as.integer(x$diagnostics$sizes)
  table <- data.frame(
    n_trajectories = x$settings$n_trajectories,
    dissimilarity = x$settings$dissimilarity,
    method = x$settings$method,
    silhouette = x$silhouette,
    mean_within_distance = stats::weighted.mean(
      x$cluster_summary$mean_within_distance,
      x$cluster_summary$n
    ),
    min_size = min(sizes),
    max_size = max(sizes),
    size_ratio = max(sizes) / min(sizes),
    stringsAsFactors = FALSE
  )
  .vasstra_fit_indices(table, "trajectories", method_label)
}

#' @rdname fit_indices
#' @param step Which clustering of a complete fit to summarize:
#'   `"states"` (default) or `"trajectories"`.
#' @export
fit_indices.vasstra <- function(
    x,
    step = c("states", "trajectories"),
    compare = FALSE,
    ...) {
  stopifnot(inherits(x, "vasstra"))
  step <- match.arg(step)
  if (identical(step, "states")) {
    if (is.null(x$states)) {
      stop(
        "This fit used precomputed states and has no state fit indices.",
        call. = FALSE
      )
    }
    return(fit_indices(x$states, compare = compare, ...))
  }
  fit_indices(x$trajectories, compare = compare, ...)
}

#' @export
print.vasstra_fit_indices <- function(x, digits = 2L, ...) {
  stopifnot(inherits(x, "vasstra_fit_indices"))
  cat(sprintf(
    "VaSStra fit indices: %s (%s)\n",
    attr(x, "unit"),
    attr(x, "method_label")
  ))
  shown <- as.data.frame(x)
  proportion_columns <- intersect(
    c("entropy", "prob_min", "prob_max", "silhouette"),
    names(shown)
  )
  shown[proportion_columns] <- lapply(
    shown[proportion_columns],
    round,
    digits = 3L
  )
  other_numeric <- setdiff(
    names(shown)[vapply(shown, is.numeric, logical(1L))],
    proportion_columns
  )
  shown[other_numeric] <- lapply(shown[other_numeric], round,
                                 digits = digits)
  print.data.frame(shown, row.names = FALSE)
  invisible(x)
}

# A model engine's fit indices are latents' tables as latents returns them:
# the fitted model's information criteria, or with `compare = TRUE` the
# candidate table of the enumeration that compares it with other counts.
.vasstra_model_fit_indices <- function(states, unit, compare, counts) {
  stopifnot(inherits(states, "vasstra_states"))
  table <- if (compare) {
    enumeration <- .vasstra_model_enumeration(states, unit, counts)
    if (is.null(enumeration)) {
      stop(
        "A fitted latents model was supplied, so there are no other counts to compare.",
        call. = FALSE
      )
    }
    latents::get_results(enumeration, "candidates")
  } else {
    latents::get_results(states$model, "information_criteria")
  }
  .vasstra_fit_indices(
    as.data.frame(table),
    unit,
    sprintf("%s, from latents", states$settings$engine)
  )
}

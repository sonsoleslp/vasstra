# Evaluation of a model engine.
#
# A model engine's states (and a two-level engine's trajectories) are
# evaluated like any others, by silhouette and group sizes, and the
# evaluation also carries latents' own: the enumeration behind the compared
# counts and latents::diagnostics() of the fitted model, returned and drawn
# as latents returns and draws them.

# The latents enumeration that compares the fitted model with other counts:
# the comparison stored by automatic selection, or a new one over `counts`
# of states (unit "states") or trajectory classes (unit "trajectories") with
# the other count held at its fitted value. A fitted latents model passed as
# the engine records no specification to refit, so it has none.
.vasstra_model_enumeration <- function(states, unit, counts = NULL) {
  stopifnot(inherits(states, "vasstra_states"))
  settings <- states$settings
  if (isTRUE(settings$supplied_fit)) {
    return(NULL)
  }
  stored <- states$diagnostics$selection$enumeration
  if (is.null(counts) && !is.null(stored)) {
    return(stored)
  }
  two_level <- settings$engine %in% .vasstra_two_level_engines()
  if (is.null(counts)) {
    counts <- .vasstra_auto_range(if (identical(unit, "states")) {
      states$diagnostics$n_rows
    } else {
      states$diagnostics$n_subjects
    })
  }
  state_counts <- if (identical(unit, "states")) {
    sort(unique(c(counts, settings$n_states)))
  } else {
    settings$n_states
  }
  trajectory_counts <- if (!two_level) {
    1L
  } else if (identical(unit, "trajectories")) {
    sort(unique(c(counts, settings$n_trajectories)))
  } else {
    settings$n_trajectories
  }
  .vasstra_enumerate_latents(
    settings$engine,
    frame = .vasstra_model_frame(states),
    variables = settings$variables,
    categorical = settings$categorical,
    id = settings$id,
    n_states = state_counts,
    n_trajectories = trajectory_counts,
    lpa_model = if (is.na(settings$lpa_model)) "EEI" else settings$lpa_model,
    missing = settings$missing,
    n_start = settings$n_start,
    seed = settings$seed
  )
}

# Add latents' own evaluation to a VaSSTra evaluation of model-engine states
# or trajectories: the enumeration behind the compared counts and the fitted
# model's classification diagnostics. VaSSTra's own tables and panels stay as
# they are.
.vasstra_attach_latents <- function(evaluation, states, enumeration) {
  stopifnot(inherits(evaluation, "vasstra_evaluation"),
            inherits(states, "vasstra_states"))
  evaluation$enumeration <- enumeration
  evaluation$diagnostics <- latents::diagnostics(states$model, plots = FALSE)
  evaluation$engine <- states$settings$engine
  class(evaluation) <- c("vasstra_model_evaluation", class(evaluation))
  evaluation
}

# The evaluation of a two-level engine's trajectory classes, in the same form
# as a clustering's: each candidate number of classes (refitted by latents at
# the fitted number of states) with the silhouette and within-trajectory
# distance of its classes on the Hamming distance, and the fitted classes'
# sizes and silhouettes. The recommended count is the lowest BIC among
# candidates that leave no class under 5 percent of the subjects.
.vasstra_model_trajectory_evaluation <- function(x, n_trajectories = NULL) {
  stopifnot(inherits(x, "vasstra_trajectories"))
  states <- x$source$source
  settings <- states$settings
  enumeration <- .vasstra_model_enumeration(states, "trajectories",
                                            n_trajectories)
  ids <- x$source$meta_data[[x$source$settings$id]]
  distance_matrix <- as.matrix(x$distance)
  describe_classes <- function(classes) {
    sizes <- tabulate(classes, nbins = max(classes))
    sizes <- sizes[sizes > 0L]
    occupied <- sort(unique(classes))
    within <- vapply(occupied, function(class_id) {
      members <- which(classes == class_id)
      if (length(members) <= 1L) {
        return(0)
      }
      block <- distance_matrix[members, members, drop = FALSE]
      mean(block[lower.tri(block)])
    }, numeric(1L))
    data.frame(
      silhouette = .vasstra_silhouette(as.numeric(classes), x$distance),
      mean_within_distance = stats::weighted.mean(within, sizes),
      min_size = min(sizes),
      max_size = max(sizes),
      size_ratio = max(sizes) / min(sizes)
    )
  }
  candidates <- if (is.null(enumeration)) {
    fitted <- describe_classes(as.integer(x$assignments))
    cbind(data.frame(n_trajectories = settings$n_trajectories,
                     bic = states$diagnostics$bic), fitted,
          data.frame(eligible = TRUE, is_recommended = TRUE))
  } else {
    rows <- .vasstra_enumeration_rows(enumeration, settings$engine,
                                      settings$lpa_model)
    rows <- rows[rows$n_states == settings$n_states & rows$status == "ok", ,
                 drop = FALSE]
    described <- do.call(rbind, lapply(seq_len(nrow(rows)), function(index) {
      fit <- .vasstra_enumerated_fit(enumeration, rows$n_states[[index]],
                                     rows$n_trajectories[[index]])
      classes <- .vasstra_fit_trajectory_classes(fit, ids)$classes
      describe_classes(classes)
    }))
    table <- cbind(rows[c("n_trajectories", "bic")], described)
    table$eligible <- rows$min_trajectory_proportion >= 0.05
    best <- which(table$eligible)
    best <- best[which.min(table$bic[best])]
    table$is_recommended <- seq_len(nrow(table)) %in% best
    table
  }
  candidates$dissimilarity <- "hamming"
  candidates$method <- "model"
  candidates$best <- candidates$is_recommended
  candidates$fitted <- candidates$n_trajectories == settings$n_trajectories
  candidates <- candidates[c(
    "n_trajectories", "dissimilarity", "method", "silhouette",
    "mean_within_distance", "min_size", "max_size", "size_ratio",
    "eligible", "best", "fitted"
  )]
  rownames(candidates) <- NULL
  assignments <- as.numeric(as.integer(x$assignments))
  widths <- .vasstra_silhouette_widths(assignments, x$distance)
  clusters <- x$cluster_summary
  clusters$silhouette <- vapply(
    seq_along(x$settings$labels),
    function(cluster_id) mean(widths[assignments == cluster_id]),
    numeric(1L)
  )
  evaluation <- .vasstra_evaluation(
    candidates = candidates,
    clusters = clusters,
    unit = "trajectories",
    size_column = "n_trajectories",
    fitted_size = settings$n_trajectories,
    fitted_silhouette = x$silhouette,
    method_label = sprintf("%s model; silhouette on hamming", settings$engine)
  )
  .vasstra_attach_latents(evaluation, states, enumeration)
}

#' @export
print.vasstra_model_evaluation <- function(x, ...) {
  stopifnot(inherits(x, "vasstra_model_evaluation"))
  NextMethod()
  cat(sprintf(
    paste0(
      "  latents' own evaluation of the %s model: ",
      "as.data.frame(x, what = \"enumeration\" | \"classification\"), ",
      "plot(x, type = \"enumeration\" | \"diagnostics\").\n"
    ),
    x$engine
  ))
  invisible(x)
}

#' @export
as.data.frame.vasstra_model_evaluation <- function(
    x,
    row.names = NULL,
    optional = FALSE,
    ...,
    what = c("candidates", "enumeration", "classification")) {
  stopifnot(inherits(x, "vasstra_model_evaluation"))
  what <- match.arg(what)
  table <- switch(
    what,
    candidates = x$candidates,
    classification = latents::get_results(x$diagnostics, "classification"),
    enumeration = if (is.null(x$enumeration)) {
      stop(
        "A fitted latents model was supplied, so latents compared no other counts.",
        call. = FALSE
      )
    } else {
      latents::get_results(x$enumeration, "candidates")
    }
  )
  as.data.frame(table, row.names = row.names, optional = optional, ...)
}

#' Plot the Evaluation of a Model Engine
#'
#' A model engine's evaluation is drawn like any other: the selection curve,
#' per-group silhouette widths, and group sizes ([plot.vasstra_evaluation()],
#' the default). It also carries latents' own evaluation, drawn by latents:
#' `type = "enumeration"` plots the information criteria across the compared
#' counts ([latents::plot_enumeration()], or the criterion plot of a latent
#' transition grid), and `type = "diagnostics"` the fitted model's
#' classification diagnostics ([latents::diagnostics()]). The latents plots
#' need the suggested ggplot2 package.
#'
#' @param x A `vasstra_model_evaluation` from [evaluate()] on states or
#'   trajectories estimated by a model engine.
#' @param type One of the panels of [plot.vasstra_evaluation()] (`"summary"`,
#'   the default, `"curve"`, `"silhouette"`, `"sizes"`), or `"enumeration"`
#'   or `"diagnostics"` for latents' plots.
#' @param ... Passed to the panel or latents plot.
#'
#' @return For the panels, what [plot.vasstra_evaluation()] returns. For
#'   latents' plots, the plot object visibly: a ggplot for `"enumeration"`, a
#'   `latents_plots` list for `"diagnostics"`.
#'
#' @examples
#' \donttest{
#' data("engagement", package = "VaSSTra")
#' states <- states(engagement, n_states = 3, n_start = 5L)
#' evaluation <- evaluate(states, n_states = 2:4)
#' plot(evaluation)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot(evaluation, type = "enumeration")
#'   plot(evaluation, type = "diagnostics")
#' }
#' }
#' @export
plot.vasstra_model_evaluation <- function(
    x,
    type = c("summary", "curve", "silhouette", "sizes", "enumeration",
             "diagnostics"),
    ...) {
  stopifnot(inherits(x, "vasstra_model_evaluation"))
  type <- match.arg(type)
  if (!type %in% c("enumeration", "diagnostics")) {
    return(NextMethod())
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("latents' evaluation plots need the ggplot2 package.",
         call. = FALSE)
  }
  if (identical(type, "diagnostics")) {
    return(plot(x$diagnostics, ...))
  }
  if (is.null(x$enumeration)) {
    stop(
      "A fitted latents model was supplied, so latents compared no other counts; use `type = \"diagnostics\"`.",
      call. = FALSE
    )
  }
  if (inherits(x$enumeration, "latents_transition_enumeration")) {
    return(plot(x$enumeration, what = "bic", ...))
  }
  latents::plot_enumeration(x$enumeration, ...)
}

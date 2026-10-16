# Evaluation of a model engine.
#
# A model engine's states (and a two-level engine's trajectories) are a
# latents model, so its evaluation is latents': the candidate counts come
# from latents' enumeration and the classification diagnostics from
# latents::diagnostics(), returned and drawn as latents returns and draws
# them. VaSSTra adds only which candidate it fitted.

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

.vasstra_model_evaluation <- function(states, unit, counts = NULL) {
  stopifnot(inherits(states, "vasstra_states"))
  settings <- states$settings
  structure(
    list(
      enumeration = .vasstra_model_enumeration(states, unit, counts),
      diagnostics = latents::diagnostics(states$model, plots = FALSE),
      unit = unit,
      engine = settings$engine,
      fitted = list(
        n_states = settings$n_states,
        n_trajectories = settings$n_trajectories
      )
    ),
    class = c("vasstra_model_evaluation", "list")
  )
}

#' @export
print.vasstra_model_evaluation <- function(x, ...) {
  stopifnot(inherits(x, "vasstra_model_evaluation"))
  cat(sprintf("VaSSTra model evaluation: %s (%s, from latents)\n",
              x$unit, x$engine))
  cat(sprintf(
    "  Fitted: %d states%s\n\n",
    x$fitted$n_states,
    if (is.na(x$fitted$n_trajectories)) {
      ""
    } else {
      sprintf(", %d trajectory classes", x$fitted$n_trajectories)
    }
  ))
  if (is.null(x$enumeration)) {
    cat("  A fitted latents model was supplied, so no other counts were fitted.\n\n")
  } else {
    print(x$enumeration)
    cat("\n")
  }
  print(x$diagnostics)
  invisible(x)
}

#' @export
summary.vasstra_model_evaluation <- function(object, ...) {
  stopifnot(inherits(object, "vasstra_model_evaluation"))
  latents::get_results(object$diagnostics, "classification")
}

#' @export
as.data.frame.vasstra_model_evaluation <- function(
    x,
    row.names = NULL,
    optional = FALSE,
    ...,
    what = c("candidates", "classification")) {
  stopifnot(inherits(x, "vasstra_model_evaluation"))
  what <- match.arg(what)
  table <- if (identical(what, "classification")) {
    latents::get_results(x$diagnostics, "classification")
  } else if (is.null(x$enumeration)) {
    stop(
      "A fitted latents model was supplied, so there are no candidates; use `what = \"classification\"`.",
      call. = FALSE
    )
  } else {
    latents::get_results(x$enumeration, "candidates")
  }
  as.data.frame(table, row.names = row.names, optional = optional, ...)
}

#' Plot the Evaluation of a Model Engine
#'
#' Draws latents' own plots of a model engine's evaluation: the information
#' criteria across the compared counts ([latents::plot_enumeration()], or the
#' criterion plot of a latent transition grid), or the classification
#' diagnostics of the fitted model ([latents::diagnostics()]). Requires the
#' suggested ggplot2 package.
#'
#' @param x A `vasstra_model_evaluation` from [evaluate()] on states or
#'   trajectories estimated by a model engine.
#' @param what `"enumeration"` (default) for the comparison of counts, or
#'   `"diagnostics"` for the fitted model's classification diagnostics.
#' @param ... Passed to the latents plot.
#'
#' @return The latents plot object, visibly: a ggplot for `"enumeration"`, a
#'   `latents_plots` list for `"diagnostics"`.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   data("engagement", package = "VaSSTra")
#'   states <- states(engagement, n_states = 3, n_start = 5L)
#'   evaluation <- evaluate(states, n_states = 2:4)
#'   plot(evaluation)
#'   plot(evaluation, what = "diagnostics")
#' }
#' }
#' @export
plot.vasstra_model_evaluation <- function(
    x,
    what = c("enumeration", "diagnostics"),
    ...) {
  stopifnot(inherits(x, "vasstra_model_evaluation"))
  what <- match.arg(what)
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Plotting a model evaluation needs the ggplot2 package.",
         call. = FALSE)
  }
  if (identical(what, "diagnostics")) {
    return(plot(x$diagnostics, ...))
  }
  if (is.null(x$enumeration)) {
    stop(
      "A fitted latents model was supplied, so there are no candidates; use `what = \"diagnostics\"`.",
      call. = FALSE
    )
  }
  if (inherits(x$enumeration, "latents_transition_enumeration")) {
    return(plot(x$enumeration, what = "bic", ...))
  }
  latents::plot_enumeration(x$enumeration, ...)
}

# A model evaluation's candidates in the columns a complete fit's evaluations
# share: the count, latents' BIC, and which candidate was fitted.
.vasstra_model_evaluation_rows <- function(evaluation, columns) {
  stopifnot(inherits(evaluation, "vasstra_model_evaluation"))
  if (is.null(evaluation$enumeration)) {
    return(NULL)
  }
  candidates <- latents::get_results(evaluation$enumeration, "candidates")
  states_unit <- identical(evaluation$unit, "states")
  counts <- if (states_unit) {
    candidates$n_profiles
  } else {
    candidates$n_group_classes
  }
  bic <- if ("bic_groups" %in% names(candidates)) {
    candidates$bic_groups
  } else {
    candidates$bic
  }
  fitted <- candidates$n_profiles == evaluation$fitted$n_states
  if (!is.na(evaluation$fitted$n_trajectories)) {
    fitted <- fitted &
      candidates$n_group_classes == evaluation$fitted$n_trajectories
  }
  rows <- data.frame(
    unit = evaluation$unit,
    k = as.integer(counts),
    bic = bic,
    fitted = fitted,
    stringsAsFactors = FALSE
  )
  rows[setdiff(columns, names(rows))] <- NA
  rows[columns]
}

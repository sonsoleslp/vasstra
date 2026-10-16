#' Run the Complete VaSSTra Workflow
#'
#' A one-call wrapper around [states()], [sequences()],
#' [trajectories()], and [describe()]. Every argument has a
#' working default, so `vasstra(data)` runs a complete analysis: subject,
#' time, and indicator roles come from explicit arguments, attached role
#' metadata, or common column names, and the numbers of states and
#' trajectories are selected automatically unless given. Every automated
#' decision is reported with a message and recorded in the fit.
#'
#' The `engine` decides how states, and possibly trajectories, are
#' estimated. With `"lpa"` (default), `"lca"` or a clustering engine, step 1
#' estimates states and step 3 clusters the state sequences. With a
#' two-level engine (`"multilpa"`, `"multilca"`, `"lta"`), one latents model
#' estimates the states and the trajectory classes together, and step 3 uses
#' its classes unless `dissimilarity` or `cluster_method` is given. A fitted
#' latents model passed as `engine` supplies its states (and, for a
#' two-level model, its trajectory classes) as estimated. See
#' [states()] for the engines.
#'
#' @param data A longitudinal data frame.
#' @param id Subject identifier column. May be omitted when the data carry
#'   VaSSTra role metadata or use a common identifier name.
#' @param time Time or ordering column. May be omitted when the data carry
#'   VaSSTra role metadata or use a common time name.
#' @param variables State indicators. Use either `variables` or
#'   `state`, but not both. When both are omitted, the numeric non-role
#'   columns are used (columns ending in `_z` are preferred when present).
#' @param state Existing state column. Use either `state` or `variables`.
#' @param n_states Number of states when `variables` is supplied: one
#'   number, a candidate vector such as `2:4` to compare and fit the
#'   recommended count, or `"auto"` (default). Supplied `state_labels`
#'   determine the count under `"auto"`.
#' @param n_trajectories Number of trajectory groups: one number, a
#'   candidate vector to compare, or `"auto"` (default). Supplied
#'   `trajectory_labels` determine the count under `"auto"`. A two-level
#'   engine estimates this count together with the states.
#' @param state_labels Optional labels ordered from low to high profile.
#' @param state_order Optional character vector giving the order the states
#'   should appear in every plot (stacking, legends, transition nodes, flow
#'   nodes). It must list exactly the same states as `state_labels` (or the
#'   observed states, for precomputed input), rearranged. When supplied, the
#'   state column becomes a factor in this order. Useful when the cluster
#'   discovery order does not match the order that reads best.
#' @param state_colors Optional state palette stored on the fit and reused by
#'   every plot without repeating `colors`. Either a named vector
#'   (`c(Low = "grey", High = "red", ...)`, matched by state name and robust to
#'   `state_order` and renaming) or one colour per state in state order. An
#'   explicit `colors` argument to a plot still overrides it.
#' @param trajectory_labels Optional labels for stable trajectory groups.
#' @param state_name Output column name when states are estimated.
#' @param standardize State-indicator standardization: `"time"` (within each
#'   time point), `"subject"` (within each subject, relative to their own
#'   baseline), `"global"`, or `"none"`. Analysis-ready package data may
#'   provide its own default.
#' @param indicator_missing Indicator missingness policy passed to step 1:
#'   `"error"`, `"median"`, or `"fiml"` for the model engines.
#'   Analysis-ready package data may provide its own default.
#' @param sequence_missing Structural sequence-gap policy passed to step 2.
#' @param missing_label Explicit missing-state label.
#' @param time_levels Explicit chronological values when needed.
#' @param dissimilarity Sequence distance passed to [trajectories()].
#' @param cluster_method Trajectory clustering method. With a two-level
#'   engine it defaults to `"model"`, the model's own trajectory classes.
#' @param backend Clustering backend: `"Nestimate"` (default) or `"base"`.
#' @param positive_states Optional states considered positive.
#' @param negative_states Optional states considered negative.
#' @param omega Later-time weighting exponent for step 4.
#' @param n_start Number of random starts of k-means or of a model engine.
#' @param seed Reproducible base seed.
#' @param engine The state engine passed to [states()]: `"lpa"`
#'   (default), `"lca"`, `"multilpa"`, `"multilca"`, `"lta"`, a clustering
#'   method, or a fitted latents model.
#' @param lpa_model Covariance structure of the numeric indicators for the
#'   model engines, as an mclust code. The default `"EEI"` is tidyLPA
#'   model 1.
#' @param state_method Superseded by `engine`; still accepted.
#'
#' @return A `vasstra` object containing all four fitted step objects.
#'
#' @examples
#' set.seed(1)
#' data <- expand.grid(student = 1:15, course = 1:4)
#' latent <- ceiling(data$student / 5)
#' data$views <- latent * 5 + rnorm(nrow(data), sd = 0.4)
#' data$duration <- latent * 10 + rnorm(nrow(data), sd = 0.4)
#'
#' # Fully automated: roles are detected and the counts are selected.
#' fit <- vasstra(data)
#' fit
#'
#' # Labels imply the counts; other choices named explicitly.
#' fit <- vasstra(
#'   data,
#'   state_labels = c("Low", "Average", "High"),
#'   positive_states = "High",
#'   negative_states = "Low"
#' )
#' fit
#' @export
vasstra <- function(
    data,
    id = NULL,
    time = NULL,
    variables = NULL,
    state = NULL,
    n_states = "auto",
    n_trajectories = "auto",
    state_labels = NULL,
    state_order = NULL,
    state_colors = NULL,
    trajectory_labels = NULL,
    state_name = "state",
    standardize = NULL,
    indicator_missing = NULL,
    sequence_missing = c("error", "explicit", "keep"),
    missing_label = "Missing",
    time_levels = NULL,
    dissimilarity = c(
      "hamming", "osa", "lv", "dl", "lcs",
      "qgram", "cosine", "jaccard", "jw"
    ),
    cluster_method = c(
      "pam",
      "ward.D2",
      "ward.D",
      "complete",
      "average",
      "single",
      "mcquitty",
      "median",
      "centroid"
    ),
    backend = c("Nestimate", "base"),
    positive_states = NULL,
    negative_states = NULL,
    omega = 1,
    n_start = 25L,
    seed = 123L,
    engine = NULL,
    lpa_model = "EEI",
    state_method = NULL) {
  stopifnot(is.data.frame(data))
  clustering_requested <- !missing(cluster_method) || !missing(dissimilarity)
  engine <- .vasstra_resolve_engine(engine, state_method, "state_method")
  supplied_fit <- .vasstra_is_latents_fit(engine)
  engine_name <- if (supplied_fit) .vasstra_fit_engine(engine) else engine
  two_level <- engine_name %in% .vasstra_two_level_engines()
  if (supplied_fit) {
    if (!is.null(state)) {
      stop("A fitted latents model supplies the states; drop `state`.",
           call. = FALSE)
    }
    variables <- engine$vars
  }
  metadata <- .vasstra_data_metadata(data)
  if (is.null(id)) {
    id <- metadata$id
  }
  if (is.null(time)) {
    time <- metadata$time
  }
  if (is.null(variables) && is.null(state)) {
    variables <- metadata$variables
  }
  if (!is.null(variables) && !is.null(state)) {
    stop("Supply at most one of `variables` or `state`.", call. = FALSE)
  }
  if (supplied_fit) {
    if (is.null(standardize)) standardize <- "none"
    if (is.null(indicator_missing)) indicator_missing <- "error"
  } else if (is.null(state)) {
    data_specification <- .vasstra_resolve_state_data(
      data,
      id = id,
      time = time,
      variables = variables,
      standardize = standardize,
      missing = indicator_missing
    )
    id <- data_specification$id
    time <- data_specification$time
    variables <- data_specification$variables
    standardize <- data_specification$standardize
    indicator_missing <- data_specification$missing
  } else {
    if (is.null(standardize)) {
      standardize <- "time"
    }
    if (is.null(indicator_missing)) {
      indicator_missing <- "error"
    }
  }
  standardize <- match.arg(standardize, c("time", "global", "subject", "none"))
  indicator_missing <- match.arg(
    indicator_missing,
    c("error", "median", "fiml")
  )
  sequence_missing <- match.arg(sequence_missing)
  dissimilarity <- match.arg(dissimilarity)
  cluster_method <- match.arg(cluster_method)
  backend <- match.arg(backend)
  if (two_level && !clustering_requested) {
    cluster_method <- "model"
  }
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) {
    stop("`seed` must be one finite number.", call. = FALSE)
  }
  if (supplied_fit) {
    states <- states(
      data = data,
      id = id,
      time = time,
      labels = state_labels,
      state_order = state_order,
      state_colors = state_colors,
      state = state_name,
      time_levels = time_levels,
      engine = engine
    )
    sequences <- sequences(
      states,
      missing = sequence_missing,
      missing_label = missing_label
    )
  } else if (!is.null(variables)) {
    states <- states(
      data = data,
      id = id,
      time = time,
      variables = variables,
      n_states = n_states,
      labels = state_labels,
      state_order = state_order,
      state_colors = state_colors,
      state = state_name,
      standardize = standardize,
      missing = indicator_missing,
      time_levels = time_levels,
      n_start = n_start,
      seed = as.integer(seed),
      engine = engine,
      lpa_model = lpa_model,
      n_trajectories = if (two_level) n_trajectories else "auto"
    )
    sequences <- sequences(
      states,
      missing = sequence_missing,
      missing_label = missing_label
    )
  } else {
    states <- NULL
    sequences <- sequences(
      data = data,
      id = id,
      time = time,
      state = state,
      state_order = state_order,
      state_colors = state_colors,
      time_levels = time_levels,
      missing = sequence_missing,
      missing_label = missing_label
    )
  }
  trajectories <- if (identical(cluster_method, "model")) {
    trajectories(
      sequences,
      method = "model",
      labels = trajectory_labels,
      seed = as.integer(seed) + 1L
    )
  } else {
    trajectories(
      sequences,
      n_trajectories = n_trajectories,
      dissimilarity = dissimilarity,
      method = cluster_method,
      backend = backend,
      labels = trajectory_labels,
      seed = as.integer(seed) + 1L
    )
  }
  description <- describe(
    trajectories,
    positive_states = positive_states,
    negative_states = negative_states,
    omega = omega
  )
  structure(
    list(
      states = states,
      sequences = sequences,
      trajectories = trajectories,
      description = description,
      settings = list(
        id = id,
        time = time,
        variables = variables,
        state = if (is.null(variables)) state else state_name,
        n_states = if (is.null(variables)) {
          sequences$diagnostics$n_states
        } else {
          states$settings$n_states
        },
        n_trajectories = trajectories$settings$n_trajectories,
        engine = if (is.null(states)) NA_character_ else engine_name,
        state_method = if (is.null(states)) NA_character_ else engine_name,
        lpa_model = if (is.null(states)) {
          NA_character_
        } else {
          states$settings$lpa_model
        },
        dissimilarity = dissimilarity,
        cluster_method = cluster_method,
        seed = as.integer(seed)
      ),
      diagnostics = list(
        n_subjects = sequences$diagnostics$n_subjects,
        n_times = sequences$diagnostics$n_times,
        n_states = sequences$diagnostics$n_states,
        n_trajectories = trajectories$settings$n_trajectories,
        silhouette = trajectories$silhouette
      )
    ),
    class = c("vasstra", "list")
  )
}

#' @export
print.vasstra <- function(x, ...) {
  stopifnot(inherits(x, "vasstra"))
  cat("VaSSTra Analysis\n")
  cat(sprintf(
    "  %d subjects | %d times | %d states | %d trajectories\n",
    x$diagnostics$n_subjects,
    x$diagnostics$n_times,
    x$diagnostics$n_states,
    x$diagnostics$n_trajectories
  ))
  settings <- x$trajectories$settings
  if (identical(settings$method, "model")) {
    cat(sprintf(
      "  %s model | trajectory relative entropy %.3f | silhouette on hamming %.3f\n",
      settings$engine,
      x$trajectories$diagnostics$relative_entropy,
      x$diagnostics$silhouette
    ))
  } else {
    cat(sprintf(
      "  %s + %s | silhouette %.3f\n",
      settings$dissimilarity,
      settings$method,
      x$diagnostics$silhouette
    ))
  }
  invisible(x)
}

#' @export
summary.vasstra <- function(object, ...) {
  stopifnot(inherits(object, "vasstra"))
  summary(object$description)
}

#' Convert a VaSSTra Analysis to a Tidy Table
#'
#' @param x A `vasstra` analysis.
#' @param row.names,optional Passed to [base::as.data.frame()].
#' @param ... Additional arguments passed to [base::as.data.frame()].
#' @param unit Analysis unit to return: one row per `"subject"` (default),
#'   `"observation"`, `"state_profile"`, or `"trajectory"`.
#'
#' @return A data frame at the requested analysis unit.
#'
#' @examples
#' data("engagement", package = "VaSSTra")
#' fit <- vasstra(engagement, n_states = 3, n_trajectories = 3)
#' head(as.data.frame(fit))
#' as.data.frame(fit, unit = "trajectory")
#' @export
as.data.frame.vasstra <- function(
    x,
    row.names = NULL,
    optional = FALSE,
    ...,
    unit = c("subject", "observation", "state_profile", "trajectory")) {
  stopifnot(inherits(x, "vasstra"))
  unit <- match.arg(unit)
  result <- switch(
    unit,
    subject = x$description$indices,
    observation = {
      observation_data <- x$sequences$long_data
      id <- x$sequences$settings$id
      membership <- x$trajectories$membership
      observation_data$trajectory <- membership$trajectory[
        match(observation_data[[id]], membership[[id]])
      ]
      observation_data
    },
    state_profile = {
      if (is.null(x$states)) {
        stop(
          "State profiles are unavailable when existing states were supplied.",
          call. = FALSE
        )
      }
      x$states$profiles
    },
    trajectory = x$description$trajectory_summary
  )
  as.data.frame(
    result,
    row.names = row.names,
    optional = optional,
    ...
  )
}

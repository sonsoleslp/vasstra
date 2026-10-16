#' Step 1: Turn Variables into States
#'
#' Estimates a state for every subject-time observation with the chosen
#' `engine`. The default engine is latent profile analysis (`"lpa"`), fitted
#' with the latents package. The other model engines are latent class
#' analysis for categorical indicators (`"lca"`) and the two-level models
#' `"multilpa"`, `"multilca"` and `"lta"`, which also estimate classes of
#' subjects: those classes are the trajectories, and [trajectories()]
#' takes them from the model. Reproducible k-means, PAM, and hierarchical
#' clustering remain available as engines. States are ordered by their
#' average indicator level, then optionally given user-supplied labels.
#'
#' A fitted latents model can be passed as `engine`. Its states are used as
#' estimated: nothing is refitted or standardized, and `data` must be the data
#' the model was fitted to, row for row.
#'
#' @section Engines:
#' \describe{
#'   \item{`"lpa"`}{Latent profile analysis ([latents::lpa()]): a Gaussian
#'     mixture over the numeric indicators, with the covariance structure
#'     `lpa_model`. Non-numeric indicators enter as categorical ones.}
#'   \item{`"lca"`}{Latent class analysis ([latents::lca()]): every indicator
#'     is categorical.}
#'   \item{`"multilpa"`, `"multilca"`}{The two-level models
#'     ([latents::multilpa()], [latents::multilca()]): states are profiles of
#'     observations, and each subject belongs to a class with its own mix of
#'     states. A trajectory is then the share of time a subject spends in
#'     each state.}
#'   \item{`"lta"`}{Latent transition analysis ([latents::lta()]) with classes
#'     of subjects: each class has its own transitions between states, so a
#'     trajectory is how a subject moves between them.}
#'   \item{`"kmeans"`, `"pam"`, `"ward.D2"`, ...}{Hard clustering of the
#'     standardized indicators.}
#' }
#' The model engines keep each observation's posterior probabilities, read
#' with `as.data.frame(x, what = "posteriors")`, and report the model's
#' classification diagnostics and information criteria.
#'
#' @param data A data frame with one row per subject and time point.
#' @param id Name of the subject identifier column. May be omitted when the
#'   data carry VaSStra role metadata.
#' @param time Name of the time or ordering column. May be omitted when the
#'   data carry VaSStra role metadata.
#' @param variables Character vector naming the state indicators: numeric for
#'   the clustering engines, numeric or categorical for the model engines.
#'   May be omitted when the data carry VaSStra role metadata.
#' @param n_states Number of states to estimate. One number fits exactly
#'   that count, several numbers (for example `2:4`) compare those
#'   candidates with [state_choices()] and fit the recommended count, and
#'   `"auto"` (default) compares 2 through 6 — or simply matches `labels`
#'   when labels are supplied. Automatic comparison never selects a
#'   solution whose smallest state holds under 5 percent of the
#'   observations. Compared candidates are kept in
#'   `diagnostics$selection` and every automated choice is reported with a
#'   message.
#' @param labels Optional unique labels, ordered from the lowest to the highest
#'   average standardized profile.
#' @param state Name of the state column created in the returned data.
#' @param standardize One of `"time"` (default for ordinary data),
#'   `"global"`, or `"none"`. Applies to numeric indicators. Analysis-ready
#'   package data may provide its own default through metadata.
#' @param missing One of `"error"` (default for ordinary data), `"median"`,
#'   or, for the model engines, `"fiml"`. Median imputation is performed
#'   within time point, with the global variable median as a fallback for an
#'   entirely missing time point. `"fiml"` leaves the values missing and lets
#'   the model use whatever each observation has. Analysis-ready package data
#'   may provide its own default through metadata.
#' @param time_levels Explicit chronological time values. Required for
#'   character or unordered-factor time columns.
#' @param n_start Number of random starts: of k-means, or of the EM algorithm
#'   of a model engine.
#' @param seed Reproducible random seed.
#' @param engine The state engine: `"lpa"` (default), `"lca"`,
#'   `"multilpa"`, `"multilca"`, `"lta"`, `"kmeans"`, `"pam"`, or a
#'   hierarchical method (`"ward.D2"`, `"ward.D"`, `"complete"`,
#'   `"average"`, `"single"`, `"mcquitty"`, `"median"`, `"centroid"`).
#'   Or a fitted latents model, whose states are used as estimated. See
#'   *Engines*.
#' @param lpa_model Covariance structure of the numeric indicators for the
#'   model engines, as an mclust code. The default `"EEI"` corresponds to
#'   tidyLPA model 1: equal variances and zero covariances.
#' @param n_trajectories Number of trajectory classes, for the two-level
#'   engines only, which estimate them together with the states. Takes the
#'   same values as `n_states`; `"auto"` compares 2 through 6 jointly with
#'   the state counts.
#' @param method Superseded by `engine`; still accepted.
#'
#' @return A `vasstra_states` object. Its `data` element is the original data
#'   with a factor state column; `profiles` is a tidy state-by-indicator table
#'   (with a `category` column, and category shares as `mean`, when some
#'   indicators are categorical).
#'
#' @examples
#' set.seed(1)
#' data <- expand.grid(student = 1:12, course = 1:3)
#' level <- rep(c(2, 8, 16), length.out = nrow(data))
#' data$views <- level + rnorm(nrow(data), sd = 0.4)
#' data$duration <- level * 3 + rnorm(nrow(data), sd = 0.4)
#' states <- states(
#'   data,
#'   id = "student",
#'   time = "course",
#'   variables = c("views", "duration"),
#'   labels = c("Low", "Average", "High")
#' )
#' states
#' @export
states <- function(
    data,
    id = NULL,
    time = NULL,
    variables = NULL,
    n_states = "auto",
    labels = NULL,
    state = "state",
    standardize = NULL,
    missing = NULL,
    time_levels = NULL,
    n_start = 25L,
    seed = 123L,
    engine = NULL,
    lpa_model = "EEI",
    n_trajectories = "auto",
    method = NULL) {
  stopifnot(is.data.frame(data))
  engine <- .vasstra_resolve_engine(engine, method)
  if (.vasstra_is_latents_fit(engine)) {
    return(.vasstra_states_from_fit(
      engine,
      data = data,
      id = id,
      time = time,
      labels = labels,
      state = state,
      time_levels = time_levels
    ))
  }
  model_engine <- engine %in% .vasstra_model_engines()
  two_level <- engine %in% .vasstra_two_level_engines()
  data_specification <- .vasstra_resolve_state_data(
    data,
    id = id,
    time = time,
    variables = variables,
    standardize = standardize,
    missing = missing
  )
  id <- data_specification$id
  time <- data_specification$time
  variables <- data_specification$variables
  standardize <- match.arg(
    data_specification$standardize,
    c("time", "global", "none")
  )
  missing <- match.arg(
    data_specification$missing,
    c("error", "median", "fiml")
  )
  if (identical(missing, "fiml") && !model_engine) {
    stop(
      "`missing = \"fiml\"` needs a model engine: lpa, lca, multilpa, multilca, or lta.",
      call. = FALSE
    )
  }
  if (!is.character(lpa_model) || length(lpa_model) != 1L ||
      is.na(lpa_model) || !lpa_model %in% .vasstra_lpa_models()) {
    stop(sprintf(
      "`lpa_model` must be one of: %s.",
      paste(.vasstra_lpa_models(), collapse = ", ")
    ), call. = FALSE)
  }
  roles <- .vasstra_validate_roles(data, id, time)
  id <- roles$id
  time <- roles$time
  variables <- .vasstra_columns(data, variables, "variables")
  if (!model_engine && length(variables) < 2L) {
    stop("`variables` must name at least two numeric indicators.",
         call. = FALSE)
  }
  if (length(intersect(c(id, time), variables)) > 0L) {
    stop("`variables` cannot include the `id` or `time` column.",
         call. = FALSE)
  }
  if (!is.character(state) || length(state) != 1L || is.na(state) ||
      !nzchar(state)) {
    stop("`state` must be one non-empty column name.", call. = FALSE)
  }
  if (state %in% names(data)) {
    stop(sprintf(
      "Output state column `%s` already exists; choose another `state` name.",
      state
    ), call. = FALSE)
  }
  selection <- NULL
  selected_fit <- NULL
  if (.vasstra_is_auto(n_states) && !is.null(labels)) {
    n_states <- length(labels)
    message(sprintf(
      "Using n_states = %d to match the supplied labels.",
      n_states
    ))
  }
  candidate_counts <- if (.vasstra_is_auto(n_states)) {
    .vasstra_auto_range(nrow(data))
  } else if (is.numeric(n_states) && length(n_states) > 1L) {
    n_states
  } else {
    NULL
  }
  trajectory_counts <- if (!two_level) {
    NULL
  } else if (.vasstra_is_auto(n_trajectories)) {
    .vasstra_auto_range(length(unique(data[[id]])))
  } else if (is.numeric(n_trajectories) && length(n_trajectories) > 1L) {
    n_trajectories
  } else {
    NULL
  }
  if (!is.null(candidate_counts) || !is.null(trajectory_counts)) {
    sweep <- state_choices(
      data = data,
      id = id,
      time = time,
      variables = variables,
      n_states = if (is.null(candidate_counts)) n_states else candidate_counts,
      n_trajectories = if (!two_level) {
        NULL
      } else if (is.null(trajectory_counts)) {
        n_trajectories
      } else {
        trajectory_counts
      },
      method = engine,
      lpa_model = lpa_model,
      standardize = standardize,
      missing = missing,
      time_levels = time_levels,
      n_start = n_start,
      seed = seed,
      minimum_proportion = 0.05
    )
    best <- .vasstra_auto_pick(sweep, "state")
    n_states <- best$n_states[[1L]]
    if (two_level) {
      n_trajectories <- best$n_trajectories[[1L]]
    }
    # A model engine's chosen candidate is used as latents fitted it.
    if (model_engine) {
      selected_fit <- .vasstra_enumerated_fit(
        sweep$enumerations[[paste(engine, best$lpa_model[[1L]], sep = ":")]],
        n_states,
        if (two_level) n_trajectories else NA_integer_
      )
    }
    message(sprintf(
      "Selected n_states = %d%s (%s, %s = %.3f); see `diagnostics$selection`.",
      n_states,
      if (two_level) sprintf(", n_trajectories = %d", n_trajectories) else "",
      engine,
      best$recommendation_criterion[[1L]],
      best[[best$recommendation_criterion[[1L]]]][[1L]]
    ))
    selection <- list(
      candidate = best,
      candidates = sweep$candidates,
      enumeration = if (model_engine) {
        sweep$enumerations[[paste(engine, best$lpa_model[[1L]], sep = ":")]]
      } else {
        NULL
      }
    )
  }
  if (!is.numeric(n_states) || length(n_states) != 1L ||
      !is.finite(n_states) || n_states != floor(n_states) ||
      n_states < 2L) {
    stop("`n_states` must be a whole number of at least 2.", call. = FALSE)
  }
  n_states <- as.integer(n_states)
  if (nrow(data) <= n_states) {
    stop("The data must contain more rows than `n_states`.", call. = FALSE)
  }
  if (two_level) {
    n_subjects <- length(unique(data[[id]]))
    if (!is.numeric(n_trajectories) || length(n_trajectories) != 1L ||
        !is.finite(n_trajectories) ||
        n_trajectories != floor(n_trajectories) ||
        n_trajectories < 2L || n_trajectories > n_subjects - 1L) {
      stop(sprintf(
        "`n_trajectories` must be a whole number from 2 through %d.",
        n_subjects - 1L
      ), call. = FALSE)
    }
    n_trajectories <- as.integer(n_trajectories)
  }
  if (!is.numeric(n_start) || length(n_start) != 1L ||
      !is.finite(n_start) || n_start != floor(n_start) || n_start < 1L) {
    stop("`n_start` must be a positive whole number.", call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) {
    stop("`seed` must be one finite number.", call. = FALSE)
  }
  prepared <- .vasstra_prepare_indicators(
    data,
    id = id,
    time = time,
    variables = variables,
    engine = engine,
    standardize = standardize,
    missing = missing,
    time_levels = time_levels
  )
  categorical <- prepared$categorical
  numeric_set <- prepared$numeric_set
  chronology <- prepared$chronology
  standardized <- prepared$standardized
  missing_counts <- prepared$missing_counts
  constant_indicators <- prepared$constant_indicators
  if (!model_engine && nrow(unique(standardized)) < n_states) {
    stop(
      "The standardized data must contain at least `n_states` distinct rows.",
      call. = FALSE
    )
  }
  if (!is.null(selected_fit)) {
    model <- selected_fit
  } else if (model_engine) {
    model <- tryCatch(
      .vasstra_fit_latents(
        engine,
        frame = prepared$frame,
        variables = variables,
        categorical = categorical,
        id = id,
        n_states = n_states,
        n_trajectories = if (two_level) n_trajectories else 1L,
        lpa_model = lpa_model,
        missing = missing,
        n_start = n_start,
        seed = seed
      ),
      error = function(error) {
        stop(
          sprintf("State estimation failed: %s", conditionMessage(error)),
          call. = FALSE
        )
      }
    )
  } else {
    standardized_matrix <- as.matrix(standardized)
    model <- tryCatch(
      .vasstra_with_seed(
        seed,
        switch(
          engine,
          kmeans = stats::kmeans(
            x = standardized_matrix,
            centers = n_states,
            nstart = as.integer(n_start)
          ),
          pam = cluster::pam(
            x = standardized_matrix,
            k = n_states
          ),
          stats::hclust(
            d = stats::dist(standardized_matrix),
            method = engine
          )
        )
      ),
      error = function(error) {
        stop(
          sprintf("State estimation failed: %s", conditionMessage(error)),
          call. = FALSE
        )
      }
    )
    raw_assignments <- if (identical(engine, "kmeans")) {
      as.integer(model$cluster)
    } else if (identical(engine, "pam")) {
      as.integer(model$clustering)
    } else {
      as.integer(stats::cutree(model, k = n_states))
    }
  }
  if (model_engine) {
    raw_assignments <- as.integer(model$subject_profiles)
  }
  .vasstra_assemble_states(
    model = model,
    raw_assignments = raw_assignments,
    n_states = n_states,
    data = data,
    values = standardized,
    id = id,
    time = time,
    variables = variables,
    categorical = categorical,
    state = state,
    labels = labels,
    chronology = chronology,
    engine = engine,
    settings = list(
      standardize = standardize,
      missing = missing,
      n_start = as.integer(n_start),
      seed = as.integer(seed),
      lpa_model = if (identical(engine, "lpa") ||
                      (model_engine && length(numeric_set) > 0L)) {
        lpa_model
      } else {
        NA_character_
      },
      n_trajectories = if (two_level) n_trajectories else NA_integer_,
      supplied_fit = FALSE
    ),
    diagnostics = list(
      missing_imputed = if (identical(missing, "median")) {
        sum(missing_counts)
      } else {
        0L
      },
      n_missing = sum(missing_counts),
      zero_variance_indicators = numeric_set[constant_indicators],
      selection = selection
    )
  )
}

# The states of a fitted latents model, as a `vasstra_states` object. Nothing
# is refitted; `data` supplies the subjects, times and other columns.
.vasstra_states_from_fit <- function(
    fit,
    data,
    id,
    time,
    labels,
    state,
    time_levels) {
  stopifnot(.vasstra_is_latents_fit(fit), is.data.frame(data))
  .vasstra_check_fit_alignment(fit, data)
  engine <- .vasstra_fit_engine(fit)
  two_level <- !isTRUE(fit$single_level)
  variables <- fit$vars
  if (two_level) {
    if (!is.null(id) && !identical(id, fit$id)) {
      stop(sprintf(
        "The latents fit groups observations by `%s`, not `%s`.",
        fit$id, id
      ), call. = FALSE)
    }
    id <- fit$id
  }
  if (is.null(time) && !is.null(fit$time)) {
    time <- fit$time
  }
  roles <- .vasstra_auto_roles(
    data,
    id = id,
    time = time,
    variables = variables
  )
  roles <- .vasstra_validate_roles(data, roles$id, roles$time)
  if (two_level) {
    fit_ids <- as.character(fit$group_ids[fit$group_index])
    if (!identical(fit_ids, as.character(data[[roles$id]]))) {
      stop(sprintf(
        "`data$%s` does not match the groups of the latents fit row for row.",
        roles$id
      ), call. = FALSE)
    }
  }
  if (!is.character(state) || length(state) != 1L || is.na(state) ||
      !nzchar(state) || state %in% names(data)) {
    stop(
      "`state` must name a new column; the data already has one called that.",
      call. = FALSE
    )
  }
  categorical <- intersect(variables, c(fit$categorical, fit$ordinal))
  chronology <- .vasstra_time_levels(data[[roles$time]], time_levels)
  .vasstra_assemble_states(
    model = fit,
    raw_assignments = as.integer(fit$subject_profiles),
    n_states = as.integer(fit$n_profiles),
    data = data,
    values = data[variables],
    id = roles$id,
    time = roles$time,
    variables = variables,
    categorical = categorical,
    state = state,
    labels = labels,
    chronology = chronology,
    engine = engine,
    settings = list(
      standardize = "none",
      missing = if (anyNA(data[variables])) "fiml" else "error",
      n_start = as.integer(nrow(fit$starts)),
      seed = NA_integer_,
      lpa_model = if (length(fit$continuous) > 0L) {
        as.character(fit$covariance_structure)
      } else {
        NA_character_
      },
      n_trajectories = if (two_level) {
        as.integer(fit$n_group_classes)
      } else {
        NA_integer_
      },
      supplied_fit = TRUE
    ),
    diagnostics = list(
      missing_imputed = 0L,
      n_missing = sum(is.na(data[variables])),
      zero_variance_indicators = character(0L),
      selection = NULL
    )
  )
}

# The shared tail of every engine: order and label the states, describe
# them, and collect the diagnostics into a `vasstra_states` object.
.vasstra_assemble_states <- function(
    model,
    raw_assignments,
    n_states,
    data,
    values,
    id,
    time,
    variables,
    categorical,
    state,
    labels,
    chronology,
    engine,
    settings,
    diagnostics) {
  stopifnot(is.data.frame(data), is.data.frame(values), is.list(settings))
  model_engine <- engine %in% .vasstra_model_engines()
  if (length(unique(raw_assignments)) != n_states) {
    stop(sprintf(
      "State estimation produced %d states instead of the requested %d.",
      length(unique(raw_assignments)),
      n_states
    ), call. = FALSE)
  }
  ordering <- .vasstra_ordering_matrix(values, variables, categorical)
  empirical_centers <- do.call(rbind, lapply(
    seq_len(n_states),
    function(state_id) {
      colMeans(ordering[raw_assignments == state_id, , drop = FALSE],
               na.rm = TRUE)
    }
  ))
  colnames(empirical_centers) <- variables
  center_scores <- rowMeans(empirical_centers, na.rm = TRUE)
  old_order <- order(center_scores)
  assignments <- match(raw_assignments, old_order)
  if (is.null(labels)) {
    labels <- paste("State", seq_len(n_states))
  }
  if (!is.character(labels) || length(labels) != n_states ||
      anyNA(labels) || any(!nzchar(labels)) || anyDuplicated(labels)) {
    stop("`labels` must contain one unique non-empty label per state.",
         call. = FALSE)
  }
  state_values <- factor(
    labels[assignments],
    levels = labels,
    ordered = TRUE
  )
  result_data <- data
  result_data[[state]] <- state_values
  profiles <- .vasstra_state_profile_rows(
    values,
    variables,
    categorical,
    assignments,
    labels
  )
  if (length(categorical) == 0L) {
    profiles$category <- NULL
  }
  model$vasstra_assignments <- assignments
  model$vasstra_centers <- empirical_centers[old_order, , drop = FALSE]
  if (identical(engine, "kmeans")) {
    model$cluster <- assignments
    model$centers <- model$centers[old_order, , drop = FALSE]
    model$size <- model$size[old_order]
    model$withinss <- model$withinss[old_order]
  }
  state_sizes <- stats::setNames(
    as.integer(tabulate(assignments, nbins = n_states)),
    labels
  )
  # Silhouette widths need distances between observations, which only
  # complete numeric indicators define.
  silhouette <- if (length(categorical) == 0L && !anyNA(values)) {
    .vasstra_silhouette(
      as.numeric(assignments),
      stats::dist(as.matrix(values))
    )
  } else {
    NA_real_
  }
  information <- if (model_engine) {
    .vasstra_latents_diagnostics(model)
  } else {
    list(
      aic = NA_real_, bic = NA_real_, sabic = NA_real_, caic = NA_real_,
      awe = NA_real_, clc = NA_real_, kic = NA_real_, icl = NA_real_,
      prob_min = NA_real_, prob_max = NA_real_, bic_native = NA_real_,
      icl_native = NA_real_, classification_entropy = NA_real_,
      mean_uncertainty = NA_real_, log_likelihood = NA_real_,
      n_parameters = NA_integer_
    )
  }
  posteriors <- if (model_engine) {
    probabilities <- .vasstra_fit_posteriors(model)[, old_order, drop = FALSE]
    probabilities <- pmin(pmax(probabilities, 0), 1)
    frame <- data.frame(
      data[[id]],
      data[[time]],
      state_values,
      pmax(1 - apply(probabilities, 1L, max), 0),
      stringsAsFactors = FALSE
    )
    names(frame) <- c(id, time, state, "uncertainty")
    posterior_columns <- as.data.frame(probabilities)
    names(posterior_columns) <- paste0("posterior_", seq_len(n_states))
    cbind(frame, posterior_columns)
  } else {
    NULL
  }
  structure(
    list(
      data = result_data,
      profiles = profiles,
      standardized = values,
      posteriors = posteriors,
      model = model,
      settings = list(
        id = id,
        time = time,
        variables = variables,
        categorical = categorical,
        state = state,
        n_states = n_states,
        labels = labels,
        standardize = settings$standardize,
        missing = settings$missing,
        time_levels = chronology,
        n_start = settings$n_start,
        seed = settings$seed,
        engine = engine,
        method = engine,
        lpa_model = settings$lpa_model,
        n_trajectories = settings$n_trajectories,
        supplied_fit = settings$supplied_fit
      ),
      diagnostics = c(
        list(
          n_rows = nrow(data),
          n_subjects = length(unique(data[[id]])),
          n_times = length(chronology),
          missing_imputed = diagnostics$missing_imputed,
          n_missing = diagnostics$n_missing,
          state_sizes = state_sizes,
          zero_variance_indicators = diagnostics$zero_variance_indicators,
          silhouette = silhouette,
          total_withinss = if (identical(engine, "kmeans")) {
            as.numeric(model$tot.withinss)
          } else {
            NA_real_
          },
          pam_objective = if (identical(engine, "pam")) {
            as.numeric(model$objective[["swap"]])
          } else {
            NA_real_
          }
        ),
        information,
        list(selection = diagnostics$selection)
      )
    ),
    class = c("vasstra_states", "vasstra_step", "list")
  )
}

#' @export
print.vasstra_states <- function(x, ...) {
  stopifnot(inherits(x, "vasstra_states"))
  cat("VaSStra Step 1: Variables -> States\n")
  engine <- x$settings$engine
  if (is.null(engine)) engine <- x$settings$method
  two_level <- engine %in% .vasstra_two_level_engines()
  cat(sprintf(
    "  %d rows | %d subjects | %d times | %d states%s | %s\n",
    x$diagnostics$n_rows,
    x$diagnostics$n_subjects,
    x$diagnostics$n_times,
    x$settings$n_states,
    if (two_level) {
      sprintf(" | %d trajectory classes", x$settings$n_trajectories)
    } else {
      ""
    },
    engine
  ))
  if (engine %in% .vasstra_model_engines()) {
    cat(sprintf(
      "  Log likelihood %.3f | BIC %.3f | relative entropy %.3f\n",
      x$diagnostics$log_likelihood,
      x$diagnostics$bic,
      x$diagnostics$classification_entropy
    ))
  } else {
    cat(sprintf("  Average silhouette: %.3f\n", x$diagnostics$silhouette))
  }
  cat("  State sizes: ", paste(
    sprintf("%s=%d", names(x$diagnostics$state_sizes),
            x$diagnostics$state_sizes),
    collapse = ", "
  ), "\n", sep = "")
  invisible(x)
}

#' @export
summary.vasstra_states <- function(object, ...) {
  stopifnot(inherits(object, "vasstra_states"))
  object$profiles
}

#' @export
as.data.frame.vasstra_states <- function(x, row.names = NULL, optional = FALSE,
                                         ..., what = c("data", "posteriors")) {
  stopifnot(inherits(x, "vasstra_states"))
  what <- match.arg(what)
  if (identical(what, "posteriors")) {
    if (is.null(x$posteriors)) {
      stop(
        "Posterior probabilities come from a model engine; these states were clustered.",
        call. = FALSE
      )
    }
    return(as.data.frame(x$posteriors, row.names = row.names,
                         optional = optional, ...))
  }
  as.data.frame(x$data, row.names = row.names, optional = optional, ...)
}

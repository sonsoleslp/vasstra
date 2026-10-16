# State engines.
#
# An engine is what turns variables into states. The hard-clustering engines
# (k-means, PAM, hierarchical) assign each observation to one state and leave
# trajectories to sequence clustering in step 3. The model engines are the
# mixture models of the latents package: `lpa` and `lca` estimate states,
# while `multilpa`, `multilca` and `lta` also estimate classes of subjects,
# which are the trajectories. Every engine returns the same `vasstra_states`
# object, so every later step, plot and accessor works on all of them.

.vasstra_model_engines <- function() {
  stopifnot(TRUE)
  c("lpa", "lca", "multilpa", "multilca", "lta")
}

.vasstra_two_level_engines <- function() {
  stopifnot(TRUE)
  c("multilpa", "multilca", "lta")
}

.vasstra_cluster_engines <- function() {
  stopifnot(TRUE)
  c(
    "kmeans", "pam", "clara", "fanny", "diana",
    "ward.D2", "ward.D", "complete", "average",
    "single", "mcquitty", "median", "centroid"
  )
}

# Engines whose every indicator is categorical.
.vasstra_categorical_engines <- function() {
  stopifnot(TRUE)
  c("lca", "multilca")
}

.vasstra_is_latents_fit <- function(x) {
  inherits(x, c("multilpa", "multilpa_transitions"))
}

# Resolve `engine` against the superseded `method` (or `state_method`)
# argument. A fitted latents model is passed through unchanged.
.vasstra_resolve_engine <- function(engine, method, method_name = "method") {
  if (.vasstra_is_latents_fit(engine)) {
    return(engine)
  }
  if (!is.null(method)) {
    if (!is.null(engine) && !identical(engine, method)) {
      stop(sprintf(
        "Supply `engine` or the superseded `%s`, not both.",
        method_name
      ), call. = FALSE)
    }
    engine <- method
  }
  if (is.null(engine)) {
    engine <- "lpa"
  }
  if (!is.character(engine) || length(engine) != 1L || is.na(engine) ||
      !engine %in% c(.vasstra_model_engines(), .vasstra_cluster_engines())) {
    stop(sprintf(
      "`engine` must be a fitted latents model or one of: %s.",
      paste(c(.vasstra_model_engines(), .vasstra_cluster_engines()),
            collapse = ", ")
    ), call. = FALSE)
  }
  engine
}

# The engine name of a fitted latents model.
.vasstra_fit_engine <- function(fit) {
  stopifnot(.vasstra_is_latents_fit(fit))
  categorical_only <- length(fit$continuous) == 0L &&
    length(fit$ordinal) == 0L && length(fit$count) == 0L
  if (inherits(fit, "multilpa_transitions")) {
    return("lta")
  }
  if (isTRUE(fit$single_level)) {
    return(if (categorical_only) "lca" else "lpa")
  }
  if (categorical_only) "multilca" else "multilpa"
}

# Indicators a model engine treats as categorical: all of them for lca and
# multilca, and the non-numeric ones for the other model engines.
.vasstra_categorical_variables <- function(data, variables, engine) {
  stopifnot(is.data.frame(data), is.character(variables))
  if (engine %in% .vasstra_categorical_engines()) {
    return(variables)
  }
  variables[!vapply(data[variables], is.numeric, logical(1L))]
}

# Fit one latents model. `frame` holds the indicators as the model should see
# them, an integer `.vasstra_position` time column and the subject column.
.vasstra_fit_latents <- function(
    engine,
    frame,
    variables,
    categorical,
    id,
    n_states,
    n_trajectories,
    lpa_model,
    missing,
    n_start,
    seed) {
  stopifnot(
    engine %in% .vasstra_model_engines(),
    is.data.frame(frame),
    is.character(variables),
    is.character(categorical)
  )
  continuous <- setdiff(variables, categorical)
  common <- list(
    data = frame,
    vars = variables,
    missing = if (identical(missing, "fiml")) "fiml" else "error",
    n_starts = as.integer(n_start),
    seed = as.integer(seed)
  )
  # The covariance structure applies to continuous indicators only.
  structure_arguments <- if (length(continuous) > 0L) {
    list(model = lpa_model)
  } else {
    list()
  }
  categorical_arguments <- if (length(categorical) > 0L &&
                               !engine %in% .vasstra_categorical_engines()) {
    list(categorical = categorical)
  } else {
    list()
  }
  arguments <- switch(
    engine,
    lpa = c(common, list(n_profiles = n_states), structure_arguments,
            categorical_arguments),
    lca = c(common, list(n_classes = n_states)),
    multilpa = c(common, list(
      id = id, n_profiles = n_states, n_group_classes = n_trajectories,
      time = ".vasstra_position"
    ), structure_arguments, categorical_arguments),
    multilca = c(common, list(
      id = id, n_profiles = n_states, n_group_classes = n_trajectories,
      time = ".vasstra_position"
    )),
    lta = c(common, list(
      id = id, n_profiles = n_states, n_group_classes = n_trajectories,
      time = ".vasstra_position"
    ), structure_arguments, categorical_arguments)
  )
  verb <- switch(
    engine,
    lpa = latents::lpa,
    lca = latents::lca,
    multilpa = latents::multilpa,
    multilca = latents::multilca,
    lta = latents::lta
  )
  .vasstra_with_seed(seed, do.call(verb, arguments))
}

# The values the states are described and ordered by: numeric indicators as
# given, categorical ones as their level position scaled to [0, 1].
.vasstra_ordering_matrix <- function(frame, variables, categorical) {
  stopifnot(is.data.frame(frame), is.character(variables))
  columns <- lapply(variables, function(variable) {
    values <- frame[[variable]]
    if (!variable %in% categorical) {
      return(as.numeric(values))
    }
    levels_seen <- if (is.factor(values)) {
      levels(values)
    } else {
      sort(unique(as.character(values[!is.na(values)])))
    }
    codes <- match(as.character(values), levels_seen)
    if (length(levels_seen) > 1L) {
      (codes - 1) / (length(levels_seen) - 1)
    } else {
      codes * 0
    }
  })
  matrix(
    unlist(columns, use.names = FALSE),
    nrow = nrow(frame),
    dimnames = list(NULL, variables)
  )
}

# One tidy row per state and indicator (numeric), or per state, indicator and
# category (categorical): the members' mean and standard deviation, or the
# share of members in that category.
.vasstra_state_profile_rows <- function(
    frame,
    variables,
    categorical,
    assignments,
    labels) {
  n_states <- length(labels)
  counts <- as.integer(tabulate(assignments, nbins = n_states))
  rows <- lapply(variables, function(variable) {
    values <- frame[[variable]]
    if (!variable %in% categorical) {
      values <- as.numeric(values)
      return(data.frame(
        state = labels,
        variable = variable,
        category = NA_character_,
        mean = vapply(seq_len(n_states), function(state_id) {
          mean(values[assignments == state_id], na.rm = TRUE)
        }, numeric(1L)),
        sd = vapply(seq_len(n_states), function(state_id) {
          stats::sd(values[assignments == state_id], na.rm = TRUE)
        }, numeric(1L)),
        n = counts,
        stringsAsFactors = FALSE
      ))
    }
    categories <- if (is.factor(values)) {
      levels(values)
    } else {
      sort(unique(as.character(values[!is.na(values)])))
    }
    values <- as.character(values)
    do.call(rbind, lapply(categories, function(category) {
      data.frame(
        state = labels,
        variable = variable,
        category = category,
        mean = vapply(seq_len(n_states), function(state_id) {
          member_values <- values[assignments == state_id]
          mean(member_values == category, na.rm = TRUE)
        }, numeric(1L)),
        sd = NA_real_,
        n = counts,
        stringsAsFactors = FALSE
      )
    }))
  })
  profiles <- do.call(rbind, rows)
  rownames(profiles) <- NULL
  profiles
}

# The scalar diagnostics every states object carries, for a model engine
# read from the fit's own latents tables. A single-level fit reports its
# criteria with the observations as sample size, a two-level fit with the
# subjects, as latents' own `bic` does. The full tables stay with latents:
# fit_indices() and evaluate() return them unchanged.
.vasstra_latents_diagnostics <- function(fit) {
  stopifnot(.vasstra_is_latents_fit(fit))
  criteria <- latents::get_results(fit, "information_criteria")
  pick <- function(name) {
    suffix <- if (isTRUE(fit$single_level)) "_individual" else "_groups"
    as.numeric(criteria[[paste0(name, suffix)]][[1L]])
  }
  entropy <- latents::get_results(fit, "entropy")
  classification <- latents::get_results(fit, "classification")
  individuals <- classification[classification$level == "individuals", ,
                                drop = FALSE]
  posteriors <- .vasstra_fit_posteriors(fit)
  bic <- pick("bic")
  icl <- pick("icl")
  list(
    aic = as.numeric(criteria$aic[[1L]]),
    bic = bic,
    sabic = pick("sabic"),
    caic = pick("caic"),
    awe = pick("awe"),
    clc = pick("clc"),
    kic = as.numeric(criteria$kic[[1L]]),
    icl = icl,
    prob_min = min(individuals$average_posterior),
    prob_max = max(individuals$average_posterior),
    bic_native = -bic,
    icl_native = -icl,
    classification_entropy = as.numeric(
      entropy$relative_entropy[entropy$level == "individuals"][[1L]]
    ),
    mean_uncertainty = mean(pmax(1 - apply(posteriors, 1L, max), 0)),
    log_likelihood = as.numeric(fit$log_likelihood),
    n_parameters = as.integer(fit$n_parameters)
  )
}

# The observation posteriors of a fit, one column per profile, without a
# noise column.
.vasstra_fit_posteriors <- function(fit) {
  posteriors <- fit$subject_posteriors
  posteriors[, seq_len(fit$n_profiles), drop = FALSE]
}

# Each subject's trajectory class from a two-level fit, ordered as `ids`,
# with the posterior of the assigned class.
.vasstra_fit_trajectory_classes <- function(fit, ids) {
  stopifnot(.vasstra_is_latents_fit(fit), !isTRUE(fit$single_level))
  position <- match(as.character(ids), as.character(fit$group_ids))
  if (anyNA(position)) {
    stop(
      "Some subjects of the sequences are not groups of the latents fit.",
      call. = FALSE
    )
  }
  posteriors <- fit$group_posteriors[position, , drop = FALSE]
  list(
    classes = as.integer(fit$group_classes[position]),
    certainty = pmin(apply(posteriors, 1L, max), 1),
    posteriors = posteriors
  )
}

# Check that `data` holds, row for row, the indicators a supplied latents fit
# was estimated from.
.vasstra_check_fit_alignment <- function(fit, data) {
  stopifnot(.vasstra_is_latents_fit(fit), is.data.frame(data))
  n_fit <- length(fit$subject_profiles)
  if (nrow(data) != n_fit) {
    stop(sprintf(
      "`data` has %d rows but the latents fit was estimated from %d.",
      nrow(data), n_fit
    ), call. = FALSE)
  }
  missing_columns <- setdiff(fit$vars, names(data))
  if (length(missing_columns) > 0L) {
    stop(sprintf(
      "`data` lacks the fit's indicator%s: %s.",
      if (length(missing_columns) == 1L) "" else "s",
      paste(missing_columns, collapse = ", ")
    ), call. = FALSE)
  }
  continuous <- intersect(fit$continuous, colnames(fit$indicator_data))
  # A centred fit stores its indicators centred within subjects or around the
  # grand mean, so only an uncentred fit can be compared value for value.
  uncentred <- is.null(fit$centering) || identical(fit$centering, "none")
  if (length(continuous) > 0L && uncentred) {
    given <- as.matrix(data[continuous])
    used <- fit$indicator_data[, continuous, drop = FALSE]
    same_missing <- identical(is.na(given), is.na(used))
    close <- same_missing &&
      isTRUE(all.equal(given[!is.na(given)], used[!is.na(used)],
                       tolerance = 1e-8, check.attributes = FALSE))
    if (!close) {
      stop(
        paste0(
          "`data` does not hold the indicator values the latents fit was ",
          "estimated from, row for row. Pass the data the model was fitted to."
        ),
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

# Step 3 for states from a two-level engine: the model's classes of subjects
# are the trajectories. Silhouette and within-trajectory distances are taken
# on the Hamming distance, a yardstick shared with the clustering methods.
.vasstra_model_trajectories <- function(data, n_trajectories, labels, seed) {
  stopifnot(inherits(data, "vasstra_sequences"))
  states <- data$source
  fit <- states$model
  fitted_count <- as.integer(fit$n_group_classes)
  if (!.vasstra_is_auto(n_trajectories) &&
      !identical(as.integer(n_trajectories), fitted_count)) {
    stop(sprintf(
      paste0(
        "The %s model estimated %d trajectory classes; refit the states with ",
        "`n_trajectories = %s` to change that."
      ),
      states$settings$engine, fitted_count,
      paste(n_trajectories, collapse = ", ")
    ), call. = FALSE)
  }
  id <- data$settings$id
  classes <- .vasstra_fit_trajectory_classes(fit, data$meta_data[[id]])
  empty <- sum(tabulate(classes$classes, nbins = fitted_count) == 0L)
  if (empty > 0L) {
    warning(sprintf(
      paste0(
        "The %s model left %d of %d trajectory classes without subjects; ",
        "fewer classes, or an engine that models the order of states ",
        "(lta), may describe these sequences better."
      ),
      states$settings$engine, empty, fitted_count
    ), call. = FALSE)
  }
  sequence_matrix <- as.matrix(data$data)
  distance <- .vasstra_hamming_distance(sequence_matrix)
  silhouette <- .vasstra_silhouette(classes$classes, distance)
  entropy <- latents::get_results(fit, "entropy")
  .vasstra_assemble_trajectories(
    data = data,
    raw_assignments = classes$classes,
    n_trajectories = fitted_count,
    labels = labels,
    distance = distance,
    silhouette = silhouette,
    model = fit,
    settings = list(
      dissimilarity = "hamming",
      method = "model",
      backend = "latents",
      engine = states$settings$engine,
      seed = if (is.numeric(seed)) as.integer(seed) else NA_integer_
    ),
    selection = states$diagnostics$selection,
    # How surely the model places the subjects of each trajectory: the mean
    # posterior probability of their assigned class.
    extra_summary = function(assignments) {
      data.frame(average_posterior = vapply(
        seq_len(fitted_count),
        function(class_id) {
          members <- assignments == class_id
          if (any(members)) mean(classes$certainty[members]) else NA_real_
        },
        numeric(1L)
      ))
    },
    extra_diagnostics = list(
      relative_entropy = as.numeric(
        entropy$relative_entropy[entropy$level == "groups"][[1L]]
      ),
      log_likelihood = as.numeric(fit$log_likelihood),
      bic = as.numeric(fit$bic)
    )
  )
}

# Prepare the indicators an engine is fitted to: categorical ones as given,
# numeric ones checked, imputed under `missing = "median"`, and standardized.
# `frame` adds the subject column and the integer time position a latents
# model is fitted with.
.vasstra_prepare_indicators <- function(
    data,
    id,
    time,
    variables,
    engine,
    standardize,
    missing,
    time_levels) {
  stopifnot(is.data.frame(data), is.character(variables))
  model_engine <- engine %in% .vasstra_model_engines()
  categorical <- if (model_engine) {
    .vasstra_categorical_variables(data, variables, engine)
  } else {
    character(0L)
  }
  numeric_set <- setdiff(variables, categorical)
  numeric_variables <- vapply(data[numeric_set], is.numeric, logical(1L))
  if (!all(numeric_variables)) {
    stop(sprintf(
      "All state indicators must be numeric; check: %s.",
      paste(numeric_set[!numeric_variables], collapse = ", ")
    ), call. = FALSE)
  }
  chronology <- .vasstra_time_levels(data[[time]], time_levels)
  indicator_data <- data[variables]
  infinite_values <- vapply(
    indicator_data,
    function(values) is.numeric(values) && any(is.infinite(values)),
    logical(1L)
  )
  if (any(infinite_values)) {
    stop(sprintf(
      "Infinite indicator values found in: %s.",
      paste(variables[infinite_values], collapse = ", ")
    ), call. = FALSE)
  }
  missing_counts <- vapply(indicator_data, function(values) {
    sum(is.na(values))
  }, integer(1L))
  if (missing == "error" && any(missing_counts > 0L)) {
    stop(sprintf(
      "Missing indicator values found in: %s. Use `missing = \"median\"`%s.",
      paste(variables[missing_counts > 0L], collapse = ", "),
      if (model_engine) " or `missing = \"fiml\"`" else " to impute"
    ), call. = FALSE)
  }
  if (missing == "median" && any(missing_counts[categorical] > 0L)) {
    stop(sprintf(
      "Median imputation needs numeric indicators; use `missing = \"fiml\"` for: %s.",
      paste(categorical[missing_counts[categorical] > 0L], collapse = ", ")
    ), call. = FALSE)
  }
  if (missing == "median" && any(missing_counts > 0L)) {
    indicator_data[numeric_set] <- lapply(
      indicator_data[numeric_set],
      function(values) {
        group_median <- stats::ave(
          values,
          data[[time]],
          FUN = function(group_values) {
            if (all(is.na(group_values))) NA_real_ else stats::median(
              group_values,
              na.rm = TRUE
            )
          }
        )
        global_median <- stats::median(values, na.rm = TRUE)
        replacement <- ifelse(is.na(group_median), global_median, group_median)
        ifelse(is.na(values), replacement, values)
      }
    )
    if (anyNA(indicator_data)) {
      stop(
        "Median imputation could not resolve an indicator that is entirely missing.",
        call. = FALSE
      )
    }
  }
  working_data <- data
  working_data[variables] <- indicator_data
  grouping <- if (standardize == "time") {
    working_data[[time]]
  } else if (standardize == "subject") {
    working_data[[id]]
  } else {
    rep.int(1L, nrow(working_data))
  }
  constant_indicators <- vapply(numeric_set, function(variable) {
    any(vapply(
      split(working_data[[variable]], grouping, drop = TRUE),
      function(values) {
        deviation <- stats::sd(values, na.rm = TRUE)
        !is.finite(deviation) || deviation == 0
      },
      logical(1L)
    ))
  }, logical(1L))
  if (standardize != "none" && any(constant_indicators)) {
    warning(sprintf(
      paste0(
        "Zero-variance standardized strata were set to zero for: %s."
      ),
      paste(numeric_set[constant_indicators], collapse = ", ")
    ), call. = FALSE)
  }
  standardized <- working_data[variables]
  if (length(numeric_set) > 0L) {
    standardized[numeric_set] <- .vasstra_standardize(
      working_data,
      numeric_set,
      time,
      standardize,
      id = id
    )
  }
  numeric_values <- as.matrix(standardized[numeric_set])
  if (!identical(missing, "fiml") &&
      (anyNA(numeric_values) || any(!is.finite(numeric_values)))) {
    stop("Standardization produced missing or non-finite values.",
         call. = FALSE)
  }
  frame <- standardized
  frame[[id]] <- data[[id]]
  frame$.vasstra_position <- match(
    as.character(data[[time]]),
    as.character(chronology)
  )
  list(
    categorical = categorical,
    numeric_set = numeric_set,
    chronology = chronology,
    standardized = standardized,
    frame = frame,
    missing_counts = missing_counts,
    constant_indicators = constant_indicators
  )
}

# Enumerate a model engine over candidate counts with latents: every count of
# states, and for a two-level engine every count of trajectory classes.
.vasstra_enumerate_latents <- function(
    engine,
    frame,
    variables,
    categorical,
    id,
    n_states,
    n_trajectories,
    lpa_model,
    missing,
    n_start,
    seed) {
  stopifnot(engine %in% .vasstra_model_engines(), is.data.frame(frame))
  continuous <- setdiff(variables, categorical)
  common <- list(
    data = frame,
    vars = variables,
    missing = if (identical(missing, "fiml")) "fiml" else "error",
    n_starts = as.integer(n_start),
    seed = as.integer(seed)
  )
  structure_arguments <- if (length(continuous) > 0L) {
    list(model = lpa_model)
  } else {
    list()
  }
  categorical_arguments <- if (length(categorical) > 0L) {
    list(categorical = categorical)
  } else {
    list()
  }
  arguments <- switch(
    engine,
    lpa = c(common, list(n_profiles = n_states), structure_arguments,
            categorical_arguments),
    lca = c(common, list(n_classes = n_states)),
    multilpa = ,
    multilca = c(common, list(
      id = id, n_profiles = n_states, n_group_classes = n_trajectories
    ), structure_arguments, categorical_arguments),
    lta = c(common, list(
      id = id, n_profiles = n_states, n_group_classes = n_trajectories,
      time = ".vasstra_position"
    ), structure_arguments, categorical_arguments)
  )
  verb <- switch(
    engine,
    lpa = latents::enumerate_lpa,
    lca = latents::enumerate_lca,
    latents::enumerate_classes
  )
  .vasstra_with_seed(seed, do.call(verb, arguments))
}

# The fitted model of one enumerated candidate, as latents stored it.
.vasstra_enumerated_fit <- function(enumeration, n_states, n_trajectories) {
  latents::get_candidate(
    enumeration,
    n_profiles = as.integer(n_states),
    n_group_classes = if (is.na(n_trajectories)) 1L else
      as.integer(n_trajectories)
  )
}

# One row per enumerated candidate in the columns VaSSTra compares engines
# by. Every number is read from latents' candidate table or its stored
# candidate fit: a single-level grid reports the individual-level criteria,
# a two-level grid the group-level ones, as latents' own `bic` does.
.vasstra_enumeration_rows <- function(enumeration, engine, lpa_model,
                                      values = NULL) {
  candidates <- latents::get_results(enumeration, "candidates")
  # Silhouette is VaSSTra's own yardstick across engines; it is taken on the
  # standardized indicators when they are complete and numeric.
  distance <- if (!is.null(values) && all(vapply(values, is.numeric,
                                                 logical(1L))) &&
                  !anyNA(values)) {
    stats::dist(as.matrix(values))
  } else {
    NULL
  }
  two_level <- engine %in% .vasstra_two_level_engines()
  suffix <- if (two_level) "_groups" else "_individual"
  column <- function(candidate, name) {
    value <- candidate[[name]]
    if (is.null(value)) {
      value <- candidate[[paste0(name, suffix)]]
    }
    if (is.null(value)) NA_real_ else as.numeric(value[[1L]])
  }
  rows <- lapply(seq_len(nrow(candidates)), function(index) {
    candidate <- candidates[index, , drop = FALSE]
    failed <- !is.na(candidate$error[[1L]])
    n_trajectories <- if (two_level) {
      as.integer(candidate$n_group_classes[[1L]])
    } else {
      NA_integer_
    }
    fit <- if (failed) {
      NULL
    } else {
      .vasstra_enumerated_fit(enumeration, candidate$n_profiles[[1L]],
                              n_trajectories)
    }
    sizes <- if (is.null(fit)) {
      NA_integer_
    } else {
      tabulate(fit$subject_profiles, nbins = fit$n_profiles)
    }
    trajectory_sizes <- if (is.null(fit) || !two_level) {
      NA_integer_
    } else {
      tabulate(fit$group_classes, nbins = fit$n_group_classes)
    }
    posteriors <- if (is.null(fit)) NULL else .vasstra_fit_posteriors(fit)
    bic <- column(candidate, "bic")
    icl <- column(candidate, "icl")
    entropy <- column(candidate, "profile_entropy")
    if (is.na(entropy)) entropy <- column(candidate, "entropy")
    warning_text <- candidate$warnings[[1L]]
    data.frame(
      n_states = as.integer(candidate$n_profiles[[1L]]),
      n_trajectories = n_trajectories,
      method = engine,
      lpa_model = lpa_model,
      status = if (failed) "failed" else "ok",
      error = if (failed) candidate$error[[1L]] else NA_character_,
      silhouette = if (is.null(fit) || is.null(distance) ||
                       length(unique(fit$subject_profiles)) < 2L) {
        NA_real_
      } else {
        .vasstra_silhouette(as.numeric(fit$subject_profiles), distance)
      },
      total_withinss = NA_real_,
      pam_objective = NA_real_,
      aic = column(candidate, "aic"),
      bic = bic,
      sabic = column(candidate, "sabic"),
      caic = column(candidate, "caic"),
      awe = column(candidate, "awe"),
      clc = column(candidate, "clc"),
      kic = column(candidate, "kic"),
      icl = icl,
      prob_min = column(candidate, "avepp_min"),
      prob_max = column(candidate, "avepp_max"),
      bic_native = -bic,
      icl_native = -icl,
      classification_entropy = entropy,
      mean_uncertainty = if (is.null(posteriors)) {
        NA_real_
      } else {
        mean(pmax(1 - apply(posteriors, 1L, max), 0))
      },
      log_likelihood = column(candidate, "log_likelihood"),
      n_parameters = as.integer(candidate$n_parameters[[1L]]),
      min_size = min(sizes),
      max_size = max(sizes),
      min_proportion = min(sizes) / sum(sizes),
      size_ratio = max(sizes) / min(sizes),
      min_trajectory_proportion = min(trajectory_sizes) /
        sum(trajectory_sizes),
      warnings = if (is.na(warning_text) || !nzchar(warning_text)) {
        NA_character_
      } else {
        warning_text
      },
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# The data a states object's model was fitted to, as latents saw it.
.vasstra_model_frame <- function(x) {
  stopifnot(inherits(x, "vasstra_states"))
  settings <- x$settings
  frame <- x$standardized
  frame[[settings$id]] <- x$data[[settings$id]]
  frame$.vasstra_position <- match(
    as.character(x$data[[settings$time]]),
    as.character(settings$time_levels)
  )
  frame
}

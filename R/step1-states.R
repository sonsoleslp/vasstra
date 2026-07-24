#' Step 1: Turn Variables into States
#'
#' Standardizes numeric indicators and clusters each subject-time observation.
#' The default method is Gaussian-mixture latent profile analysis (LPA);
#' reproducible k-means, PAM, and hierarchical clustering are available
#' through `method`. Clusters are ordered by their average standardized
#' indicator level, then optionally given user-supplied labels.
#'
#' @param data A data frame with one row per subject and time point.
#' @param id Name of the subject identifier column. May be omitted when the
#'   data carry VaSStra role metadata.
#' @param time Name of the time or ordering column. May be omitted when the
#'   data carry VaSStra role metadata.
#' @param variables Character vector naming numeric state indicators. May be
#'   omitted when the data carry VaSStra role metadata.
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
#'   `"global"`, or `"none"`. Analysis-ready package data may provide its
#'   own default through metadata.
#' @param missing One of `"error"` (default for ordinary data) or
#'   `"median"`. Analysis-ready package data may provide its own default
#'   through metadata. Median imputation
#'   is performed within time point, with the global variable median as a
#'   fallback for an entirely missing time point.
#' @param time_levels Explicit chronological time values. Required for
#'   character or unordered-factor time columns.
#' @param n_start Number of random k-means starts.
#' @param seed Reproducible random seed.
#' @param method State-clustering method. Choose `"lpa"` (default,
#'   Gaussian-mixture latent profile analysis via mclust), `"kmeans"`,
#'   `"pam"`, or a hierarchical method: `"ward.D2"`, `"ward.D"`,
#'   `"complete"`, `"average"`, `"single"`, `"mcquitty"`, `"median"`, or
#'   `"centroid"`.
#' @param lpa_model Covariance model used only by `method = "lpa"`. The
#'   default `"EEI"` corresponds to tidyLPA model 1: equal variances and zero
#'   covariances.
#'
#' @return A `vasstra_states` object. Its `data` element is the original data
#'   with a factor state column; `profiles` is a tidy state-by-indicator table.
#'
#' @examples
#' set.seed(1)
#' data <- expand.grid(student = 1:12, course = 1:3)
#' level <- rep(c(2, 8, 16), length.out = nrow(data))
#' data$views <- level + rnorm(nrow(data), sd = 0.4)
#' data$duration <- level * 3 + rnorm(nrow(data), sd = 0.4)
#' states <- step1_states(
#'   data,
#'   id = "student",
#'   time = "course",
#'   variables = c("views", "duration"),
#'   labels = c("Low", "Average", "High")
#' )
#' states
#' @export
step1_states <- function(
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
    method = c(
      "lpa", "kmeans", "pam", "ward.D2", "ward.D", "complete", "average",
      "single", "mcquitty", "median", "centroid"
    ),
    lpa_model = "EEI") {
  stopifnot(is.data.frame(data))
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
    c("error", "median")
  )
  method <- match.arg(method)
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
  if (length(variables) < 2L) {
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
  if (!is.null(candidate_counts)) {
    sweep <- suppressWarnings(state_choices(
      data = data,
      id = id,
      time = time,
      variables = variables,
      n_states = candidate_counts,
      method = method,
      lpa_model = lpa_model,
      standardize = standardize,
      missing = missing,
      time_levels = time_levels,
      n_start = n_start,
      seed = seed,
      minimum_proportion = 0.05
    ))
    best <- .vasstra_auto_pick(sweep, "state")
    n_states <- best$n_states[[1L]]
    message(sprintf(
      "Selected n_states = %d (%s, %s = %.3f); see `diagnostics$selection`.",
      n_states,
      method,
      best$recommendation_criterion[[1L]],
      best[[best$recommendation_criterion[[1L]]]][[1L]]
    ))
    selection <- list(candidate = best, candidates = sweep$candidates)
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
  if (!is.numeric(n_start) || length(n_start) != 1L ||
      !is.finite(n_start) || n_start != floor(n_start) || n_start < 1L) {
    stop("`n_start` must be a positive whole number.", call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) {
    stop("`seed` must be one finite number.", call. = FALSE)
  }
  numeric_variables <- vapply(data[variables], is.numeric, logical(1L))
  if (!all(numeric_variables)) {
    stop(sprintf(
      "All state indicators must be numeric; check: %s.",
      paste(variables[!numeric_variables], collapse = ", ")
    ), call. = FALSE)
  }
  chronology <- .vasstra_time_levels(data[[time]], time_levels)
  indicator_data <- data[variables]
  infinite_values <- vapply(
    indicator_data,
    function(values) any(is.infinite(values)),
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
      "Missing indicator values found in: %s. Use `missing = \"median\"` to impute.",
      paste(variables[missing_counts > 0L], collapse = ", ")
    ), call. = FALSE)
  }
  if (missing == "median" && any(missing_counts > 0L)) {
    indicator_data <- as.data.frame(lapply(indicator_data, function(values) {
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
    }), stringsAsFactors = FALSE)
    names(indicator_data) <- variables
  }
  if (anyNA(indicator_data)) {
    stop(
      "Median imputation could not resolve an indicator that is entirely missing.",
      call. = FALSE
    )
  }
  working_data <- data
  working_data[variables] <- indicator_data
  grouping <- if (standardize == "time") {
    working_data[[time]]
  } else {
    rep.int(1L, nrow(working_data))
  }
  constant_indicators <- vapply(variables, function(variable) {
    any(vapply(
      split(working_data[[variable]], grouping, drop = TRUE),
      function(values) {
        deviation <- stats::sd(values)
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
      paste(variables[constant_indicators], collapse = ", ")
    ), call. = FALSE)
  }
  standardized <- .vasstra_standardize(
    working_data,
    variables,
    time,
    standardize
  )
  if (anyNA(standardized) ||
      any(!is.finite(as.matrix(standardized)))) {
    stop("Standardization produced missing or non-finite values.",
         call. = FALSE)
  }
  standardized_matrix <- as.matrix(standardized)
  if (nrow(unique(standardized)) < n_states) {
    stop(
      "The standardized data must contain at least `n_states` distinct rows.",
      call. = FALSE
    )
  }
  model <- tryCatch(
    .vasstra_with_seed(
      seed,
      switch(
        method,
        kmeans = stats::kmeans(
          x = standardized_matrix,
          centers = n_states,
          nstart = as.integer(n_start)
        ),
        pam = cluster::pam(
          x = standardized_matrix,
          k = n_states
        ),
        lpa = do.call(
          mclust::Mclust,
          list(
            data = standardized_matrix,
            G = n_states,
            modelNames = lpa_model,
            verbose = FALSE
          ),
          envir = asNamespace("mclust")
        ),
        stats::hclust(
          d = stats::dist(standardized_matrix),
          method = method
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
  raw_assignments <- if (identical(method, "kmeans")) {
    as.integer(model$cluster)
  } else if (identical(method, "pam")) {
    as.integer(model$clustering)
  } else if (identical(method, "lpa")) {
    as.integer(model$classification)
  } else {
    as.integer(stats::cutree(model, k = n_states))
  }
  if (length(unique(raw_assignments)) != n_states) {
    stop(sprintf(
      "State estimation produced %d states instead of the requested %d.",
      length(unique(raw_assignments)),
      n_states
    ), call. = FALSE)
  }
  empirical_centers <- do.call(rbind, lapply(
    seq_len(n_states),
    function(state_id) {
      colMeans(
        standardized_matrix[raw_assignments == state_id, , drop = FALSE]
      )
    }
  ))
  colnames(empirical_centers) <- variables
  center_scores <- rowMeans(empirical_centers)
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
  profile_rows <- lapply(seq_along(variables), function(index) {
    variable <- variables[[index]]
    values <- standardized[[variable]]
    means <- vapply(seq_len(n_states), function(state_id) {
      mean(values[assignments == state_id])
    }, numeric(1L))
    standard_deviations <- vapply(seq_len(n_states), function(state_id) {
      stats::sd(values[assignments == state_id])
    }, numeric(1L))
    data.frame(
      state = labels,
      variable = variable,
      mean = means,
      sd = standard_deviations,
      n = as.integer(tabulate(assignments, nbins = n_states)),
      stringsAsFactors = FALSE
    )
  })
  profiles <- do.call(rbind, profile_rows)
  rownames(profiles) <- NULL
  model$vasstra_assignments <- assignments
  model$vasstra_centers <- empirical_centers[old_order, , drop = FALSE]
  if (identical(method, "kmeans")) {
    model$cluster <- assignments
    model$centers <- model$centers[old_order, , drop = FALSE]
    model$size <- model$size[old_order]
    model$withinss <- model$withinss[old_order]
  }
  state_sizes <- stats::setNames(
    as.integer(tabulate(assignments, nbins = n_states)),
    labels
  )
  silhouette <- .vasstra_silhouette(
    as.numeric(assignments),
    stats::dist(standardized_matrix)
  )
  if (identical(method, "lpa")) {
    posterior <- model$z
    positive <- posterior > 0
    entropy_sum <- -sum(posterior[positive] * log(posterior[positive]))
    classification_entropy <- 1 - entropy_sum /
      (nrow(posterior) * log(ncol(posterior)))
    assigned_probabilities <- vapply(
      seq_len(ncol(posterior)),
      function(class_id) {
        mean(posterior[raw_assignments == class_id, class_id])
      },
      numeric(1L)
    )
    log_likelihood <- as.numeric(model$loglik)
    n_parameters <- as.numeric(model$df)
    n_observations <- nrow(posterior)
    deviance <- -2 * log_likelihood
    information <- list(
      aic = deviance + 2 * n_parameters,
      bic = deviance + n_parameters * log(n_observations),
      sabic = deviance + n_parameters * log((n_observations + 2) / 24),
      caic = deviance + n_parameters * (log(n_observations) + 1),
      awe = deviance + 2 * n_parameters * (log(n_observations) + 1.5),
      clc = deviance + 2 * entropy_sum,
      kic = deviance + 3 * (n_parameters + 1),
      icl = deviance + n_parameters * log(n_observations) +
        2 * entropy_sum,
      prob_min = min(assigned_probabilities),
      prob_max = max(assigned_probabilities)
    )
  } else {
    classification_entropy <- NA_real_
    information <- list(
      aic = NA_real_, bic = NA_real_, sabic = NA_real_, caic = NA_real_,
      awe = NA_real_, clc = NA_real_, kic = NA_real_, icl = NA_real_,
      prob_min = NA_real_, prob_max = NA_real_
    )
  }
  structure(
    list(
      data = result_data,
      profiles = profiles,
      standardized = standardized,
      model = model,
      settings = list(
        id = id,
        time = time,
        variables = variables,
        state = state,
        n_states = n_states,
        labels = labels,
        standardize = standardize,
        missing = missing,
        time_levels = chronology,
        n_start = as.integer(n_start),
        seed = as.integer(seed),
        method = method,
        lpa_model = if (identical(method, "lpa")) {
          lpa_model
        } else {
          NA_character_
        }
      ),
      diagnostics = list(
        n_rows = nrow(data),
        n_subjects = length(unique(data[[id]])),
        n_times = length(chronology),
        missing_imputed = sum(missing_counts),
        state_sizes = state_sizes,
        zero_variance_indicators = variables[constant_indicators],
        silhouette = silhouette,
        total_withinss = if (identical(method, "kmeans")) {
          as.numeric(model$tot.withinss)
        } else {
          NA_real_
        },
        pam_objective = if (identical(method, "pam")) {
          as.numeric(model$objective[["swap"]])
        } else {
          NA_real_
        },
        aic = information$aic,
        bic = information$bic,
        sabic = information$sabic,
        caic = information$caic,
        awe = information$awe,
        clc = information$clc,
        kic = information$kic,
        icl = information$icl,
        prob_min = information$prob_min,
        prob_max = information$prob_max,
        bic_native = if (identical(method, "lpa")) {
          as.numeric(model$bic)
        } else {
          NA_real_
        },
        icl_native = if (identical(method, "lpa")) {
          as.numeric(model$icl)
        } else {
          NA_real_
        },
        classification_entropy = classification_entropy,
        mean_uncertainty = if (identical(method, "lpa")) {
          mean(model$uncertainty)
        } else {
          NA_real_
        },
        log_likelihood = if (identical(method, "lpa")) {
          as.numeric(model$loglik)
        } else {
          NA_real_
        },
        n_parameters = if (identical(method, "lpa")) {
          as.integer(model$df)
        } else {
          NA_integer_
        },
        selection = selection
      )
    ),
    class = c("vasstra_states", "vasstra_step", "list")
  )
}

#' @export
print.vasstra_states <- function(x, ...) {
  stopifnot(inherits(x, "vasstra_states"))
  cat("VaSStra Step 1: Variables -> States\n")
  cat(sprintf(
    "  %d rows | %d subjects | %d times | %d states | %s\n",
    x$diagnostics$n_rows,
    x$diagnostics$n_subjects,
    x$diagnostics$n_times,
    x$settings$n_states,
    x$settings$method
  ))
  cat(sprintf("  Average silhouette: %.3f\n", x$diagnostics$silhouette))
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
                                         ...) {
  stopifnot(inherits(x, "vasstra_states"))
  as.data.frame(x$data, row.names = row.names, optional = optional, ...)
}

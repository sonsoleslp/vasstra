#' Compare Choices for the Number and Method of States
#'
#' Fits a tidy grid of state solutions. No solution is silently selected.
#' Hard-clustering recommendations maximize silhouette within each method;
#' LPA recommendations use the explicit `lpa_criterion`, subject to the
#' requested size constraints. Use [fit_state_choice()] with a `candidate_id`
#' after inspecting the table.
#'
#' @param data A data frame with one row per subject and time point.
#' @param id Name of the subject identifier column. May be omitted when the
#'   data carry VaSStra role metadata.
#' @param time Name of the time or ordering column. May be omitted when the
#'   data carry VaSStra role metadata.
#' @param variables Character vector naming numeric state indicators. May be
#'   omitted when the data carry VaSStra role metadata.
#' @param n_states Whole-number candidate state counts.
#' @param method State-clustering methods to compare. See [step1_states()].
#' @param lpa_model Optional mclust covariance models evaluated when
#'   `method` includes `"lpa"`. `"EEI"` corresponds to tidyLPA model 1.
#' @param lpa_criterion Criterion used only to mark LPA recommendations:
#'   conventional `"bic"` (default) or `"aic"` (smaller is better),
#'   `"silhouette"` (larger is better), or native mclust `"icl_native"`
#'   (larger is better).
#' @param standardize,missing,time_levels,n_start,seed Passed to
#'   [step1_states()].
#' @param minimum_size Minimum acceptable number of observations in every
#'   state.
#' @param minimum_proportion Minimum acceptable proportion in every state.
#' @param maximum_size_ratio Maximum acceptable largest-to-smallest state-size
#'   ratio.
#'
#' @return A `vasstra_state_choices` object. `as.data.frame()` returns one
#'   tidy row per candidate; `$recommendations` contains one transparent
#'   recommendation per method and LPA model.
#'
#' @examples
#' data <- expand.grid(student = 1:12, course = 1:3)
#' group <- rep(rep(1:3, each = 4), times = 3)
#' data$views <- group * 5 + data$course * 0.01
#' data$duration <- group * 10 - data$course * 0.01
#' choices <- state_choices(
#'   data,
#'   id = "student",
#'   time = "course",
#'   variables = c("views", "duration"),
#'   n_states = 2:4,
#'   method = c("kmeans", "pam")
#' )
#' choices
#' states <- fit_state_choice(
#'   choices,
#'   candidate_id = choices$recommendations$candidate_id[[1L]]
#' )
#' @export
state_choices <- function(
    data,
    id = NULL,
    time = NULL,
    variables = NULL,
    n_states = 2:6,
    method = c("lpa", "kmeans", "pam", "ward.D2"),
    lpa_model = "EEI",
    lpa_criterion = c("bic", "aic", "silhouette", "icl_native"),
    standardize = NULL,
    missing = NULL,
    time_levels = NULL,
    n_start = 25L,
    seed = 123L,
    minimum_size = 2L,
    minimum_proportion = 0,
    maximum_size_ratio = Inf) {
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
  lpa_criterion <- match.arg(lpa_criterion)
  method <- unique(match.arg(
    method,
    .vasstra_state_methods(),
    several.ok = TRUE
  ))
  if (!is.numeric(n_states) || length(n_states) == 0L ||
      anyNA(n_states) || any(!is.finite(n_states)) ||
      any(n_states != floor(n_states)) || any(n_states < 2L) ||
      any(n_states > nrow(data) - 1L)) {
    stop(
      "`n_states` must contain whole numbers from 2 through n - 1.",
      call. = FALSE
    )
  }
  n_states <- unique(as.integer(n_states))
  if (!is.character(lpa_model) || length(lpa_model) == 0L ||
      anyNA(lpa_model) ||
      any(!lpa_model %in% .vasstra_lpa_models())) {
    stop(sprintf(
      "`lpa_model` must contain values from: %s.",
      paste(.vasstra_lpa_models(), collapse = ", ")
    ), call. = FALSE)
  }
  lpa_model <- unique(lpa_model)
  constraints <- .vasstra_choice_constraints(
    minimum_size,
    minimum_proportion,
    maximum_size_ratio
  )
  roles <- .vasstra_validate_roles(data, id, time)
  variables <- .vasstra_columns(data, variables, "variables")
  if (length(variables) < 2L) {
    stop("`variables` must name at least two numeric indicators.",
         call. = FALSE)
  }
  if (length(intersect(c(roles$id, roles$time), variables)) > 0L) {
    stop("`variables` cannot include the `id` or `time` column.",
         call. = FALSE)
  }
  numeric_variables <- vapply(data[variables], is.numeric, logical(1L))
  if (!all(numeric_variables)) {
    stop(sprintf(
      "All state indicators must be numeric; check: %s.",
      paste(variables[!numeric_variables], collapse = ", ")
    ), call. = FALSE)
  }
  if (!is.numeric(n_start) || length(n_start) != 1L ||
      !is.finite(n_start) || n_start != floor(n_start) || n_start < 1L) {
    stop("`n_start` must be a positive whole number.", call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) {
    stop("`seed` must be one finite number.", call. = FALSE)
  }
  invisible(.vasstra_time_levels(data[[roles$time]], time_levels))
  state_name <- make.unique(
    c(names(data), ".vasstra_choice_state")
  )[[ncol(data) + 1L]]
  candidate_grid <- do.call(rbind, lapply(method, function(method_name) {
    models <- if (identical(method_name, "lpa")) {
      lpa_model
    } else {
      NA_character_
    }
    grid <- expand.grid(
      n_states = n_states,
      lpa_model = models,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    grid$method <- method_name
    grid[c("n_states", "method", "lpa_model")]
  }))
  rownames(candidate_grid) <- NULL
  candidate_rows <- lapply(seq_len(nrow(candidate_grid)), function(index) {
    candidate <- candidate_grid[index, , drop = FALSE]
    candidate_lpa_model <- if (is.na(candidate$lpa_model[[1L]])) {
      "EEI"
    } else {
      candidate$lpa_model[[1L]]
    }
    attempt <- tryCatch(
      list(
        result = step1_states(
          data = data,
          id = roles$id,
          time = roles$time,
          variables = variables,
          n_states = candidate$n_states[[1L]],
          state = state_name,
          standardize = standardize,
          missing = missing,
          time_levels = time_levels,
          n_start = n_start,
          seed = seed,
          method = candidate$method[[1L]],
          lpa_model = candidate_lpa_model
        ),
        error = NULL
      ),
      error = function(error) {
        list(result = NULL, error = conditionMessage(error))
      }
    )
    if (is.null(attempt$result)) {
      return(data.frame(
        n_states = candidate$n_states,
        method = candidate$method,
        lpa_model = candidate$lpa_model,
        status = "failed",
        error = attempt$error,
        silhouette = NA_real_,
        total_withinss = NA_real_,
        pam_objective = NA_real_,
        aic = NA_real_,
        bic = NA_real_,
        sabic = NA_real_,
        caic = NA_real_,
        awe = NA_real_,
        clc = NA_real_,
        kic = NA_real_,
        icl = NA_real_,
        prob_min = NA_real_,
        prob_max = NA_real_,
        bic_native = NA_real_,
        icl_native = NA_real_,
        classification_entropy = NA_real_,
        mean_uncertainty = NA_real_,
        log_likelihood = NA_real_,
        n_parameters = NA_integer_,
        min_size = NA_integer_,
        max_size = NA_integer_,
        min_proportion = NA_real_,
        size_ratio = NA_real_,
        stringsAsFactors = FALSE
      ))
    }
    diagnostics <- attempt$result$diagnostics
    sizes <- as.integer(diagnostics$state_sizes)
    data.frame(
      n_states = candidate$n_states,
      method = candidate$method,
      lpa_model = candidate$lpa_model,
      status = "ok",
      error = NA_character_,
      silhouette = diagnostics$silhouette,
      total_withinss = diagnostics$total_withinss,
      pam_objective = diagnostics$pam_objective,
      aic = diagnostics$aic,
      bic = diagnostics$bic,
      sabic = diagnostics$sabic,
      caic = diagnostics$caic,
      awe = diagnostics$awe,
      clc = diagnostics$clc,
      kic = diagnostics$kic,
      icl = diagnostics$icl,
      prob_min = diagnostics$prob_min,
      prob_max = diagnostics$prob_max,
      bic_native = diagnostics$bic_native,
      icl_native = diagnostics$icl_native,
      classification_entropy = diagnostics$classification_entropy,
      mean_uncertainty = diagnostics$mean_uncertainty,
      log_likelihood = diagnostics$log_likelihood,
      n_parameters = diagnostics$n_parameters,
      min_size = min(sizes),
      max_size = max(sizes),
      min_proportion = min(sizes) / nrow(data),
      size_ratio = max(sizes) / min(sizes),
      stringsAsFactors = FALSE
    )
  })
  candidates <- do.call(rbind, candidate_rows)
  rownames(candidates) <- NULL
  candidates$candidate_id <- seq_len(nrow(candidates))
  candidates$eligible <- candidates$status == "ok" &
    candidates$min_size >= constraints$minimum_size &
    candidates$min_proportion >= constraints$minimum_proportion &
    candidates$size_ratio <= constraints$maximum_size_ratio
  candidates$silhouette_rank <- NA_integer_
  rankable <- which(
    candidates$status == "ok" & is.finite(candidates$silhouette)
  )
  candidates$silhouette_rank[rankable] <- as.integer(rank(
    -candidates$silhouette[rankable],
    ties.method = "min"
  ))
  candidates$is_max_silhouette <- FALSE
  eligible <- which(
    candidates$eligible & is.finite(candidates$silhouette)
  )
  if (length(eligible) > 0L) {
    maximum <- max(candidates$silhouette[eligible])
    candidates$is_max_silhouette[eligible] <-
      candidates$silhouette[eligible] == maximum
  }
  candidates$recommendation_criterion <- ifelse(
    candidates$method == "lpa",
    lpa_criterion,
    "silhouette"
  )
  candidates$recommendation_direction <- ifelse(
    candidates$method == "lpa" & lpa_criterion %in% c("bic", "aic"),
    "min",
    "max"
  )
  candidates$is_recommended <- FALSE
  hard_indices <- which(candidates$method != "lpa")
  if (length(hard_indices) > 0L) {
    candidates$is_recommended[hard_indices] <-
      .vasstra_recommendation_flags(
        candidates[hard_indices, , drop = FALSE],
        group_columns = c("method", "lpa_model"),
        size_column = "n_states",
        score_column = "silhouette",
        direction = "max"
      )
  }
  lpa_indices <- which(candidates$method == "lpa")
  if (length(lpa_indices) > 0L) {
    candidates$is_recommended[lpa_indices] <-
      .vasstra_recommendation_flags(
        candidates[lpa_indices, , drop = FALSE],
        group_columns = c("method", "lpa_model"),
        size_column = "n_states",
        score_column = lpa_criterion,
        direction = if (lpa_criterion %in% c("bic", "aic")) "min" else "max"
      )
  }
  candidates <- candidates[c(
    "candidate_id", "n_states", "method", "lpa_model", "status", "error",
    "silhouette", "silhouette_rank", "total_withinss", "pam_objective",
    "aic", "bic", "sabic", "caic", "awe", "clc", "kic", "icl",
    "prob_min", "prob_max", "bic_native", "icl_native",
    "classification_entropy",
    "mean_uncertainty", "log_likelihood", "n_parameters",
    "min_size", "max_size", "min_proportion",
    "size_ratio", "eligible", "is_max_silhouette",
    "recommendation_criterion", "recommendation_direction",
    "is_recommended"
  )]
  structure(
    list(
      candidates = candidates,
      recommendations = candidates[
        candidates$is_recommended,
        ,
        drop = FALSE
      ],
      failures = candidates[candidates$status == "failed", , drop = FALSE],
      settings = c(
        list(
          n_states = n_states,
          method = method,
          lpa_model = lpa_model,
          lpa_criterion = lpa_criterion
        ),
        constraints
      ),
      source = list(
        data = data,
        id = roles$id,
        time = roles$time,
        variables = variables,
        standardize = standardize,
        missing = missing,
        time_levels = time_levels,
        n_start = as.integer(n_start),
        seed = as.integer(seed)
      )
    ),
    class = c("vasstra_state_choices", "vasstra_choices", "list")
  )
}

# Resolve one candidate row from an explicit id, descriptive filters, or
# the stored recommendation. Reports non-obvious resolutions with a message.
.vasstra_select_candidate <- function(choices, candidate_id, filters) {
  stopifnot(inherits(choices, "vasstra_choices"), is.list(filters))
  candidates <- choices$candidates
  if (!is.null(candidate_id)) {
    if (!is.numeric(candidate_id) || length(candidate_id) != 1L ||
        is.na(candidate_id) || !is.finite(candidate_id) ||
        candidate_id != floor(candidate_id)) {
      stop("`candidate_id` must be one whole number.", call. = FALSE)
    }
    selected <- candidates[
      candidates$candidate_id == as.integer(candidate_id),
      ,
      drop = FALSE
    ]
    if (nrow(selected) != 1L) {
      stop("`candidate_id` is not present in these choices.", call. = FALSE)
    }
    return(selected)
  }
  filters <- filters[!vapply(filters, is.null, logical(1L))]
  if (length(filters) == 0L) {
    selected <- .vasstra_auto_pick(choices, "compared")
    message(sprintf(
      "Fitting recommended candidate %d.",
      selected$candidate_id[[1L]]
    ))
    return(selected)
  }
  keep <- Reduce(
    function(current, name) {
      values <- candidates[[name]]
      current & !is.na(values) & values == filters[[name]]
    },
    names(filters),
    init = rep(TRUE, nrow(candidates))
  )
  matched <- candidates[keep, , drop = FALSE]
  if (nrow(matched) == 0L) {
    stop(sprintf(
      "No candidate matches %s.",
      paste(sprintf("%s = %s", names(filters), unlist(filters)),
            collapse = ", ")
    ), call. = FALSE)
  }
  if (nrow(matched) == 1L) {
    return(matched)
  }
  recommended <- matched[matched$is_recommended, , drop = FALSE]
  if (nrow(recommended) == 1L) {
    message(sprintf(
      "Fitting recommended candidate %d among the %d matches.",
      recommended$candidate_id[[1L]],
      nrow(matched)
    ))
    return(recommended)
  }
  stop(sprintf(
    "Several candidates match (ids %s); narrow the selection or pass `candidate_id`.",
    paste(matched$candidate_id, collapse = ", ")
  ), call. = FALSE)
}

#' Fit One State Choice
#'
#' Fits a candidate from [state_choices()], selected by `candidate_id`, by
#' any combination of `n_states`, `method`, and `lpa_model`, or — when
#' nothing is specified — the recommended candidate.
#'
#' @param choices A `vasstra_state_choices` object from [state_choices()].
#' @param candidate_id Optional explicit candidate number to fit.
#' @param labels Optional state labels ordered from low to high profile.
#' @param state Name of the state column created in the returned data.
#' @param n_states Optional state count used to select the candidate.
#' @param method Optional clustering method used to select the candidate.
#' @param lpa_model Optional LPA covariance model used to select the
#'   candidate.
#'
#' @return A `vasstra_states` object from [step1_states()] with the selected
#'   candidate and complete comparison table recorded in `diagnostics`.
#'
#' @examples
#' data <- expand.grid(student = 1:12, course = 1:3)
#' group <- rep(rep(1:3, each = 4), times = 3)
#' data$views <- group * 5 + data$course * 0.01
#' data$duration <- group * 10 - data$course * 0.01
#' choices <- state_choices(
#'   data, "student", "course", c("views", "duration"),
#'   n_states = 2:3, method = "kmeans"
#' )
#' fit_state_choice(choices)             # the recommended candidate
#' fit_state_choice(choices, n_states = 3)
#' @export
fit_state_choice <- function(
    choices,
    candidate_id = NULL,
    labels = NULL,
    state = "state",
    n_states = NULL,
    method = NULL,
    lpa_model = NULL) {
  stopifnot(inherits(choices, "vasstra_state_choices"))
  selected <- .vasstra_select_candidate(
    choices,
    candidate_id,
    filters = list(
      n_states = n_states,
      method = method,
      lpa_model = lpa_model
    )
  )
  if (selected$status[[1L]] != "ok") {
    stop(sprintf(
      "Candidate %d failed: %s",
      selected$candidate_id[[1L]],
      selected$error[[1L]]
    ), call. = FALSE)
  }
  if (!selected$eligible[[1L]]) {
    warning(
      "The selected candidate does not meet the recorded size constraints.",
      call. = FALSE
    )
  }
  source <- choices$source
  lpa_model <- if (is.na(selected$lpa_model[[1L]])) {
    "EEI"
  } else {
    selected$lpa_model[[1L]]
  }
  result <- step1_states(
    data = source$data,
    id = source$id,
    time = source$time,
    variables = source$variables,
    n_states = selected$n_states[[1L]],
    labels = labels,
    state = state,
    standardize = source$standardize,
    missing = source$missing,
    time_levels = source$time_levels,
    n_start = source$n_start,
    seed = source$seed,
    method = selected$method[[1L]],
    lpa_model = lpa_model
  )
  result$diagnostics$selection <- list(
    candidate = selected,
    candidates = choices$candidates
  )
  result
}

#' @export
print.vasstra_state_choices <- function(x, ...) {
  stopifnot(inherits(x, "vasstra_state_choices"))
  cat("VaSStra state choices\n")
  cat(sprintf(
    "  %d candidates | %d successful | %d recommended\n",
    nrow(x$candidates),
    sum(x$candidates$status == "ok"),
    nrow(x$recommendations)
  ))
  if (nrow(x$recommendations) > 0L) {
    print.data.frame(
      x$recommendations[c(
        "candidate_id", "n_states", "method", "lpa_model",
        "recommendation_criterion", "silhouette", "bic",
        "min_size", "eligible"
      )],
      row.names = FALSE
    )
  }
  invisible(x)
}

#' @export
summary.vasstra_state_choices <- function(object, ...) {
  stopifnot(inherits(object, "vasstra_state_choices"))
  object$candidates
}

#' @export
as.data.frame.vasstra_state_choices <- function(
    x,
    row.names = NULL,
    optional = FALSE,
    ...) {
  stopifnot(inherits(x, "vasstra_state_choices"))
  as.data.frame(
    x$candidates,
    row.names = row.names,
    optional = optional,
    ...
  )
}

#' Compare Choices for the Number and Method of Trajectories
#'
#' Uses [Nestimate::cluster_choice()] to compare a tidy grid of sequence
#' distances, clustering methods, and trajectory counts. Recommendations are
#' made within each distance-method pair; no distance or algorithm is silently
#' chosen for the user.
#'
#' @param data A `vasstra_sequences` object.
#' @param n_trajectories Whole-number candidate trajectory counts.
#' @param dissimilarity Sequence distances to compare.
#' @param method Clustering methods to compare.
#' @param seed Reproducible seed passed to Nestimate.
#' @param minimum_size Minimum acceptable number of sequences in every group.
#' @param minimum_proportion Minimum acceptable proportion in every group.
#' @param maximum_size_ratio Maximum acceptable largest-to-smallest group-size
#'   ratio.
#'
#' @return A `vasstra_trajectory_choices` object with one tidy candidate row
#'   per Nestimate fit and transparent recommendations within method-distance
#'   combinations.
#'
#' @examples
#' sequences <- step2_sequences(
#'   data.frame(
#'     id = rep(1:6, each = 3),
#'     time = rep(1:3, 6),
#'     state = rep(c("A", "A", "A", "B", "B", "B"), each = 3)
#'   ),
#'   "id", "time", "state"
#' )
#' choices <- trajectory_choices(
#'   sequences,
#'   n_trajectories = 2:3,
#'   dissimilarity = "hamming",
#'   method = c("pam", "ward.D2")
#' )
#' choices
#' @export
trajectory_choices <- function(
    data,
    n_trajectories = 2:6,
    dissimilarity = c("hamming", "lcs"),
    method = c("pam", "ward.D2"),
    seed = 123L,
    minimum_size = 2L,
    minimum_proportion = 0,
    maximum_size_ratio = Inf) {
  stopifnot(inherits(data, "vasstra_sequences"))
  n_sequences <- nrow(data$data)
  if (!is.numeric(n_trajectories) || length(n_trajectories) == 0L ||
      anyNA(n_trajectories) || any(!is.finite(n_trajectories)) ||
      any(n_trajectories != floor(n_trajectories)) ||
      any(n_trajectories < 2L) ||
      any(n_trajectories > n_sequences - 1L)) {
    stop(
      "`n_trajectories` must contain whole numbers from 2 through n - 1.",
      call. = FALSE
    )
  }
  n_trajectories <- unique(as.integer(n_trajectories))
  dissimilarity <- unique(match.arg(
    dissimilarity,
    .vasstra_sequence_distances(),
    several.ok = TRUE
  ))
  method <- unique(match.arg(
    method,
    .vasstra_cluster_methods(),
    several.ok = TRUE
  ))
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) {
    stop("`seed` must be one finite number.", call. = FALSE)
  }
  constraints <- .vasstra_choice_constraints(
    minimum_size,
    minimum_proportion,
    maximum_size_ratio
  )
  raw_choices <- tryCatch(
    .vasstra_with_seed(
      seed,
      Nestimate::cluster_choice(
        data$data,
        k = n_trajectories,
        dissimilarity = dissimilarity,
        method = method,
        seed = as.integer(seed)
      )
    ),
    error = function(error) {
      stop(sprintf(
        "Nestimate trajectory comparison failed: %s",
        conditionMessage(error)
      ), call. = FALSE)
    }
  )
  raw_choices <- as.data.frame(raw_choices)
  candidates <- data.frame(
    candidate_id = seq_len(nrow(raw_choices)),
    n_trajectories = as.integer(raw_choices$k),
    dissimilarity = as.character(raw_choices$dissimilarity),
    method = as.character(raw_choices$method),
    status = "ok",
    error = NA_character_,
    silhouette = as.numeric(raw_choices$silhouette),
    silhouette_rank = as.integer(rank(
      -raw_choices$silhouette,
      ties.method = "min"
    )),
    mean_within_distance = as.numeric(raw_choices$mean_within_dist),
    min_size = as.integer(raw_choices$min_size),
    max_size = as.integer(raw_choices$max_size),
    min_proportion = as.numeric(raw_choices$min_size) / n_sequences,
    size_ratio = as.numeric(raw_choices$size_ratio),
    stringsAsFactors = FALSE
  )
  candidates$eligible <-
    candidates$min_size >= constraints$minimum_size &
    candidates$min_proportion >= constraints$minimum_proportion &
    candidates$size_ratio <= constraints$maximum_size_ratio
  candidates$is_max_silhouette <- FALSE
  eligible <- which(
    candidates$eligible & is.finite(candidates$silhouette)
  )
  if (length(eligible) > 0L) {
    maximum <- max(candidates$silhouette[eligible])
    candidates$is_max_silhouette[eligible] <-
      candidates$silhouette[eligible] == maximum
  }
  candidates$is_recommended <- .vasstra_recommendation_flags(
    candidates,
    group_columns = c("dissimilarity", "method"),
    size_column = "n_trajectories"
  )
  structure(
    list(
      candidates = candidates,
      recommendations = candidates[
        candidates$is_recommended,
        ,
        drop = FALSE
      ],
      failures = candidates[FALSE, , drop = FALSE],
      settings = c(
        list(
          n_trajectories = n_trajectories,
          dissimilarity = dissimilarity,
          method = method,
          backend = "Nestimate",
          seed = as.integer(seed)
        ),
        constraints
      ),
      source = data
    ),
    class = c("vasstra_trajectory_choices", "vasstra_choices", "list")
  )
}

#' Fit One Trajectory Choice
#'
#' Fits a candidate from [trajectory_choices()], selected by
#' `candidate_id`, by any combination of `n_trajectories`,
#' `dissimilarity`, and `method`, or — when nothing is specified — the
#' recommended candidate.
#'
#' @param choices A `vasstra_trajectory_choices` object.
#' @param candidate_id Optional explicit candidate number to fit.
#' @param labels Optional trajectory labels.
#' @param n_trajectories Optional trajectory count used to select the
#'   candidate.
#' @param dissimilarity Optional sequence distance used to select the
#'   candidate.
#' @param method Optional clustering method used to select the candidate.
#'
#' @return A `vasstra_trajectories` object with the selected candidate and
#'   complete comparison table recorded in `diagnostics`.
#'
#' @examples
#' sequences <- step2_sequences(
#'   data.frame(
#'     id = rep(1:6, each = 3),
#'     time = rep(1:3, 6),
#'     state = rep(c("A", "A", "A", "B", "B", "B"), each = 3)
#'   ),
#'   "id", "time", "state"
#' )
#' choices <- trajectory_choices(
#'   sequences, n_trajectories = 2, dissimilarity = "hamming",
#'   method = "pam"
#' )
#' fit_trajectory_choice(choices)
#' @export
fit_trajectory_choice <- function(
    choices,
    candidate_id = NULL,
    labels = NULL,
    n_trajectories = NULL,
    dissimilarity = NULL,
    method = NULL) {
  stopifnot(inherits(choices, "vasstra_trajectory_choices"))
  selected <- .vasstra_select_candidate(
    choices,
    candidate_id,
    filters = list(
      n_trajectories = n_trajectories,
      dissimilarity = dissimilarity,
      method = method
    )
  )
  if (!selected$eligible[[1L]]) {
    warning(
      "The selected candidate does not meet the recorded size constraints.",
      call. = FALSE
    )
  }
  result <- step3_trajectories(
    data = choices$source,
    n_trajectories = selected$n_trajectories[[1L]],
    dissimilarity = selected$dissimilarity[[1L]],
    method = selected$method[[1L]],
    backend = "Nestimate",
    labels = labels,
    seed = choices$settings$seed
  )
  result$diagnostics$selection <- list(
    candidate = selected,
    candidates = choices$candidates
  )
  result
}

#' @export
print.vasstra_trajectory_choices <- function(x, ...) {
  stopifnot(inherits(x, "vasstra_trajectory_choices"))
  cat("VaSStra trajectory choices (Nestimate)\n")
  cat(sprintf(
    "  %d candidates | %d recommended distance-method solutions\n",
    nrow(x$candidates),
    nrow(x$recommendations)
  ))
  if (nrow(x$recommendations) > 0L) {
    print.data.frame(
      x$recommendations[c(
        "candidate_id", "n_trajectories", "dissimilarity", "method",
        "silhouette", "min_size", "eligible"
      )],
      row.names = FALSE
    )
  }
  invisible(x)
}

#' @export
summary.vasstra_trajectory_choices <- function(object, ...) {
  stopifnot(inherits(object, "vasstra_trajectory_choices"))
  object$candidates
}

#' @export
as.data.frame.vasstra_trajectory_choices <- function(
    x,
    row.names = NULL,
    optional = FALSE,
    ...) {
  stopifnot(inherits(x, "vasstra_trajectory_choices"))
  as.data.frame(
    x$candidates,
    row.names = row.names,
    optional = optional,
    ...
  )
}

# Internal validation and transformation helpers.

.vasstra_data_metadata <- function(data) {
  stopifnot(is.data.frame(data))
  metadata <- attr(data, "vasstra")
  if (is.null(metadata)) {
    return(list())
  }
  if (!is.list(metadata)) {
    stop("The `vasstra` data attribute must be a named list.",
         call. = FALSE)
  }
  metadata
}

.vasstra_resolve_state_data <- function(
    data,
    id = NULL,
    time = NULL,
    variables = NULL,
    standardize = NULL,
    missing = NULL) {
  stopifnot(is.data.frame(data))
  metadata <- .vasstra_data_metadata(data)
  roles <- .vasstra_auto_roles(data, id, time, variables)
  if (is.null(standardize)) {
    standardize <- if (is.null(metadata$standardize)) {
      "time"
    } else {
      metadata$standardize
    }
  }
  if (is.null(missing)) {
    missing <- if (is.null(metadata$missing)) {
      "error"
    } else {
      metadata$missing
    }
  }
  list(
    id = roles$id,
    time = roles$time,
    variables = roles$variables,
    standardize = standardize,
    missing = missing
  )
}

.vasstra_column <- function(data, column, role) {
  stopifnot(is.data.frame(data), is.character(role), length(role) == 1L)
  if (!is.character(column) || length(column) != 1L || is.na(column) ||
      !nzchar(column)) {
    stop(sprintf("`%s` must be one column name.", role), call. = FALSE)
  }
  if (!column %in% names(data)) {
    stop(sprintf("Column `%s` supplied as `%s` was not found.", column, role),
         call. = FALSE)
  }
  column
}

.vasstra_columns <- function(data, columns, role) {
  stopifnot(is.data.frame(data), is.character(role), length(role) == 1L)
  if (!is.character(columns) || length(columns) == 0L ||
      anyNA(columns) || any(!nzchar(columns))) {
    stop(sprintf("`%s` must contain at least one column name.", role),
         call. = FALSE)
  }
  missing_columns <- setdiff(columns, names(data))
  if (length(missing_columns) > 0L) {
    stop(sprintf(
      "Column%s supplied as `%s` not found: %s.",
      if (length(missing_columns) == 1L) "" else "s",
      role,
      paste(missing_columns, collapse = ", ")
    ), call. = FALSE)
  }
  if (anyDuplicated(columns)) {
    stop(sprintf("`%s` contains duplicated column names.", role),
         call. = FALSE)
  }
  columns
}

.vasstra_validate_roles <- function(data, id, time) {
  stopifnot(is.data.frame(data))
  id <- .vasstra_column(data, id, "id")
  time <- .vasstra_column(data, time, "time")
  if (identical(id, time)) {
    stop("`id` and `time` must identify different columns.", call. = FALSE)
  }
  if (anyNA(data[[id]])) {
    stop(sprintf("`%s` contains missing subject identifiers.", id),
         call. = FALSE)
  }
  if (anyNA(data[[time]])) {
    stop(sprintf("`%s` contains missing time values.", time),
         call. = FALSE)
  }
  key <- interaction(data[c(id, time)], drop = TRUE, lex.order = TRUE)
  if (anyDuplicated(key)) {
    stop("Each subject-time pair must occur exactly once.", call. = FALSE)
  }
  invisible(list(id = id, time = time))
}

.vasstra_time_levels <- function(time, supplied = NULL) {
  stopifnot(length(time) > 0L)
  observed <- unique(time)
  if (!is.null(supplied)) {
    if (length(supplied) == 0L || anyNA(supplied) ||
        anyDuplicated(supplied)) {
      stop("`time_levels` must be a non-empty vector of unique values.",
           call. = FALSE)
    }
    missing_levels <- setdiff(as.character(observed), as.character(supplied))
    if (length(missing_levels) > 0L) {
      stop(sprintf(
        "`time_levels` omits observed values: %s.",
        paste(missing_levels, collapse = ", ")
      ), call. = FALSE)
    }
    return(supplied)
  }
  if (is.factor(time)) {
    if (!is.ordered(time)) {
      stop(
        "Unordered-factor time requires explicit `time_levels`; chronology is not guessed.",
        call. = FALSE
      )
    }
    return(levels(time)[levels(time) %in% as.character(observed)])
  }
  if (is.character(time)) {
    stop(
      "Character time requires explicit `time_levels`; chronology is not guessed.",
      call. = FALSE
    )
  }
  sort(observed)
}

# Resolve the order the states should appear in every plot. `state_order`,
# when supplied, must list exactly the same labels rearranged; otherwise the
# discovery order (`labels`) is kept.
.vasstra_state_order <- function(state_order, labels) {
  if (is.null(state_order)) {
    return(labels)
  }
  if (!is.character(state_order) || anyNA(state_order) ||
      length(state_order) != length(labels) ||
      !setequal(state_order, labels)) {
    stop(
      sprintf(
        "`state_order` must list the same states in the desired order: %s.",
        paste(labels, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  state_order
}

# Normalise a user state palette into a vector named by state, so it survives
# reordering (`state_order`) and subset plots. Accepts a named vector (matched
# by name) or a positional one (one colour per state, in `states` order).
.vasstra_name_palette <- function(colors, states) {
  if (is.null(colors)) {
    return(NULL)
  }
  if (!is.null(names(colors))) {
    if (!setequal(names(colors), states)) {
      stop(
        sprintf(
          "`state_colors` names must match the states: %s.",
          paste(states, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    return(colors[states])
  }
  if (length(colors) != length(states)) {
    stop("`state_colors` must give one colour per state.", call. = FALSE)
  }
  stats::setNames(as.character(colors), states)
}

.vasstra_with_seed <- function(seed, code) {
  stopifnot(is.numeric(seed), length(seed) == 1L, is.finite(seed))
  seed <- as.integer(seed)
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  force(code)
}

.vasstra_standardize <- function(data, variables, time, standardize) {
  stopifnot(
    is.data.frame(data),
    is.character(variables),
    is.character(time),
    length(time) == 1L,
    is.character(standardize),
    length(standardize) == 1L
  )
  if (identical(standardize, "none")) {
    values <- lapply(data[variables], as.numeric)
    return(as.data.frame(values, stringsAsFactors = FALSE))
  }
  groups <- if (identical(standardize, "time")) {
    data[[time]]
  } else {
    rep.int(1L, nrow(data))
  }
  standardized <- lapply(data[variables], function(values) {
    means <- stats::ave(values, groups, FUN = mean)
    standard_deviations <- stats::ave(
      values,
      groups,
      FUN = stats::sd
    )
    constant <- !is.finite(standard_deviations) | standard_deviations == 0
    ifelse(constant, 0, (values - means) / standard_deviations)
  })
  names(standardized) <- variables
  as.data.frame(standardized, stringsAsFactors = FALSE)
}

.vasstra_wide_matrix <- function(data, id, time, state, time_levels) {
  stopifnot(
    is.data.frame(data),
    is.character(id),
    is.character(time),
    is.character(state),
    length(time_levels) > 0L
  )
  ids <- unique(data[[id]])
  id_index <- match(data[[id]], ids)
  time_index <- match(as.character(data[[time]]), as.character(time_levels))
  sequence_matrix <- matrix(
    NA_character_,
    nrow = length(ids),
    ncol = length(time_levels)
  )
  sequence_matrix[cbind(id_index, time_index)] <- as.character(data[[state]])
  colnames(sequence_matrix) <- paste0("T", seq_along(time_levels))
  rownames(sequence_matrix) <- NULL
  list(ids = ids, matrix = sequence_matrix)
}

.vasstra_transition_table <- function(sequence_matrix, states) {
  stopifnot(is.matrix(sequence_matrix), is.character(states))
  if (ncol(sequence_matrix) < 2L) {
    grid <- expand.grid(
      from = states,
      to = states,
      stringsAsFactors = FALSE
    )
    grid$count <- 0L
    grid$probability <- 0
    return(grid)
  }
  from <- as.vector(sequence_matrix[, -ncol(sequence_matrix), drop = FALSE])
  to <- as.vector(sequence_matrix[, -1L, drop = FALSE])
  valid <- !is.na(from) & !is.na(to)
  counts <- table(
    factor(from[valid], levels = states),
    factor(to[valid], levels = states)
  )
  result <- as.data.frame(counts, stringsAsFactors = FALSE)
  names(result) <- c("from", "to", "count")
  result$from <- as.character(result$from)
  result$to <- as.character(result$to)
  totals <- stats::ave(result$count, result$from, FUN = sum)
  result$probability <- ifelse(totals == 0, 0, result$count / totals)
  result
}

.vasstra_distribution_table <- function(sequence_matrix, states, time_levels) {
  stopifnot(
    is.matrix(sequence_matrix),
    is.character(states),
    length(time_levels) == ncol(sequence_matrix)
  )
  counts <- table(
    factor(
      rep(seq_len(ncol(sequence_matrix)), each = nrow(sequence_matrix)),
      levels = seq_len(ncol(sequence_matrix))
    ),
    factor(as.vector(sequence_matrix), levels = states)
  )
  result <- as.data.frame(counts, stringsAsFactors = FALSE)
  names(result) <- c("position", "state", "count")
  result$position <- as.integer(as.character(result$position))
  result$time <- time_levels[result$position]
  totals <- stats::ave(result$count, result$position, FUN = sum)
  result$proportion <- ifelse(totals == 0, 0, result$count / totals)
  result[c("position", "time", "state", "count", "proportion")]
}

.vasstra_missing_sentinel <- function(values) {
  stopifnot(is.character(values))
  longest <- if (length(values) == 0L) 0L else max(nchar(values))
  paste0("<VaSSTra missing", strrep("_", longest + 1L), ">")
}

.vasstra_hamming_distance <- function(sequence_matrix) {
  stopifnot(is.matrix(sequence_matrix))
  values <- as.character(sequence_matrix)
  sentinel <- .vasstra_missing_sentinel(values[!is.na(values)])
  encoded <- sequence_matrix
  encoded[is.na(encoded)] <- sentinel
  distance_matrix <- Reduce(
    `+`,
    lapply(seq_len(ncol(encoded)), function(position) {
      outer(encoded[, position], encoded[, position], FUN = "!=") * 1
    }),
    init = matrix(0, nrow(encoded), nrow(encoded))
  )
  stats::as.dist(distance_matrix)
}

.vasstra_lcs_length <- function(x, y) {
  stopifnot(is.character(x), is.character(y))
  if (length(x) == 0L || length(y) == 0L) {
    return(0L)
  }
  final_row <- Reduce(
    function(previous_row, current_x) {
      Reduce(
        function(current_row, position) {
          value <- if (identical(current_x, y[[position]])) {
            previous_row[[position]] + 1L
          } else {
            max(
              previous_row[[position + 1L]],
              current_row[[length(current_row)]]
            )
          }
          c(current_row, value)
        },
        seq_along(y),
        init = 0L
      )
    },
    x,
    init = integer(length(y) + 1L)
  )
  unname(final_row[[length(final_row)]])
}

.vasstra_trimmed_sequences <- function(sequence_matrix) {
  stopifnot(is.matrix(sequence_matrix))
  values <- as.character(sequence_matrix)
  sentinel <- .vasstra_missing_sentinel(values[!is.na(values)])
  lapply(seq_len(nrow(sequence_matrix)), function(index) {
    row <- sequence_matrix[index, ]
    observed <- which(!is.na(row))
    if (length(observed) == 0L) {
      return(character(0L))
    }
    row <- as.character(row[seq_len(max(observed))])
    row[is.na(row)] <- sentinel
    row
  })
}

.vasstra_lcs_distance <- function(sequence_matrix) {
  stopifnot(is.matrix(sequence_matrix))
  sequences <- .vasstra_trimmed_sequences(sequence_matrix)
  pairs <- utils::combn(seq_along(sequences), 2L)
  distances <- vapply(seq_len(ncol(pairs)), function(index) {
    first <- sequences[[pairs[1L, index]]]
    second <- sequences[[pairs[2L, index]]]
    length(first) + length(second) -
      2L * .vasstra_lcs_length(first, second)
  }, numeric(1L))
  distance_matrix <- matrix(0, nrow(sequence_matrix), nrow(sequence_matrix))
  distance_matrix[t(pairs)] <- distances
  distance_matrix[t(pairs[2:1, , drop = FALSE])] <- distances
  stats::as.dist(distance_matrix)
}

.vasstra_silhouette_widths <- function(assignments, distance) {
  stopifnot(is.numeric(assignments), inherits(distance, "dist"))
  distance_matrix <- as.matrix(distance)
  clusters <- sort(unique(assignments))
  vapply(seq_along(assignments), function(index) {
    own <- which(assignments == assignments[[index]])
    if (length(own) == 1L) {
      return(0)
    }
    a <- mean(distance_matrix[index, setdiff(own, index)])
    other <- setdiff(clusters, assignments[[index]])
    b <- min(vapply(other, function(cluster_id) {
      mean(distance_matrix[index, assignments == cluster_id])
    }, numeric(1L)))
    denominator <- max(a, b)
    if (denominator == 0) 0 else (b - a) / denominator
  }, numeric(1L))
}

.vasstra_silhouette <- function(assignments, distance) {
  mean(.vasstra_silhouette_widths(assignments, distance))
}

.vasstra_relabel_assignments <- function(assignments, sequence_matrix) {
  stopifnot(is.numeric(assignments), is.matrix(sequence_matrix))
  signatures <- vapply(sort(unique(assignments)), function(cluster_id) {
    rows <- sequence_matrix[assignments == cluster_id, , drop = FALSE]
    paste(vapply(seq_len(ncol(rows)), function(position) {
      values <- rows[, position]
      values <- values[!is.na(values)]
      if (length(values) == 0L) {
        return("")
      }
      names(sort(table(values), decreasing = TRUE))[[1L]]
    }, character(1L)), collapse = "\r")
  }, character(1L))
  old_order <- sort(unique(assignments))[order(signatures)]
  match(assignments, old_order)
}

.vasstra_lpa_models <- function() {
  stopifnot(TRUE)
  c(
    "EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE",
    "VEE", "EVE", "VVE", "EEV", "VEV", "EVV", "VVV"
  )
}

.vasstra_state_methods <- function() {
  stopifnot(TRUE)
  c(
    "kmeans", "pam", "ward.D2", "ward.D", "complete", "average",
    "single", "mcquitty", "median", "centroid", "lpa"
  )
}

.vasstra_sequence_distances <- function() {
  stopifnot(TRUE)
  c(
    "hamming", "osa", "lv", "dl", "lcs",
    "qgram", "cosine", "jaccard", "jw"
  )
}

.vasstra_cluster_methods <- function() {
  stopifnot(TRUE)
  c(
    "pam", "ward.D2", "ward.D", "complete", "average",
    "single", "mcquitty", "median", "centroid"
  )
}

.vasstra_choice_constraints <- function(
    minimum_size,
    minimum_proportion,
    maximum_size_ratio) {
  stopifnot(
    length(minimum_size) == 1L,
    length(minimum_proportion) == 1L,
    length(maximum_size_ratio) == 1L
  )
  if (!is.numeric(minimum_size) || is.na(minimum_size) ||
      !is.finite(minimum_size) || minimum_size != floor(minimum_size) ||
      minimum_size < 1L) {
    stop("`minimum_size` must be a positive whole number.", call. = FALSE)
  }
  if (!is.numeric(minimum_proportion) || is.na(minimum_proportion) ||
      !is.finite(minimum_proportion) || minimum_proportion < 0 ||
      minimum_proportion > 1) {
    stop("`minimum_proportion` must be between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(maximum_size_ratio) || is.na(maximum_size_ratio) ||
      maximum_size_ratio < 1) {
    stop("`maximum_size_ratio` must be at least 1.", call. = FALSE)
  }
  list(
    minimum_size = as.integer(minimum_size),
    minimum_proportion = as.numeric(minimum_proportion),
    maximum_size_ratio = as.numeric(maximum_size_ratio)
  )
}

.vasstra_recommendation_flags <- function(
    candidates,
    group_columns,
    size_column,
    score_column = "silhouette",
    direction = c("max", "min")) {
  stopifnot(
    is.data.frame(candidates),
    is.character(group_columns),
    is.character(size_column),
    length(size_column) == 1L,
    is.character(score_column),
    length(score_column) == 1L,
    all(c(group_columns, size_column, score_column) %in% names(candidates))
  )
  direction <- match.arg(direction)
  group_data <- lapply(candidates[group_columns], function(values) {
    ifelse(is.na(values), "<not applicable>", as.character(values))
  })
  groups <- do.call(
    interaction,
    c(group_data, list(drop = TRUE, lex.order = TRUE))
  )
  selected <- unlist(lapply(
    split(seq_len(nrow(candidates)), groups, drop = TRUE),
    function(indices) {
      eligible <- indices[
        candidates$eligible[indices] &
          candidates$status[indices] == "ok" &
          is.finite(candidates[[score_column]][indices])
      ]
      if (length(eligible) == 0L) {
        return(integer(0L))
      }
      score <- candidates[[score_column]][eligible]
      ordered_score <- if (identical(direction, "max")) -score else score
      eligible[order(
        ordered_score,
        candidates[[size_column]][eligible],
        candidates$candidate_id[eligible]
      )][[1L]]
    }
  ), use.names = FALSE)
  seq_len(nrow(candidates)) %in% selected
}

.vasstra_sequence_metrics <- function(
    sequence,
    alphabet,
    positive_states,
    negative_states,
    omega) {
  stopifnot(
    is.character(sequence),
    is.character(alphabet),
    is.character(positive_states),
    is.character(negative_states),
    is.numeric(omega),
    length(omega) == 1L
  )
  observed <- sequence[!is.na(sequence)]
  n_observed <- length(observed)
  if (n_observed == 0L) {
    return(c(
      n_observed = 0,
      unique_states = 0,
      transitions = NA_real_,
      entropy = NA_real_,
      complexity = NA_real_,
      volatility = NA_real_,
      integrative_potential = NA_real_,
      negative_exposure = NA_real_
    ))
  }
  proportions <- table(factor(observed, levels = alphabet)) / n_observed
  positive_proportions <- proportions[proportions > 0]
  entropy <- if (length(alphabet) <= 1L) {
    0
  } else {
    -sum(positive_proportions * log(positive_proportions)) /
      log(length(alphabet))
  }
  transitions <- if (n_observed <= 1L) {
    0
  } else {
    sum(observed[-1L] != observed[-n_observed])
  }
  transition_rate <- if (n_observed <= 1L) {
    0
  } else {
    transitions / (n_observed - 1L)
  }
  weights <- seq_len(n_observed)^omega
  integrative <- if (length(positive_states) == 0L) {
    NA_real_
  } else {
    sum(weights[observed %in% positive_states]) / sum(weights)
  }
  negative <- if (length(negative_states) == 0L) {
    NA_real_
  } else {
    sum(weights[observed %in% negative_states]) / sum(weights)
  }
  c(
    n_observed = n_observed,
    unique_states = sum(proportions > 0),
    transitions = transitions,
    entropy = entropy,
    complexity = sqrt(entropy * transition_rate),
    volatility = (
      sum(proportions > 0) / length(alphabet) + transition_rate
    ) / 2,
    integrative_potential = integrative,
    negative_exposure = negative
  )
}

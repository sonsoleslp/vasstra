#' Step 3: Turn Sequences into Trajectories
#'
#' Clusters aligned state sequences with Nestimate sequence distances and
#' clustering methods. The default Hamming plus PAM combination follows the
#' Nestimate and Carm sequence-clustering interface. Set
#' `dissimilarity = "lcs"` and `method = "ward.D2"` to follow the worked
#' VaSSTra chapter.
#'
#' When the states come from a two-level engine (`"multilpa"`, `"multilca"`
#' or `"lta"` in [states()]), the model has already estimated the
#' trajectory classes, and they are used as estimated (`method = "model"`,
#' the default for such states). Their silhouette and mean within-trajectory
#' distance are then reported on the Hamming distance, as a yardstick that
#' can be compared with the clustering methods. Naming a `dissimilarity` or a
#' clustering `method` clusters the sequences instead.
#'
#' @param data A `vasstra_sequences` object.
#' @param n_trajectories Number of trajectory groups. One number fits
#'   exactly that count, several numbers (for example `2:4`) compare those
#'   candidates with [trajectory_choices()] and fit the recommended count,
#'   and `"auto"` (default) compares 2 through 6 — or simply matches
#'   `labels` when labels are supplied. Automatic comparison never selects
#'   a solution whose smallest group holds under 5 percent of the
#'   sequences. Compared candidates are kept in `diagnostics$selection`
#'   and every automated choice is reported with a message.
#' @param dissimilarity One of `"hamming"`, `"osa"`, `"lv"`, `"dl"`, `"lcs"`,
#'   `"qgram"`, `"cosine"`, `"jaccard"`, or `"jw"`.
#' @param method One of `"pam"`, `"ward.D2"`, `"ward.D"`, `"complete"`,
#'   `"average"`, `"single"`, `"mcquitty"`, `"median"`, `"centroid"`, or
#'   `"model"` for the trajectory classes of a two-level engine.
#' @param backend `"Nestimate"` (default) uses
#'   [Nestimate::build_clusters()]. Use `"base"` for the small internal
#'   equivalence backend, which supports only Hamming/LCS distances.
#' @param labels Optional unique trajectory labels.
#' @param seed Reproducible random seed passed to the clustering backend.
#'
#' @return A `vasstra_trajectories` object with tidy `membership`, cluster
#'   sizes, silhouette, and the complete distance matrix.
#'
#' @examples
#' sequences <- sequences(
#'   data.frame(
#'     id = rep(1:6, each = 3),
#'     time = rep(1:3, 6),
#'     state = c(
#'       "A", "A", "A", "A", "A", "B",
#'       "B", "B", "B", "B", "B", "A",
#'       "C", "C", "C", "C", "C", "B"
#'     )
#'   ),
#'   id = "id",
#'   time = "time",
#'   state = "state"
#' )
#' trajectories <- trajectories(sequences, n_trajectories = 3)
#' trajectories
#' @export
trajectories <- function(
    data,
    n_trajectories = "auto",
    dissimilarity = c(
      "hamming", "osa", "lv", "dl", "lcs",
      "qgram", "cosine", "jaccard", "jw"
    ),
    method = c(
      "pam", "ward.D2", "ward.D", "complete", "average",
      "single", "mcquitty", "median", "centroid", "model"
    ),
    backend = c("Nestimate", "base"),
    labels = NULL,
    seed = 123L) {
  stopifnot(inherits(data, "vasstra_sequences"))
  clustering_requested <- !missing(method) || !missing(dissimilarity)
  dissimilarity <- match.arg(dissimilarity)
  method <- match.arg(method)
  backend <- match.arg(backend)
  source_states <- data$source
  model_source <- inherits(source_states, "vasstra_states") &&
    isTRUE(source_states$settings$engine %in% .vasstra_two_level_engines())
  if (identical(method, "model") ||
      (model_source && !clustering_requested)) {
    if (!model_source) {
      stop(
        paste0(
          "`method = \"model\"` needs states from a two-level engine: ",
          "multilpa, multilca, or lta."
        ),
        call. = FALSE
      )
    }
    return(.vasstra_model_trajectories(
      data,
      n_trajectories = n_trajectories,
      labels = labels,
      seed = seed
    ))
  }
  if (identical(backend, "base") &&
      !dissimilarity %in% c("hamming", "lcs")) {
    stop(
      "The `base` backend supports only `hamming` and `lcs` distances.",
      call. = FALSE
    )
  }
  selection <- NULL
  if (.vasstra_is_auto(n_trajectories) && !is.null(labels)) {
    n_trajectories <- length(labels)
    message(sprintf(
      "Using n_trajectories = %d to match the supplied labels.",
      n_trajectories
    ))
  }
  candidate_counts <- if (.vasstra_is_auto(n_trajectories)) {
    .vasstra_auto_range(nrow(data$data))
  } else if (is.numeric(n_trajectories) && length(n_trajectories) > 1L) {
    n_trajectories
  } else {
    NULL
  }
  if (!is.null(candidate_counts)) {
    sweep <- trajectory_choices(
      data,
      n_trajectories = candidate_counts,
      dissimilarity = dissimilarity,
      method = method,
      seed = seed,
      minimum_proportion = 0.05
    )
    best <- .vasstra_auto_pick(sweep, "trajectory")
    n_trajectories <- best$n_trajectories[[1L]]
    message(sprintf(
      paste0(
        "Selected n_trajectories = %d (%s + %s, silhouette = %.3f); ",
        "see `diagnostics$selection`."
      ),
      n_trajectories,
      dissimilarity,
      method,
      best$silhouette[[1L]]
    ))
    selection <- list(candidate = best, candidates = sweep$candidates)
  }
  if (!is.numeric(n_trajectories) || length(n_trajectories) != 1L ||
      !is.finite(n_trajectories) ||
      n_trajectories != floor(n_trajectories) ||
      n_trajectories < 2L) {
    stop("`n_trajectories` must be a whole number of at least 2.",
         call. = FALSE)
  }
  n_trajectories <- as.integer(n_trajectories)
  n_sequences <- nrow(data$data)
  if (n_trajectories > n_sequences - 1L) {
    stop(sprintf(
      "`n_trajectories` must be at most n - 1 (%d).",
      n_sequences - 1L
    ), call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) {
    stop("`seed` must be one finite number.", call. = FALSE)
  }
  resolved_backend <- backend
  sequence_matrix <- as.matrix(data$data)
  if (resolved_backend == "Nestimate") {
    model <- tryCatch(
      .vasstra_with_seed(
        seed,
        Nestimate::build_clusters(
          data$data,
          k = n_trajectories,
          dissimilarity = dissimilarity,
          method = method,
          seed = as.integer(seed)
        )
      ),
      error = function(error) {
        stop(sprintf(
          "Nestimate trajectory clustering failed: %s",
          conditionMessage(error)
        ), call. = FALSE)
      }
    )
    assignments <- as.integer(model$assignments)
    distance <- model$distance
    silhouette <- as.numeric(model$silhouette)
  } else {
    distance <- if (dissimilarity == "hamming") {
      .vasstra_hamming_distance(sequence_matrix)
    } else {
      .vasstra_lcs_distance(sequence_matrix)
    }
    if (method == "pam") {
      model <- tryCatch(
        cluster::pam(
          distance,
          diss = TRUE,
          k = n_trajectories
        ),
        error = function(error) {
          stop(sprintf(
            "PAM trajectory clustering failed: %s",
            conditionMessage(error)
          ), call. = FALSE)
        }
      )
      assignments <- as.integer(model$clustering)
      silhouette <- as.numeric(model$silinfo$avg.width)
    } else {
      model <- tryCatch(
        stats::hclust(distance, method = method),
        error = function(error) {
          stop(sprintf(
            "Hierarchical trajectory clustering failed: %s",
            conditionMessage(error)
          ), call. = FALSE)
        }
      )
      assignments <- as.integer(stats::cutree(model, k = n_trajectories))
      silhouette <- .vasstra_silhouette(assignments, distance)
    }
  }
  .vasstra_assemble_trajectories(
    data = data,
    raw_assignments = assignments,
    n_trajectories = n_trajectories,
    labels = labels,
    distance = distance,
    silhouette = silhouette,
    model = model,
    settings = list(
      dissimilarity = dissimilarity,
      method = method,
      backend = resolved_backend,
      seed = as.integer(seed)
    ),
    selection = selection
  )
}

# The shared tail of step 3: order and label the trajectories and describe
# them, whether they were clustered or estimated by a two-level model.
.vasstra_assemble_trajectories <- function(
    data,
    raw_assignments,
    n_trajectories,
    labels,
    distance,
    silhouette,
    model,
    settings,
    selection,
    extra_summary = NULL,
    extra_diagnostics = list()) {
  stopifnot(inherits(data, "vasstra_sequences"), is.list(settings))
  sequence_matrix <- as.matrix(data$data)
  n_sequences <- nrow(data$data)
  assignments <- raw_assignments
  assignments <- .vasstra_relabel_assignments(
    assignments,
    sequence_matrix
  )
  if (is.null(labels)) {
    labels <- paste("Trajectory", seq_len(n_trajectories))
  }
  if (!is.character(labels) || length(labels) != n_trajectories ||
      anyNA(labels) || any(!nzchar(labels)) || anyDuplicated(labels)) {
    stop(
      "`labels` must contain one unique non-empty label per trajectory.",
      call. = FALSE
    )
  }
  assignment_labels <- factor(
    labels[assignments],
    levels = labels,
    ordered = TRUE
  )
  id <- data$settings$id
  membership <- as.data.frame(
    stats::setNames(
      list(data$meta_data[[id]], assignment_labels),
      c(id, "trajectory")
    ),
    stringsAsFactors = FALSE
  )
  sizes <- stats::setNames(
    as.integer(tabulate(assignments, nbins = n_trajectories)),
    labels
  )
  distance_matrix <- as.matrix(distance)
  mean_within <- vapply(seq_len(n_trajectories), function(cluster_id) {
    members <- which(assignments == cluster_id)
    if (length(members) <= 1L) {
      return(0)
    }
    within <- distance_matrix[members, members, drop = FALSE]
    mean(within[lower.tri(within)])
  }, numeric(1L))
  cluster_summary <- data.frame(
    trajectory = factor(labels, levels = labels, ordered = TRUE),
    n = as.integer(sizes),
    proportion = as.integer(sizes) / n_sequences,
    mean_within_distance = mean_within,
    stringsAsFactors = FALSE
  )
  if (!is.null(extra_summary)) {
    cluster_summary <- cbind(cluster_summary, extra_summary(assignments))
  }
  structure(
    list(
      data = data$data,
      membership = membership,
      assignments = assignment_labels,
      cluster_summary = cluster_summary,
      distance = distance,
      silhouette = silhouette,
      model = model,
      settings = c(
        list(n_trajectories = n_trajectories),
        settings,
        list(labels = labels)
      ),
      diagnostics = c(
        list(
          n_sequences = n_sequences,
          sizes = sizes,
          silhouette = silhouette,
          selection = selection
        ),
        extra_diagnostics
      ),
      source = data
    ),
    class = c("vasstra_trajectories", "vasstra_step", "list")
  )
}

#' @export
print.vasstra_trajectories <- function(x, ...) {
  stopifnot(inherits(x, "vasstra_trajectories"))
  cat("VaSSTra Step 3: Sequences -> Trajectories\n")
  if (identical(x$settings$method, "model")) {
    cat(sprintf(
      paste0(
        "  %d sequences | %d trajectories | %s model | relative entropy %.3f",
        " | silhouette on hamming %.3f\n"
      ),
      x$diagnostics$n_sequences,
      x$settings$n_trajectories,
      x$settings$engine,
      x$diagnostics$relative_entropy,
      x$silhouette
    ))
  } else {
    cat(sprintf(
      "  %d sequences | %d trajectories | %s + %s | silhouette %.3f\n",
      x$diagnostics$n_sequences,
      x$settings$n_trajectories,
      x$settings$dissimilarity,
      x$settings$method,
      x$silhouette
    ))
  }
  cat("  Sizes: ", paste(
    sprintf("%s=%d", names(x$diagnostics$sizes), x$diagnostics$sizes),
    collapse = ", "
  ), "\n", sep = "")
  invisible(x)
}

#' @export
summary.vasstra_trajectories <- function(object, ...) {
  stopifnot(inherits(object, "vasstra_trajectories"))
  object$cluster_summary
}

#' @export
as.data.frame.vasstra_trajectories <- function(
    x,
    row.names = NULL,
    optional = FALSE,
    ...) {
  stopifnot(inherits(x, "vasstra_trajectories"))
  as.data.frame(
    x$membership,
    row.names = row.names,
    optional = optional,
    ...
  )
}

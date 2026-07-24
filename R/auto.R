# Automated role detection and automated cluster-count selection.
#
# Every automated decision is reported with one message() so the analysis
# stays transparent. Explicit arguments and attached data metadata always
# take precedence over detection.

.vasstra_role_aliases <- function(role) {
  stopifnot(is.character(role), length(role) == 1L)
  switch(
    role,
    id = c(
      "id", "user_id", "userid", "student", "student_id", "subject",
      "subject_id", "person", "person_id", "actor", "participant",
      "participant_id", "user", "case", "case_id"
    ),
    time = c(
      "time", "sequence_position", "position", "wave", "occasion",
      "course", "timepoint", "time_point", "t", "week", "session",
      "measurement", "visit", "period", "day", "month", "year"
    ),
    stop("Unknown role.", call. = FALSE)
  )
}

.vasstra_detect_role <- function(data, role, exclude = character(0L)) {
  stopifnot(is.data.frame(data), is.character(exclude))
  aliases <- .vasstra_role_aliases(role)
  candidates <- setdiff(names(data), exclude)
  matched <- candidates[match(aliases, tolower(candidates), nomatch = 0L)]
  if (length(matched) == 0L) {
    stop(sprintf(
      paste0(
        "Supply `%s` explicitly; no column matching a common %s name ",
        "(%s, ...) was found."
      ),
      role, role, paste(utils::head(aliases, 4L), collapse = ", ")
    ), call. = FALSE)
  }
  matched[[1L]]
}

.vasstra_detect_variables <- function(data, id, time) {
  stopifnot(is.data.frame(data))
  numeric_columns <- names(data)[
    vapply(data, is.numeric, logical(1L))
  ]
  variables <- setdiff(numeric_columns, c(id, time))
  standardized <- variables[endsWith(variables, "_z")]
  if (length(standardized) >= 2L) {
    variables <- standardized
  }
  if (length(variables) < 2L) {
    stop(
      paste0(
        "Supply `variables` explicitly; fewer than two numeric indicator ",
        "columns were found."
      ),
      call. = FALSE
    )
  }
  variables
}

# Resolve id/time/variables from explicit arguments, metadata, or detection.
# Returns the resolved roles plus a character vector describing what was
# detected (empty when everything was explicit or metadata-provided).
.vasstra_auto_roles <- function(data, id, time, variables,
                                need_variables = TRUE) {
  stopifnot(is.data.frame(data))
  metadata <- .vasstra_data_metadata(data)
  detected <- character(0L)
  if (is.null(id)) {
    id <- metadata$id
  }
  if (is.null(time)) {
    time <- metadata$time
  }
  if (is.null(variables) && need_variables) {
    variables <- metadata$variables
  }
  if (is.null(id)) {
    id <- .vasstra_detect_role(data, "id")
    detected <- c(detected, sprintf("id = \"%s\"", id))
  }
  if (is.null(time)) {
    time <- .vasstra_detect_role(data, "time", exclude = id)
    detected <- c(detected, sprintf("time = \"%s\"", time))
  }
  if (is.null(variables) && need_variables) {
    variables <- .vasstra_detect_variables(data, id, time)
    detected <- c(
      detected,
      sprintf("variables = %d numeric indicators", length(variables))
    )
  }
  if (length(detected) > 0L) {
    message("Detected ", paste(detected, collapse = ", "), ".")
  }
  list(id = id, time = time, variables = variables)
}

# Detect an existing state column: the single non-numeric column that is
# not the id or time role.
.vasstra_detect_state <- function(data, id, time) {
  stopifnot(is.data.frame(data))
  categorical <- names(data)[
    vapply(
      data,
      function(column) is.character(column) || is.factor(column),
      logical(1L)
    )
  ]
  candidates <- setdiff(categorical, c(id, time))
  if (length(candidates) == 1L) {
    message(sprintf("Detected state = \"%s\".", candidates))
    return(candidates)
  }
  if (length(candidates) == 0L) {
    stop(
      "Supply `state` explicitly; no categorical state column was found.",
      call. = FALSE
    )
  }
  stop(sprintf(
    "Supply `state` explicitly; several categorical columns qualify: %s.",
    paste(candidates, collapse = ", ")
  ), call. = FALSE)
}

.vasstra_is_auto <- function(value) {
  is.null(value) ||
    (is.character(value) && length(value) == 1L && identical(value, "auto"))
}

.vasstra_auto_range <- function(n_units) {
  stopifnot(is.numeric(n_units), length(n_units) == 1L)
  upper <- min(6L, as.integer(n_units) - 1L)
  if (upper < 2L) {
    stop("Too few observations to compare cluster counts.", call. = FALSE)
  }
  2L:upper
}

# Pick one candidate from a choices object: the recommended candidate of the
# requested method group, reported with one message.
.vasstra_auto_pick <- function(choices, label) {
  stopifnot(inherits(choices, "vasstra_choices"), is.character(label))
  recommended <- choices$recommendations
  if (nrow(recommended) == 0L) {
    failures <- choices$failures
    if (is.data.frame(failures) &&
        nrow(failures) == nrow(choices$candidates) &&
        nrow(failures) > 0L) {
      stop(failures$error[[1L]], call. = FALSE)
    }
    stop(sprintf(
      "Automatic selection found no eligible %s solution; compare candidates manually.",
      label
    ), call. = FALSE)
  }
  best <- recommended[order(-recommended$silhouette,
                            recommended$candidate_id), , drop = FALSE][1L, ]
  best
}

test_that("step1 state methods reproduce their direct clustering backends", {
  data <- make_vasstra_data()
  variables <- c("views", "sessions", "duration")
  seed <- 73L
  n_start <- 17L

  kmeans_result <- step1_states(
    data,
    id = "student",
    time = "course",
    variables = variables,
    n_states = 3,
    n_start = n_start,
    seed = seed,
    method = "kmeans"
  )
  standardized <- as.matrix(kmeans_result$standardized)
  set.seed(seed)
  direct_kmeans <- stats::kmeans(
    standardized,
    centers = 3,
    nstart = n_start
  )

  expect_true(same_partition(
    kmeans_result$model$vasstra_assignments,
    direct_kmeans$cluster
  ))
  expect_equal(
    kmeans_result$diagnostics$total_withinss,
    direct_kmeans$tot.withinss,
    tolerance = 1e-12
  )
  expect_identical(kmeans_result$settings$method, "kmeans")
  expect_true(is.na(kmeans_result$settings$lpa_model))

  pam_result <- step1_states(
    data,
    id = "student",
    time = "course",
    variables = variables,
    n_states = 3,
    seed = seed,
    method = "pam"
  )
  direct_pam <- cluster::pam(standardized, k = 3)

  expect_true(same_partition(
    pam_result$model$vasstra_assignments,
    direct_pam$clustering
  ))
  expect_equal(
    pam_result$diagnostics$pam_objective,
    direct_pam$objective[["swap"]],
    tolerance = 1e-12
  )
  expect_identical(pam_result$settings$method, "pam")

  hierarchical_methods <- c(
    "ward.D2", "ward.D", "complete", "average",
    "single", "mcquitty", "median", "centroid"
  )
  invisible(lapply(hierarchical_methods, function(method_name) {
    result <- step1_states(
      data,
      id = "student",
      time = "course",
      variables = variables,
      n_states = 3,
      seed = seed,
      method = method_name
    )
    direct <- stats::hclust(
      stats::dist(standardized),
      method = method_name
    )

    expect_true(same_partition(
      result$model$vasstra_assignments,
      stats::cutree(direct, k = 3)
    ), info = method_name)
    expect_equal(
      result$model$height,
      direct$height,
      tolerance = 1e-12,
      info = method_name
    )
    expect_identical(result$settings$method, method_name)
  }))
})

test_that("LPA states reproduce mclust and expose mixture diagnostics", {
  skip_if_not_installed("mclust")
  data <- make_vasstra_data()
  variables <- c("views", "sessions", "duration")
  seed <- 79L
  set.seed(910L)
  random_state <- .Random.seed

  result <- step1_states(
    data,
    id = "student",
    time = "course",
    variables = variables,
    n_states = 3,
    seed = seed,
    method = "lpa",
    lpa_model = "EEI"
  )

  expect_identical(.Random.seed, random_state)
  set.seed(seed)
  direct <- do.call(
    mclust::Mclust,
    list(
      data = as.matrix(result$standardized),
      G = 3,
      modelNames = "EEI",
      verbose = FALSE
    ),
    envir = asNamespace("mclust")
  )
  direct_entropy <- {
    positive <- direct$z > 0
    1 + sum(direct$z[positive] * log(direct$z[positive])) /
      (nrow(direct$z) * log(ncol(direct$z)))
  }

  expect_true(same_partition(
    result$model$vasstra_assignments,
    direct$classification
  ))
  expect_equal(
    result$diagnostics$bic_native,
    as.numeric(direct$bic),
    tolerance = 1e-10
  )
  expect_equal(
    result$diagnostics$bic,
    -as.numeric(direct$bic),
    tolerance = 1e-10
  )
  expect_equal(
    result$diagnostics$aic,
    -2 * as.numeric(direct$loglik) + 2 * as.numeric(direct$df),
    tolerance = 1e-10
  )
  expect_equal(
    result$diagnostics$icl_native,
    as.numeric(direct$icl),
    tolerance = 1e-10
  )
  expect_equal(
    result$diagnostics$classification_entropy,
    direct_entropy,
    tolerance = 1e-12
  )
  expect_equal(
    result$diagnostics$mean_uncertainty,
    mean(direct$uncertainty),
    tolerance = 1e-12
  )
  expect_equal(
    result$diagnostics$log_likelihood,
    as.numeric(direct$loglik),
    tolerance = 1e-12
  )
  expect_identical(
    result$diagnostics$n_parameters,
    as.integer(direct$df)
  )
  expect_identical(result$settings$lpa_model, "EEI")
})

test_that("state choices provide transparent constrained recommendations", {
  data <- make_vasstra_data()
  variables <- c("views", "sessions", "duration")
  set.seed(920L)
  random_state <- .Random.seed

  choices <- state_choices(
    data,
    id = "student",
    time = "course",
    variables = variables,
    n_states = 2:4,
    method = c("kmeans", "pam", "complete"),
    seed = 83L,
    minimum_size = 40L,
    maximum_size_ratio = 3
  )

  expect_identical(.Random.seed, random_state)
  expect_s3_class(choices, "vasstra_state_choices")
  expect_equal(nrow(choices$candidates), 9L)
  expect_identical(choices$candidates$candidate_id, seq_len(9L))
  expect_true(all(c(
    "candidate_id", "n_states", "method", "lpa_model", "status",
    "error", "silhouette", "total_withinss", "pam_objective",
    "aic", "bic", "log_likelihood", "n_parameters",
    "min_size", "min_proportion", "size_ratio", "eligible",
    "recommendation_criterion", "recommendation_direction",
    "is_recommended"
  ) %in% names(choices$candidates)))
  expect_true(all(choices$candidates$status == "ok"))
  expect_equal(nrow(choices$failures), 0L)
  expect_equal(nrow(choices$recommendations), 3L)
  expect_equal(choices$recommendations$n_states, rep(3L, 3L))
  expect_setequal(
    choices$recommendations$method,
    c("kmeans", "pam", "complete")
  )
  expect_true(all(
    choices$candidates$eligible[choices$candidates$n_states == 3L]
  ))
  expect_false(any(
    choices$candidates$eligible[choices$candidates$n_states == 4L]
  ))
  expect_identical(summary(choices), choices$candidates)
  expect_identical(as.data.frame(choices), choices$candidates)

  selected <- choices$candidates[
    choices$candidates$method == "kmeans" &
      choices$candidates$n_states == 3L,
    ,
    drop = FALSE
  ]
  set.seed(921L)
  refit_random_state <- .Random.seed
  refit <- fit_state_choice(
    choices,
    selected$candidate_id[[1L]],
    labels = c("Low", "Average", "High"),
    state = "chosen_state"
  )
  expect_identical(.Random.seed, refit_random_state)
  direct <- step1_states(
    data,
    id = "student",
    time = "course",
    variables = variables,
    n_states = 3,
    labels = c("Low", "Average", "High"),
    state = "chosen_state",
    seed = 83L,
    method = "kmeans"
  )

  expect_true(same_partition(
    refit$data$chosen_state,
    direct$data$chosen_state
  ))
  expect_equal(
    refit$diagnostics$total_withinss,
    direct$diagnostics$total_withinss,
    tolerance = 1e-12
  )
  expect_identical(
    refit$diagnostics$selection$candidate$candidate_id,
    selected$candidate_id
  )
  expect_identical(
    refit$diagnostics$selection$candidates,
    choices$candidates
  )
})

test_that("state choices retain candidate failures and enforce fit selection", {
  data <- expand.grid(
    student = seq_len(4L),
    course = seq_len(2L)
  )
  data$first <- rep(c(0, 1), each = 4L)
  data$second <- data$first
  choices <- state_choices(
    data,
    id = "student",
    time = "course",
    variables = c("first", "second"),
    n_states = 2:3,
    method = "kmeans",
    standardize = "none"
  )

  expect_identical(choices$candidates$status, c("ok", "failed"))
  expect_equal(nrow(choices$failures), 1L)
  expect_match(
    choices$failures$error,
    "at least `n_states` distinct rows",
    fixed = TRUE
  )
  expect_error(
    fit_state_choice(choices, choices$failures$candidate_id[[1L]]),
    "Candidate 2 failed"
  )
  expect_error(
    fit_state_choice(choices, 99L),
    "not present"
  )
  expect_error(
    fit_state_choice(choices, 1.5),
    "whole number"
  )

  constrained <- state_choices(
    data,
    id = "student",
    time = "course",
    variables = c("first", "second"),
    n_states = 2,
    method = "kmeans",
    standardize = "none",
    minimum_size = 5L
  )
  expect_false(constrained$candidates$eligible)
  expect_equal(nrow(constrained$recommendations), 0L)
  expect_warning(
    fit_state_choice(constrained, 1L),
    "does not meet"
  )
  expect_error(
    state_choices(
      data,
      "student",
      "course",
      c("first", "second"),
      n_states = 1L
    ),
    "2 through n - 1"
  )
  expect_error(
    state_choices(
      data,
      "student",
      "course",
      c("first", "second"),
      n_states = 2L,
      maximum_size_ratio = 0.9
    ),
    "at least 1"
  )
})

test_that("LPA choices use the requested criterion within each model", {
  skip_if_not_installed("mclust")
  data <- make_vasstra_data()
  choices <- state_choices(
    data,
    id = "student",
    time = "course",
    variables = c("views", "sessions", "duration"),
    n_states = 2:4,
    method = "lpa",
    lpa_model = c("EEI", "VVI"),
    lpa_criterion = "bic",
    seed = 89L
  )

  expect_equal(nrow(choices$candidates), 6L)
  expect_true(all(choices$candidates$status == "ok"))
  expect_equal(
    choices$candidates$bic,
    -choices$candidates$bic_native,
    tolerance = 1e-10
  )
  expect_equal(
    choices$candidates$aic,
    -2 * choices$candidates$log_likelihood +
      2 * choices$candidates$n_parameters,
    tolerance = 1e-10
  )
  expect_true(all(
    choices$candidates$recommendation_criterion == "bic"
  ))
  expect_true(all(
    choices$candidates$recommendation_direction == "min"
  ))
  expect_equal(nrow(choices$recommendations), 2L)
  expect_equal(choices$recommendations$n_states, c(3L, 3L))
  expect_setequal(choices$recommendations$lpa_model, c("EEI", "VVI"))

  minimum_bic <- vapply(
    split(choices$candidates, choices$candidates$lpa_model),
    function(candidate_group) min(candidate_group$bic),
    numeric(1L)
  )
  recommended_bic <- stats::setNames(
    choices$recommendations$bic,
    choices$recommendations$lpa_model
  )
  expect_equal(
    recommended_bic[names(minimum_bic)],
    minimum_bic,
    tolerance = 1e-10
  )

  criteria <- c("aic", "silhouette", "icl_native")
  criterion_choices <- lapply(criteria, function(criterion_name) {
    state_choices(
      data,
      id = "student",
      time = "course",
      variables = c("views", "sessions", "duration"),
      n_states = 2:4,
      method = "lpa",
      lpa_model = "EEI",
      lpa_criterion = criterion_name,
      seed = 89L
    )
  })
  invisible(Map(function(criterion_choice, criterion_name) {
    direction <- if (criterion_name == "aic") "min" else "max"
    scores <- criterion_choice$candidates[[criterion_name]]
    expected_score <- if (direction == "min") min(scores) else max(scores)

    expect_identical(
      criterion_choice$settings$lpa_criterion,
      criterion_name
    )
    expect_equal(nrow(criterion_choice$recommendations), 1L)
    expect_identical(
      criterion_choice$recommendations$recommendation_criterion,
      criterion_name
    )
    expect_identical(
      criterion_choice$recommendations$recommendation_direction,
      direction
    )
    expect_equal(
      criterion_choice$recommendations[[criterion_name]],
      expected_score,
      tolerance = 1e-10
    )
  }, criterion_choices, criteria))
})

test_that("step3 supports every Nestimate distance and clustering method", {
  data <- make_vasstra_data()
  sequences <- step2_sequences(
    data,
    id = "student",
    time = "course",
    state = "true_state"
  )
  distances <- c(
    "hamming", "osa", "lv", "dl", "lcs",
    "qgram", "cosine", "jaccard", "jw"
  )
  methods <- c(
    "pam", "ward.D2", "ward.D", "complete", "average",
    "single", "mcquitty", "median", "centroid"
  )
  seed <- 97L
  set.seed(930L)
  random_state <- .Random.seed

  distance_results <- lapply(distances, function(distance_name) {
    step3_trajectories(
      sequences,
      n_trajectories = 3,
      dissimilarity = distance_name,
      method = "pam",
      backend = "Nestimate",
      seed = seed
    )
  })
  method_results <- lapply(methods, function(method_name) {
    step3_trajectories(
      sequences,
      n_trajectories = 3,
      dissimilarity = "hamming",
      method = method_name,
      backend = "Nestimate",
      seed = seed
    )
  })

  expect_identical(.Random.seed, random_state)
  invisible(Map(function(result, distance_name) {
    direct <- Nestimate::build_clusters(
      sequences$data,
      k = 3,
      dissimilarity = distance_name,
      method = "pam",
      seed = seed
    )
    expect_equal(
      as.matrix(result$distance),
      as.matrix(direct$distance),
      tolerance = 1e-12,
      info = distance_name
    )
    expect_equal(
      result$silhouette,
      direct$silhouette,
      tolerance = 1e-12,
      info = distance_name
    )
    expect_true(same_partition(
      result$assignments,
      direct$assignments
    ), info = distance_name)
    expect_identical(result$settings$dissimilarity, distance_name)
  }, distance_results, distances))
  invisible(Map(function(result, method_name) {
    direct <- Nestimate::build_clusters(
      sequences$data,
      k = 3,
      dissimilarity = "hamming",
      method = method_name,
      seed = seed
    )
    expect_equal(
      as.matrix(result$distance),
      as.matrix(direct$distance),
      tolerance = 1e-12,
      info = method_name
    )
    expect_equal(
      result$silhouette,
      direct$silhouette,
      tolerance = 1e-12,
      info = method_name
    )
    expect_true(same_partition(
      result$assignments,
      direct$assignments
    ), info = method_name)
    expect_identical(result$settings$method, method_name)
  }, method_results, methods))

  expect_error(
    step3_trajectories(
      sequences,
      n_trajectories = 3,
      dissimilarity = "osa",
      backend = "base"
    ),
    "supports only `hamming` and `lcs`"
  )
})

test_that("trajectory choices cover the complete Nestimate candidate grid", {
  data <- make_vasstra_data()
  sequences <- step2_sequences(
    data,
    id = "student",
    time = "course",
    state = "true_state"
  )
  distances <- c(
    "hamming", "osa", "lv", "dl", "lcs",
    "qgram", "cosine", "jaccard", "jw"
  )
  methods <- c(
    "pam", "ward.D2", "ward.D", "complete", "average",
    "single", "mcquitty", "median", "centroid"
  )
  seed <- 101L
  set.seed(940L)
  random_state <- .Random.seed

  choices <- trajectory_choices(
    sequences,
    n_trajectories = 2:3,
    dissimilarity = distances,
    method = methods,
    seed = seed,
    minimum_size = 10L,
    maximum_size_ratio = 3
  )

  expect_identical(.Random.seed, random_state)
  expect_s3_class(choices, "vasstra_trajectory_choices")
  expect_equal(nrow(choices$candidates), 162L)
  expect_identical(choices$candidates$candidate_id, seq_len(162L))
  expect_true(all(c(
    "candidate_id", "n_trajectories", "dissimilarity", "method",
    "status", "error", "silhouette", "mean_within_distance",
    "min_size", "min_proportion", "size_ratio", "eligible",
    "is_recommended"
  ) %in% names(choices$candidates)))
  expect_true(all(choices$candidates$status == "ok"))
  expect_equal(nrow(choices$failures), 0L)
  expect_equal(
    length(unique(interaction(
      choices$candidates$dissimilarity,
      choices$candidates$method,
      drop = TRUE
    ))),
    81L
  )
  expect_equal(nrow(choices$recommendations), 81L)
  expect_true(all(choices$recommendations$n_trajectories == 3L))
  expect_true(all(choices$recommendations$eligible))
  expect_identical(summary(choices), choices$candidates)
  expect_identical(as.data.frame(choices), choices$candidates)

  direct <- as.data.frame(Nestimate::cluster_choice(
    sequences$data,
    k = 2:3,
    dissimilarity = distances,
    method = methods,
    seed = seed
  ))
  expect_equal(
    choices$candidates$silhouette,
    direct$silhouette,
    tolerance = 1e-12
  )
  expect_equal(
    choices$candidates$mean_within_distance,
    direct$mean_within_dist,
    tolerance = 1e-12
  )
  expect_identical(
    choices$candidates$n_trajectories,
    as.integer(direct$k)
  )
  expect_identical(
    choices$candidates$dissimilarity,
    as.character(direct$dissimilarity)
  )
  expect_identical(
    choices$candidates$method,
    as.character(direct$method)
  )
})

test_that("trajectory choices refit explicitly and enforce constraints", {
  data <- make_vasstra_data()
  sequences <- step2_sequences(
    data,
    id = "student",
    time = "course",
    state = "true_state"
  )
  choices <- trajectory_choices(
    sequences,
    n_trajectories = 2:4,
    dissimilarity = "lcs",
    method = "ward.D2",
    seed = 103L,
    minimum_size = 10L
  )

  expect_equal(choices$recommendations$n_trajectories, 3L)
  expect_false(
    choices$candidates$eligible[
      choices$candidates$n_trajectories == 4L
    ]
  )
  selected <- choices$recommendations[1L, , drop = FALSE]
  set.seed(950L)
  random_state <- .Random.seed
  refit <- fit_trajectory_choice(
    choices,
    selected$candidate_id[[1L]],
    labels = c("First", "Second", "Third")
  )
  expect_identical(.Random.seed, random_state)
  direct <- step3_trajectories(
    sequences,
    n_trajectories = 3,
    dissimilarity = "lcs",
    method = "ward.D2",
    backend = "Nestimate",
    labels = c("First", "Second", "Third"),
    seed = 103L
  )

  expect_true(same_partition(refit$assignments, direct$assignments))
  expect_equal(
    as.matrix(refit$distance),
    as.matrix(direct$distance),
    tolerance = 1e-12
  )
  expect_equal(refit$silhouette, direct$silhouette, tolerance = 1e-12)
  expect_identical(
    refit$diagnostics$selection$candidate$candidate_id,
    selected$candidate_id
  )
  expect_identical(
    refit$diagnostics$selection$candidates,
    choices$candidates
  )

  ineligible_id <- choices$candidates$candidate_id[
    choices$candidates$n_trajectories == 4L
  ][[1L]]
  expect_warning(
    fit_trajectory_choice(choices, ineligible_id),
    "does not meet"
  )
  expect_error(
    fit_trajectory_choice(choices, 99L),
    "not present"
  )
  expect_error(
    fit_trajectory_choice(choices, NA_real_),
    "whole number"
  )
  expect_error(
    trajectory_choices(sequences, n_trajectories = 1L),
    "2 through n - 1"
  )
  expect_error(
    trajectory_choices(
      sequences,
      n_trajectories = 2L,
      minimum_proportion = 1.1
    ),
    "between 0 and 1"
  )
})

test_that("vasstra passes state_method through the complete wrapper", {
  data <- make_vasstra_data()
  variables <- c("views", "sessions", "duration")
  methods <- c("pam", "complete")

  invisible(lapply(methods, function(method_name) {
    fit <- vasstra(
      data,
      id = "student",
      time = "course",
      variables = variables,
      n_states = 3,
      n_trajectories = 3,
      state_labels = c("Low", "Average", "High"),
      backend = "base",
      state_method = method_name,
      seed = 107L
    )
    direct <- step1_states(
      data,
      id = "student",
      time = "course",
      variables = variables,
      n_states = 3,
      labels = c("Low", "Average", "High"),
      seed = 107L,
      method = method_name
    )

    expect_identical(fit$settings$state_method, method_name)
    expect_identical(fit$states$settings$method, method_name)
    expect_true(same_partition(
      fit$states$data$state,
      direct$data$state
    ), info = method_name)
    expect_true(is.na(fit$settings$lpa_model))
  }))
})

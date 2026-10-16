# The state engines: latents mixture models (lpa, lca, multilpa, multilca,
# lta), the clustering engines, and fitted latents models passed as `engine`.

engine_variables <- c("views", "sessions", "duration")

# The synthetic panel with categorical copies of two indicators.
categorical_vasstra_data <- function() {
  data <- make_vasstra_data()
  data$views_level <- cut(data$views, 3, labels = c("low", "mid", "high"))
  data$duration_level <- cut(data$duration, 3,
                             labels = c("low", "mid", "high"))
  data
}

fit_states <- function(engine, ..., data = make_vasstra_data(),
                       variables = engine_variables) {
  allow_failed_starts(suppressMessages(states(
    data,
    id = "student",
    time = "course",
    variables = variables,
    engine = engine,
    seed = 11L,
    n_start = 10L,
    ...
  )))
}

test_that("engine resolves names, the superseded method, and refuses clashes", {
  expect_identical(.vasstra_resolve_engine(NULL, NULL), "lpa")
  expect_identical(.vasstra_resolve_engine(NULL, "kmeans"), "kmeans")
  expect_identical(.vasstra_resolve_engine("lta", NULL), "lta")
  expect_error(.vasstra_resolve_engine("lpa", "kmeans"), "not both")
  expect_error(.vasstra_resolve_engine("gmm", NULL), "must be a fitted latents")
  states <- fit_states(NULL, method = "kmeans", n_states = 3)
  expect_identical(states$settings$engine, "kmeans")
})

test_that("the lpa engine is latents::lpa on the standardized indicators", {
  states <- fit_states("lpa", n_states = 3)
  frame <- states$standardized
  direct <- allow_failed_starts(latents::lpa(
    frame, engine_variables, n_profiles = 3, model = "EEI", n_starts = 10,
    seed = 11
  ))
  expect_equal(states$diagnostics$log_likelihood, direct$log_likelihood,
               tolerance = 1e-10)
  expect_true(same_partition(states$model$vasstra_assignments,
                             direct$subject_profiles))
  criteria <- latents::get_results(direct, "information_criteria")
  expect_equal(states$diagnostics$bic, criteria$bic_individual,
               tolerance = 1e-10)
  expect_equal(states$diagnostics$awe, criteria$awe_individual,
               tolerance = 1e-10)
  expect_identical(states$settings$lpa_model, "EEI")
})

test_that("the lpa engine reaches at least mclust's likelihood", {
  skip_if_not_installed("mclust")
  states <- fit_states("lpa", n_states = 3)
  direct <- do.call(
    mclust::Mclust,
    list(data = as.matrix(states$standardized), G = 3, modelNames = "EEI",
         verbose = FALSE),
    envir = asNamespace("mclust")
  )
  expect_gte(states$diagnostics$log_likelihood,
             as.numeric(direct$loglik) - 1e-8)
})

test_that("posteriors are probabilities whose largest names the state", {
  states <- fit_states("lpa", n_states = 3)
  posteriors <- as.data.frame(states, what = "posteriors")
  probability_columns <- paste0("posterior_", 1:3)
  probabilities <- as.matrix(posteriors[probability_columns])
  expect_true(all(probabilities >= 0 & probabilities <= 1))
  expect_equal(unname(rowSums(probabilities)), rep(1, nrow(probabilities)),
               tolerance = 1e-10)
  expect_identical(
    unname(max.col(probabilities, ties.method = "first")),
    as.integer(posteriors$state)
  )
  expect_equal(posteriors$uncertainty,
               unname(pmax(1 - apply(probabilities, 1L, max), 0)))
  expect_error(
    as.data.frame(fit_states("kmeans", n_states = 3), what = "posteriors"),
    "model engine"
  )
})

test_that("states are ordered from the lowest to the highest profile", {
  states <- fit_states("lpa", n_states = 3)
  centers <- rowMeans(states$model$vasstra_centers)
  expect_identical(order(centers), 1:3)
})

test_that("the lca engine describes categorical states by category shares", {
  data <- categorical_vasstra_data()
  states <- fit_states(
    "lca",
    n_states = 3,
    data = data,
    variables = c("views_level", "duration_level")
  )
  expect_identical(states$settings$categorical,
                   c("views_level", "duration_level"))
  profiles <- summary(states)
  expect_true("category" %in% names(profiles))
  shares <- stats::aggregate(mean ~ state + variable, data = profiles,
                             FUN = sum)
  expect_equal(shares$mean, rep(1, nrow(shares)), tolerance = 1e-12)
  expect_true(is.na(states$diagnostics$silhouette))
  profile_matrix <- .vasstra_state_profile_matrix(states)
  expect_identical(nrow(profile_matrix), 6L)
  expect_true(all(grepl(": ", rownames(profile_matrix))))
})

test_that("a model engine mixes numeric and categorical indicators", {
  data <- categorical_vasstra_data()
  states <- fit_states(
    "lpa",
    n_states = 3,
    data = data,
    variables = c("views", "duration_level")
  )
  expect_identical(states$settings$categorical, "duration_level")
  expect_identical(states$model$continuous, "views")
})

test_that("a two-level engine supplies the trajectories as estimated", {
  states <- fit_states("multilpa", n_states = 3, n_trajectories = 3,
                       data = make_composition_data())
  expect_identical(states$settings$n_trajectories, 3L)
  sequences <- sequences(states)
  trajectories <- trajectories(sequences)
  expect_identical(trajectories$settings$method, "model")
  expect_identical(trajectories$settings$engine, "multilpa")
  ids <- sequences$meta_data$student
  classes <- states$model$group_classes[
    match(ids, states$model$group_ids)
  ]
  expect_true(same_partition(as.integer(trajectories$assignments), classes))
  expect_true(all(trajectories$cluster_summary$average_posterior <= 1))
  # The groups were built from different state mixes; the model recovers them.
  truth <- unique(make_composition_data()[c("student", "true_group")])
  recovered <- trajectories$membership
  recovered$true_group <- truth$true_group[match(recovered$student,
                                                  truth$student)]
  expect_true(same_partition(recovered$trajectory, recovered$true_group))
  expect_error(
    trajectories(sequences, n_trajectories = 2),
    "estimated 3 trajectory classes"
  )
  clustered <- trajectories(sequences, method = "pam",
                                  n_trajectories = 3)
  expect_identical(clustered$settings$method, "pam")
})

test_that("model trajectories need a two-level engine", {
  sequences <- sequences(fit_states("lpa", n_states = 3))
  expect_error(trajectories(sequences, method = "model"),
               "two-level engine")
})

test_that("the lta and multilca engines run the whole workflow", {
  lta_fit <- allow_failed_starts(suppressMessages(vasstra(
    make_vasstra_data(),
    id = "student",
    time = "course",
    variables = engine_variables,
    n_states = 3,
    n_trajectories = 2,
    engine = "lta",
    n_start = 5L
  )))
  expect_identical(lta_fit$settings$engine, "lta")
  expect_identical(lta_fit$trajectories$settings$method, "model")
  expect_s3_class(lta_fit$states$model, "multilpa_transitions")

  data <- make_composition_data()
  data$views_level <- cut(data$views, 3, labels = c("low", "mid", "high"))
  data$duration_level <- cut(data$duration, 3,
                             labels = c("low", "mid", "high"))
  multilca_fit <- allow_failed_starts(suppressMessages(vasstra(
    data,
    id = "student",
    time = "course",
    variables = c("views_level", "duration_level"),
    n_states = 3,
    n_trajectories = 2,
    engine = "multilca",
    n_start = 5L
  )))
  expect_identical(multilca_fit$trajectories$settings$engine, "multilca")
  expect_identical(nrow(as.data.frame(multilca_fit)), 45L)
})

test_that("a two-level engine compares states and trajectories jointly", {
  states <- fit_states("multilpa", n_states = 2:3, n_trajectories = 2:3,
                       data = make_composition_data())
  candidates <- states$diagnostics$selection$candidates
  expect_identical(nrow(candidates), 4L)
  expect_setequal(paste(candidates$n_states, candidates$n_trajectories),
                  c("2 2", "2 3", "3 2", "3 3"))
  eligible <- candidates[candidates$eligible, , drop = FALSE]
  chosen <- eligible[which.min(eligible$bic), , drop = FALSE]
  expect_identical(states$settings$n_states, chosen$n_states)
  expect_identical(states$settings$n_trajectories, chosen$n_trajectories)
})

test_that("candidate warnings are recorded in the comparison table", {
  data <- make_vasstra_data()
  choices <- state_choices(
    data, "student", "course", engine_variables,
    n_states = 2:4, method = "lpa", n_start = 25L, seed = 123L
  )
  expect_true("warnings" %in% names(choices$candidates))
  expect_type(choices$candidates$warnings, "character")
})

test_that("missing = 'fiml' keeps incomplete observations in a model engine", {
  data <- make_vasstra_data()
  data$views[c(3, 40, 77)] <- NA
  states <- fit_states("lpa", n_states = 3, data = data, missing = "fiml")
  expect_identical(states$diagnostics$n_missing, 3L)
  expect_identical(nrow(states$data), nrow(data))
  expect_false(anyNA(states$data$state))
  expect_error(
    fit_states("kmeans", n_states = 3, data = data, missing = "fiml"),
    "needs a model engine"
  )
})

test_that("a fitted latents model passed as engine supplies its states", {
  data <- make_composition_data()
  fit <- latents::multilpa(data, engine_variables, "student", n_profiles = 3,
                           n_group_classes = 3, time = "course",
                           model = "EEI", n_starts = 5, seed = 3)
  analysis <- suppressMessages(vasstra(data, engine = fit))
  expect_identical(analysis$settings$engine, "multilpa")
  expect_true(analysis$states$settings$supplied_fit)
  expect_null(evaluate(analysis$states)$enumeration)
  expect_true(same_partition(analysis$states$model$vasstra_assignments,
                             fit$subject_profiles))
  expect_identical(analysis$trajectories$settings$method, "model")
  expect_s3_class(evaluate(analysis), "vasstra_evaluations")

  expect_error(vasstra(data[-1, ], engine = fit), "rows")
  shifted <- data
  shifted$views <- rev(shifted$views)
  expect_error(vasstra(shifted, engine = fit), "row for row")
  expect_error(
    states(data, id = "course", engine = fit),
    "groups observations by `student`"
  )
})

test_that("a single-level fit passed as engine leaves trajectories to step 3", {
  data <- make_vasstra_data()
  fit <- latents::lpa(data, engine_variables, n_profiles = 3, model = "EEI",
                      n_starts = 5, seed = 3)
  analysis <- suppressMessages(vasstra(
    data, id = "student", time = "course", engine = fit, n_trajectories = 3
  ))
  expect_identical(analysis$states$settings$engine, "lpa")
  expect_identical(analysis$trajectories$settings$method, "pam")
})

test_that("evaluate hands a model engine to latents", {
  states <- fit_states("lpa", n_states = 3)
  evaluation <- allow_failed_starts(evaluate(states, n_states = 2:4))
  expect_s3_class(evaluation, "vasstra_model_evaluation")
  expect_s3_class(evaluation$enumeration, "multilpa_enumeration")
  expect_s3_class(evaluation$diagnostics, "multilpa_diagnostics")
  # latents' candidate table, unchanged.
  candidates <- as.data.frame(evaluation)
  direct <- latents::get_results(evaluation$enumeration, "candidates")
  expect_identical(candidates, direct)
  expect_setequal(candidates$n_profiles, 2:4)
  expect_identical(
    summary(evaluation),
    latents::get_results(latents::diagnostics(states$model, plots = FALSE),
                         "classification")
  )
  # The fitted candidate of the enumeration is the fitted model.
  expect_equal(
    candidates$log_likelihood[candidates$n_profiles == 3],
    states$model$log_likelihood,
    tolerance = 1e-8
  )

  kmeans_evaluation <- evaluate(fit_states("kmeans", n_states = 3),
                                n_states = 2:4)
  expect_s3_class(kmeans_evaluation, "vasstra_evaluation")

  multilevel <- fit_states("multilpa", n_states = 3, n_trajectories = 2,
                           data = make_composition_data())
  trajectories <- trajectories(sequences(multilevel))
  trajectory_evaluation <- allow_failed_starts(
    evaluate(trajectories, n_trajectories = 2:3)
  )
  expect_s3_class(trajectory_evaluation, "vasstra_model_evaluation")
  expect_setequal(as.data.frame(trajectory_evaluation)$n_group_classes, 2:3)
})

test_that("fit_indices of a model engine are latents' tables unchanged", {
  states <- fit_states("lpa", n_states = 3)
  expect_equal(
    as.data.frame(fit_indices(states)),
    as.data.frame(latents::get_results(states$model, "information_criteria")),
    ignore_attr = TRUE
  )
  compared <- allow_failed_starts(
    fit_indices(states, compare = TRUE, n_states = 2:4)
  )
  expect_setequal(compared$n_profiles, 2:4)
})

test_that("a model evaluation plots with latents", {
  skip_if_not_installed("ggplot2")
  evaluation <- allow_failed_starts(
    evaluate(fit_states("lpa", n_states = 3), n_states = 2:4)
  )
  expect_s3_class(plot(evaluation), "ggplot")
  expect_s3_class(plot(evaluation, what = "diagnostics"), "latents_plots")
})

test_that("set_labels relabels the posteriors with the states", {
  states <- set_labels(fit_states("lpa", n_states = 3),
                       c("Low", "Average", "High"))
  expect_identical(levels(as.data.frame(states, what = "posteriors")$state),
                   c("Low", "Average", "High"))
})

test_that("order-only trajectories leave composition classes empty, with a warning", {
  # Every synthetic group visits each state twice, in a different order:
  # multilpa sees one kind of subject, lta tells the orders apart.
  states <- fit_states("multilpa", n_states = 3, n_trajectories = 3)
  expect_warning(
    trajectories(sequences(states)),
    "without subjects"
  )
  lta_states <- fit_states("lta", n_states = 3, n_trajectories = 3)
  trajectories <- trajectories(sequences(lta_states))
  truth <- unique(make_vasstra_data()[c("student", "true_trajectory")])
  recovered <- trajectories$membership
  recovered$truth <- truth$true_trajectory[match(recovered$student,
                                                 truth$student)]
  expect_true(same_partition(recovered$trajectory, recovered$truth))
})

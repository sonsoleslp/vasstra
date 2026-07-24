# Extracted from test-step1-states.R:133

# test -------------------------------------------------------------------------
data <- make_vasstra_data()
data$views[c(1L, 20L)] <- NA_real_
data$constant <- 1
expect_error(
    step1_states(
      data,
      "student",
      "course",
      c("views", "sessions")
    ),
    "Missing indicator"
  )
imputed <- step1_states(
    data,
    "student",
    "course",
    c("views", "sessions"),
    missing = "median"
  )
expect_equal(imputed$diagnostics$missing_imputed, 2L)
expect_false(anyNA(imputed$standardized))
expect_warning(
    constant <- step1_states(
      data,
      "student",
      "course",
      c("sessions", "constant"),
      missing = "median"
    ),
    "Zero-variance"
  )

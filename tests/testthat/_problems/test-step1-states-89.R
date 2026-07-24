# Extracted from test-step1-states.R:89

# test -------------------------------------------------------------------------
data <- make_vasstra_data()
duplicated <- rbind(data, data[1L, ])
nonnumeric <- data
nonnumeric$views <- as.character(nonnumeric$views)
expect_error(
    step1_states(
      duplicated,
      "student",
      "course",
      c("views", "sessions")
    ),
    "subject-time"
  )
expect_error(
    step1_states(
      nonnumeric,
      "student",
      "course",
      c("views", "sessions")
    ),
    "numeric"
  )
expect_error(
    step1_states(data, "student", "course", "views"),
    "at least two"
  )
expect_error(
    step1_states(
      data,
      "student",
      "course",
      c("views", "sessions"),
      labels = c("Low", "High")
    ),
    "one unique"
  )

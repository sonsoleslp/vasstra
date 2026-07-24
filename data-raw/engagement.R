# Build the analysis-ready Chapter 11 engagement data.
#
# Source file SHA-256:
# 10118df59093e1f50f6856e23cee380cd64b1a1b702a28f394008f9292b6a77d

source_path <- file.path("tmp", "LongitudinalEngagement.csv")
if (!file.exists(source_path)) {
  stop(
    "Place LongitudinalEngagement.csv in ./tmp/ before rebuilding the data.",
    call. = FALSE
  )
}

source_data <- utils::read.csv(
  source_path,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

expected_names <- c(
  "UserID",
  "CourseID",
  "Sequence",
  "Freq_Course_View",
  "Freq_Forum_Consume",
  "Freq_Forum_Contribute",
  "Freq_Lecture_View",
  "Regularity_Course_View",
  "Regularity_Lecture_View",
  "Regularity_Forum_Consume",
  "Regularity_Forum_Contribute",
  "Session_Count",
  "Total_Duration",
  "Active_Days",
  "Final_Grade"
)
stopifnot(
  identical(names(source_data), expected_names),
  identical(dim(source_data), c(1136L, 15L)),
  !anyNA(source_data),
  anyDuplicated(source_data[c("UserID", "Sequence")]) == 0L
)

role_data <- data.frame(
  user_id = source_data$UserID,
  course_id = source_data$CourseID,
  sequence_position = source_data$Sequence,
  stringsAsFactors = FALSE
)
raw_indicators <- data.frame(
  course_view_count = source_data$Freq_Course_View,
  forum_consume_count = source_data$Freq_Forum_Consume,
  forum_contribute_count = source_data$Freq_Forum_Contribute,
  lecture_view_count = source_data$Freq_Lecture_View,
  course_view_regularity = source_data$Regularity_Course_View,
  session_count = source_data$Session_Count,
  total_duration = source_data$Total_Duration,
  active_days = source_data$Active_Days,
  stringsAsFactors = FALSE
)

standardize_within_course <- function(values, course_id) {
  stopifnot(is.numeric(values), length(values) == length(course_id))
  course_means <- stats::ave(values, course_id, FUN = mean)
  course_deviations <- stats::ave(values, course_id, FUN = stats::sd)
  (values - course_means) / course_deviations
}

standardized_indicators <- as.data.frame(
  lapply(
    raw_indicators,
    standardize_within_course,
    course_id = role_data$course_id
  ),
  stringsAsFactors = FALSE
)
names(standardized_indicators) <- paste0(
  names(raw_indicators),
  "_z"
)

engagement <- cbind(
  role_data,
  raw_indicators,
  standardized_indicators
)
standardized_names <- names(standardized_indicators)
attr(engagement, "vasstra") <- list(
  id = "user_id",
  time = "sequence_position",
  variables = standardized_names,
  standardize = "none",
  missing = "median",
  standardized_by = "course_id"
)

stopifnot(
  identical(dim(engagement), c(1136L, 19L)),
  sum(is.na(engagement[standardized_names])) == 24L,
  all(rowSums(is.na(engagement[standardized_names])) %in% c(0L, 8L)),
  !anyNA(engagement[setdiff(names(engagement), standardized_names)])
)

dir.create("data", showWarnings = FALSE)
save(
  engagement,
  file = file.path("data", "engagement.rda"),
  compress = "xz",
  version = 3
)

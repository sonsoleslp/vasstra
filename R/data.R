#' Longitudinal Student Engagement
#'
#' Analysis-ready data from the VaSSTra chapter. The object preserves the
#' chapter's eight raw state indicators and adds `_z` versions standardized
#' within `course_id`. It also carries VaSSTra role metadata, so `id`, `time`,
#' `variables`, standardization, and indicator missingness do not need to be
#' repeated in common package calls.
#'
#' @format A data frame with 1,136 observations and 19 variables:
#' \describe{
#'   \item{user_id}{Pseudonymous student identifier.}
#'   \item{course_id}{Pseudonymous course identifier used for
#'     standardization.}
#'   \item{sequence_position}{Course position from 1 through 8.}
#'   \item{course_view_count}{Raw course-view frequency.}
#'   \item{forum_consume_count}{Raw forum-consumption frequency.}
#'   \item{forum_contribute_count}{Raw forum-contribution frequency.}
#'   \item{lecture_view_count}{Raw lecture-view frequency.}
#'   \item{course_view_regularity}{Raw course-view regularity.}
#'   \item{session_count}{Raw session count.}
#'   \item{total_duration}{Raw total activity duration.}
#'   \item{active_days}{Raw number of active days.}
#'   \item{course_view_count_z,forum_consume_count_z,
#'     forum_contribute_count_z,lecture_view_count_z,
#'     course_view_regularity_z,session_count_z,total_duration_z,
#'     active_days_z}{The eight indicators standardized within
#'     `course_id` using the sample standard deviation. Three singleton
#'     courses create 24 intentional missing values; VaSSTra's attached
#'     metadata requests within-time median imputation.}
#' }
#'
#' @source
#' [Learning Analytics Methods, Chapter 11](https://lamethods.org/book1/chapters/ch11-vasstra/ch11-vasstra.html)
#' and the
#' [lamethods data repository](https://github.com/lamethods/data/tree/main/9_longitudinalEngagement).
#'
#' @examples
#' data(engagement)
#' str(engagement)
#' states <- step1_states(engagement, n_states = 3)
"engagement"

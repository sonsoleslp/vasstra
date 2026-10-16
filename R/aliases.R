#' Former Names of the Steps
#'
#' The four steps were called `step1_states()`, `step2_sequences()`,
#' `step3_trajectories()` and `step4_describe()` before VaSStra 0.4.0. The old
#' names call [states()], [sequences()], [trajectories()] and [describe()]
#' unchanged, so existing code keeps working; they will be removed in the
#' next release.
#'
#' @param ... Arguments passed to the renamed function.
#'
#' @return What the renamed function returns.
#'
#' @examples
#' set.seed(1)
#' data <- expand.grid(student = 1:12, course = 1:3)
#' data$views <- rep(c(2, 8, 16), length.out = nrow(data)) + rnorm(36, sd = 0.4)
#' data$duration <- data$views * 3 + rnorm(36, sd = 0.4)
#' step1_states(data, "student", "course", c("views", "duration"),
#'              n_states = 3, engine = "kmeans")
#' @name vasstra-former-names
NULL

#' @rdname vasstra-former-names
#' @export
step1_states <- function(...) states(...)

#' @rdname vasstra-former-names
#' @export
step2_sequences <- function(...) sequences(...)

#' @rdname vasstra-former-names
#' @export
step3_trajectories <- function(...) trajectories(...)

#' @rdname vasstra-former-names
#' @export
step4_describe <- function(...) describe(...)

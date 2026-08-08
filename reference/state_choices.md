# Compare Choices for the Number and Method of States

Fits a tidy grid of state solutions. No solution is silently selected.
Hard-clustering recommendations maximize silhouette within each method;
LPA recommendations use the explicit `lpa_criterion`, subject to the
requested size constraints. Use
[`fit_state_choice()`](https://pak.dynasite.org/VaSStra/reference/fit_state_choice.md)
with a `candidate_id` after inspecting the table.

## Usage

``` r
state_choices(
  data,
  id = NULL,
  time = NULL,
  variables = NULL,
  n_states = 2:6,
  method = c("lpa", "kmeans", "pam", "ward.D2"),
  lpa_model = "EEI",
  lpa_criterion = c("bic", "aic", "silhouette", "icl_native"),
  standardize = NULL,
  missing = NULL,
  time_levels = NULL,
  n_start = 25L,
  seed = 123L,
  minimum_size = 2L,
  minimum_proportion = 0,
  maximum_size_ratio = Inf
)
```

## Arguments

- data:

  A data frame with one row per subject and time point.

- id:

  Name of the subject identifier column. May be omitted when the data
  carry VaSStra role metadata.

- time:

  Name of the time or ordering column. May be omitted when the data
  carry VaSStra role metadata.

- variables:

  Character vector naming numeric state indicators. May be omitted when
  the data carry VaSStra role metadata.

- n_states:

  Whole-number candidate state counts.

- method:

  State-clustering methods to compare. See
  [`step1_states()`](https://pak.dynasite.org/VaSStra/reference/step1_states.md).

- lpa_model:

  Optional mclust covariance models evaluated when `method` includes
  `"lpa"`. `"EEI"` corresponds to tidyLPA model 1.

- lpa_criterion:

  Criterion used only to mark LPA recommendations: conventional `"bic"`
  (default) or `"aic"` (smaller is better), `"silhouette"` (larger is
  better), or native mclust `"icl_native"` (larger is better).

- standardize, missing, time_levels, n_start, seed:

  Passed to
  [`step1_states()`](https://pak.dynasite.org/VaSStra/reference/step1_states.md).

- minimum_size:

  Minimum acceptable number of observations in every state.

- minimum_proportion:

  Minimum acceptable proportion in every state.

- maximum_size_ratio:

  Maximum acceptable largest-to-smallest state-size ratio.

## Value

A `vasstra_state_choices` object.
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
one tidy row per candidate; `$recommendations` contains one transparent
recommendation per method and LPA model.

## Examples

``` r
data <- expand.grid(student = 1:12, course = 1:3)
group <- rep(rep(1:3, each = 4), times = 3)
data$views <- group * 5 + data$course * 0.01
data$duration <- group * 10 - data$course * 0.01
choices <- state_choices(
  data,
  id = "student",
  time = "course",
  variables = c("views", "duration"),
  n_states = 2:4,
  method = c("kmeans", "pam")
)
choices
#> VaSStra state choices
#>   6 candidates | 6 successful | 2 recommended
#>  candidate_id n_states method lpa_model recommendation_criterion silhouette bic
#>             2        3 kmeans      <NA>               silhouette          1  NA
#>             5        3    pam      <NA>               silhouette          1  NA
#>  min_size eligible
#>        12     TRUE
#>        12     TRUE
states <- fit_state_choice(
  choices,
  candidate_id = choices$recommendations$candidate_id[[1L]]
)
```

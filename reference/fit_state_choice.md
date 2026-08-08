# Fit One State Choice

Fits a candidate from
[`state_choices()`](https://pak.dynasite.org/VaSStra/reference/state_choices.md),
selected by `candidate_id`, by any combination of `n_states`, `method`,
and `lpa_model`, or — when nothing is specified — the recommended
candidate.

## Usage

``` r
fit_state_choice(
  choices,
  candidate_id = NULL,
  labels = NULL,
  state = "state",
  n_states = NULL,
  method = NULL,
  lpa_model = NULL
)
```

## Arguments

- choices:

  A `vasstra_state_choices` object from
  [`state_choices()`](https://pak.dynasite.org/VaSStra/reference/state_choices.md).

- candidate_id:

  Optional explicit candidate number to fit.

- labels:

  Optional state labels ordered from low to high profile.

- state:

  Name of the state column created in the returned data.

- n_states:

  Optional state count used to select the candidate.

- method:

  Optional clustering method used to select the candidate.

- lpa_model:

  Optional LPA covariance model used to select the candidate.

## Value

A `vasstra_states` object from
[`step1_states()`](https://pak.dynasite.org/VaSStra/reference/step1_states.md)
with the selected candidate and complete comparison table recorded in
`diagnostics`.

## Examples

``` r
data <- expand.grid(student = 1:12, course = 1:3)
group <- rep(rep(1:3, each = 4), times = 3)
data$views <- group * 5 + data$course * 0.01
data$duration <- group * 10 - data$course * 0.01
choices <- state_choices(
  data, "student", "course", c("views", "duration"),
  n_states = 2:3, method = "kmeans"
)
fit_state_choice(choices)             # the recommended candidate
#> Fitting recommended candidate 2.
#> VaSStra Step 1: Variables -> States
#>   36 rows | 12 subjects | 3 times | 3 states | kmeans
#>   Average silhouette: 1.000
#>   State sizes: State 1=12, State 2=12, State 3=12
fit_state_choice(choices, n_states = 3)
#> VaSStra Step 1: Variables -> States
#>   36 rows | 12 subjects | 3 times | 3 states | kmeans
#>   Average silhouette: 1.000
#>   State sizes: State 1=12, State 2=12, State 3=12
```

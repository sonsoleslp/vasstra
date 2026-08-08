# Step 4: Describe the Trajectories

Produces tidy per-subject sequence indices and trajectory-level state,
distribution, and transition summaries. Entropy is normalized Shannon
entropy. Complexity is the geometric mean of entropy and transition
rate, matching the common sequence complexity formulation. Integrative
potential and negative exposure are time-weighted state proportions.

## Usage

``` r
step4_describe(data, positive_states = NULL, negative_states = NULL, omega = 1)
```

## Arguments

- data:

  A `vasstra_trajectories` object.

- positive_states:

  Optional states considered positive.

- negative_states:

  Optional states considered negative.

- omega:

  Positive exponent controlling how strongly later time points are
  weighted for integrative potential and negative exposure.

## Value

A `vasstra_description` object with tidy `indices`,
`trajectory_summary`, `mean_time`, `distribution`, and `transitions`.

## Examples

``` r
long <- data.frame(
  id = rep(1:6, each = 3),
  time = rep(1:3, 6),
  state = c(
    "Low", "Low", "Low", "Low", "Low", "Average",
    "Average", "Average", "Average", "Average", "Average", "Low",
    "High", "High", "High", "High", "High", "Average"
  )
)
trajectories <- long |>
  step2_sequences(id = "id", time = "time", state = "state") |>
  step3_trajectories(n_trajectories = 3)
description <- step4_describe(
  trajectories,
  positive_states = "High",
  negative_states = "Low"
)
description
#> VaSStra Step 4: Describe Trajectories
#>   6 subjects | 3 trajectories | 3 states
```

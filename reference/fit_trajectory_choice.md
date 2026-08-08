# Fit One Trajectory Choice

Fits a candidate from
[`trajectory_choices()`](https://pak.dynasite.org/VaSStra/reference/trajectory_choices.md),
selected by `candidate_id`, by any combination of `n_trajectories`,
`dissimilarity`, and `method`, or — when nothing is specified — the
recommended candidate.

## Usage

``` r
fit_trajectory_choice(
  choices,
  candidate_id = NULL,
  labels = NULL,
  n_trajectories = NULL,
  dissimilarity = NULL,
  method = NULL
)
```

## Arguments

- choices:

  A `vasstra_trajectory_choices` object.

- candidate_id:

  Optional explicit candidate number to fit.

- labels:

  Optional trajectory labels.

- n_trajectories:

  Optional trajectory count used to select the candidate.

- dissimilarity:

  Optional sequence distance used to select the candidate.

- method:

  Optional clustering method used to select the candidate.

## Value

A `vasstra_trajectories` object with the selected candidate and complete
comparison table recorded in `diagnostics`.

## Examples

``` r
sequences <- step2_sequences(
  data.frame(
    id = rep(1:6, each = 3),
    time = rep(1:3, 6),
    state = rep(c("A", "A", "A", "B", "B", "B"), each = 3)
  ),
  "id", "time", "state"
)
choices <- trajectory_choices(
  sequences, n_trajectories = 2, dissimilarity = "hamming",
  method = "pam"
)
fit_trajectory_choice(choices)
#> Fitting recommended candidate 1.
#> VaSStra Step 3: Sequences -> Trajectories
#>   6 sequences | 2 trajectories | hamming + pam | silhouette 1.000
#>   Sizes: Trajectory 1=3, Trajectory 2=3
```

# Compare Choices for the Number and Method of Trajectories

Uses
[`Nestimate::cluster_choice()`](https://saqr.me/Nestimate/reference/cluster_choice.html)
to compare a tidy grid of sequence distances, clustering methods, and
trajectory counts. Recommendations are made within each distance-method
pair; no distance or algorithm is silently chosen for the user.

## Usage

``` r
trajectory_choices(
  data,
  n_trajectories = 2:6,
  dissimilarity = c("hamming", "lcs"),
  method = c("pam", "ward.D2"),
  seed = 123L,
  minimum_size = 2L,
  minimum_proportion = 0,
  maximum_size_ratio = Inf
)
```

## Arguments

- data:

  A `vasstra_sequences` object.

- n_trajectories:

  Whole-number candidate trajectory counts.

- dissimilarity:

  Sequence distances to compare.

- method:

  Clustering methods to compare.

- seed:

  Reproducible seed passed to Nestimate.

- minimum_size:

  Minimum acceptable number of sequences in every group.

- minimum_proportion:

  Minimum acceptable proportion in every group.

- maximum_size_ratio:

  Maximum acceptable largest-to-smallest group-size ratio.

## Value

A `vasstra_trajectory_choices` object with one tidy candidate row per
Nestimate fit and transparent recommendations within method-distance
combinations.

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
  sequences,
  n_trajectories = 2:3,
  dissimilarity = "hamming",
  method = c("pam", "ward.D2")
)
choices
#> VaSStra trajectory choices (Nestimate)
#>   4 candidates | 2 recommended distance-method solutions
#>  candidate_id n_trajectories dissimilarity  method silhouette min_size eligible
#>             1              2       hamming     pam          1        3     TRUE
#>             3              2       hamming ward.D2          1        3     TRUE
```

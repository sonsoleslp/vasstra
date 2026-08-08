# State Transition Network Centralities

Tidy centralities of the state transition network, one row per state.
The network is built by
[`Nestimate::build_tna()`](https://saqr.me/Nestimate/reference/build_tna.html)
and the measures come from
[`Nestimate::net_centrality()`](https://saqr.me/Nestimate/reference/net_centrality.html),
so they match `tna::centralities()`.

## Usage

``` r
transition_centrality(x, ...)

# S3 method for class 'vasstra_sequences'
transition_centrality(
  x,
  measures = c("InStrength", "OutStrength"),
  weights = c("probability", "count"),
  loops = FALSE,
  ...
)

# S3 method for class 'vasstra_trajectories'
transition_centrality(
  x,
  measures = c("InStrength", "OutStrength"),
  weights = c("probability", "count"),
  loops = FALSE,
  group = NULL,
  ...
)

# S3 method for class 'vasstra'
transition_centrality(x, ...)
```

## Arguments

- x:

  A `vasstra_sequences`, `vasstra_trajectories`, or `vasstra` object.

- ...:

  Not used.

- measures:

  Centrality measures to compute, passed to
  [`Nestimate::net_centrality()`](https://saqr.me/Nestimate/reference/net_centrality.html).
  Defaults to `c("InStrength", "OutStrength")`; `"all"` returns every
  built-in measure.

- weights:

  `"probability"` (default) uses row-normalized transition probabilities
  from
  [`Nestimate::build_tna()`](https://saqr.me/Nestimate/reference/build_tna.html);
  `"count"` uses raw transition counts from
  [`Nestimate::build_ftna()`](https://saqr.me/Nestimate/reference/build_ftna.html).

- loops:

  Include self-transitions in the computation. Default `FALSE`, matching
  [`Nestimate::net_centrality()`](https://saqr.me/Nestimate/reference/net_centrality.html).

- group:

  Optional trajectory label restricting the network to one trajectory's
  subjects.

## Value

A tidy data frame with one row per state and one column per requested
measure.

## Details

In-strength is the total incoming transition weight, so the state with
the largest in-strength is the one the cohort most often moves *into*.
Self-transitions are excluded by default: with `loops = TRUE` a
persistent state scores highly merely because its members stay put,
which is a different claim from attracting movement.

## See also

[`transition_plot()`](https://pak.dynasite.org/VaSStra/reference/transition_plot.md)
to draw the network.

## Examples

``` r
data("engagement", package = "VaSStra")
fit <- vasstra(engagement, n_states = 3, n_trajectories = 3)
transition_centrality(fit)
#>     state InStrength OutStrength
#> 1 State 1  0.1592262   0.3333333
#> 2 State 2  0.5726801   0.3020833
#> 3 State 3  0.1713675   0.2678571
transition_centrality(fit, weights = "count")
#>     state InStrength OutStrength
#> 1 State 1         75          78
#> 2 State 2        146         145
#> 3 State 3         77          75
```

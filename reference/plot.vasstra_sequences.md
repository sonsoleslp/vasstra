# Plot VaSSTra State Sequences with Nestimate

Delegates sequence index, distribution, and heatmap rendering to
[`Nestimate::sequence_plot()`](https://saqr.me/Nestimate/reference/sequence_plot.html).

## Usage

``` r
# S3 method for class 'vasstra_sequences'
plot(
  x,
  type = c("heatmap", "distribution", "index"),
  colors = NULL,
  main = NULL,
  sort = "lcs",
  na = x$diagnostics$n_missing > 0L,
  ...
)
```

## Arguments

- x:

  A `vasstra_sequences` object.

- type:

  One of `"heatmap"` (default), `"distribution"`, or `"index"`. The
  heatmap shows every aligned sequence at full resolution and is the
  most informative single view of the complete cohort.

- colors:

  Optional colors, one per state, passed to Nestimate as `state_colors`.

- main:

  Plot title. A type-specific title is used by default.

- sort:

  Sequence ordering passed to
  [`Nestimate::sequence_plot()`](https://saqr.me/Nestimate/reference/sequence_plot.html).

- na:

  Show missing cells as a separate distribution band. By default, this
  is `TRUE` only when the sequence data contain `NA`.

- ...:

  Additional arguments passed to
  [`Nestimate::sequence_plot()`](https://saqr.me/Nestimate/reference/sequence_plot.html).

## Value

The value returned by
[`Nestimate::sequence_plot()`](https://saqr.me/Nestimate/reference/sequence_plot.html).

## See also

[`flow_plot()`](https://sonsoles.me/vasstra/reference/flow_plot.md) for
alluvial and individual flow views.

## Examples

``` r
data("engagement", package = "VaSSTra")
fit <- vasstra(engagement, n_states = 3, n_trajectories = 3)
plot(fit$sequences)

plot(fit$sequences, type = "distribution")
```

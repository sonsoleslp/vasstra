# Plot VaSStra Trajectories with Nestimate

Delegates grouped sequence index, distribution, and heatmap rendering to
[`Nestimate::sequence_plot()`](https://saqr.me/Nestimate/reference/sequence_plot.html).

## Usage

``` r
# S3 method for class 'vasstra_trajectories'
plot(
  x,
  type = c("index", "distribution", "heatmap"),
  colors = NULL,
  main = "Sequences grouped by trajectory",
  sort = "start",
  na = x$source$diagnostics$n_missing > 0L,
  ...
)
```

## Arguments

- x:

  A `vasstra_trajectories` object.

- type:

  One of `"index"` (default), `"distribution"`, or `"heatmap"`.

- colors:

  Optional colors, one per state, passed to Nestimate as `state_colors`.

- main:

  Plot title.

- sort:

  Sequence ordering passed to
  [`Nestimate::sequence_plot()`](https://saqr.me/Nestimate/reference/sequence_plot.html).
  The default is `"start"` for robust within-trajectory index plots,
  including trajectories containing one sequence.

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

[`flow_plot()`](https://pak.dynasite.org/VaSStra/reference/flow_plot.md)
for alluvial and individual flow views.

## Examples

``` r
data("engagement", package = "VaSStra")
fit <- vasstra(engagement, n_states = 3, n_trajectories = 3)
plot(fit$trajectories)

plot(fit$trajectories, type = "distribution")
```

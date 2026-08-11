# Plot a Complete VaSSTra Analysis

Plot a Complete VaSSTra Analysis

## Usage

``` r
# S3 method for class 'vasstra'
plot(x, which = c("trajectories", "states", "sequences"), ...)
```

## Arguments

- x:

  A `vasstra` object.

- which:

  One of `"trajectories"`, `"states"`, or `"sequences"`.

- ...:

  Passed to the selected step's plot method. Sequence and trajectory
  plots are rendered by Nestimate. For the trajectory step, `type`
  accepts a vector of views (e.g.
  `type = c("transition", "index", "distribution")`) to draw a grid with
  one row per trajectory; see
  [`plot.vasstra_trajectories()`](https://sonsoles.me/vasstra/reference/plot.vasstra_trajectories.md).

## Value

The selected plot method's invisible result.

## Examples

``` r
data("engagement", package = "VaSSTra")
fit <- vasstra(engagement, n_states = 3, n_trajectories = 3)
plot(fit)

plot(fit, which = "states", type = "profile")

plot(fit, which = "sequences")

# \donttest{
if (requireNamespace("cograph", quietly = TRUE)) {
  plot(fit, type = c("transition", "index", "distribution"))
}

# }
```

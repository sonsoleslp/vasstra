# Plot a Complete VaSStra Analysis

Plot a Complete VaSStra Analysis

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
  plots are rendered by Nestimate.

## Value

The selected plot method's invisible result.

## Examples

``` r
data("engagement", package = "VaSStra")
fit <- vasstra(engagement, n_states = 3, n_trajectories = 3)
plot(fit)

plot(fit, which = "states", type = "profile")

plot(fit, which = "sequences")
```

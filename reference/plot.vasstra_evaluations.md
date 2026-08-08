# Plot the Evaluations of a Complete Fit

Draws one row of evaluation panels per clustering step: the state
clustering on top and the trajectory clustering below.

## Usage

``` r
# S3 method for class 'vasstra_evaluations'
plot(x, colors = NULL, ...)
```

## Arguments

- x:

  A `vasstra_evaluations` object from
  [`evaluate()`](https://pak.dynasite.org/VaSStra/reference/evaluate.md)
  on a complete `vasstra` fit.

- colors:

  Optional colors recycled within each evaluation row.

- ...:

  Additional graphical arguments passed to the panel functions.

## Value

The evaluated candidate and cluster tables, invisibly.

## Examples

``` r
data("engagement", package = "VaSStra")
fit <- vasstra(engagement, n_states = 3, n_trajectories = 3)
plot(evaluate(fit))
```

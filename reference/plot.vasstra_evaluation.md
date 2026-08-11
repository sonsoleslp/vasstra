# Plot a Clustering Evaluation

Draws the selection curve, per-cluster silhouette widths, and cluster
sizes for one
[`evaluate()`](https://sonsoles.me/vasstra/reference/evaluate.md)
result. The default `"summary"` layout places the three panels side by
side.

## Usage

``` r
# S3 method for class 'vasstra_evaluation'
plot(
  x,
  type = c("summary", "curve", "silhouette", "sizes"),
  colors = NULL,
  main = NULL,
  ...
)
```

## Arguments

- x:

  A `vasstra_evaluation` object from
  [`evaluate()`](https://sonsoles.me/vasstra/reference/evaluate.md).

- type:

  One of `"summary"` (default), `"curve"`, `"silhouette"`, or `"sizes"`.

- colors:

  Optional colors, one per fitted cluster. The selection curve uses the
  first color.

- main:

  Optional plot title. A panel-specific title is used by default.

- ...:

  Additional graphical arguments passed to the panel functions.

## Value

The tidy data drawn by the requested panel, invisibly. The `"summary"`
layout returns both the candidate and cluster tables.

## Examples

``` r
data("engagement", package = "VaSSTra")
fit <- vasstra(engagement, n_states = 3, n_trajectories = 3)
evaluation <- evaluate(fit$trajectories)
plot(evaluation)

plot(evaluation, type = "silhouette")
```

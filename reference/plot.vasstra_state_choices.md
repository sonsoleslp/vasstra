# Plot State-Clustering Choices

Draws a compact model-selection plot from
[`state_choices()`](https://pak.dynasite.org/VaSStra/reference/state_choices.md).
Each line is one clustering method (and LPA covariance model). A ring
marks the optimal eligible value of the plotted metric within each
group; hollow points mark candidates that fail the requested size
constraints.

## Usage

``` r
# S3 method for class 'vasstra_state_choices'
plot(
  x,
  metric = c("silhouette", "bic", "aic", "classification_entropy", "mean_uncertainty",
    "min_size", "size_ratio", "bic_native", "icl_native"),
  colors = NULL,
  main = NULL,
  groups = NULL,
  show_legend = TRUE,
  ...
)
```

## Arguments

- x:

  A `vasstra_state_choices` object.

- metric:

  Candidate metric to plot.

- colors:

  Optional colors, one per displayed method/model.

- main:

  Optional plot title.

- groups:

  Optional character vector of method/model labels to display, such as
  `"kmeans"` or `"lpa/EEI"`.

- show_legend:

  Show the group and marker legends.

- ...:

  Additional arguments passed to
  [`graphics::plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

The tidy candidate data used in the plot, invisibly.

## Examples

``` r
data("engagement", package = "VaSStra")
options <- state_choices(engagement, n_states = 2:4, method = "kmeans")
plot(options)

plot(options, metric = "silhouette")
```

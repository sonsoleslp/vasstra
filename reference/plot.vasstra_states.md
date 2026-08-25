# Plot Estimated VaSSTra States

Plot Estimated VaSSTra States

## Usage

``` r
# S3 method for class 'vasstra_states'
plot(
  x,
  colors = NULL,
  main = NULL,
  ...,
  type = c("profile", "heatmap", "bars", "sizes", "variables", "all"),
  scale = c("standardized", "original"),
  by_time = TRUE
)
```

## Arguments

- x:

  A `vasstra_states` object.

- colors:

  Optional colors, one per state. Used by profile, bar, and size plots;
  heatmaps use a value-based sequential or diverging palette.

- main:

  Optional plot title. A type-specific title is used by default.

- ...:

  Additional graphical arguments passed to
  [`graphics::matplot()`](https://rdrr.io/r/graphics/matplot.html),
  [`graphics::image()`](https://rdrr.io/r/graphics/image.html),
  [`graphics::barplot()`](https://rdrr.io/r/graphics/barplot.html), or
  [`graphics::boxplot()`](https://rdrr.io/r/graphics/boxplot.html).

- type:

  One of `"profile"` (default), `"heatmap"`, `"bars"`, `"sizes"`,
  `"variables"`, or `"all"` for a two-by-two overview of the first four.
  `"variables"` draws grouped boxplots of the clustering indicators, one
  panel per time point and one box per state within each indicator, so
  it shows whether each indicator keeps separating the states at every
  point rather than only on average.

- scale:

  Show state profiles on the `"standardized"` (default) or `"original"`
  indicator scale. Ignored by `type = "sizes"` and `type = "variables"`
  (the latter always uses the scale the states were clustered on).

- by_time:

  Used only by `type = "variables"`: draw one panel per time point
  (`TRUE`, default) or pool the time points into a single panel
  (`FALSE`).

## Value

For profile, bar, heatmap, and overview plots, the plotted
indicator-by-state matrix, invisibly. For a size plot, a tidy state-size
data frame. For `type = "variables"`, a tidy time-by-variable-by-state
table of medians, invisibly.

## Examples

``` r
data("engagement", package = "VaSSTra")
states <- step1_states(engagement, n_states = 3)
plot(states)

plot(states, type = "sizes")

plot(states, type = "variables")

plot(states, type = "variables", by_time = FALSE)

plot(states, type = "all")
```

# Plot Estimated VaSStra States

Plot Estimated VaSStra States

## Usage

``` r
# S3 method for class 'vasstra_states'
plot(
  x,
  colors = NULL,
  main = NULL,
  ...,
  type = c("profile", "heatmap", "bars", "sizes", "all"),
  scale = c("standardized", "original")
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
  [`graphics::image()`](https://rdrr.io/r/graphics/image.html), or
  [`graphics::barplot()`](https://rdrr.io/r/graphics/barplot.html).

- type:

  One of `"profile"` (default), `"heatmap"`, `"bars"`, `"sizes"`, or
  `"all"` for a two-by-two overview of the four types.

- scale:

  Show state profiles on the `"standardized"` (default) or `"original"`
  indicator scale. Ignored by `type = "sizes"`.

## Value

For profile, bar, heatmap, and overview plots, the plotted
indicator-by-state matrix, invisibly. For a size plot, a tidy state-size
data frame.

## Examples

``` r
data("engagement", package = "VaSStra")
states <- step1_states(engagement, n_states = 3)
plot(states)

plot(states, type = "sizes")

plot(states, type = "all")
```

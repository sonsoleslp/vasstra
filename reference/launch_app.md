# Launch the Interactive VaSStra App

Opens a Shiny application that runs the complete VaSStra workflow
interactively: load data, map the subject, time, and indicator roles,
fit states and trajectories with automated or explicit counts, inspect
evaluation panels and tidy fit indices, rename groups, and export every
tidy table. The app requires the suggested `shiny` and `DT` packages.

## Usage

``` r
launch_app(...)
```

## Arguments

- ...:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

Called for its side effect of running the app.

## Examples

``` r
if (interactive()) {
  launch_app()
}
```

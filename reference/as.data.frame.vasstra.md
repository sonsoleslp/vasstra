# Convert a VaSSTra Analysis to a Tidy Table

Convert a VaSSTra Analysis to a Tidy Table

## Usage

``` r
# S3 method for class 'vasstra'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  unit = c("subject", "observation", "state_profile", "trajectory")
)
```

## Arguments

- x:

  A `vasstra` analysis.

- row.names, optional:

  Passed to
  [`base::as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- ...:

  Additional arguments passed to
  [`base::as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- unit:

  Analysis unit to return: one row per `"subject"` (default),
  `"observation"`, `"state_profile"`, or `"trajectory"`.

## Value

A data frame at the requested analysis unit.

## Examples

``` r
data("engagement", package = "VaSSTra")
fit <- vasstra(engagement, n_states = 3, n_trajectories = 3)
head(as.data.frame(fit))
#>    user_id   trajectory n_observed unique_states transitions   entropy
#> 1 D2C5F64E Trajectory 3          8             3           3 0.9850568
#> 2 D7E3D0DC Trajectory 3          8             3           4 0.8194484
#> 3 2926CF64 Trajectory 1          8             1           0 0.0000000
#> 4 985AFF35 Trajectory 1          8             2           2 0.6021808
#> 5 F89100C7 Trajectory 3          8             3           4 0.8868595
#> 6 AB944042 Trajectory 1          8             2           2 0.3429510
#>   complexity volatility integrative_potential negative_exposure
#> 1  0.6497440  0.7142857                    NA                NA
#> 2  0.6842925  0.7857143                    NA                NA
#> 3  0.0000000  0.1666667                    NA                NA
#> 4  0.4147911  0.4761905                    NA                NA
#> 5  0.7118826  0.7857143                    NA                NA
#> 6  0.3130271  0.4761905                    NA                NA
as.data.frame(fit, unit = "trajectory")
#>     trajectory  n proportion mean_within_distance n_observed unique_states
#> 1 Trajectory 1 32  0.2253521             3.002016          8      1.843750
#> 2 Trajectory 2 68  0.4788732             3.115013          8      1.985294
#> 3 Trajectory 3 42  0.2957746             2.874564          8      1.880952
#>   transitions   entropy complexity volatility integrative_potential
#> 1    2.062500 0.4260927  0.3491121  0.4546131                   NaN
#> 2    2.220588 0.4625493  0.3735486  0.4894958                   NaN
#> 3    1.928571 0.4240368  0.3348902  0.4512472                   NaN
#>   negative_exposure
#> 1               NaN
#> 2               NaN
#> 3               NaN
```

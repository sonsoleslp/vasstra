# State Transition Table

The transition counts behind the flow and network views, as a tidy data
frame. By default every consecutive move is aggregated over time (the
numbers
[`transition_plot()`](https://sonsoles.me/vasstra/reference/transition_plot.md)
draws); `by_time = TRUE` instead returns one block per consecutive time
pair, matching the alluvial
[`flow_plot()`](https://sonsoles.me/vasstra/reference/flow_plot.md).

## Usage

``` r
transition_table(x, ...)

# S3 method for class 'vasstra_sequences'
transition_table(x, by_time = FALSE, ...)

# S3 method for class 'vasstra_trajectories'
transition_table(x, by_time = FALSE, group = NULL, ...)

# S3 method for class 'vasstra'
transition_table(x, ...)
```

## Arguments

- x:

  A `vasstra_sequences`, `vasstra_trajectories`, or `vasstra` object.

- ...:

  Not used.

- by_time:

  Return per-time-step transitions (`TRUE`) or the time-aggregated table
  (`FALSE`, default).

- group:

  Optional trajectory label restricting the table to one trajectory's
  subjects.

## Value

A tidy data frame. Aggregated: `from`, `to`, `count`, and `probability`
(row-normalized within `from`). With `by_time = TRUE`, also `from_time`
and `to_time`, with `probability` normalized within each
`(from_time, from)`.

## See also

[`transition_plot()`](https://sonsoles.me/vasstra/reference/transition_plot.md)
and [`flow_plot()`](https://sonsoles.me/vasstra/reference/flow_plot.md)
to draw these movements,
[`transition_centrality()`](https://sonsoles.me/vasstra/reference/transition_centrality.md)
for network centralities.

## Examples

``` r
data("engagement", package = "VaSSTra")
fit <- vasstra(engagement, n_states = 3, n_trajectories = 3)
transition_table(fit)
#>      from      to count probability
#> 1 State 1 State 1   156 0.666666667
#> 2 State 2 State 1    73 0.152083333
#> 3 State 3 State 1     2 0.007142857
#> 4 State 1 State 2    73 0.311965812
#> 5 State 2 State 2   335 0.697916667
#> 6 State 3 State 2    73 0.260714286
#> 7 State 1 State 3     5 0.021367521
#> 8 State 2 State 3    72 0.150000000
#> 9 State 3 State 3   205 0.732142857
transition_table(fit, by_time = TRUE)
#>    from_time to_time    from      to count probability
#> 1          1       2 State 1 State 1    19  0.54285714
#> 2          1       2 State 2 State 1    16  0.22857143
#> 3          1       2 State 3 State 1     0  0.00000000
#> 4          1       2 State 1 State 2    14  0.40000000
#> 5          1       2 State 2 State 2    41  0.58571429
#> 6          1       2 State 3 State 2     7  0.18918919
#> 7          1       2 State 1 State 3     2  0.05714286
#> 8          1       2 State 2 State 3    13  0.18571429
#> 9          1       2 State 3 State 3    30  0.81081081
#> 10         2       3 State 1 State 1    24  0.68571429
#> 11         2       3 State 2 State 1    11  0.17741935
#> 12         2       3 State 3 State 1     1  0.02222222
#> 13         2       3 State 1 State 2    11  0.31428571
#> 14         2       3 State 2 State 2    42  0.67741935
#> 15         2       3 State 3 State 2    14  0.31111111
#> 16         2       3 State 1 State 3     0  0.00000000
#> 17         2       3 State 2 State 3     9  0.14516129
#> 18         2       3 State 3 State 3    30  0.66666667
#> 19         3       4 State 1 State 1    22  0.61111111
#> 20         3       4 State 2 State 1     5  0.07462687
#> 21         3       4 State 3 State 1     0  0.00000000
#> 22         3       4 State 1 State 2    12  0.33333333
#> 23         3       4 State 2 State 2    53  0.79104478
#> 24         3       4 State 3 State 2    13  0.33333333
#> 25         3       4 State 1 State 3     2  0.05555556
#> 26         3       4 State 2 State 3     9  0.13432836
#> 27         3       4 State 3 State 3    26  0.66666667
#> 28         4       5 State 1 State 1    19  0.70370370
#> 29         4       5 State 2 State 1    12  0.15384615
#> 30         4       5 State 3 State 1     0  0.00000000
#> 31         4       5 State 1 State 2     8  0.29629630
#> 32         4       5 State 2 State 2    51  0.65384615
#> 33         4       5 State 3 State 2    11  0.29729730
#> 34         4       5 State 1 State 3     0  0.00000000
#> 35         4       5 State 2 State 3    15  0.19230769
#> 36         4       5 State 3 State 3    26  0.70270270
#> 37         5       6 State 1 State 1    22  0.70967742
#> 38         5       6 State 2 State 1    13  0.18571429
#> 39         5       6 State 3 State 1     0  0.00000000
#> 40         5       6 State 1 State 2     9  0.29032258
#> 41         5       6 State 2 State 2    48  0.68571429
#> 42         5       6 State 3 State 2    12  0.29268293
#> 43         5       6 State 1 State 3     0  0.00000000
#> 44         5       6 State 2 State 3     9  0.12857143
#> 45         5       6 State 3 State 3    29  0.70731707
#> 46         6       7 State 1 State 1    23  0.65714286
#> 47         6       7 State 2 State 1    11  0.15942029
#> 48         6       7 State 3 State 1     1  0.02631579
#> 49         6       7 State 1 State 2    12  0.34285714
#> 50         6       7 State 2 State 2    47  0.68115942
#> 51         6       7 State 3 State 2     5  0.13157895
#> 52         6       7 State 1 State 3     0  0.00000000
#> 53         6       7 State 2 State 3    11  0.15942029
#> 54         6       7 State 3 State 3    32  0.84210526
#> 55         7       8 State 1 State 1    27  0.77142857
#> 56         7       8 State 2 State 1     5  0.07812500
#> 57         7       8 State 3 State 1     0  0.00000000
#> 58         7       8 State 1 State 2     7  0.20000000
#> 59         7       8 State 2 State 2    53  0.82812500
#> 60         7       8 State 3 State 2    11  0.25581395
#> 61         7       8 State 1 State 3     1  0.02857143
#> 62         7       8 State 2 State 3     6  0.09375000
#> 63         7       8 State 3 State 3    32  0.74418605
```

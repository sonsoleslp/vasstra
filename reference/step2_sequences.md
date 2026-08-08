# Step 2: Turn States into Sequences

Aligns every subject on the same observed time axis and returns wide
sequences plus tidy long data, state distributions, and transitions.

## Usage

``` r
step2_sequences(
  data,
  id = NULL,
  time = NULL,
  state = NULL,
  time_levels = NULL,
  missing = c("error", "explicit", "keep"),
  missing_label = "Missing"
)
```

## Arguments

- data:

  A `vasstra_states` object or a data frame containing states.

- id:

  Subject identifier column. Inferred from `vasstra_states`, attached
  role metadata, or a common identifier name.

- time:

  Time or ordering column. Inferred from `vasstra_states`, attached role
  metadata, or a common time name.

- state:

  State column. Inferred from `vasstra_states`, or detected as the
  single categorical non-role column of a plain data frame.

- time_levels:

  Explicit chronological values. Required for character or
  unordered-factor time when `data` is a plain data frame.

- missing:

  Structural-gap policy: `"error"` (default), `"explicit"` to create a
  state for missing subject-time cells, or `"keep"` to preserve `NA`
  cells for advanced use.

- missing_label:

  Label used when `missing = "explicit"`.

## Value

A `vasstra_sequences` object with `data` (wide state sequences),
`long_data`, `meta_data`, `distribution`, and `transitions`.

## Examples

``` r
states <- data.frame(
  student = rep(1:3, each = 3),
  course = rep(1:3, 3),
  engagement = c("Low", "Average", "High",
                 "Average", "Average", "High",
                 "Low", "Low", "Average")
)
sequences <- step2_sequences(
  states,
  id = "student",
  time = "course",
  state = "engagement"
)
sequences
#> VaSStra Step 2: States -> Sequences
#>   3 subjects | 3 times | 3 states | 6 transitions
```

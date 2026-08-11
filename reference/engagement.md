# Longitudinal Student Engagement

Analysis-ready data from the VaSSTra chapter. The object preserves the
chapter's eight raw state indicators and adds `_z` versions standardized
within `course_id`. It also carries VaSSTra role metadata, so `id`,
`time`, `variables`, standardization, and indicator missingness do not
need to be repeated in common package calls.

## Usage

``` r
engagement
```

## Format

A data frame with 1,136 observations and 19 variables:

- user_id:

  Pseudonymous student identifier.

- course_id:

  Pseudonymous course identifier used for standardization.

- sequence_position:

  Course position from 1 through 8.

- course_view_count:

  Raw course-view frequency.

- forum_consume_count:

  Raw forum-consumption frequency.

- forum_contribute_count:

  Raw forum-contribution frequency.

- lecture_view_count:

  Raw lecture-view frequency.

- course_view_regularity:

  Raw course-view regularity.

- session_count:

  Raw session count.

- total_duration:

  Raw total activity duration.

- active_days:

  Raw number of active days.

- course_view_count_z,forum_consume_count_z,
  forum_contribute_count_z,lecture_view_count_z,
  course_view_regularity_z,session_count_z,total_duration_z,
  active_days_z:

  The eight indicators standardized within `course_id` using the sample
  standard deviation. Three singleton courses create 24 intentional
  missing values; VaSSTra's attached metadata requests within-time
  median imputation.

## Source

[Learning Analytics Methods, Chapter
11](https://lamethods.org/book1/chapters/ch11-vasstra/ch11-vasstra.html)
and the [lamethods data
repository](https://github.com/lamethods/data/tree/main/9_longitudinalEngagement).

## Examples

``` r
data(engagement)
str(engagement)
#> 'data.frame':    1136 obs. of  19 variables:
#>  $ user_id                 : chr  "D2C5F64E" "D2C5F64E" "D2C5F64E" "D2C5F64E" ...
#>  $ course_id               : chr  "C6107FC4" "4C3F37F0" "E54A52A3" "AB7EC624" ...
#>  $ sequence_position       : int  1 2 3 4 5 6 7 8 1 2 ...
#>  $ course_view_count       : int  150 98 254 332 386 261 250 287 186 241 ...
#>  $ forum_consume_count     : int  251 84 354 825 960 1026 652 697 597 580 ...
#>  $ forum_contribute_count  : int  79 17 34 185 233 208 171 161 171 188 ...
#>  $ lecture_view_count      : int  132 108 284 256 356 154 239 230 202 261 ...
#>  $ course_view_regularity  : num  0.42 0.26 0.29 0.63 0.74 0.37 0.44 0.55 0.3 0.45 ...
#>  $ session_count           : int  121 53 159 287 321 212 191 206 174 182 ...
#>  $ total_duration          : int  53330 14465 64324 122821 148792 121008 85067 92707 79427 81394 ...
#>  $ active_days             : int  12 5 12 19 23 15 16 24 13 14 ...
#>  $ course_view_count_z     : num  -1.135 -2.027 0.519 1.875 2.776 ...
#>  $ forum_consume_count_z   : num  -1.87 -2.69 -1.24 1.33 2.11 ...
#>  $ forum_contribute_count_z: num  -1.8356 -2.7901 -2.3865 -0.0489 1.109 ...
#>  $ lecture_view_count_z    : num  -1.075 -1.507 1.352 0.989 2.967 ...
#>  $ course_view_regularity_z: num  -0.791 -1.542 -1.244 1.582 1.53 ...
#>  $ session_count_z         : num  -1.304 -2.258 -0.474 1.892 2.513 ...
#>  $ total_duration_z        : num  -1.221 -2.614 -0.711 1.711 2.868 ...
#>  $ active_days_z           : num  -1.09 -2.421 -0.819 0.779 1.492 ...
#>  - attr(*, "vasstra")=List of 6
#>   ..$ id             : chr "user_id"
#>   ..$ time           : chr "sequence_position"
#>   ..$ variables      : chr [1:8] "course_view_count_z" "forum_consume_count_z" "forum_contribute_count_z" "lecture_view_count_z" ...
#>   ..$ standardize    : chr "none"
#>   ..$ missing        : chr "median"
#>   ..$ standardized_by: chr "course_id"
states <- step1_states(engagement, n_states = 3)
```

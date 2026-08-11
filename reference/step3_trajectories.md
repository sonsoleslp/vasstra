# Step 3: Turn Sequences into Trajectories

Clusters aligned state sequences with Nestimate sequence distances and
clustering methods. The default Hamming plus PAM combination follows the
Nestimate and Carm sequence-clustering interface. Set
`dissimilarity = "lcs"` and `method = "ward.D2"` to follow the worked
VaSSTra chapter.

## Usage

``` r
step3_trajectories(
  data,
  n_trajectories = "auto",
  dissimilarity = c("hamming", "osa", "lv", "dl", "lcs", "qgram", "cosine", "jaccard",
    "jw"),
  method = c("pam", "ward.D2", "ward.D", "complete", "average", "single", "mcquitty",
    "median", "centroid"),
  backend = c("Nestimate", "base"),
  labels = NULL,
  seed = 123L
)
```

## Arguments

- data:

  A `vasstra_sequences` object.

- n_trajectories:

  Number of trajectory groups. One number fits exactly that count,
  several numbers (for example `2:4`) compare those candidates with
  [`trajectory_choices()`](https://sonsoles.me/vasstra/reference/trajectory_choices.md)
  and fit the recommended count, and `"auto"` (default) compares 2
  through 6 — or simply matches `labels` when labels are supplied.
  Automatic comparison never selects a solution whose smallest group
  holds under 5 percent of the sequences. Compared candidates are kept
  in `diagnostics$selection` and every automated choice is reported with
  a message.

- dissimilarity:

  One of `"hamming"`, `"osa"`, `"lv"`, `"dl"`, `"lcs"`, `"qgram"`,
  `"cosine"`, `"jaccard"`, or `"jw"`.

- method:

  One of `"pam"`, `"ward.D2"`, `"ward.D"`, `"complete"`, `"average"`,
  `"single"`, `"mcquitty"`, `"median"`, or `"centroid"`.

- backend:

  `"Nestimate"` (default) uses
  [`Nestimate::build_clusters()`](https://saqr.me/Nestimate/reference/build_clusters.html).
  Use `"base"` for the small internal equivalence backend, which
  supports only Hamming/LCS distances.

- labels:

  Optional unique trajectory labels.

- seed:

  Reproducible random seed passed to the clustering backend.

## Value

A `vasstra_trajectories` object with tidy `membership`, cluster sizes,
silhouette, and the complete distance matrix.

## Examples

``` r
sequences <- step2_sequences(
  data.frame(
    id = rep(1:6, each = 3),
    time = rep(1:3, 6),
    state = c(
      "A", "A", "A", "A", "A", "B",
      "B", "B", "B", "B", "B", "A",
      "C", "C", "C", "C", "C", "B"
    )
  ),
  id = "id",
  time = "time",
  state = "state"
)
trajectories <- step3_trajectories(sequences, n_trajectories = 3)
trajectories
#> VaSSTra Step 3: Sequences -> Trajectories
#>   6 sequences | 3 trajectories | hamming + pam | silhouette 0.611
#>   Sizes: Trajectory 1=2, Trajectory 2=2, Trajectory 3=2
```

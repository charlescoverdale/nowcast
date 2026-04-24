# Summarise Ragged-Edge Data Availability

Shows which series have data through which date, highlighting the ragged
edge where indicators have different publication lags.

## Usage

``` r
nc_ragged_edge(data)
```

## Arguments

- data:

  An `nc_dataset` object, or a data frame with a `date` column.

## Value

A data frame with columns `series`, `first_date`, `last_date`, `n_obs`,
and `n_missing`.

## Examples

``` r
target <- data.frame(
  date = as.Date(c("2020-01-01", "2020-04-01", "2020-07-01")),
  value = c(0.5, -0.3, 0.8)
)
ind <- data.frame(
  date = seq(as.Date("2020-01-01"), as.Date("2020-09-01"), by = "month"),
  value = rnorm(9)
)
ds <- nc_align(target, indicator = ind)
nc_ragged_edge(ds)
#>      series first_date  last_date n_obs n_missing
#> 1    target 2020-01-01 2020-07-01     3         0
#> 2 indicator 2020-01-01 2020-07-01     3         0
```

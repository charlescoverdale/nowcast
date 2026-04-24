# Stationarity Transformations

Apply common transformations to make a time series stationary. Supports
first differencing, log differencing (growth rates), and
standardisation.

## Usage

``` r
nc_transform(data, method = c("diff", "log_diff", "standardize"))
```

## Arguments

- data:

  A data frame with columns `date` and `value`, or a numeric vector.

- method:

  Character. One of `"diff"` (first difference), `"log_diff"` (log first
  difference, i.e. percentage growth rate), or `"standardize"` (zero
  mean, unit variance).

## Value

A data frame with columns `date` and `value` (if input was a data
frame), or a numeric vector (if input was numeric). The output is
shorter by one observation for `"diff"` and `"log_diff"`.

## Examples

``` r
df <- data.frame(
  date = seq(as.Date("2020-01-01"), as.Date("2020-06-01"), by = "month"),
  value = c(100, 102, 101, 105, 103, 108)
)
nc_transform(df, method = "diff")
#>         date value
#> 1 2020-02-01     2
#> 2 2020-03-01    -1
#> 3 2020-04-01     4
#> 4 2020-05-01    -2
#> 5 2020-06-01     5
nc_transform(df, method = "log_diff")
#>         date      value
#> 1 2020-02-01  1.9802627
#> 2 2020-03-01 -0.9852296
#> 3 2020-04-01  3.8839833
#> 4 2020-05-01 -1.9231362
#> 5 2020-06-01  4.7402239
nc_transform(df, method = "standardize")
#>         date       value
#> 1 2020-01-01 -1.08192316
#> 2 2020-02-01 -0.39860327
#> 3 2020-03-01 -0.74026321
#> 4 2020-04-01  0.62637656
#> 5 2020-05-01 -0.05694332
#> 6 2020-06-01  1.65135640
```

# Temporal Aggregation

Aggregate a time series from a higher frequency to a lower frequency.
For example, convert monthly data to quarterly by taking the mean, sum,
or last observation.

## Usage

``` r
nc_aggregate(data, to = c("quarterly", "annual"), fun = mean)
```

## Arguments

- data:

  A data frame with columns `date` (Date) and `value` (numeric).

- to:

  Character. Target frequency: `"quarterly"` or `"annual"`.

- fun:

  Function used for aggregation (default
  [mean](https://rdrr.io/r/base/mean.html)).

## Value

A data frame with columns `date` and `value` at the target frequency.

## Details

The default aggregation function is
[mean](https://rdrr.io/r/base/mean.html), which is appropriate for
**flow variables** measured as rates or indices (e.g. GDP growth, CPI).
For flow variables measured in levels (e.g. total retail sales), use
`fun = sum`. For **stock variables** (e.g. interest rates, exchange
rates), use `fun = function(x, ...) tail(x, 1)` to take the
end-of-period value.

## Examples

``` r
monthly <- data.frame(
  date = seq(as.Date("2020-01-01"), as.Date("2020-12-01"), by = "month"),
  value = rnorm(12)
)
nc_aggregate(monthly, to = "quarterly")
#>         date       value
#> 1 2020-01-01 -1.19399669
#> 2 2020-04-01  0.58813101
#> 3 2020-07-01 -0.77111419
#> 4 2020-10-01 -0.06914093
nc_aggregate(monthly, to = "annual", fun = sum)
#>         date     value
#> 1 2020-01-01 -4.338362
```

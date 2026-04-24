# Compute a Nowcast by Method Name

A generic dispatcher that calls the appropriate `nc_*` function based on
a string method name. Useful for programmatic workflows.

## Usage

``` r
nc_compute(data, method = "bridge", ...)
```

## Arguments

- data:

  A data frame or `nc_dataset`.

- method:

  Character. Name of the method: `"bridge"`.

- ...:

  Additional arguments passed to the underlying function.

## Value

A `nowcast_result` object.

## Examples

``` r
# \donttest{
set.seed(42)
d <- data.frame(
  date = as.Date(paste0(2015:2024, "-01-01")),
  gdp = cumsum(rnorm(10, 0.5, 0.3)),
  ind1 = cumsum(rnorm(10, 0.4, 0.2))
)
nc_compute(d, method = "bridge", formula = gdp ~ ind1)
#> ℹ Durbin-Watson = 3.133 suggests residual autocorrelation.
#> ℹ OLS standard errors may be unreliable. Consider HAC SEs via sandwich.
#> 
#> ── Nowcast (Bridge Equation) ───────────────────────────────────────────────────
#> • Nowcast: 6.9103
#> • SE: 0.3336
#> • "95%" CI: [6.094, 7.7266]
#> • Target period: 2024-01-01
#> • R-squared: 0.9838 | Obs: 9
# }
```

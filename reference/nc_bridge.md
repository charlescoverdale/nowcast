# Bridge Equation Nowcast

Estimate a bridge equation via OLS and produce a nowcast for the current
or specified period. Bridge equations translate higher-frequency
indicators (e.g. monthly) into estimates of a lower-frequency target
(e.g. quarterly GDP). Following standard central bank practice (Baffigi
et al. 2004, Runstler and Sedillot 2003), an autoregressive term for the
target variable can be included via the `ar_order` argument.

## Usage

``` r
nc_bridge(formula, data, newdata = NULL, ar_order = 1L, alpha = 0.05)
```

## Arguments

- formula:

  A formula with the target variable on the left-hand side and indicator
  variables on the right (e.g. `target ~ ind1 + ind2`).

- data:

  A data frame (or `nc_dataset`) containing the variables in `formula`
  plus a `date` column.

- newdata:

  A one-row data frame with indicator values for the nowcast period. If
  `NULL`, the last complete row of `data` is used.

- ar_order:

  Integer. Number of autoregressive lags of the target to include
  (default 1). Set to 0 for a static bridge equation with no AR terms.

- alpha:

  Numeric. Significance level for prediction intervals (default 0.05).

## Value

A `nowcast_result` object.

## Details

Prediction intervals are computed using the full prediction standard
error, which accounts for both residual variance and estimation
uncertainty in the coefficients, evaluated against a *t* distribution
with appropriate degrees of freedom.

When `ar_order > 0`, the model includes a lagged dependent variable. The
reported standard errors assume homoskedastic, serially uncorrelated
errors. If residuals exhibit autocorrelation (indicated by the
Durbin-Watson statistic in `details$dw_stat`), consider extracting the
fitted model via `result$model` and applying HAC standard errors from
the sandwich package.

## References

Baffigi, A., Golinelli, R. and Parigi, G. (2004). Bridge models to
forecast the euro area GDP. *International Journal of Forecasting*,
20(3), 447–460.

## Examples

``` r
# \donttest{
# Synthetic example
set.seed(42)
d <- data.frame(
  date = as.Date(paste0(2015:2024, "-01-01")),
  gdp = cumsum(rnorm(10, 0.5, 0.3)),
  ind1 = cumsum(rnorm(10, 0.4, 0.2)),
  ind2 = cumsum(rnorm(10, 0.3, 0.4))
)
nc_bridge(gdp ~ ind1 + ind2, data = d)
#> ℹ Durbin-Watson = 2.884 suggests residual autocorrelation.
#> ℹ OLS standard errors may be unreliable. Consider HAC SEs via sandwich.
#> 
#> ── Nowcast (Bridge Equation) ───────────────────────────────────────────────────
#> • Nowcast: 6.7919
#> • SE: 0.3325
#> • "95%" CI: [5.9373, 7.6466]
#> • Target period: 2024-01-01
#> • R-squared: 0.9877 | Obs: 9
# }
```

# Pseudo-Real-Time Backtesting

Evaluate a nowcasting method by simulating its real-time performance on
final revised data. At each evaluation step, the model is estimated
using only data from periods 1 to *i*-1 (expanding window) or a rolling
window ending at *i*-1, then used to produce a nowcast for period *i*.
The nowcast is compared against the actual value at period *i*.

## Usage

``` r
nc_backtest(
  formula,
  data,
  method = "bridge",
  ar_order = 1L,
  start = 10L,
  window = c("expanding", "rolling"),
  window_size = 20L,
  alpha = 0.05
)
```

## Arguments

- formula:

  A formula for the bridge equation (e.g. `target ~ ind1 + ind2`).

- data:

  A data frame (or `nc_dataset`) with a `date` column.

- method:

  Character. Currently only `"bridge"` is supported.

- ar_order:

  Integer. Number of autoregressive lags of the target to include
  (default 1). Set to 0 for a static bridge equation.

- start:

  Integer. Minimum number of observations before the first backtest
  evaluation (default 10).

- window:

  Character. `"expanding"` (default) or `"rolling"`.

- window_size:

  Integer. Number of observations in the rolling window (ignored if
  `window = "expanding"`).

- alpha:

  Numeric. Significance level for prediction intervals (default 0.05).

## Value

A `nowcast_backtest` object with components:

- results:

  A data frame with columns `date`, `nowcast`, `actual`, `error`,
  `ci_lower`, `ci_upper`.

- actuals:

  The actual values used for evaluation.

- method:

  The method used.

- window_type:

  `"expanding"` or `"rolling"`.

- metrics:

  A data frame with RMSE, MAE, and bias.

## Details

This is a **pseudo-real-time** exercise: it uses final revised data
throughout, not the data that would have been available at each point in
time (vintage data). As noted by Banbura et al. (2013, ECB WP 1564),
data revisions can be material for some variables, so results may
overstate true real-time accuracy.

Prediction intervals are computed using the full prediction standard
error from `predict.lm(..., interval = "prediction")`, accounting for
both residual variance and coefficient estimation uncertainty.

## Examples

``` r
# \donttest{
set.seed(42)
n <- 30
d <- data.frame(
  date = seq(as.Date("2015-01-01"), by = "quarter", length.out = n),
  gdp = cumsum(rnorm(n, 0.5, 0.3)),
  ind1 = cumsum(rnorm(n, 0.4, 0.2))
)
bt <- nc_backtest(gdp ~ ind1, data = d, start = 15)
bt
#> 
#> ── Nowcast Backtest (bridge, expanding) ────────────────────────────────────────
#> • Evaluations: 15
#> • Period: 2018-10-01 to 2022-04-01
#> • RMSE: 0.526
#> • MAE: 0.3943
#> • Bias: -0.0845
# }
```

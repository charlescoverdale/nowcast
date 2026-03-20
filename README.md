# nowcast

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) [![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

**nowcast** is an R package for economic nowcasting — estimating the current state of a macroeconomic variable (like GDP) before the official data are released, using higher-frequency indicators that are available sooner.

## Installation

```r
# install.packages("devtools")
devtools::install_github("charlescoverdale/nowcast")
```

```r
library(nowcast)

# Nowcast GDP from monthly indicators — 3 lines of code
aligned <- nc_align(gdp_quarterly, retail = monthly_retail, ip = monthly_ip)
result <- nc_bridge(target ~ retail + ip, data = aligned)
result$nowcast
```

---

## Why nowcasting?

GDP is published with a lag — typically 6 to 8 weeks after the quarter ends. But monthly indicators like retail sales, industrial production, and labour market data arrive much sooner. Nowcasting uses these higher-frequency series to estimate GDP growth in real time, before the official number appears.

The fundamental challenge is the **ragged edge**: different indicators have different publication lags. Retail sales might be available through February, industrial production only through January, and employment through December — all at the same calendar date. A nowcasting model must work with this jagged pattern of data availability.

**nowcast** provides the tools to align mixed-frequency data, estimate bridge equations (the workhorse model at most central banks), and rigorously evaluate nowcast performance through pseudo-real-time backtesting.

---

## How does nowcast compare to other packages?

| Feature | **nowcast** | **bridgr** | **midasr** | **dfms** |
|---|---|---|---|---|
| Bridge equations | Yes (with AR terms) | Yes | No | No |
| MIDAS | Planned (v0.2.0) | No | Yes | No |
| Dynamic factor models | Planned (v0.2.0) | No | No | Yes |
| Mixed-frequency alignment | Yes | No | No | Yes |
| Ragged-edge diagnostics | Yes | No | No | Yes |
| Pseudo-real-time backtest | Yes | No | Yes (rolling) | No |
| Diebold-Mariano test | Yes (HLN corrected) | No | No | No |
| Unified interface | Yes | N/A | N/A | N/A |
| Dependencies | 1 (cli) | 8+ (tidyverse) | 6+ | 3 |
| Last updated | 2026 | 2026 | 2025 | 2026 |

**nowcast** is designed to be the lightweight, unified framework that connects these methods. `bridgr` does bridge equations within the tidyverse ecosystem. `midasr` is a mature MIDAS implementation. `dfms` is a high-quality DFM package with a C++ backend. **nowcast** aims to bring bridge equations, MIDAS, and DFM under one interface with integrated evaluation — so you can compare methods on the same data with the same metrics.

---

## Quick start

### 1. Align mixed-frequency data

```r
library(nowcast)

# Quarterly target (e.g. GDP growth)
gdp <- data.frame(
  date = seq(as.Date("2015-01-01"), by = "quarter", length.out = 40),
  value = rnorm(40, 0.5, 0.3)
)

# Monthly indicators (e.g. retail sales, industrial production)
retail <- data.frame(
  date = seq(as.Date("2015-01-01"), by = "month", length.out = 120),
  value = rnorm(120, 0.4, 0.5)
)
ip <- data.frame(
  date = seq(as.Date("2015-01-01"), by = "month", length.out = 118),  # ragged edge
  value = rnorm(118, 0.3, 0.4)
)

# Align: monthly indicators are averaged to quarterly frequency
aligned <- nc_align(gdp, retail = retail, ip = ip)
aligned
#> ── Aligned Nowcasting Dataset ──
#> • Target: target (quarterly)
#> • Indicators: 2 (retail, ip)
#> • Periods: 40 (2015-01-01 to 2024-10-01)

# See the ragged edge
nc_ragged_edge(aligned)
```

### 2. Estimate a bridge equation

```r
# Bridge equation with AR(1) term (default)
result <- nc_bridge(target ~ retail + ip, data = aligned)
result
#> ── Nowcast (Bridge Equation) ──
#> • Nowcast: 0.4832
#> • SE: 0.3156
#> • 95% CI: [-0.1467, 1.1131]
#> • Target period: 2024-10-01
#> • R-squared: 0.1523 | Obs: 38

# Full summary with coefficients
summary(result)

# Static bridge equation (no AR terms)
nc_bridge(target ~ retail + ip, data = aligned, ar_order = 0)
```

### 3. Evaluate with pseudo-real-time backtesting

```r
# Expanding-window backtest: train on 1:i-1, nowcast i
bt <- nc_backtest(target ~ retail + ip, data = aligned, start = 20)
bt
#> ── Nowcast Backtest (bridge, expanding) ──
#> • Evaluations: 18
#> • RMSE: 0.2847
#> • MAE: 0.2312
#> • Bias: 0.0043

# Compare two specifications
bt_static <- nc_backtest(target ~ retail + ip, data = aligned,
                          start = 20, ar_order = 0)
nc_dm_test(bt$results$error, bt_static$results$error)
#> Diebold-Mariano (HLN): p = 0.42

# Plot actual vs nowcast
plot(bt)
```

---

## Functions

| Category | Function | Description |
|---|---|---|
| Data | `nc_align()` | Align mixed-frequency target and indicators |
| Data | `nc_ragged_edge()` | Summarise data availability across series |
| Data | `nc_aggregate()` | Temporal aggregation (monthly to quarterly/annual) |
| Data | `nc_transform()` | Stationarity transforms (diff, log_diff, standardize) |
| Estimation | `nc_bridge()` | Bridge equation with optional AR terms |
| Evaluation | `nc_evaluate()` | RMSE, MAE, and bias |
| Evaluation | `nc_dm_test()` | Diebold-Mariano test (HLN corrected) |
| Evaluation | `nc_backtest()` | Pseudo-real-time expanding/rolling window evaluation |
| Utility | `nc_available()` | List available methods |
| Utility | `nc_compute()` | Generic dispatcher by method name |
| S3 | `print()` / `summary()` | cli-formatted output for all objects |
| S3 | `plot()` | Actual vs fitted/nowcast plots |
| S3 | `predict()` | Predict from fitted nowcast model |

---

## Methodology

### Bridge equations

Bridge equations are the simplest and most widely used nowcasting method. They were developed at the ECB (Runstler and Sedillot 2003, Baffigi et al. 2004) and remain the workhorse model at most central banks and treasuries. The idea: aggregate monthly indicators to the quarterly frequency, then regress the quarterly target on those aggregated indicators.

Following standard practice, `nc_bridge()` includes an autoregressive term by default (`ar_order = 1`), capturing GDP momentum. The model is:

$$y_t = \alpha + \phi y_{t-1} + \sum_i \beta_i \bar{x}_{i,t} + \varepsilon_t$$

where $\bar{x}_{i,t}$ is the within-quarter mean of monthly indicator $i$.

Prediction intervals are proper prediction intervals from `predict.lm(..., interval = "prediction")`, accounting for both residual variance and coefficient estimation uncertainty, evaluated against the *t* distribution.

### Diebold-Mariano test

The `nc_dm_test()` function implements the Harvey, Leybourne, and Newbold (1997) modification of the Diebold-Mariano (1995) test. The HLN correction applies a finite-sample scaling factor and uses the *t*_{n-1} distribution rather than the standard normal, which matters at the sample sizes typical in nowcasting (20-60 quarters). The Bartlett (triangular) kernel is used for HAC variance estimation, which guarantees non-negative variance.

### Backtesting

`nc_backtest()` performs pseudo-real-time evaluation on final revised data. At each step, the model sees only past data (expanding or rolling window) and produces a nowcast for the next period. This simulates real-time performance but does not account for data revisions — true real-time evaluation requires vintage data (e.g. from FRED's ALFRED database).

---

## Design decisions

- **`nc_` prefix** — short, distinctive, easy to type and autocomplete.
- **AR terms by default** — GDP growth is serially correlated. Following ECB/BoE practice, `nc_bridge()` includes an AR(1) term by default. Set `ar_order = 0` for a static specification.
- **Proper prediction intervals** — uses the full prediction standard error (estimation uncertainty + residual variance) and *t* quantiles, not just residual standard deviation with normal quantiles.
- **HLN-corrected DM test** — the original Diebold-Mariano test over-rejects in small samples. The HLN correction is now the standard (used by `forecast::dm.test` in R and `statsmodels` in Python).
- **No heavy dependencies** — depends only on `cli` and `stats`. No tidyverse, no Rcpp, no external system libraries.
- **Pure computation** — does not download data. Pair with [`ons`](https://github.com/charlescoverdale/ons), [`boe`](https://github.com/charlescoverdale/boe), [`fred`](https://github.com/charlescoverdale/fred), or any other data source.

---

## Limitations

- **Bridge equations only (v0.1.0).** MIDAS regressions and dynamic factor models are planned for v0.2.0.
- **Pseudo-real-time evaluation only.** The backtest uses final revised data, not vintage data. Data revisions can be material (Banbura et al. 2013, ECB WP 1564). True real-time evaluation requires vintage data that the user must supply.
- **No automatic indicator forecasting at the ragged edge.** When only 1-2 months of the current quarter are available, `nc_bridge()` uses whatever the user provides in `newdata`. It does not automatically forecast the missing months with AR/ARIMA. Central bank implementations typically do this, but it introduces model-within-model complexity.
- **OLS standard errors.** The reported coefficient standard errors assume homoskedastic, serially uncorrelated errors. For HAC-robust inference, users can extract the fitted model via `result$model` and apply the `sandwich` package.
- **No Kalman filter.** The PCA-based approach planned for DFM (v0.2.0) handles estimation but not ragged-edge filtering properly. For production DFM nowcasting with ragged edges, use `dfms`.

---

## References

- Baffigi, A., Golinelli, R. and Parigi, G. (2004). Bridge models to forecast the euro area GDP. *International Journal of Forecasting*, 20(3), 447--460.
- Diebold, F.X. and Mariano, R.S. (1995). Comparing predictive accuracy. *Journal of Business & Economic Statistics*, 13(3), 253--263.
- Harvey, D., Leybourne, S. and Newbold, P. (1997). Testing the equality of prediction mean squared errors. *International Journal of Forecasting*, 13(2), 281--291.
- Runstler, G. and Sedillot, F. (2003). Short-term estimates of euro area real GDP by means of monthly data. *ECB Working Paper No. 276*.

---

## Related packages

| Package | Description |
|---|---|
| [ons](https://github.com/charlescoverdale/ons) | UK Office for National Statistics data |
| [boe](https://github.com/charlescoverdale/boe) | Bank of England data |
| [fred](https://github.com/charlescoverdale/fred) | Federal Reserve Economic Data (FRED) |
| [readecb](https://github.com/charlescoverdale/readecb) | European Central Bank data |
| [readoecd](https://github.com/charlescoverdale/readoecd) | OECD data |
| [predictset](https://github.com/charlescoverdale/predictset) | Conformal prediction and uncertainty quantification |
| [climatekit](https://github.com/charlescoverdale/climatekit) | Climate indices from weather data |
| [inflateR](https://github.com/charlescoverdale/inflateR) | Inflation adjustment |

---

## Issues

Found a bug or have a feature request? Please [open an issue](https://github.com/charlescoverdale/nowcast/issues) on GitHub.

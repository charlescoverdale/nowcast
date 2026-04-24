# Changelog

## nowcast 0.1.0

CRAN release: 2026-03-25

- Initial CRAN release.
- Bridge equation nowcasting via
  [`nc_bridge()`](https://charlescoverdale.github.io/nowcast/reference/nc_bridge.md)
  with optional AR terms.
- Mixed-frequency alignment via
  [`nc_align()`](https://charlescoverdale.github.io/nowcast/reference/nc_align.md)
  with ragged-edge diagnostics.
- Temporal aggregation
  ([`nc_aggregate()`](https://charlescoverdale.github.io/nowcast/reference/nc_aggregate.md))
  and stationarity transforms
  ([`nc_transform()`](https://charlescoverdale.github.io/nowcast/reference/nc_transform.md)).
- Pseudo-real-time backtesting via
  [`nc_backtest()`](https://charlescoverdale.github.io/nowcast/reference/nc_backtest.md)
  with expanding or rolling windows.
- Diebold-Mariano test
  ([`nc_dm_test()`](https://charlescoverdale.github.io/nowcast/reference/nc_dm_test.md))
  with Harvey-Leybourne-Newbold (1997) finite-sample correction.
- Evaluation metrics
  ([`nc_evaluate()`](https://charlescoverdale.github.io/nowcast/reference/nc_evaluate.md)):
  RMSE, MAE, and bias.
- S3 methods: [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html),
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html), and
  [`predict()`](https://rdrr.io/r/stats/predict.html) for all core
  objects.

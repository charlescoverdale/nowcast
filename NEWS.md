# nowcast 0.1.0

* Initial CRAN release.
* Bridge equation nowcasting via `nc_bridge()` with optional AR terms.
* Mixed-frequency alignment via `nc_align()` with ragged-edge diagnostics.
* Temporal aggregation (`nc_aggregate()`) and stationarity transforms (`nc_transform()`).
* Pseudo-real-time backtesting via `nc_backtest()` with expanding or rolling windows.
* Diebold-Mariano test (`nc_dm_test()`) with Harvey-Leybourne-Newbold (1997) finite-sample correction.
* Evaluation metrics (`nc_evaluate()`): RMSE, MAE, and bias.
* S3 methods: `print()`, `summary()`, `plot()`, and `predict()` for all core objects.

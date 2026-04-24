# Align Mixed-Frequency Time Series

Merges a quarterly target series with one or more higher-frequency
indicator series into a single aligned dataset. Monthly indicators are
aggregated to the quarterly frequency using the specified function
(default: mean).

## Usage

``` r
nc_align(target, ..., freq_ratio = 3L, agg_fun = mean)
```

## Arguments

- target:

  A data frame with columns `date` (Date) and `value` (numeric)
  containing the quarterly target variable (e.g. GDP growth).

- ...:

  One or more data frames, each with columns `date` and `value`. Names
  are used as column names in the output; if unnamed, generic names are
  assigned.

- freq_ratio:

  Integer. The number of high-frequency observations per low-frequency
  period (default 3 for monthly-to-quarterly).

- agg_fun:

  Function used to aggregate indicators to the target frequency (default
  [mean](https://rdrr.io/r/base/mean.html)).

## Value

An `nc_dataset` object — a list with components:

- data:

  A data frame with `date` (quarter start dates) plus columns for the
  target and each indicator.

- target_col:

  Name of the target column.

- indicator_cols:

  Character vector of indicator column names.

- target_freq:

  Detected frequency of the target series.

- indicator_freq:

  Detected frequency of the indicator series.

- availability:

  A data frame summarising data availability per series.

## Details

The default `agg_fun = mean` is appropriate for **flow variables**
measured as rates or indices (GDP growth, CPI). For stock variables
(interest rates, exchange rates), pass
`agg_fun = function(x, ...) tail(x, 1)` to take end-of-period values.
For flow variables in levels (total sales), use `agg_fun = sum` – but
note that `sum` will understate the true quarterly total for partial
quarters at the ragged edge (a warning is emitted).

When a quarter has fewer than `freq_ratio` monthly observations (a
partial quarter at the ragged edge), the aggregation proceeds on the
available data and a message is emitted. The `n_months` column in the
output data frame records how many observations contributed to each
quarterly value for each indicator.

## Examples

``` r
# Create synthetic quarterly target and monthly indicators
target <- data.frame(
  date = as.Date(paste0(2020:2023, "-01-01")),
  value = c(0.5, -0.3, 0.8, 0.2)
)
ind1 <- data.frame(
  date = seq(as.Date("2020-01-01"), as.Date("2023-12-01"), by = "month"),
  value = rnorm(48)
)
aligned <- nc_align(target, indicator1 = ind1)
```

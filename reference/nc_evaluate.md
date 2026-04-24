# Evaluate Nowcast Accuracy

Compute standard forecast evaluation metrics: root mean squared error
(RMSE), mean absolute error (MAE), and mean bias.

## Usage

``` r
nc_evaluate(forecast, actual)
```

## Arguments

- forecast:

  Numeric vector of nowcast/forecast values.

- actual:

  Numeric vector of realised values (same length as `forecast`).

## Value

A data frame with one row and columns `rmse`, `mae`, and `bias`.

## Examples

``` r
nc_evaluate(c(1.0, 2.0, 3.0), c(1.1, 1.8, 3.2))
#>        rmse       mae        bias
#> 1 0.1732051 0.1666667 -0.03333333
```

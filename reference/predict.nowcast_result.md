# Predict Method for Nowcast Results

Generate predictions from a fitted nowcast model for new indicator data.

## Usage

``` r
# S3 method for class 'nowcast_result'
predict(object, newdata, ...)
```

## Arguments

- object:

  A `nowcast_result` object.

- newdata:

  A data frame with indicator values.

- ...:

  Additional arguments (currently unused).

## Value

A numeric vector of predictions.

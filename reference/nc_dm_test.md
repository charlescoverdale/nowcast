# Diebold-Mariano Test for Equal Predictive Accuracy

Tests whether two sets of forecast errors have equal predictive
accuracy. Implements the modified test of Harvey, Leybourne, and Newbold
(1997), which applies a finite-sample correction to the original Diebold
and Mariano (1995) statistic and uses the *t* distribution rather than
the normal. The Bartlett (triangular) kernel is used for HAC variance
estimation, which guarantees a non-negative variance estimate.

## Usage

``` r
nc_dm_test(
  e1,
  e2,
  alternative = c("two.sided", "less", "greater"),
  h = 1L,
  loss = c("squared", "absolute")
)
```

## Arguments

- e1:

  Numeric vector. Forecast errors from model 1.

- e2:

  Numeric vector. Forecast errors from model 2 (same length).

- alternative:

  Character. `"two.sided"`, `"less"` (model 1 better), or `"greater"`
  (model 2 better).

- h:

  Integer. Forecast horizon (default 1). Used for the Newey-West
  bandwidth and HLN correction.

- loss:

  Character. Loss function: `"squared"` or `"absolute"`.

## Value

A list with components `statistic`, `p_value`, `alternative`, `method`,
and `n`.

## References

Diebold, F.X. and Mariano, R.S. (1995). Comparing predictive accuracy.
*Journal of Business & Economic Statistics*, 13(3), 253–263.
[doi:10.1080/07350015.1995.10524599](https://doi.org/10.1080/07350015.1995.10524599)

Harvey, D., Leybourne, S. and Newbold, P. (1997). Testing the equality
of prediction mean squared errors. *International Journal of
Forecasting*, 13(2), 281–291.
[doi:10.1016/S0169-2070(96)00719-4](https://doi.org/10.1016/S0169-2070%2896%2900719-4)

## Examples

``` r
# \donttest{
set.seed(1)
e1 <- rnorm(50, sd = 1)
e2 <- rnorm(50, sd = 1.5)
nc_dm_test(e1, e2)
#> $statistic
#> [1] -3.334762
#> 
#> $p_value
#> [1] 0.001633046
#> 
#> $alternative
#> [1] "two.sided"
#> 
#> $method
#> [1] "Diebold-Mariano (HLN)"
#> 
#> $n
#> [1] 50
#> 
# }
```

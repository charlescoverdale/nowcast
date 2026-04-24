# List Available Nowcasting Methods

Returns a data frame describing the nowcasting methods implemented in
the package.

## Usage

``` r
nc_available()
```

## Value

A data frame with columns `method`, `description`, and `available`.

## Examples

``` r
nc_available()
#>   method                                    description available
#> 1 bridge Bridge equation via OLS with optional AR terms      TRUE
```

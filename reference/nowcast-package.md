# nowcast: Economic Nowcasting with Bridge Equations and Real-Time Evaluation

Provides bridge equations with optional autoregressive terms for
nowcasting low-frequency macroeconomic variables (e.g. quarterly GDP)
from higher-frequency indicators (e.g. monthly retail sales). Handles
the ragged-edge problem where different indicators have different
publication lags via mixed-frequency alignment. Includes
pseudo-real-time evaluation with expanding or rolling windows, and the
Diebold-Mariano test for comparing forecast accuracy following Harvey,
Leybourne, and Newbold (1997)
[doi:10.1016/S0169-2070(96)00719-4](https://doi.org/10.1016/S0169-2070%2896%2900719-4)
. No API calls; designed to work with data from any source.

## See also

Useful links:

- <https://github.com/charlescoverdale/nowcast>

- Report bugs at <https://github.com/charlescoverdale/nowcast/issues>

## Author

**Maintainer**: Charles Coverdale <charlesfcoverdale@gmail.com>
\[copyright holder\]

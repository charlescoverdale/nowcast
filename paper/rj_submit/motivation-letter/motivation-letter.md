---
output: pdf_document
fontsize: 12pt
---

\thispagestyle{empty}
\today

The Editor
The R Journal
\bigskip

Dear Editor,
\bigskip

Please consider the article *Nowcast: Economic Nowcasting with Bridge Equations and Real-Time Evaluation in R* for publication in the R Journal.

The nowcast package implements the full bridge-equation nowcasting workflow that central bank research departments have used as their workhorse macroeconomic forecasting tool since the early 2000s. It handles mixed-frequency alignment with explicit ragged-edge diagnostics, estimates bridge equations with optional autoregressive terms, runs pseudo-real-time backtests over expanding or rolling windows, and implements the Diebold-Mariano test with the Harvey-Leybourne-Newbold finite-sample correction for comparing competing specifications. Existing CRAN packages cover related mixed-frequency methods (midasr for MIDAS regressions, bigtime for large-scale time-series models, bridgr for bridge equations in the tidyverse) but none provides the complete pseudo-real-time evaluation pipeline that nowcasting analysts need. The package is pure R with cli as its only non-base import, and plugs into any user-supplied data source.

Readers of the R Journal working in applied macroeconomics, monetary policy, sovereign fiscal analysis, and macro hedge-fund research will use the package to produce institution-quality nowcasts from any combination of quarterly target and monthly indicators. The uniform nc_ function prefix, plain-data-frame inputs, and S3 print/summary/plot methods make it straightforward to integrate into existing reproducible workflows. The paper's worked example (four US monthly indicators bridged to quarterly GDP growth, 2012 to present) demonstrates the end-to-end workflow in four lines of code.

The manuscript has not been published in a peer-reviewed journal, is not currently under review elsewhere, and all rights to submit rest with the sole author.

\bigskip
\bigskip

Regards,
\bigskip
\bigskip

Charles Coverdale
London, United Kingdom
charles.f.coverdale@gmail.com

# Figure generator for the nowcast R Journal paper.
#
# Five PDF figures + one LaTeX table, using real US FRED data.
# Run: RSTUDIO_PANDOC=/Applications/quarto/bin/tools Rscript paper/make_figures.R

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
  library(ggplot2)
  library(showtext)
  library(scales)
})

font_add("HelveticaNeue",
         regular = "/System/Library/Fonts/Helvetica.ttc",
         bold = "/System/Library/Fonts/Helvetica.ttc",
         italic = "/System/Library/Fonts/Helvetica.ttc")
showtext_auto()
showtext_opts(dpi = 300)

fig_dir <- "paper/figures"
tab_dir <- "paper/tables"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
if (!dir.exists(tab_dir)) dir.create(tab_dir, recursive = TRUE)

ok_blue   <- "#0072B2"
ok_orange <- "#E69F00"
ok_green  <- "#009E73"
ok_red    <- "#D55E00"
ok_purple <- "#CC79A7"
ok_sky    <- "#56B4E9"
ok_grey   <- "#999999"

fam <- "HelveticaNeue"

theme_wp <- function(base_size = 10) {
  theme_bw(base_size = base_size, base_family = fam) +
    theme(
      plot.title = element_blank(), plot.subtitle = element_blank(),
      plot.caption = element_blank(), panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.25, colour = "grey85"),
      axis.line = element_line(linewidth = 0.35, colour = "grey25"),
      axis.ticks = element_line(linewidth = 0.35, colour = "grey25"),
      axis.ticks.length = unit(2.5, "pt"),
      axis.text = element_text(size = base_size, colour = "grey20"),
      axis.title = element_text(size = base_size, colour = "grey20"),
      legend.position = "bottom", legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1, family = fam),
      legend.key.height = unit(10, "pt"),
      legend.key.width = unit(22, "pt"),
      legend.spacing.x = unit(10, "pt"),
      legend.margin = margin(4, 0, 0, 0),
      plot.margin = margin(6, 10, 6, 6)
    )
}

tex_esc <- function(x) gsub("_", "\\\\_", as.character(x))

# -----------------------------------------------------------------------------
# Load FRED series.
# -----------------------------------------------------------------------------
read_fred <- function(id) {
  df <- read.csv(file.path("paper/data", paste0(id, ".csv")))
  df$date <- as.Date(df$observation_date)
  data.frame(date = df$date,
             value = suppressWarnings(as.numeric(df[[id]])))
}

gdp     <- read_fred("GDPC1")     # Quarterly real GDP
retail  <- read_fred("RSAFS")     # Monthly retail sales
indpro  <- read_fred("INDPRO")    # Monthly industrial production
payems  <- read_fred("PAYEMS")    # Monthly nonfarm payrolls
umcsent <- read_fred("UMCSENT")   # Monthly consumer sentiment

# Restrict to 2002 onwards.
trim <- function(df, start = as.Date("2001-10-01")) {
  df[df$date >= start & !is.na(df$value), ]
}
gdp     <- trim(gdp)
retail  <- trim(retail)
indpro  <- trim(indpro)
payems  <- trim(payems)
umcsent <- trim(umcsent)

# Convert to growth rates: log-diff for levels.
growth <- function(df, scale = 100) {
  x <- df$value
  g <- c(NA, 100 * diff(log(x)))
  data.frame(date = df$date, value = g)[-1, ]
}
gdp_g     <- growth(gdp)
retail_g  <- growth(retail)
indpro_g  <- growth(indpro)
payems_g  <- growth(payems)
# Sentiment: already diff-stationary, use first difference.
umcsent_d <- data.frame(date = umcsent$date[-1],
                        value = diff(umcsent$value))

# Align via nc_align: quarterly target + monthly indicators (freq_ratio = 3).
aligned <- nc_align(
  gdp_g,
  retail = retail_g, indpro = indpro_g,
  payems = payems_g, sentiment = umcsent_d
)

# -----------------------------------------------------------------------------
# Figure 1: ragged-edge heatmap of data availability.
# -----------------------------------------------------------------------------
# Show the last 24 months of each monthly indicator: available (1) vs missing (0).
last_24m <- seq(max(c(retail_g$date, indpro_g$date, payems_g$date,
                      umcsent_d$date)) - 730,
                max(c(retail_g$date, indpro_g$date, payems_g$date,
                      umcsent_d$date)),
                by = "month")

ind_list <- list(
  `Retail sales (RSAFS)` = retail_g,
  `Industrial production (INDPRO)` = indpro_g,
  `Nonfarm payrolls (PAYEMS)` = payems_g,
  `Consumer sentiment (UMCSENT)` = umcsent_d
)

df1_rows <- list()
for (nm in names(ind_list)) {
  df <- ind_list[[nm]]
  for (d in last_24m) {
    ym <- format(as.Date(d), "%Y-%m")
    df_ym <- df[format(df$date, "%Y-%m") == ym, ]
    val <- if (nrow(df_ym) > 0) 1 else 0
    df1_rows[[length(df1_rows) + 1L]] <- data.frame(
      indicator = nm, month = as.Date(d), available = val
    )
  }
}
df1 <- do.call(rbind, df1_rows)
df1$indicator <- factor(df1$indicator, levels = rev(names(ind_list)))
df1$available <- factor(df1$available, levels = c(0, 1),
                        labels = c("Missing", "Available"))

p1 <- ggplot(df1, aes(x = month, y = indicator, fill = available)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  scale_fill_manual(values = c(Missing = "grey85", Available = ok_blue)) +
  scale_x_date(expand = c(0, 0), date_breaks = "6 months",
               date_labels = "%b %Y") +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_wp(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(fig_dir, "fig1_ragged_edge.pdf"),
       p1, width = 5.5, height = 2.6, device = cairo_pdf)

# -----------------------------------------------------------------------------
# Figure 2: GDP nowcast vs realised, expanding-window backtest.
# -----------------------------------------------------------------------------
bt <- nc_backtest(target ~ retail + indpro + payems + sentiment,
                  data = aligned, start = 40, method = "bridge",
                  ar_order = 1)

df2 <- data.frame(
  date = bt$results$date,
  realised = bt$results$actual,
  nowcast  = bt$results$nowcast
)

df2_long <- rbind(
  data.frame(date = df2$date, value = df2$realised, series = "Realised GDP growth"),
  data.frame(date = df2$date, value = df2$nowcast,  series = "Bridge-equation nowcast")
)
df2_long$series <- factor(df2_long$series,
                          levels = c("Realised GDP growth",
                                     "Bridge-equation nowcast"))

p2 <- ggplot(df2_long, aes(x = date, y = value,
                            colour = series, linetype = series)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey60") +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = c("Realised GDP growth" = "grey30",
                                  "Bridge-equation nowcast" = ok_blue)) +
  scale_linetype_manual(values = c("Realised GDP growth" = "solid",
                                    "Bridge-equation nowcast" = "longdash")) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = NULL, y = "Quarterly GDP growth (log-diff)") +
  guides(colour = guide_legend(nrow = 1,
                               override.aes = list(linewidth = 0.8)),
         linetype = guide_legend(nrow = 1)) +
  theme_wp(base_size = 10)

ggsave(file.path(fig_dir, "fig2_backtest.pdf"),
       p2, width = 5.5, height = 3.2, device = cairo_pdf)

cat(sprintf("fig2: backtest RMSE = %.3f, MAE = %.3f, bias = %.3f\n",
            bt$metrics$rmse[1], bt$metrics$mae[1], bt$metrics$bias[1]))

# -----------------------------------------------------------------------------
# Figure 3: nowcast error by vintage window (expanding vs rolling).
# -----------------------------------------------------------------------------
bt_exp <- nc_backtest(target ~ retail + indpro + payems + sentiment,
                      data = aligned, start = 40, method = "bridge",
                      window = "expanding", ar_order = 1)
bt_rol <- nc_backtest(target ~ retail + indpro + payems + sentiment,
                      data = aligned, start = 40, method = "bridge",
                      window = "rolling", window_size = 40, ar_order = 1)

err_exp <- bt_exp$results$nowcast - bt_exp$results$actual
err_rol <- bt_rol$results$nowcast - bt_rol$results$actual

df3 <- rbind(
  data.frame(date = bt_exp$results$date, err = err_exp,
             window = "Expanding"),
  data.frame(date = bt_rol$results$date, err = err_rol,
             window = "Rolling (40 quarters)")
)
df3$window <- factor(df3$window,
                     levels = c("Expanding", "Rolling (40 quarters)"))

p3 <- ggplot(df3, aes(x = date, y = err,
                       colour = window, linetype = window)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50",
             linetype = "dashed") +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = c("Expanding" = ok_blue,
                                  "Rolling (40 quarters)" = ok_red)) +
  scale_linetype_manual(values = c("Expanding" = "solid",
                                    "Rolling (40 quarters)" = "longdash")) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = NULL, y = "Nowcast error (nowcast minus realised)") +
  guides(colour = guide_legend(nrow = 1,
                               override.aes = list(linewidth = 0.8)),
         linetype = guide_legend(nrow = 1)) +
  theme_wp(base_size = 10)

ggsave(file.path(fig_dir, "fig3_window.pdf"),
       p3, width = 5.5, height = 3.2, device = cairo_pdf)

cat(sprintf("fig3: expanding RMSE = %.3f, rolling RMSE = %.3f\n",
            bt_exp$metrics$rmse, bt_rol$metrics$rmse))

# -----------------------------------------------------------------------------
# Figure 4: contribution of each indicator (coefficient and t-stat).
# -----------------------------------------------------------------------------
br_full <- nc_bridge(target ~ retail + indpro + payems + sentiment,
                     data = aligned, ar_order = 1)
cf <- coef(br_full$model)
se <- sqrt(diag(vcov(br_full$model)))

df4 <- data.frame(
  var = names(cf),
  coef = as.numeric(cf),
  se   = as.numeric(se)
)
df4 <- df4[df4$var != "(Intercept)" & df4$var != "target_lag1", ]
df4$var <- factor(df4$var, levels = rev(df4$var))

p4 <- ggplot(df4, aes(x = coef, y = var)) +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey50") +
  geom_errorbarh(aes(xmin = coef - 1.96 * se, xmax = coef + 1.96 * se),
                 height = 0.2, colour = ok_blue, linewidth = 0.5) +
  geom_point(colour = ok_blue, size = 2.5) +
  labs(x = "Coefficient estimate (95% CI)",
       y = "Monthly indicator (quarterly-aggregated)") +
  theme_wp(base_size = 10)

ggsave(file.path(fig_dir, "fig4_coefficients.pdf"),
       p4, width = 5.5, height = 2.6, device = cairo_pdf)

# -----------------------------------------------------------------------------
# Figure 5: Diebold-Mariano pairwise comparison of two specifications.
# -----------------------------------------------------------------------------
# Compare bridge with and without AR term.
bt_ar <- nc_backtest(target ~ retail + indpro + payems + sentiment,
                     data = aligned, start = 40, method = "bridge",
                     ar_order = 1)
bt_no <- nc_backtest(target ~ retail + indpro + payems + sentiment,
                     data = aligned, start = 40, method = "bridge",
                     ar_order = 0)

e1 <- bt_ar$results$nowcast - bt_ar$results$actual
e2 <- bt_no$results$nowcast - bt_no$results$actual
dm <- nc_dm_test(e1 = e1, e2 = e2, h = 1, loss = "squared")

df5 <- data.frame(
  spec = c("With AR(1)", "Without AR"),
  rmse = c(bt_ar$metrics$rmse[1], bt_no$metrics$rmse[1]),
  mae  = c(bt_ar$metrics$mae[1],  bt_no$metrics$mae[1])
)
df5$spec <- factor(df5$spec, levels = c("With AR(1)", "Without AR"))

p5 <- ggplot(df5, aes(x = spec, y = rmse, fill = spec)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = sprintf("RMSE %.3f", rmse)),
            vjust = 1.8, size = 3.2, family = fam, colour = "white") +
  scale_fill_manual(values = c("With AR(1)" = ok_blue,
                                "Without AR" = ok_red)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "Backtest RMSE") +
  guides(fill = "none") +
  theme_wp(base_size = 10) +
  annotate("label",
           x = 1.5, y = max(df5$rmse) * 1.06,
           label = sprintf("DM stat = %.2f, p = %.3f",
                           dm$statistic, dm$p_value),
           family = fam, size = 3.1, colour = "grey20",
           fill = "white", label.size = 0.25)

ggsave(file.path(fig_dir, "fig5_dm.pdf"),
       p5, width = 5.5, height = 3.0, device = cairo_pdf)

cat(sprintf("fig5: DM stat = %.3f, p = %.4f\n",
            dm$statistic, dm$p_value))

# -----------------------------------------------------------------------------
# Table: RMSE/MAE/bias across specifications.
# -----------------------------------------------------------------------------
specs <- list(
  `Bridge + AR(1), all four indicators` = bt_ar,
  `Bridge no AR, all four indicators` = bt_no,
  `Bridge + AR(1), retail + INDPRO only` = NULL
)
specs[[3]] <- nc_backtest(target ~ retail + indpro,
                          data = aligned, start = 40, method = "bridge",
                          ar_order = 1)

# Naive AR(1) benchmark on same target dates.
bt_dates <- bt_ar$results$date
naive <- numeric(length(bt_dates))
for (i in seq_along(bt_dates)) {
  idx <- which(aligned$data$date == bt_dates[i])
  train <- aligned$data$target[seq_len(idx - 1L)]
  fit <- ar(train, order.max = 1, aic = FALSE)
  naive[i] <- predict(fit, n.ahead = 1)$pred[1]
}
naive_actuals <- bt_ar$results$actual
naive_rmse <- sqrt(mean((naive - naive_actuals)^2, na.rm = TRUE))
naive_mae  <- mean(abs(naive - naive_actuals), na.rm = TRUE)
naive_bias <- mean(naive - naive_actuals, na.rm = TRUE)

# DM test bridge vs naive.
dm_bench <- nc_dm_test(e1 = naive - naive_actuals,
                        e2 = bt_ar$results$nowcast - naive_actuals,
                        h = 1, loss = "squared")

tab_lines <- c(
  "\\begin{tabular}{lrrr}",
  "\\toprule",
  "Specification & RMSE & MAE & Bias \\\\",
  "\\midrule",
  sprintf("Naive AR(1) on target only (benchmark) & %.3f & %.3f & %.3f \\\\",
          naive_rmse, naive_mae, naive_bias),
  "\\midrule"
)
for (nm in names(specs)) {
  s <- specs[[nm]]
  tab_lines <- c(tab_lines,
    sprintf("%s & %.3f & %.3f & %.3f \\\\",
            tex_esc(nm),
            s$metrics$rmse[1], s$metrics$mae[1], s$metrics$bias[1]))
}
tab_lines <- c(tab_lines, "\\bottomrule", "\\end{tabular}")
writeLines(tab_lines, file.path(tab_dir, "metrics.tex"))

cat(sprintf("benchmark: AR(1) RMSE %.3f; bridge %.3f; DM stat %.3f p %.3f\n",
            naive_rmse, bt_ar$metrics$rmse[1],
            dm_bench$statistic, dm_bench$p_value))

cat("\n--- done ---\n")

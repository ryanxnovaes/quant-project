# =====================================================
# ITUB4 Exploratory Data Analysis — Brazil
# Technical Indicators, Returns, Volatility, Correlation, ACF/PACF
# Period: 2010-01-01 to Today
# =====================================================

# -----------------------------------------------------
# 0. Load Required Packages
# -----------------------------------------------------
load_or_install(c(
  "ggplot2", "dplyr", "tidyr", "lubridate", "patchwork", "zoo",
  "psych", "GGally", "forecast"
))

# =====================================================
# 1. Summary Statistics
# =====================================================

numeric_db <- db |>
  dplyr::select(-c(Date, Month, OBV, ITUB4.SA.Volume))

psych::describe(numeric_db) |>
  (\(desc) {
    desc$cv <- desc$sd / desc$mean
    desc[, c(names(desc)[1:5], names(desc)[8:9], 
             names(desc)[11:12], names(desc)[14])]
  })()

# =====================================================
# 2. Price, Return and Volatility Plots
# =====================================================

# --- 2.1 Closing Price --- #
ggplot2::ggplot(db, ggplot2::aes(Date, ITUB4.SA.Close)) +
  ggplot2::geom_line(color = "#FF6600", linewidth = 0.4) +
  ggplot2::geom_smooth(
    se = FALSE, color = "#003399", linewidth = 0.6,
    method = "loess", span = 0.3
  ) +
  ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  ggplot2::labs(
    title = "ITUB4 Closing Price (2010–Present)",
    x = "Date", y = "Price (R$)"
  ) +
  ggplot2::theme_minimal(base_family = "sans") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

# --- 2.2 Monthly Price Distribution --- #
ggplot2::ggplot(db, ggplot2::aes(Month, ITUB4.SA.Close)) +
  ggplot2::geom_boxplot(
    fill  = "#FF6600",
    color = "#003399",
    outlier.color = "#FF6600",
    outlier.alpha = 0.4
  ) +
  ggplot2::scale_x_discrete(limits = month.abb) +
  ggplot2::labs(title = "Monthly Distribution of ITUB4 Price",
                x = "Month", y = "Price (R$)") +
  ggplot2::theme_minimal(base_family = "sans") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))

# --- 2.3 Daily Log Returns --- #
ggplot2::ggplot(db, ggplot2::aes(Date, ITUB4.SA.Return)) +
  ggplot2::geom_hline(yintercept = 0, color = "#666666", linetype = "dashed") +
  ggplot2::geom_line(color = "#003399", linewidth = 0.3) +
  ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  ggplot2::labs(title = "Daily Log Return of ITUB4",
                x = "Date", y = "Log-return") +
  ggplot2::theme_minimal(base_family = "sans") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

# --- 2.4 Rolling Volatility (20-day) --- #
ggplot2::ggplot(db, ggplot2::aes(Date, ITUB4.SA.Rolling_vol)) +
  ggplot2::geom_line(color = "#FF6600", linewidth = 0.4) +
  ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  ggplot2::labs(title = "ITUB4 Rolling Volatility (20-Day Window)",
                x = "Date", y = "Volatility (σ)") +
  ggplot2::theme_minimal(base_family = "sans") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

# =====================================================
# 3. Monthly Return Distribution
# =====================================================

ggplot2::ggplot(db, ggplot2::aes(Month, ITUB4.SA.Return)) +
  ggplot2::geom_boxplot(
    fill  = "#FF6600",
    color = "#003399",
    outlier.color = "#003399",
    outlier.alpha = 0.4
  ) +
  ggplot2::scale_x_discrete(limits = month.abb) +
  ggplot2::labs(title = "Monthly Distribution of ITUB4 Log Returns",
                x = "Month", y = "Daily log-return") +
  ggplot2::theme_minimal(base_family = "sans") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))

# =====================================================
# 4. Return Distribution (Histogram + Density)
# =====================================================

# --- 4.1 Histogram + Density --- #
ggplot2::ggplot(db, ggplot2::aes(ITUB4.SA.Return)) +
  ggplot2::geom_histogram(
    ggplot2::aes(y = ggplot2::after_stat(density)),
    bins  = 80,
    alpha = 0.6,
    fill  = "#FF6600",
    color = "white"
  ) +
  ggplot2::geom_density(
    linewidth = 0.7,
    color     = "#003399"
  ) +
  ggplot2::labs(
    title = "Distribution of ITUB4 Daily Log Returns",
    x = "Daily log-return",
    y = "Density"
  ) +
  ggplot2::theme_minimal(base_family = "sans") +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
  )


# --- 4.2 Histogram + Normal Curve --- #
mu_ret  <- mean(db$ITUB4.SA.Return, na.rm = TRUE)
sig_ret <- sd(db$ITUB4.SA.Return, na.rm = TRUE)

ggplot2::ggplot(db, ggplot2::aes(ITUB4.SA.Return)) +
  ggplot2::geom_histogram(ggplot2::aes(y = ..density..),
                          bins = 80, alpha = 0.6,
                          fill = "#FF6600", color = "white"
  ) +
  ggplot2::stat_function(fun = dnorm,
                         args = list(mean = mu_ret, sd = sig_ret),
                         linewidth = 0.7, linetype = "dashed", color = "#666666"
  ) +
  ggplot2::geom_density(linewidth = 0.7, color = "#003399") +
  ggplot2::labs(title = "ITUB4 Returns vs Normal Approximation",
                x = "Daily log-return", y = "Density") +
  ggplot2::theme_minimal(base_family = "sans") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))

# =====================================================
# 5. Closing Price Distribution
# =====================================================

mu_close  <- mean(db$ITUB4.SA.Close, na.rm = TRUE)
sig_close <- sd(db$ITUB4.SA.Close,   na.rm = TRUE)

ggplot2::ggplot(db, ggplot2::aes(ITUB4.SA.Close)) +
  ggplot2::geom_histogram(ggplot2::aes(y = ..density..),
                          bins = 80, alpha = 0.6,
                          fill = "#FF6600", color = "white"
  ) +
  ggplot2::stat_function(fun = dnorm,
                         args = list(mean = mu_close, sd = sig_close),
                         linewidth = 0.7, linetype = "dashed", color = "#666666"
  ) +
  ggplot2::geom_density(linewidth = 0.7, color = "#003399") +
  ggplot2::labs(title = "ITUB4 Price vs Normal Approximation",
                x = "Closing price (R$)", y = "Density") +
  ggplot2::theme_minimal(base_family = "sans") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))

# =====================================================
# 6. Moving Average Indicators
# =====================================================

db |>
  dplyr::select(
    Date, SMA_05, SMA_20, EMA_05, WMA_05, WMA_20
  ) |>
  tidyr::pivot_longer(cols = -Date,
                      names_to = "indicator",
                      values_to = "value") |>
  ggplot2::ggplot(ggplot2::aes(Date, value)) +
  ggplot2::geom_line(linewidth = 0.3, color = "#003399") +
  ggplot2::facet_wrap(
    ~ indicator, ncol = 1, scales = "free_y",
    labeller = ggplot2::labeller(
      indicator = c(
        SMA_05 = "Simple Moving Average (5-day)",
        SMA_20 = "Simple Moving Average (20-day)",
        EMA_05 = "Exponential Moving Average (5-day)",
        WMA_05 = "Weighted Moving Average (5-day)",
        WMA_20 = "Weighted Moving Average (20-day)"
      )
    )
  ) +
  ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
  ggplot2::labs(
    title = "ITUB4 Moving Average Indicators",
    x = "Date", y = "Indicator value"
  ) +
  ggplot2::theme_minimal(base_family = "sans") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

# =====================================================
# 7. Oscillators, Trend and Volatility Indicators
# =====================================================
db |>
  dplyr::select(
    Date,
    MOM_10, ROC_14, RSI_14, RSI_20,
    Stoch_K, MFI_14, MACD_Hist,
    ADX_ADX14, DIp_ADX14, DIn_ADX14,
    BB20_Width, ITUB4.SA.Rolling_vol
  ) |>
  tidyr::pivot_longer(-Date, names_to = "indicator", values_to = "value") |>
  ggplot2::ggplot(ggplot2::aes(Date, value)) +
  ggplot2::geom_line(linewidth = 0.3, color = "#FF6600") +
  ggplot2::facet_wrap(
    ~ indicator, scales = "free_y", ncol = 2,
    labeller = ggplot2::labeller(
      indicator = c(
        MOM_10               = "Momentum (10)",
        ROC_14               = "Rate of Change (14)",
        RSI_14               = "RSI (14)",
        RSI_20               = "RSI (20)",
        Stoch_K              = "Stochastic %K",
        MFI_14               = "Money Flow Index (14)",
        MACD_Hist            = "MACD Histogram",
        ADX_ADX14            = "ADX (14)",
        DIp_ADX14            = "+DI (14)",
        DIn_ADX14            = "-DI (14)",
        BB20_Width           = "Bollinger Band Width (20)",
        ITUB4.SA.Rolling_vol = "Rolling Volatility"
      )
    )
  ) +
  ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
  ggplot2::labs(
    title = "Technical Indicators: Oscillators, Trend and Volatility",
    x = "Date", y = "Indicator value"
  ) +
  ggplot2::theme_minimal(base_family = "sans") +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )

# =====================================================
# 8. Spearman Correlation
# =====================================================

GGally::ggcorr(
  numeric_db,
  method = c("pairwise.complete.obs", "spearman"),
  label = TRUE, label_round = 2,
  hjust = 0.9, size = 3,
  low = "#FF6600", mid = "white", high = "#003399"
) +
  ggplot2::ggtitle("Spearman Correlation – ITUB4") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))

# =====================================================
# 9. Return vs RSI(14)
# =====================================================

ggplot2::ggplot(db, ggplot2::aes(RSI_14, ITUB4.SA.Return)) +
  ggplot2::geom_point(alpha = 0.3, color = "#FF6600") +
  ggplot2::geom_smooth(
    se = FALSE, method = "loess",
    linewidth = 0.6, color = "#003399"
  ) +
  ggplot2::labs(
    title = "Return vs RSI(14)",
    x = "RSI(14)", y = "Daily log-return"
  ) +
  ggplot2::theme_minimal(base_family = "sans") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))

# =====================================================
# 10. ACF and PACF for ITUB4 Close
# =====================================================

plot_acf_pacf_itau <- function(x, max_lag = 30) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 10) return(NULL)
  conf <- 1.96 / sqrt(n)
  
  acf_df  <- data.frame(
    lag = forecast::Acf(x, plot = FALSE, lag.max = max_lag)$lag,
    val = forecast::Acf(x, plot = FALSE, lag.max = max_lag)$acf
  ) |>
    dplyr::filter(lag != 0) |>
    dplyr::mutate(sig = ifelse(abs(val) > conf, "Yes", "No"))
  
  pacf_df <- data.frame(
    lag = forecast::Pacf(x, plot = FALSE, lag.max = max_lag)$lag,
    val = forecast::Pacf(x, plot = FALSE, lag.max = max_lag)$acf
  ) |>
    dplyr::filter(lag != 0) |>
    dplyr::mutate(sig = ifelse(abs(val) > conf, "Yes", "No"))
  
  p1 <- ggplot2::ggplot(acf_df, ggplot2::aes(lag, val, fill = sig)) +
    ggplot2::geom_col(color = "white") +
    ggplot2::geom_hline(yintercept = 0, color = "#666666") +
    ggplot2::geom_hline(yintercept = c(conf, -conf),
                        color = "#666666", linetype = "dashed") +
    ggplot2::scale_fill_manual(values = c("Yes" = "#FF6600", "No" = "#CCCCCC")) +
    ggplot2::labs(title = "ACF – ITUB4 Close", x = "Lag", y = "ACF", fill = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   legend.position = "bottom")
  
  p2 <- ggplot2::ggplot(pacf_df, ggplot2::aes(lag, val, fill = sig)) +
    ggplot2::geom_col(color = "white") +
    ggplot2::geom_hline(yintercept = 0, color = "#666666") +
    ggplot2::geom_hline(yintercept = c(conf, -conf),
                        color = "#666666", linetype = "dashed") +
    ggplot2::scale_fill_manual(values = c("Yes" = "#003399", "No" = "#CCCCCC")) +
    ggplot2::labs(title = "PACF – ITUB4 Close", x = "Lag", y = "PACF", fill = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   legend.position = "bottom")
  
  p1 / p2
}

plot_acf_pacf_itau(db$ITUB4.SA.Close)

# =====================================================
rm(list = setdiff(ls(), c("db", "load_or_install")))

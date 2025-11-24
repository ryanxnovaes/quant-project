# =====================================================
# ITUB4 KNN Rolling Forecast — Brazil
# Grid Search, Model Selection, Forecasting and Diagnostics
# Period: 2010–Present
# =====================================================

# =====================================================
# 0. Load Required Packages
# =====================================================

load_or_install(c("FNN", "tibble", "car", "purrr"))

# =====================================================
# 1. Build Base Dataset (Close_lag1)
# =====================================================

db_base <- db |>
  dplyr::arrange(Date) |>
  dplyr::mutate(
    ITUB4.SA.Close_lag01 = dplyr::lag(ITUB4.SA.Close, 1)
  )

# Predictor set previously selected by VIF
predictors <- c(
  "ITUB4.SA.Close_lag01",
  "ITUB4.SA.Volume",
  "ITUB4.SA.Return",
  "ITUB4.SA.Range",
  "ITUB4.SA.Rolling_vol",
  "ROC_14",
  "MOM_10",
  "Stoch_K",
  "MACD_Hist",
  "OBV",
  "MFI_14",
  "DIp_ADX14",
  "DIn_ADX14",
  "ADX_ADX14",
  "BB20_Width"
)

# =====================================================
# 2. Grid Search Specification
# =====================================================

horizons       <- c(1, 5, 14, 20, 60)
train_windows  <- c(60, 126, 252, 504)
ks             <- 1:9

results <- tibble::tibble(
  horizon      = integer(),
  train_window = integer(),
  k            = integer(),
  mse          = double(),
  rmse         = double(),
  mae          = double(),
  mape         = double(),
  smape        = double()
)

plots <- list()

# =====================================================
# 3. Rolling Window Grid Search
# =====================================================

for (h in horizons) {
  
  message("===== Horizon h = ", h, " =====")
  
  db_h <- db_base |>
    dplyr::mutate(Target_lead = dplyr::lead(ITUB4.SA.Close, h)) |>
    dplyr::select(Date, Target_lead, dplyr::all_of(predictors)) |>
    tidyr::drop_na()
  
  X_raw <- as.matrix(db_h[, predictors])
  y     <- as.numeric(db_h$Target_lead)
  n_obs <- nrow(db_h)
  
  X <- scale(X_raw)
  
  for (win in train_windows) {
    if (win >= n_obs) next
    
    for (k_val in ks) {
      
      message("h = ", h, " | window = ", win, " | k = ", k_val)
      
      preds <- rep(NA_real_, n_obs)
      
      for (i in seq(win + 1, n_obs)) {
        X_train <- X[(i - win):(i - 1), , drop = FALSE]
        y_train <- y[(i - win):(i - 1)]
        X_test  <- X[i, , drop = FALSE]
        
        fit <- FNN::knn.reg(
          train = X_train,
          test  = X_test,
          y     = y_train,
          k     = k_val
        )
        
        preds[i] <- fit$pred
      }
      
      idx <- (win + 1):n_obs
      pred_val <- preds[idx]
      y_val    <- y[idx]
      date_val <- db_h$Date[idx]
      
      # --- Metrics ---
      mse_val  <- mean((y_val - pred_val)^2)
      rmse_val <- sqrt(mse_val)
      mae_val  <- mean(abs(y_val - pred_val))
      mape_val <- mean(abs((y_val - pred_val) / y_val)) * 100
      
      smape_val <- mean(
        2 * abs(pred_val - y_val) /
          (abs(pred_val) + abs(y_val)),
        na.rm = TRUE
      ) * 100
      
      results <- dplyr::bind_rows(
        results,
        tibble::tibble(
          horizon      = h,
          train_window = win,
          k            = k_val,
          mse          = mse_val,
          rmse         = rmse_val,
          mae          = mae_val,
          mape         = mape_val,
          smape        = smape_val
        )
      )
      
      # --- Forecast plot ---
      df_plot <- data.frame(
        Date     = date_val,
        Observed = y_val,
        Forecast = pred_val
      )
      
      p <- ggplot2::ggplot(df_plot, ggplot2::aes(Date)) +
        ggplot2::geom_line(
          ggplot2::aes(y = Observed, color = "Observed"),
          linewidth = 0.4
        ) +
        ggplot2::geom_line(
          ggplot2::aes(y = Forecast, color = "Forecasted"),
          linewidth = 0.4, linetype = "dashed"
        ) +
        ggplot2::scale_color_manual(
          values = c("Observed" = "#003399", "Forecasted" = "#FF6600")
        ) +
        ggplot2::labs(
          title = paste0(
            "KNN Rolling Forecast – h = ", h,
            ", window = ", win,
            ", k = ", k_val
          ),
          x = "Date", y = "Closing Price (R$)", color = ""
        ) +
        ggplot2::theme_minimal(base_family = "sans") +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )
      
      key <- paste0("h_", h, "_w_", win, "_k_", k_val)
      plots[[key]] <- p
    }
  }
}

# =====================================================
# 4. Select Best Model (by MSE)
# =====================================================

results_sorted <- results |> dplyr::arrange(mse)
best <- results_sorted[1, ]

best

best_key <- paste0(
  "h_", best$horizon,
  "_w_", best$train_window,
  "_k_", best$k
)

plots[[best_key]]

# =====================================================
# 5. Refit Best Model and Build Result Table
# =====================================================

h_best   <- best$horizon
win_best <- best$train_window
k_best   <- best$k

db_best <- db_base |>
  dplyr::mutate(Target_lead = dplyr::lead(ITUB4.SA.Close, h_best)) |>
  dplyr::select(Date, Target_lead, dplyr::all_of(predictors)) |>
  tidyr::drop_na()

X_best  <- X
y_best  <- y
dates_b <- db_best$Date

X_raw <- as.matrix(db_best[, predictors])
y     <- as.numeric(db_best$Target_lead)
n_obs <- nrow(db_best)

X <- scale(X_raw)
preds <- rep(NA_real_, n_obs)

for (i in seq(win_best + 1, n_obs)) {
  X_train <- X[(i - win_best):(i - 1), , drop = FALSE]
  y_train <- y[(i - win_best):(i - 1)]
  X_test  <- X[i, , drop = FALSE]
  
  fit <- FNN::knn.reg(
    train = X_train,
    test  = X_test,
    y     = y_train,
    k     = k_best
  )
  
  preds[i] <- fit$pred
}

idx <- (win_best + 1):n_obs

results_knn <- tibble::tibble(
  date      = db_best$Date[idx],
  actual    = y[idx],
  forecast  = preds[idx]
) |>
  dplyr::mutate(
    error         = actual - forecast,
    abs_error     = abs(error),
    cum_abs_error = cumsum(abs_error)
  )

# =====================================================
# 6. Forecast Plot (Best Model)
# =====================================================

forec_plot <- ggplot2::ggplot(results_knn, ggplot2::aes(date)) +
  ggplot2::geom_line(
    ggplot2::aes(y = actual,   color = "Observed"), linewidth = 0.4
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = forecast, color = "Forecasted"),
    linewidth = 0.4, linetype = "dashed"
  ) +
  ggplot2::scale_color_manual(
    values = c("Observed" = "#003399", "Forecasted" = "#FF6600")
  ) +
  ggplot2::labs(
    title = "KNN Forecast vs Observed (Best Rolling Model)",
    subtitle = paste0(
      "ITUB4 Close — h = ", h_best,
      ", window = ", win_best,
      ", k = ", k_best
    ),
    x = "Date", y = "Closing Price (R$)", color = ""
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title    = ggplot2::element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )

forec_plot

# =====================================================
# 7. Diagnostic Plots (Residuals)
# =====================================================

g1 <- ggplot2::ggplot(results_knn, ggplot2::aes(date, error)) +
  ggplot2::geom_line(color = "#FF6600", linewidth = 0.4) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::labs(
    title = "Residuals over Time",
    x = "Date", y = "Residual"
  ) +
  ggplot2::theme_minimal()

g2 <- ggplot2::ggplot(results_knn, ggplot2::aes(error)) +
  ggplot2::geom_histogram(
    bins = 30, fill = "#FF6600", color = "#003399", alpha = 0.7
  ) +
  ggplot2::labs(
    title = "Residual Distribution",
    x = "Error", y = "Frequency"
  ) +
  ggplot2::theme_minimal()

g3 <- ggplot2::ggplot(results_knn, ggplot2::aes(date, cum_abs_error)) +
  ggplot2::geom_line(color = "#003399", linewidth = 0.4) +
  ggplot2::labs(
    title = "Cumulative Absolute Error",
    x = "Date", y = "Cumulative |Error|"
  ) +
  ggplot2::theme_minimal()


(g1 / g2 / g3) &
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
  )

# ============================================================
# 8. KNN Permutation Importance — Rolling Window Forecasting
# ============================================================

# ------------------------------------------------------------
# 8.1 Baseline MSE from the Best KNN Model
# ------------------------------------------------------------

mse_base <- mean((results_knn$actual - results_knn$forecast)^2)

# ------------------------------------------------------------
# 8.2. Helper Function to Recompute Rolling KNN Predictions
# ------------------------------------------------------------

run_knn_with_X <- function(X_mat, y_vec, dates_vec, win, k) {
  
  n_obs <- length(y_vec)
  preds <- rep(NA_real_, n_obs)
  
  # Rolling window: trained on window [i-win, …, i-1], predict i
  for (i in seq(win + 1, n_obs)) {
    X_train <- X_mat[(i - win):(i - 1), , drop = FALSE]
    y_train <- y_vec[(i - win):(i - 1)]
    X_test  <- X_mat[i, , drop = FALSE]
    
    model_i   <- FNN::knn.reg(train = X_train, test = X_test,
                              y = y_train, k = k)
    preds[i]  <- model_i$pred
  }
  
  idx_valid <- (win + 1):n_obs
  
  list(
    preds    = preds[idx_valid],
    y_valid  = y_vec[idx_valid],
    dates    = dates_vec[idx_valid]
  )
}

# ------------------------------------------------------------
# 8.3. Permutation Importance Loop
# ------------------------------------------------------------

importance_list <- purrr::map(predictors, function(var_name) {
  
  message("Computing permutation importance for: ", var_name)
  
  # Make a permuted copy of X
  X_perm <- X_best
  col_idx <- which(colnames(X_perm) == var_name)
  
  # Permute predictor column
  X_perm[, col_idx] <- sample(X_perm[, col_idx])
  
  # Run KNN with permuted predictor
  res_perm <- run_knn_with_X(
    X_mat    = X_perm,
    y_vec    = y_best,
    dates_vec = dates_b,
    win      = win_best,
    k        = k_best
  )
  
  mse_perm <- mean((res_perm$y_valid - res_perm$preds)^2)
  
  tibble::tibble(
    variable   = var_name,
    mse_perm   = mse_perm,
    mse_base   = mse_base,
    importance = mse_perm - mse_base
  )
})

importance_knn <- dplyr::bind_rows(importance_list) |>
  dplyr::arrange(dplyr::desc(importance))

importance_knn

# ------------------------------------------------------------
# 8.4. Permutation Importance Plot
# ------------------------------------------------------------

imp_knn_plot <- ggplot2::ggplot(
  importance_knn,
  ggplot2::aes(
    x    = reorder(variable, importance),
    y    = importance,
    fill = importance
  )
) +
  ggplot2::geom_bar(stat = "identity", color = "white") +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "KNN – ΔMSE by Predictor Permutation (Rolling Window)",
    x     = "Predictor variable",
    y     = "Δ MSE (perm − base)"
  ) +
  ggplot2::scale_fill_gradient(
    low  = "#FFCC99",
    high = "#FF6600"   # Itaú Orange
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title  = ggplot2::element_text(hjust = 0.5, face = "bold"),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10))
  )

print(imp_knn_plot)

# ggsave("figures/knn_importance_best.png", imp_knn_plot, 
# width = 10, height = 6, dpi = 300)

# =====================================================
# ITUB4 Random Walk Benchmark — Brazil
# =====================================================

# =====================================================
# 9. Horizons to Evaluate
# =====================================================
horizons <- c(1, 5, 14, 20, 60)

rw_results <- tibble::tibble(
  horizon = integer(),
  mse     = double(),
  rmse    = double(),
  mae     = double(),
  mape    = double(),
  smape   = double()
)

# =====================================================
# 9.1 Random Walk Forecasting Loop
# =====================================================

for (h in horizons) {
  
  # Build horizon-specific dataset
  db_rw_h <- db |>
    dplyr::arrange(Date) |>
    dplyr::mutate(
      target_h = dplyr::lead(ITUB4.SA.Close, h),   # y_{t+h}
      rw_naive = ITUB4.SA.Close                    # forecast = y_t
    ) |>
    tidyr::drop_na()
  
  y      <- db_rw_h$target_h
  fcast  <- db_rw_h$rw_naive
  
  # --- Metrics ---
  mse_val  <- mean((y - fcast)^2)
  rmse_val <- sqrt(mse_val)
  mae_val  <- mean(abs(y - fcast))
  mape_val <- mean(abs((y - fcast) / y)) * 100
  
  den       <- abs(fcast) + abs(y)
  smape_vec <- ifelse(den == 0, 0, 2 * abs(fcast - y) / den)
  smape_val <- mean(smape_vec) * 100
  
  rw_results <- dplyr::bind_rows(
    rw_results,
    tibble::tibble(
      horizon = h,
      mse     = mse_val,
      rmse    = rmse_val,
      mae     = mae_val,
      mape    = mape_val,
      smape   = smape_val
    )
  )
}

# Display results
rw_results

# =====================================================
# 10. Forecast Plot for a Selected Horizon (default: h = 1)
# =====================================================
h_plot <- 1

db_rw_plot <- db |>
  dplyr::arrange(Date) |>
  dplyr::mutate(
    target_h = dplyr::lead(ITUB4.SA.Close, h_plot),
    rw_naive = ITUB4.SA.Close
  ) |>
  tidyr::drop_na()

rw_plot <- ggplot2::ggplot(db_rw_plot, ggplot2::aes(Date)) +
  ggplot2::geom_line(
    ggplot2::aes(y = target_h, color = "Observed"),
    linewidth = 0.4
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = rw_naive, color = "Naive RW"),
    linewidth = 0.4, linetype = "dashed"
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "Observed" = "#003399",  # Itaú blue
      "Naive RW" = "#FF6600"   # Itaú orange
    )
  ) +
  ggplot2::labs(
    title = paste0("Random Walk Forecast vs Observed — h = ", h_plot),
    x = "Date",
    y = "Closing Price (R$)",
    color = ""
  ) +
  ggplot2::theme_minimal(base_family = "sans") +
  ggplot2::theme(
    plot.title  = ggplot2::element_text(hjust = 0.5, face = "bold"),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )

rw_plot

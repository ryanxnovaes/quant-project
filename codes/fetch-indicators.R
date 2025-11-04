# =====================================================
# ITUB4 Stock Analysis — Brazil
# Technical Indicators: SMA, EMA, Momentum, RSI, MACD, ADX, Stochastic, MFI, Bollinger Bands
# 15-Year Period: 2010-01-01 to Today
# =====================================================

# -----------------------------------------------------
# 0. Helper: Install & Load Packages if Missing
# -----------------------------------------------------
load_or_install <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(sprintf("Installing package: %s", pkg))
      install.packages(pkg, dependencies = TRUE)
    } else {
      message(sprintf("Package already available: %s", pkg))
    }
  }
}

load_or_install(c("quantmod", "dplyr", "TTR", "lubridate", "zoo", "arrow"))

# -----------------------------------------------------
# 1. Download ITUB4 Data from Yahoo
# -----------------------------------------------------
quantmod::getSymbols("ITUB4.SA", src = "yahoo", from = "2010-01-01", to = Sys.Date(), auto.assign = TRUE)

db <- data.frame(
  Date = as.Date(time(ITUB4.SA)),
  ITUB4.SA
)
row.names(db) <- NULL

# --- Save Bronze Dataset ---
arrow::write_parquet(db, "../data/bronze/itub4_bronze.parquet")

# -----------------------------------------------------
# 2. Compute Technical Indicators
# -----------------------------------------------------

db <- db |> 
  dplyr::mutate(
    # --- Simple Moving Averages (SMA) ---
    SMA_05  = TTR::SMA(ITUB4.SA.Close, n = 5),
    SMA_10  = TTR::SMA(ITUB4.SA.Close, n = 10),
    SMA_20  = TTR::SMA(ITUB4.SA.Close, n = 20),
    SMA_50  = TTR::SMA(ITUB4.SA.Close, n = 50),
    SMA_100 = TTR::SMA(ITUB4.SA.Close, n = 100),
    SMA_200 = TTR::SMA(ITUB4.SA.Close, n = 200),
    
    # --- Exponential Moving Averages (EMA) ---
    EMA_12 = TTR::EMA(ITUB4.SA.Close, n = 12),
    EMA_20 = TTR::EMA(ITUB4.SA.Close, n = 20),
    EMA_26 = TTR::EMA(ITUB4.SA.Close, n = 26),
    EMA_50 = TTR::EMA(ITUB4.SA.Close, n = 50),
    
    # --- Weighted Moving Averages (WMA) ---
    WMA_10 = TTR::WMA(ITUB4.SA.Close, n = 10),
    WMA_20 = TTR::WMA(ITUB4.SA.Close, n = 20),
    
    # --- Momentum & Rate of Change (ROC) ---
    MOM_10 = TTR::momentum(ITUB4.SA.Close, n = 10),
    MOM_14 = TTR::momentum(ITUB4.SA.Close, n = 14),
    ROC_12 = TTR::ROC(ITUB4.SA.Close, n = 12, type = "discrete") * 100,
    ROC_14 = TTR::ROC(ITUB4.SA.Close, n = 14, type = "discrete") * 100,
    
    # --- Relative Strength Index (RSI) ---
    RSI_07 = TTR::RSI(ITUB4.SA.Close, n = 7),
    RSI_14 = TTR::RSI(ITUB4.SA.Close, n = 14),
    RSI_21 = TTR::RSI(ITUB4.SA.Close, n = 21),
    
    # --- On-Balance Volume (OBV) ---
    OBV       = TTR::OBV(ITUB4.SA.Close, ITUB4.SA.Volume),
    OBV_SMA20 = TTR::SMA(OBV, n = 20),
    OBV_SMA50 = TTR::SMA(OBV, n = 50)
  )

# --- ADX & Directional Indicators ---
calc_adx <- function(data, n) {
  hlc <- data |> dplyr::select(ITUB4.SA.High, ITUB4.SA.Low, ITUB4.SA.Close)
  TTR::ADX(as.matrix(hlc), n = n) |>
    as.data.frame() |>
    dplyr::rename_with(~ paste0(., "_ADX", n))
}

db <- dplyr::bind_cols(
  db,
  calc_adx(db, 14),
  calc_adx(db, 50)
)

# --- Stochastic Oscillator ---
hlc <- as.matrix(db[, c("ITUB4.SA.High", "ITUB4.SA.Low", "ITUB4.SA.Close")])
stoch_osc <- TTR::stoch(hlc, nFastK = 14, nFastD = 3, nSlowD = 3)
stoch_osc <- data.frame(stoch_osc)
colnames(stoch_osc) <- paste0(c("fastK","fastD","slowD"), "_STO")

db <- cbind(db, stoch_osc[, c("fastK_STO","fastD_STO")])
colnames(db)[(ncol(db)-1):ncol(db)] <- c("Stoch_K", "Stoch_D")

# --- MACD ---
macd <- TTR::MACD(as.numeric(db$ITUB4.SA.Close), nFast = 12, nSlow = 26, nSig = 9, maType = TTR::EMA)
MACD_Hist <- macd[, "macd"] - macd[, "signal"]

db <- cbind(db,
            MACD        = macd[, "macd"],
            MACD_Signal = macd[, "signal"],
            MACD_Hist   = MACD_Hist)

# --- Money Flow Index (MFI) ---
ITUB4.SA.MFI_14 <- TTR::MFI(
  HLC = as.matrix(db[, c("ITUB4.SA.High", "ITUB4.SA.Low", "ITUB4.SA.Close")]),
  volume = db$ITUB4.SA.Volume, n = 14)

db$MFI_14 <- as.numeric(ITUB4.SA.MFI_14)

# --- Bollinger Bands ---
BB_20 <- TTR::BBands(as.numeric(db$ITUB4.SA.Close), n = 20, sd = 2)
BB_50 <- TTR::BBands(as.numeric(db$ITUB4.SA.Close), n = 50, sd = 2)

db <- cbind(db,
            BB20_Central = BB_20[, "mavg"],
            BB20_Upper   = BB_20[, "up"],
            BB20_Lower   = BB_20[, "dn"],
            
            BB50_Central = BB_50[, "mavg"],
            BB50_Upper   = BB_50[, "up"],
            BB50_Lower   = BB_50[, "dn"])

rm(list = setdiff(ls(), c("db")))

# -----------------------------------------------------
# 3. Data Cleaning
# -----------------------------------------------------
db <- db |> 
  dplyr::mutate(
    Month = lubridate::month(Date, label = TRUE),
    ITUB4.SA.Return = c(NA, diff(log(ITUB4.SA.Close))),
    ITUB4.SA.Rolling_vol = zoo::rollapply(ITUB4.SA.Return, width = 20, FUN = var, fill = NA, align = "right")
  )

# --- Scale Variables (exclude Date, Month, Close) ---
db <- db |>
  # Remove Adjusted (not needed)
  dplyr::select(-ITUB4.SA.Adjusted) |>
  
  # Scale all numeric variables except Date, Month, Close
  dplyr::mutate(
    dplyr::across(
      .cols = -c(Date, Month, ITUB4.SA.Close),
      .fns = ~ as.numeric(scale(.x))
    )
  ) |>
  
  # Reorder columns for readability
  dplyr::relocate(Month, .after = Date) |>
  dplyr::relocate(ITUB4.SA.Close, .after = Month) |>
  dplyr::relocate(c(ITUB4.SA.Return, ITUB4.SA.Rolling_vol), .after = ITUB4.SA.Volume) |>
  
  # Remove rows with NA from initial indicator periods
  na.omit()

# -----------------------------------------------------
# 4. Save Final Silver Dataset
# -----------------------------------------------------
arrow::write_parquet(db, "../data/silver/itub4_silver.parquet")

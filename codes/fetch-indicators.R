# =====================================================
# ITUB4 Stock Analysis — Brazil
# Technical Indicators and Fetch Data
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
quantmod::getSymbols("ITUB4.SA", src = "yahoo",
                     from = "2010-01-01", to = Sys.Date(),
                     auto.assign = TRUE)
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
    SMA_20  = TTR::SMA(ITUB4.SA.Close, n = 20),

    # --- Exponential Moving Averages (EMA) ---
    EMA_05 = TTR::EMA(ITUB4.SA.Close, n = 5),

    # --- Weighted Moving Averages (WMA) ---
    WMA_05 = TTR::WMA(ITUB4.SA.Close, n = 5),
    WMA_20 = TTR::WMA(ITUB4.SA.Close, n = 20),
    
    # --- Momentum & Rate of Change (ROC) ---
    MOM_10 = TTR::momentum(ITUB4.SA.Close, n = 10),
    ROC_14 = TTR::ROC(ITUB4.SA.Close, n = 14, type = "discrete") * 100,

    # --- Relative Strength Index (RSI) ---
    RSI_14 = TTR::RSI(ITUB4.SA.Close, n = 14),
    RSI_20 = TTR::RSI(ITUB4.SA.Close, n = 20),
    
    # --- On-Balance Volume (OBV) ---
    OBV = TTR::OBV(ITUB4.SA.Close, ITUB4.SA.Volume)
  )

# --- ADX & Directional Indicators ---
calc_adx <- function(data, n) {
  hlc <- data |> 
    dplyr::select(ITUB4.SA.High, ITUB4.SA.Low, ITUB4.SA.Close)
  
  TTR::ADX(as.matrix(hlc), n = n) |>
    as.data.frame() |>
    dplyr::rename_with(~ paste0(., "_ADX", n))
}

db <- dplyr::bind_cols(db, calc_adx(db, 14))

db <- db |>
  dplyr::select(-DX_ADX14)

# --- Stochastic Oscillator ---
hlc <- as.matrix(db[, c("ITUB4.SA.High", "ITUB4.SA.Low", "ITUB4.SA.Close")])
stoch_osc <- TTR::stoch(hlc, nFastK = 14, nFastD = 3, nSlowD = 3)
stoch_osc <- data.frame(stoch_osc)
db$Stoch_K <- as.numeric(stoch_osc$fastK)

# --- MACD ---
macd <- TTR::MACD(as.numeric(db$ITUB4.SA.Close), maType = TTR::EMA)
db$MACD_Hist <- as.numeric(macd[, "macd"] - macd[, "signal"])

# --- Money Flow Index (MFI) ---
ITUB4.SA.MFI_14 <- TTR::MFI(
  HLC = as.matrix(db[, c("ITUB4.SA.High", "ITUB4.SA.Low", "ITUB4.SA.Close")]),
  volume = db$ITUB4.SA.Volume, n = 14)

db$MFI_14 <- as.numeric(ITUB4.SA.MFI_14)

# --- Bollinger Bands ---
BB_20 <- TTR::BBands(as.numeric(db$ITUB4.SA.Close), n = 20, sd = 2)

db <- db |>
  dplyr::mutate(
    # Largura dos canais
    BB20_Width = BB_20[, "up"] - BB_20[, "dn"])

rm(list = setdiff(ls(), c("db", "load_or_install")))

# -----------------------------------------------------
# 3. Data Cleaning
# -----------------------------------------------------
db <- db |> 
  dplyr::mutate(
    Month = factor(month.abb[lubridate::month(Date)], levels = month.abb),
    ITUB4.SA.Return = c(NA, diff(log(ITUB4.SA.Close))),
    ITUB4.SA.Range = (ITUB4.SA.High - ITUB4.SA.Low),
    ITUB4.SA.Rolling_vol = zoo::rollapply(
      ITUB4.SA.Return, width = 20, 
      FUN = sd, fill = NA, align = "right"
      )
    ) |>
  dplyr::select(-c(ITUB4.SA.Adjusted,
                   ITUB4.SA.Open,
                   ITUB4.SA.High,
                   ITUB4.SA.Low)) |>
  
  # Reorder columns for readability
  dplyr::relocate(Month, .after = Date) |>
  dplyr::relocate(ITUB4.SA.Close, .after = Month) |>
  dplyr::relocate(c(ITUB4.SA.Return,  ITUB4.SA.Range, 
                    ITUB4.SA.Rolling_vol), .after = ITUB4.SA.Volume) |>
  
  # Remove rows with NA from initial indicator periods
  na.omit()

# -----------------------------------------------------
# 4. Save Final Silver Dataset
# -----------------------------------------------------
arrow::write_parquet(db, "../data/silver/itub4_silver.parquet")

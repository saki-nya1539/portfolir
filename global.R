# global.R
#
# アプリ起動時に一度だけ読み込まれる共通設定・依存パッケージ・ヘルパー関数群。
# Shinyは同じディレクトリに global.R / ui.R / server.R がある場合、自動的にこの3ファイルを
# まとめて読み込む（shiny::runApp("portfolir") でそのまま起動できる）。

library(shiny)
library(bslib)

source("R/data_source.R")
source("R/risk_metrics.R")
source("R/monte_carlo.R")

# ---- アプリ全体で使う既定値 ----

DEFAULT_TICKERS <- c("AAPL", "MSFT", "GOOGL", "AMZN")
DEFAULT_WEIGHTS <- c(30, 30, 20, 20)
DEFAULT_FROM_DATE <- Sys.Date() - 365 * 3
DEFAULT_TO_DATE <- Sys.Date()
DEFAULT_INITIAL_VALUE <- 1000000 # 円
DEFAULT_RISK_FREE_RATE_PCT <- 0.5 # 年率 %
DEFAULT_CONFIDENCE <- 0.95
DEFAULT_SIM_DAYS <- 252
DEFAULT_SIM_COUNT <- 1000

# ---- 表示用フォーマットヘルパー ----

#' 割合を「12.34%」のような文字列にする
format_percent <- function(x, digits = 2) {
  if (length(x) == 0 || is.na(x)) return("—")
  sprintf(paste0("%.", digits, "f%%"), x * 100)
}

#' 金額を「¥1,234,567」のような文字列にする
format_currency <- function(x, digits = 0) {
  if (length(x) == 0 || is.na(x)) return("—")
  paste0("¥", formatC(round(x, digits), format = "d", big.mark = ","))
}

#' 比率を「1.23」のような文字列にする（シャープレシオなど）
format_ratio <- function(x, digits = 2) {
  if (length(x) == 0 || is.na(x)) return("—")
  sprintf(paste0("%.", digits, "f"), x)
}

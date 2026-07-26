# R/risk_metrics.R
#
# ポートフォリオのリスク指標を計算する純粋関数群。
# 入力はいずれも「日次リターンの数値ベクトル」または「価格のwide形式tibble」で、
# Shinyのreactive文脈に依存しない設計にしている（testthatで単体テストしやすくするため）。

#' long形式の株価データ(date, symbol, adjusted)をwide形式(date, 銘柄1, 銘柄2, ...)に変換する
#'
#' @param prices_long tibble(date, symbol, adjusted)
#' @return wide形式のtibble
prices_to_wide <- function(prices_long) {
  prices_long |>
    dplyr::select(date, symbol, adjusted) |>
    tidyr::pivot_wider(names_from = symbol, values_from = adjusted) |>
    dplyr::arrange(date)
}

#' wide形式の価格データから日次の単純リターンを計算する
#'
#' @param wide_prices wide形式のtibble(date, 銘柄1, 銘柄2, ...)
#' @return wide形式のtibble(date, 銘柄1, 銘柄2, ...)。先頭行（リターン計算不能）は除外される。
calc_returns <- function(wide_prices) {
  asset_cols <- setdiff(names(wide_prices), "date")
  returns <- wide_prices |>
    dplyr::arrange(date) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(asset_cols), ~ . / dplyr::lag(.) - 1))
  returns[-1, ]
}

#' 銘柄ごとの日次リターンと保有比率から、ポートフォリオ全体の日次リターン系列を計算する
#' （毎日、目標比率にリバランスされる前提の加重平均）
#'
#' @param returns_wide wide形式のリターン tibble(date, 銘柄1, 銘柄2, ...)
#' @param weights 銘柄名をキーとした比率の名前付きnumericベクトル（合計1でなくても内部で正規化する）
#' @return tibble(date, portfolio_return)
calc_portfolio_returns <- function(returns_wide, weights) {
  asset_cols <- names(weights)
  missing_cols <- setdiff(asset_cols, names(returns_wide))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "リターンデータに存在しない銘柄が指定されました: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }

  w <- as.numeric(weights) / sum(as.numeric(weights))
  names(w) <- asset_cols

  returns_matrix <- as.matrix(returns_wide[, asset_cols, drop = FALSE])
  portfolio_return <- as.numeric(returns_matrix %*% w)

  tibble::tibble(date = returns_wide$date, portfolio_return = portfolio_return)
}

#' 年率換算リターン（幾何平均ベースのCAGR）
#'
#' @param daily_returns 日次リターンの数値ベクトル
#' @param periods_per_year 年間営業日数（既定252）
annualize_return <- function(daily_returns, periods_per_year = 252) {
  daily_returns <- daily_returns[!is.na(daily_returns)]
  n <- length(daily_returns)
  if (n == 0) return(NA_real_)
  growth <- prod(1 + daily_returns)
  growth^(periods_per_year / n) - 1
}

#' 年率換算ボラティリティ（日次リターンの標準偏差を年率化）
annualize_volatility <- function(daily_returns, periods_per_year = 252) {
  daily_returns <- daily_returns[!is.na(daily_returns)]
  if (length(daily_returns) < 2) return(NA_real_)
  stats::sd(daily_returns) * sqrt(periods_per_year)
}

#' シャープレシオ（年率超過リターン ÷ 年率ボラティリティ）
#'
#' @param risk_free_rate 年率の無リスク金利（例: 0.005 = 0.5%）
sharpe_ratio <- function(daily_returns, risk_free_rate = 0, periods_per_year = 252) {
  ann_return <- annualize_return(daily_returns, periods_per_year)
  ann_vol <- annualize_volatility(daily_returns, periods_per_year)
  if (is.na(ann_vol) || ann_vol == 0) return(NA_real_)
  (ann_return - risk_free_rate) / ann_vol
}

#' 最大ドローダウン（累積リターン系列のピークからの最大下落率。正の割合として返す）
max_drawdown <- function(daily_returns) {
  daily_returns <- daily_returns[!is.na(daily_returns)]
  if (length(daily_returns) == 0) return(NA_real_)
  cum_value <- cumprod(1 + daily_returns)
  running_peak <- cummax(cum_value)
  drawdown <- (cum_value - running_peak) / running_peak
  abs(min(drawdown))
}

#' ヒストリカル法によるVaR（信頼水準confidenceでの、正の割合として表した損失の大きさ）
#'
#' @param confidence 信頼水準（例: 0.95）
historical_var <- function(daily_returns, confidence = 0.95) {
  daily_returns <- daily_returns[!is.na(daily_returns)]
  if (length(daily_returns) == 0) return(NA_real_)
  loss_quantile <- stats::quantile(daily_returns, probs = 1 - confidence, na.rm = TRUE)
  -as.numeric(loss_quantile)
}

#' パラメトリック法（正規分布仮定）によるVaR
parametric_var <- function(daily_returns, confidence = 0.95) {
  daily_returns <- daily_returns[!is.na(daily_returns)]
  if (length(daily_returns) < 2) return(NA_real_)
  mu <- mean(daily_returns)
  sigma <- stats::sd(daily_returns)
  z <- stats::qnorm(1 - confidence)
  -(mu + z * sigma)
}

#' 銘柄間の相関行列を計算する
#'
#' @param returns_wide wide形式のリターン tibble(date, 銘柄1, 銘柄2, ...)
#' @return 銘柄 x 銘柄 の相関行列
correlation_matrix <- function(returns_wide) {
  asset_cols <- setdiff(names(returns_wide), "date")
  stats::cor(as.matrix(returns_wide[, asset_cols, drop = FALSE]), use = "pairwise.complete.obs")
}

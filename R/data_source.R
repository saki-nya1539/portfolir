# R/data_source.R
#
# 株価データ取得モジュール。
#
# - fetch_prices_online(): quantmod::getSymbols() で Yahoo Finance から実データを取得する。
# - fetch_prices_sample(): ネット接続なしでもデモを安定して回せるよう、幾何ブラウン運動で
#   「それらしい」合成株価データを生成する。ハッカソンのデモ本番でネットワークや
#   Yahoo Finance側の障害に足を引っ張られないためのフォールバック経路として用意している。
#
# どちらの経路で取得しても、戻り値は共通の形式
#   tibble(date, symbol, adjusted)
# に正規化されるので、呼び出し側（server.R）はデータの出どころを意識しなくてよい。

#' Yahoo Financeから調整後終値を取得する
#'
#' @param symbols character vector ティッカーシンボル（例: c("AAPL", "MSFT")）
#' @param from 取得開始日（Date変換可能な値）
#' @param to 取得終了日（Date変換可能な値）
#' @return tibble(date, symbol, adjusted)。取得に失敗した銘柄はwarningを出しつつ除外される。
fetch_prices_online <- function(symbols, from, to) {
  symbols <- unique(toupper(trimws(symbols)))

  results <- lapply(symbols, function(sym) {
    tryCatch({
      xts_data <- quantmod::getSymbols(
        Symbols = sym, src = "yahoo",
        from = as.Date(from), to = as.Date(to),
        auto.assign = FALSE, warnings = FALSE
      )
      adjusted_col <- quantmod::Ad(xts_data)
      tibble::tibble(
        date = as.Date(zoo::index(adjusted_col)),
        symbol = sym,
        adjusted = as.numeric(adjusted_col)
      )
    }, error = function(e) {
      warning(sprintf("銘柄 %s の取得に失敗しました: %s", sym, conditionMessage(e)))
      NULL
    })
  })

  out <- dplyr::bind_rows(results)
  if (nrow(out) == 0) {
    stop("指定した銘柄の株価データを1件も取得できませんでした。ティッカーシンボルを確認するか、サンプルデータをお試しください。")
  }
  out
}

#' オフラインで使えるサンプル株価データを生成する（幾何ブラウン運動）
#'
#' 実際のYahoo Financeのデータではなく、銘柄ごとに異なるドリフト・ボラティリティを
#' 設定した幾何ブラウン運動で「それらしい」価格推移を作る。ネットワーク環境に依存せず
#' デモを安定して見せるためのフォールバック用途。
#'
#' 同じ銘柄シンボル・同じseedであれば毎回同じ値動きを再現する（銘柄名から疑似乱数シードを
#' 導出しているため）。
#'
#' @param symbols character vector
#' @param from 開始日
#' @param to 終了日
#' @param seed 乱数シードのベース値（再現性のため）
#' @return tibble(date, symbol, adjusted)
fetch_prices_sample <- function(symbols, from, to, seed = 42) {
  symbols <- unique(toupper(trimws(symbols)))

  dates <- seq.Date(as.Date(from), as.Date(to), by = "day")
  # 土日を除外して営業日相当にする（weekdays()はロケール依存のため、
  # ロケールに依存しない POSIXlt$wday（0=日曜, 6=土曜）で判定する）
  dates <- dates[!as.POSIXlt(dates)$wday %in% c(0, 6)]
  n <- length(dates)

  results <- lapply(symbols, function(sym) {
    sym_seed <- sum(utf8ToInt(sym)) + as.integer(seed)
    set.seed(sym_seed)

    start_price <- stats::runif(1, 50, 400)
    annual_drift <- stats::runif(1, -0.05, 0.20)
    annual_vol <- stats::runif(1, 0.15, 0.55)

    dt <- 1 / 252
    daily_drift <- (annual_drift - 0.5 * annual_vol^2) * dt
    daily_vol <- annual_vol * sqrt(dt)

    shocks <- stats::rnorm(n, mean = daily_drift, sd = daily_vol)
    log_prices <- log(start_price) + cumsum(shocks)
    prices <- exp(log_prices)

    tibble::tibble(date = dates, symbol = sym, adjusted = prices)
  })

  dplyr::bind_rows(results)
}

#' 指定したソースに応じて株価データを取得する共通の入口
#'
#' source = "online" のとき取得に失敗した場合は、エラーで落とさずサンプルデータに
#' 自動フォールバックする（デモ中にオフラインになっても分析を止めないための設計）。
#'
#' @param symbols character vector
#' @param from 開始日
#' @param to 終了日
#' @param source "online" または "sample"
#' @return tibble(date, symbol, adjusted)
fetch_prices <- function(symbols, from, to, source = c("online", "sample")) {
  source <- match.arg(source)

  if (source == "online") {
    tryCatch(
      fetch_prices_online(symbols, from, to),
      error = function(e) {
        warning(sprintf(
          "実データの取得に失敗したため、サンプルデータにフォールバックしました: %s",
          conditionMessage(e)
        ))
        fetch_prices_sample(symbols, from, to)
      }
    )
  } else {
    fetch_prices_sample(symbols, from, to)
  }
}

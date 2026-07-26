# R/monte_carlo.R
#
# ポートフォリオ全体の将来価値を、資産間の相関を保ったままモンテカルロシミュレーションする。
#
# 手順:
#   1. 過去の日次リターンから、資産ごとの平均リターンと共分散行列を推定する。
#   2. 共分散行列をコレスキー分解し、標準正規乱数に掛けることで「相関を保った」
#      日次リターンの乱数を大量に生成する（多変量正規分布からのサンプリング）。
#   3. 各シミュレーションについて、保有比率で加重平均した日次ポートフォリオリターンを
#      毎日複利計算し、評価額のパスを作る（毎日、目標比率にリバランスされる前提）。

#' 相関を保った将来のポートフォリオ評価額パスをシミュレーションする
#'
#' @param returns_wide wide形式の日次リターン tibble(date, 銘柄1, 銘柄2, ...)
#' @param weights 銘柄名をキーとした保有比率の名前付きnumericベクトル（合計1でなくても正規化される）
#' @param n_days シミュレーションする日数
#' @param n_sims シミュレーションの試行回数
#' @param initial_value 初期評価額
#' @param seed 乱数シード（NULLなら固定しない）
#' @return (n_days + 1) 行 x n_sims 列の行列。1行目はすべてinitial_value。
simulate_portfolio_paths <- function(returns_wide, weights, n_days = 252, n_sims = 1000,
                                      initial_value = 1000000, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  asset_cols <- names(weights)
  missing_cols <- setdiff(asset_cols, names(returns_wide))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "リターンデータに存在しない銘柄が指定されました: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }

  returns_matrix <- as.matrix(returns_wide[, asset_cols, drop = FALSE])
  n_assets <- length(asset_cols)

  mu <- colMeans(returns_matrix, na.rm = TRUE)
  sigma <- stats::cov(returns_matrix, use = "pairwise.complete.obs")
  # 数値誤差でわずかに非正定値になることがあるため、対角に微小値を足して安定化する
  sigma <- sigma + diag(1e-10, n_assets)

  chol_upper <- tryCatch(
    chol(sigma),
    error = function(e) {
      stop("共分散行列のコレスキー分解に失敗しました。対象期間のデータ点数が銘柄数に対して少なすぎる可能性があります。")
    }
  )

  w <- as.numeric(weights[asset_cols])
  w <- w / sum(w)

  paths <- matrix(NA_real_, nrow = n_days + 1, ncol = n_sims)
  paths[1, ] <- initial_value

  for (sim in seq_len(n_sims)) {
    z <- matrix(stats::rnorm(n_days * n_assets), nrow = n_days, ncol = n_assets)
    # z %*% chol_upper の各行は、共分散sigmaを持つ多変量正規分布からのサンプルになる
    correlated_shocks <- z %*% chol_upper
    daily_asset_returns <- sweep(correlated_shocks, 2, mu, FUN = "+")
    daily_portfolio_returns <- as.numeric(daily_asset_returns %*% w)

    paths[-1, sim] <- initial_value * cumprod(1 + daily_portfolio_returns)
  }

  paths
}

#' シミュレーション結果のパス行列を、ファンチャート用のパーセンタイル系列や
#' 損失確率などの要約統計量に変換する
#'
#' @param paths simulate_portfolio_paths() が返す行列（(n_days+1) x n_sims）
#' @param initial_value 初期評価額
#' @param confidence VaR算出に使う信頼水準
#' @return list(fan_chart, prob_loss, simulated_var_value, simulated_var_pct, final_values)
summarize_simulation <- function(paths, initial_value, confidence = 0.95) {
  quantile_probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)

  fan_rows <- lapply(seq_len(nrow(paths)), function(row_idx) {
    values <- paths[row_idx, ]
    q <- stats::quantile(values, probs = quantile_probs, na.rm = TRUE)
    tibble::tibble(
      day = row_idx - 1,
      p05 = unname(q["5%"]),
      p25 = unname(q["25%"]),
      p50 = unname(q["50%"]),
      p75 = unname(q["75%"]),
      p95 = unname(q["95%"])
    )
  })
  fan_chart <- dplyr::bind_rows(fan_rows)

  final_values <- paths[nrow(paths), ]
  prob_loss <- mean(final_values < initial_value)

  loss_quantile_prob <- 1 - confidence
  simulated_var_value <- as.numeric(stats::quantile(final_values, probs = loss_quantile_prob, na.rm = TRUE))
  simulated_var_pct <- (initial_value - simulated_var_value) / initial_value

  list(
    fan_chart = fan_chart,
    prob_loss = prob_loss,
    simulated_var_value = simulated_var_value,
    simulated_var_pct = simulated_var_pct,
    final_values = final_values
  )
}

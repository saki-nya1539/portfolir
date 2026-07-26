# tests/testthat/test-risk_metrics.R
# R/risk_metrics.R のリスク指標計算ロジックに対する単体テスト。

source(testthat::test_path("..", "..", "R", "risk_metrics.R"))

test_that("annualize_return computes correct CAGR for a constant daily return", {
  daily <- rep(0.0004, 252)
  result <- annualize_return(daily, periods_per_year = 252)
  expected <- (1.0004)^252 - 1
  expect_equal(result, expected, tolerance = 1e-8)
})

test_that("annualize_volatility scales sd by sqrt(periods_per_year)", {
  set.seed(1)
  daily <- rnorm(500, mean = 0, sd = 0.01)
  result <- annualize_volatility(daily, periods_per_year = 252)
  expected <- sd(daily) * sqrt(252)
  expect_equal(result, expected, tolerance = 1e-8)
})

test_that("sharpe_ratio matches manual (annual_return - rf) / annual_vol", {
  set.seed(4)
  daily <- rnorm(500, mean = 0.0006, sd = 0.011)
  rf <- 0.01
  result <- sharpe_ratio(daily, risk_free_rate = rf, periods_per_year = 252)
  expected <- (annualize_return(daily) - rf) / annualize_volatility(daily)
  expect_equal(result, expected, tolerance = 1e-8)
})

test_that("max_drawdown detects a known drawdown pattern (100 -> 120 -> 90 -> 110)", {
  prices <- c(100, 120, 90, 110)
  daily_returns <- prices[-1] / prices[-length(prices)] - 1
  result <- max_drawdown(daily_returns)
  # ピーク120からの最大下落は (90 - 120) / 120 = -0.25
  expect_equal(result, 0.25, tolerance = 1e-8)
})

test_that("historical_var returns the negative of the (1-confidence) quantile", {
  set.seed(2)
  daily <- rnorm(1000, mean = 0.0005, sd = 0.01)
  result <- historical_var(daily, confidence = 0.95)
  expect_true(result > 0)
  expect_equal(result, -as.numeric(quantile(daily, 0.05)), tolerance = 1e-8)
})

test_that("parametric_var matches the manual normal-distribution formula", {
  set.seed(3)
  daily <- rnorm(1000, mean = 0.0003, sd = 0.012)
  result <- parametric_var(daily, confidence = 0.99)
  mu <- mean(daily)
  sigma <- sd(daily)
  expected <- -(mu + qnorm(0.01) * sigma)
  expect_equal(result, expected, tolerance = 1e-8)
})

test_that("calc_portfolio_returns normalizes weights and computes the weighted sum", {
  returns_wide <- tibble::tibble(
    date = as.Date("2024-01-01") + 0:2,
    A = c(0.01, -0.02, 0.03),
    B = c(-0.01, 0.02, 0.00)
  )
  weights <- c(A = 2, B = 2)  # 正規化されて 0.5 / 0.5 になるはず
  result <- calc_portfolio_returns(returns_wide, weights)
  expected <- 0.5 * returns_wide$A + 0.5 * returns_wide$B
  expect_equal(result$portfolio_return, expected, tolerance = 1e-8)
})

test_that("calc_portfolio_returns errors when a weighted ticker is missing from the data", {
  returns_wide <- tibble::tibble(date = as.Date("2024-01-01"), A = 0.01)
  weights <- c(A = 1, Z = 1)
  expect_error(calc_portfolio_returns(returns_wide, weights), "存在しない銘柄")
})

test_that("correlation_matrix returns 1 on the diagonal", {
  set.seed(6)
  returns_wide <- tibble::tibble(
    date = as.Date("2024-01-01") + 0:9,
    A = rnorm(10), B = rnorm(10)
  )
  m <- correlation_matrix(returns_wide)
  expect_equal(unname(diag(m)), c(1, 1), tolerance = 1e-8)
})

test_that("calc_returns drops the first unobservable row and computes simple returns", {
  wide_prices <- tibble::tibble(
    date = as.Date("2024-01-01") + 0:2,
    A = c(100, 110, 99)
  )
  result <- calc_returns(wide_prices)
  expect_equal(nrow(result), 2)
  expect_equal(result$A, c(0.10, 99 / 110 - 1), tolerance = 1e-8)
})

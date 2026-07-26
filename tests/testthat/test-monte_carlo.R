# tests/testthat/test-monte_carlo.R
# R/monte_carlo.R のモンテカルロシミュレーションロジックに対する単体テスト。

source(testthat::test_path("..", "..", "R", "monte_carlo.R"))

test_that("simulate_portfolio_paths returns a matrix with the correct dimensions", {
  set.seed(1)
  returns_wide <- tibble::tibble(
    date = as.Date("2024-01-01") + 0:99,
    A = rnorm(100, 0.0005, 0.01),
    B = rnorm(100, 0.0003, 0.012)
  )
  weights <- c(A = 0.6, B = 0.4)
  paths <- simulate_portfolio_paths(returns_wide, weights, n_days = 20, n_sims = 50,
                                     initial_value = 1000, seed = 42)
  expect_equal(dim(paths), c(21, 50))
  expect_true(all(paths[1, ] == 1000))
})

test_that("simulate_portfolio_paths is reproducible with the same seed", {
  returns_wide <- tibble::tibble(
    date = as.Date("2024-01-01") + 0:99,
    A = rnorm(100), B = rnorm(100)
  )
  weights <- c(A = 0.5, B = 0.5)
  p1 <- simulate_portfolio_paths(returns_wide, weights, n_days = 10, n_sims = 5,
                                  initial_value = 1000, seed = 7)
  p2 <- simulate_portfolio_paths(returns_wide, weights, n_days = 10, n_sims = 5,
                                  initial_value = 1000, seed = 7)
  expect_equal(p1, p2)
})

test_that("simulate_portfolio_paths errors when a weighted ticker is missing from the data", {
  returns_wide <- tibble::tibble(date = as.Date("2024-01-01") + 0:9, A = rnorm(10))
  weights <- c(A = 0.5, Z = 0.5)
  expect_error(
    simulate_portfolio_paths(returns_wide, weights, n_days = 5, n_sims = 5),
    "存在しない銘柄"
  )
})

test_that("summarize_simulation computes probability of loss correctly for a known 2-path case", {
  # 2本のシミュレーションパス：1本は必ず得（1000->1200）、1本は必ず損（1000->800）
  paths <- rbind(
    c(1000, 1000), # day 0
    c(1200, 800)   # day 1（最終日）
  )
  result <- summarize_simulation(paths, initial_value = 1000, confidence = 0.95)
  expect_equal(result$prob_loss, 0.5)
  expect_equal(nrow(result$fan_chart), 2)
  expect_equal(result$fan_chart$day, c(0, 1))
})

test_that("summarize_simulation reports zero loss probability when all paths gain", {
  paths <- rbind(
    c(1000, 1000, 1000),
    c(1100, 1200, 1050)
  )
  result <- summarize_simulation(paths, initial_value = 1000, confidence = 0.95)
  expect_equal(result$prob_loss, 0)
})

# tests/testthat/test-data_source.R
# R/data_source.R のデータ取得モジュールに対する単体テスト。
# ネットワークに依存する fetch_prices_online() 自体はテスト対象外とし、
# オフラインで完結する fetch_prices_sample() / fetch_prices() を検証する。

source(testthat::test_path("..", "..", "R", "data_source.R"))

test_that("fetch_prices_sample returns only the requested symbols, on business days", {
  result <- fetch_prices_sample(c("AAA", "BBB"), from = "2024-01-01", to = "2024-01-31", seed = 1)
  expect_setequal(unique(result$symbol), c("AAA", "BBB"))
  expect_true(all(!as.POSIXlt(result$date)$wday %in% c(0, 6)))
  expect_true(all(result$adjusted > 0))
})

test_that("fetch_prices_sample is reproducible for the same symbol and seed", {
  r1 <- fetch_prices_sample("XYZ", from = "2024-01-01", to = "2024-03-01", seed = 5)
  r2 <- fetch_prices_sample("XYZ", from = "2024-01-01", to = "2024-03-01", seed = 5)
  expect_equal(r1$adjusted, r2$adjusted)
})

test_that("fetch_prices_sample produces different paths for different symbols", {
  result <- fetch_prices_sample(c("AAA", "BBB"), from = "2024-01-01", to = "2024-06-01", seed = 1)
  aaa <- result$adjusted[result$symbol == "AAA"]
  bbb <- result$adjusted[result$symbol == "BBB"]
  expect_false(isTRUE(all.equal(aaa, bbb)))
})

test_that("fetch_prices routes to sample data when source = 'sample'", {
  result <- fetch_prices(c("TEST"), from = "2024-01-01", to = "2024-01-10", source = "sample")
  expect_true(nrow(result) > 0)
  expect_equal(unique(result$symbol), "TEST")
})

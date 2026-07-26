# run_tests.R
#
# PortfoliRの単体テストを実行するスクリプト。プロジェクトのルートディレクトリで
# 以下のいずれかの方法で実行してください。
#
#   Rscript run_tests.R
#
# または RStudio のコンソールで
#
#   source("run_tests.R")

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("testthatパッケージがインストールされていません。先に install_packages.R を実行してください。")
}

results <- testthat::test_dir("tests/testthat", reporter = "summary")

# install_packages.R
#
# PortfoliRの実行に必要なパッケージをまとめてインストールするスクリプト。
# R または RStudio のコンソールで、プロジェクトのルートディレクトリで一度だけ実行してください。
#
#   source("install_packages.R")
#
# 前提: R 4.1 以降（ネイティブパイプ |> を使用しています）

required_packages <- c(
  "shiny",       # Webアプリフレームワーク
  "bslib",       # Bootstrap 5ベースのモダンなUIコンポーネント
  "dplyr",       # データ操作
  "tidyr",       # wide/long変換
  "tibble",      # tibble形式のデータフレーム
  "ggplot2",     # グラフ作成
  "plotly",      # ggplotをインタラクティブ化
  "DT",          # インタラクティブなテーブル表示
  "quantmod",    # Yahoo Financeからの株価データ取得
  "zoo",         # quantmodが内部で使う時系列インデックス
  "rmarkdown",   # レポート(HTML)生成
  "knitr",       # レポート内の表組み生成
  "testthat"     # 単体テスト
)

installed <- rownames(utils::installed.packages())
to_install <- setdiff(required_packages, installed)

if (length(to_install) > 0) {
  message("以下のパッケージをインストールします: ", paste(to_install, collapse = ", "))
  utils::install.packages(to_install)
} else {
  message("必要なパッケージはすべてインストール済みです。")
}

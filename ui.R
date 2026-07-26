# ui.R
#
# PortfoliR のUI定義。bslib(Bootstrap 5)ベースのモダンなレイアウトを使用。
# 左サイドバーが全タブで共通の入力パネル、右側がタブ切り替え式のメインコンテンツ。

ui <- bslib::page_sidebar(
  title = "PortfoliR — ポートフォリオ・リスク分析ダッシュボード",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#4F46E5"),
  fillable = FALSE,

  sidebar = bslib::sidebar(
    width = 340,
    open = "desktop",

    shiny::h5("ポートフォリオ設定"),
    shiny::textAreaInput(
      "tickers_input", "ティッカーシンボル（カンマ区切り）",
      value = paste(DEFAULT_TICKERS, collapse = ", "), rows = 2
    ),
    shiny::textAreaInput(
      "weights_input", "保有比率（カンマ区切り、合計は自動正規化）",
      value = paste(DEFAULT_WEIGHTS, collapse = ", "), rows = 1
    ),
    shiny::dateRangeInput(
      "date_range", "取得期間",
      start = DEFAULT_FROM_DATE, end = DEFAULT_TO_DATE
    ),
    shiny::radioButtons(
      "data_source", "データソース",
      choices = c(
        "サンプルデータ（オフライン・推奨）" = "sample",
        "Yahoo Financeから取得" = "online"
      ),
      selected = "sample"
    ),
    shiny::numericInput(
      "initial_value", "初期投資額（円）",
      value = DEFAULT_INITIAL_VALUE, min = 10000, step = 10000
    ),
    shiny::sliderInput(
      "risk_free_rate_pct", "無リスク金利（年率 %）",
      min = 0, max = 5, value = DEFAULT_RISK_FREE_RATE_PCT, step = 0.1
    ),
    shiny::sliderInput(
      "confidence", "VaR信頼水準",
      min = 0.90, max = 0.99, value = DEFAULT_CONFIDENCE, step = 0.01
    ),

    shiny::hr(),
    shiny::h5("モンテカルロ設定"),
    shiny::numericInput("sim_days", "シミュレーション日数", value = DEFAULT_SIM_DAYS, min = 30, max = 756, step = 1),
    shiny::numericInput("sim_count", "試行回数", value = DEFAULT_SIM_COUNT, min = 100, max = 5000, step = 100),

    shiny::actionButton(
      "run_analysis", "分析を実行",
      class = "btn-primary w-100", icon = shiny::icon("play")
    ),
    shiny::helpText("入力を変更したら「分析を実行」を押してください。"),

    shiny::hr(),
    shiny::tags$small(
      class = "text-muted",
      "※本アプリの分析結果はデモ・教育目的のシミュレーションです。投資助言ではありません。実際の投資判断はご自身の責任で行ってください。"
    )
  ),

  bslib::navset_card_tab(
    bslib::nav_panel(
      "ダッシュボード",
      shiny::uiOutput("dashboard_alert"),
      shiny::fluidRow(
        shiny::column(4, shiny::uiOutput("vb_annual_return")),
        shiny::column(4, shiny::uiOutput("vb_annual_vol")),
        shiny::column(4, shiny::uiOutput("vb_sharpe"))
      ),
      shiny::fluidRow(
        shiny::column(4, shiny::uiOutput("vb_max_dd")),
        shiny::column(4, shiny::uiOutput("vb_hist_var")),
        shiny::column(4, shiny::uiOutput("vb_param_var"))
      ),
      bslib::card(
        bslib::card_header("評価額の推移（過去実績ベース）"),
        plotly::plotlyOutput("cum_value_chart", height = "360px")
      ),
      bslib::card(
        bslib::card_header("銘柄別サマリー"),
        DT::DTOutput("asset_summary_table")
      )
    ),

    bslib::nav_panel(
      "相関",
      bslib::card(
        bslib::card_header("銘柄間の相関ヒートマップ"),
        plotly::plotlyOutput("correlation_heatmap", height = "480px")
      )
    ),

    bslib::nav_panel(
      "モンテカルロシミュレーション",
      shiny::fluidRow(
        shiny::column(4, shiny::uiOutput("vb_prob_loss")),
        shiny::column(4, shiny::uiOutput("vb_sim_var")),
        shiny::column(4, shiny::uiOutput("vb_sim_median"))
      ),
      bslib::card(
        bslib::card_header("将来シナリオのファンチャート（5%〜95%区間）"),
        plotly::plotlyOutput("fan_chart", height = "420px")
      ),
      bslib::card(
        bslib::card_header("シミュレーション終了時点の評価額分布"),
        plotly::plotlyOutput("final_value_histogram", height = "320px")
      )
    ),

    bslib::nav_panel(
      "レポート",
      bslib::card(
        bslib::card_header("分析レポートのダウンロード"),
        shiny::p("現在の分析結果をもとに、HTML形式のレポートを生成します。"),
        shiny::downloadButton("download_report", "レポートをダウンロード（HTML）", class = "btn-primary"),
        shiny::hr(),
        shiny::tags$small(
          class = "text-muted",
          "本アプリの分析結果はデモ・教育目的のシミュレーションです。投資助言ではありません。実際の投資判断はご自身の責任で行ってください。"
        )
      )
    )
  )
)


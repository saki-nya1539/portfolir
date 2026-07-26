# server.R
#
# PortfoliR のサーバーロジック。
# 入力パース -> データ取得 -> リスク指標計算 -> モンテカルロシミュレーション -> 各出力の描画、
# という一直線のパイプラインを analysis() という1つの eventReactive にまとめている。

server <- function(input, output, session) {

  # ---- 入力のパース ----

  parsed_tickers <- shiny::reactive({
    input$tickers_input |>
      strsplit(",") |>
      unlist() |>
      trimws() |>
      toupper() |>
      (\(x) x[nzchar(x)])()
  })

  parsed_weights <- shiny::reactive({
    tickers <- parsed_tickers()
    w <- input$weights_input |> strsplit(",") |> unlist() |> trimws()
    w <- suppressWarnings(as.numeric(w))

    if (length(tickers) == 0) {
      return(stats::setNames(numeric(0), character(0)))
    }

    if (length(w) != length(tickers) || any(is.na(w)) || sum(w, na.rm = TRUE) <= 0) {
      # 比率の指定が銘柄数と合わない・不正な場合は均等配分にフォールバックする
      w <- rep(1, length(tickers))
    }

    stats::setNames(w / sum(w), tickers)
  })

  # ---- メインの分析パイプライン ----

  analysis <- shiny::eventReactive(input$run_analysis, {
    tickers <- parsed_tickers()
    shiny::validate(
      shiny::need(length(tickers) >= 1, "ティッカーシンボルを1つ以上入力してください。"),
      shiny::need(
        input$date_range[2] > input$date_range[1],
        "取得期間の終了日は開始日より後にしてください。"
      )
    )

    weights <- parsed_weights()
    confidence <- input$confidence
    initial_value <- input$initial_value

    shiny::withProgress(message = "分析を実行しています...", value = 0, {
      shiny::incProgress(0.2, detail = "価格データを取得中")
      prices_long <- fetch_prices(
        symbols = tickers,
        from = input$date_range[1],
        to = input$date_range[2],
        source = input$data_source
      )

      shiny::incProgress(0.25, detail = "リターンを計算中")
      wide_prices <- prices_to_wide(prices_long)
      returns_wide <- calc_returns(wide_prices)

      shiny::validate(
        shiny::need(
          nrow(returns_wide) >= 10,
          "リターン計算に十分なデータ点数がありません。取得期間を長くしてください。"
        )
      )

      portfolio_returns <- calc_portfolio_returns(returns_wide, weights)
      daily_returns <- portfolio_returns$portfolio_return

      shiny::incProgress(0.2, detail = "リスク指標を計算中")
      metrics <- list(
        annual_return = annualize_return(daily_returns),
        annual_vol = annualize_volatility(daily_returns),
        sharpe = sharpe_ratio(daily_returns, risk_free_rate = input$risk_free_rate_pct / 100),
        max_dd = max_drawdown(daily_returns),
        hist_var = historical_var(daily_returns, confidence),
        param_var = parametric_var(daily_returns, confidence)
      )
      corr_matrix <- correlation_matrix(returns_wide)

      shiny::incProgress(0.25, detail = "モンテカルロシミュレーション中")
      sim_paths <- simulate_portfolio_paths(
        returns_wide = returns_wide,
        weights = weights,
        n_days = input$sim_days,
        n_sims = input$sim_count,
        initial_value = initial_value,
        seed = 42
      )
      sim_summary <- summarize_simulation(sim_paths, initial_value, confidence)

      shiny::incProgress(0.1, detail = "完了")

      list(
        prices_long = prices_long,
        wide_prices = wide_prices,
        returns_wide = returns_wide,
        portfolio_returns = portfolio_returns,
        weights = weights,
        metrics = metrics,
        corr_matrix = corr_matrix,
        sim_paths = sim_paths,
        sim_summary = sim_summary,
        initial_value = initial_value,
        confidence = confidence
      )
    })
  })

  # ---- ダッシュボード：案内バナー ----

  output$dashboard_alert <- shiny::renderUI({
    if (is.null(input$run_analysis) || input$run_analysis == 0) {
      bslib::card(
        class = "border-info mb-3",
        bslib::card_body(
          shiny::p(
            class = "mb-0",
            "左側の設定を確認し、「分析を実行」ボタンを押してください。初期状態はオフラインのサンプルデータなので、通信環境に関わらず数秒で結果が表示されます。"
          )
        )
      )
    } else {
      NULL
    }
  })

  # ---- ダッシュボード：指標カード ----

  output$vb_annual_return <- shiny::renderUI({
    shiny::req(analysis())
    bslib::value_box(
      title = "年率リターン", value = format_percent(analysis()$metrics$annual_return),
      theme = "primary", showcase = shiny::icon("chart-line")
    )
  })

  output$vb_annual_vol <- shiny::renderUI({
    shiny::req(analysis())
    bslib::value_box(
      title = "年率ボラティリティ", value = format_percent(analysis()$metrics$annual_vol),
      theme = "secondary", showcase = shiny::icon("wave-square")
    )
  })

  output$vb_sharpe <- shiny::renderUI({
    shiny::req(analysis())
    bslib::value_box(
      title = "シャープレシオ", value = format_ratio(analysis()$metrics$sharpe),
      theme = "info", showcase = shiny::icon("scale-balanced")
    )
  })

  output$vb_max_dd <- shiny::renderUI({
    shiny::req(analysis())
    bslib::value_box(
      title = "最大ドローダウン", value = format_percent(analysis()$metrics$max_dd),
      theme = "warning", showcase = shiny::icon("arrow-trend-down")
    )
  })

  output$vb_hist_var <- shiny::renderUI({
    shiny::req(analysis())
    a <- analysis()
    bslib::value_box(
      title = sprintf("ヒストリカルVaR（%.0f%%）", a$confidence * 100),
      value = format_percent(a$metrics$hist_var),
      theme = "danger", showcase = shiny::icon("triangle-exclamation")
    )
  })

  output$vb_param_var <- shiny::renderUI({
    shiny::req(analysis())
    a <- analysis()
    bslib::value_box(
      title = sprintf("パラメトリックVaR（%.0f%%）", a$confidence * 100),
      value = format_percent(a$metrics$param_var),
      theme = "danger", showcase = shiny::icon("chart-simple")
    )
  })

  # ---- ダッシュボード：評価額推移チャート ----

  output$cum_value_chart <- plotly::renderPlotly({
    shiny::req(analysis())
    a <- analysis()

    cum_df <- a$portfolio_returns |>
      dplyr::mutate(cum_value = a$initial_value * cumprod(1 + portfolio_return))

    p <- ggplot2::ggplot(cum_df, ggplot2::aes(x = date, y = cum_value)) +
      ggplot2::geom_line(color = "#4F46E5", linewidth = 1) +
      ggplot2::geom_hline(yintercept = a$initial_value, linetype = "dashed", color = "#8A8F9E") +
      ggplot2::labs(x = NULL, y = "評価額（円）") +
      ggplot2::theme_minimal()

    plotly::ggplotly(p) |> plotly::config(displayModeBar = FALSE)
  })

  # ---- ダッシュボード：銘柄別サマリー表 ----

  output$asset_summary_table <- DT::renderDT({
    shiny::req(analysis())
    a <- analysis()

    rows <- lapply(names(a$weights), function(sym) {
      r <- a$returns_wide[[sym]]
      data.frame(
        銘柄 = sym,
        保有比率 = format_percent(a$weights[[sym]]),
        年率リターン = format_percent(annualize_return(r)),
        年率ボラティリティ = format_percent(annualize_volatility(r)),
        stringsAsFactors = FALSE
      )
    })
    summary_df <- do.call(rbind, rows)

    DT::datatable(
      summary_df,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE)
    )
  })

  # ---- 相関ヒートマップ ----

  output$correlation_heatmap <- plotly::renderPlotly({
    shiny::req(analysis())
    corr_matrix <- analysis()$corr_matrix

    corr_df <- as.data.frame(as.table(corr_matrix))
    names(corr_df) <- c("asset1", "asset2", "correlation")

    p <- ggplot2::ggplot(corr_df, ggplot2::aes(x = asset1, y = asset2, fill = correlation)) +
      ggplot2::geom_tile() +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", correlation)), size = 3) +
      ggplot2::scale_fill_gradient2(
        low = "#DC2626", mid = "white", high = "#4F46E5", midpoint = 0, limits = c(-1, 1)
      ) +
      ggplot2::labs(x = NULL, y = NULL, fill = "相関係数") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    plotly::ggplotly(p)
  })

  # ---- モンテカルロ：指標カード ----

  output$vb_prob_loss <- shiny::renderUI({
    shiny::req(analysis())
    bslib::value_box(
      title = "損失確率（期間終了時）", value = format_percent(analysis()$sim_summary$prob_loss),
      theme = "warning", showcase = shiny::icon("circle-exclamation")
    )
  })

  output$vb_sim_var <- shiny::renderUI({
    shiny::req(analysis())
    a <- analysis()
    bslib::value_box(
      title = sprintf("シミュレーションVaR（%.0f%%）", a$confidence * 100),
      value = format_percent(a$sim_summary$simulated_var_pct),
      theme = "danger", showcase = shiny::icon("dice")
    )
  })

  output$vb_sim_median <- shiny::renderUI({
    shiny::req(analysis())
    a <- analysis()
    final_median <- stats::median(a$sim_summary$final_values)
    bslib::value_box(
      title = "期間終了時の中央値評価額", value = format_currency(final_median),
      theme = "primary", showcase = shiny::icon("coins")
    )
  })

  # ---- モンテカルロ：ファンチャート ----

  output$fan_chart <- plotly::renderPlotly({
    shiny::req(analysis())
    a <- analysis()
    fan_df <- a$sim_summary$fan_chart

    p <- ggplot2::ggplot(fan_df, ggplot2::aes(x = day)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = p05, ymax = p95), fill = "#4F46E5", alpha = 0.15) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = p25, ymax = p75), fill = "#4F46E5", alpha = 0.30) +
      ggplot2::geom_line(ggplot2::aes(y = p50), color = "#4F46E5", linewidth = 1) +
      ggplot2::geom_hline(yintercept = a$initial_value, linetype = "dashed", color = "#8A8F9E") +
      ggplot2::labs(x = "経過日数", y = "ポートフォリオ評価額（円）") +
      ggplot2::theme_minimal()

    plotly::ggplotly(p) |> plotly::config(displayModeBar = FALSE)
  })

  # ---- モンテカルロ：最終評価額ヒストグラム ----

  output$final_value_histogram <- plotly::renderPlotly({
    shiny::req(analysis())
    a <- analysis()
    final_df <- data.frame(final_value = a$sim_summary$final_values)

    p <- ggplot2::ggplot(final_df, ggplot2::aes(x = final_value)) +
      ggplot2::geom_histogram(bins = 40, fill = "#4F46E5", alpha = 0.8) +
      ggplot2::geom_vline(xintercept = a$initial_value, linetype = "dashed", color = "#8A8F9E") +
      ggplot2::labs(x = "評価額（円）", y = "試行回数") +
      ggplot2::theme_minimal()

    plotly::ggplotly(p) |> plotly::config(displayModeBar = FALSE)
  })

  # ---- レポート出力 ----

  output$download_report <- shiny::downloadHandler(
    filename = function() {
      sprintf("portfolir_report_%s.html", format(Sys.Date(), "%Y%m%d"))
    },
    content = function(file) {
      shiny::req(analysis())
      a <- analysis()

      # Shinyアプリのディレクトリが書き込み不可な環境（デプロイ先など）でも失敗しないよう、
      # レポートのRmdは一時ディレクトリにコピーしてからレンダリングする（rmarkdown公式の推奨パターン）
      temp_report <- file.path(tempdir(), "report_template.Rmd")
      file.copy("report/report_template.Rmd", temp_report, overwrite = TRUE)

      cum_df <- a$portfolio_returns |>
        dplyr::mutate(cum_value = a$initial_value * cumprod(1 + portfolio_return))

      rmarkdown::render(
        input = temp_report,
        output_file = file,
        params = list(
          tickers = names(a$weights),
          weights = a$weights,
          metrics = a$metrics,
          corr_matrix = a$corr_matrix,
          cum_df = cum_df,
          fan_chart = a$sim_summary$fan_chart,
          prob_loss = a$sim_summary$prob_loss,
          initial_value = a$initial_value,
          confidence = a$confidence,
          generated_at = format(Sys.time(), "%Y-%m-%d %H:%M")
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
}

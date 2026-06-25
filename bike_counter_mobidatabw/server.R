server <- function(input, output, session) {

  # ════════════════════════════════════════════════════════════
  # 1. ARCHITECTURE — populate every Standort dropdown ONCE.
  # Previously each tab had its own observe() with a hard-coded
  # selected = "Renzstraße". Several of those were applied to
  # year-month dropdowns where that value does not exist (bug).
  # ════════════════════════════════════════════════════════════
  standort_dropdowns <- c(
    "analysis_standort", "last14_station",
    "cumulative_standort", "cumulative_partial_standort",
    "cumulative_bar_standort", "raw_standort", "monthly_bar_standort"
  )
  observe({
    sel <- pick_default(STANDORT_CHOICES)        # validated default
    for (id in standort_dropdowns) {
      updateSelectInput(session, id, choices = STANDORT_CHOICES, selected = sel)
    }
  })

  # Year-month choices (own default — NOT a Standort name) -------
  ym_choices <- sort(
    unique(sprintf("%04d-%02d", bike_counter$year, bike_counter$month)),
    decreasing = TRUE
  )
  observe({
    updateSelectInput(
      session, "monthly_standort_compare_ym",
      choices = ym_choices,
      selected = pick_default(ym_choices, ym_choices[1])   # 2. BUGFIX
    )
  })

  # ---- Übersicht ----
  output$overview_n_standorte <- renderValueBox({
    valueBox(
      value = length(unique(bike_counter$Standort)),
      subtitle = "Anzahl Standorte",
      icon = icon("map-marker"), color = "blue"
    )
  })

  output$overview_latest_obs <- renderValueBox({
    latest <- max(bike_counter$date, na.rm = TRUE)
    valueBox(
      value = format(latest, "%d.%m.%Y"),
      subtitle = "Letzte Beobachtung",
      icon = icon("calendar-check"), color = "green"
    )
  })

  output$overview_total_obs <- renderValueBox({
    valueBox(
      value = format(nrow(bike_counter), big.mark = ".", decimal.mark = ","),
      subtitle = "Gesamte Datensätze",
      icon = icon("database"), color = "purple"
    )
  })

  output$overview_standort_table <- renderDT({
    df_summary <- bike_counter %>%
      group_by(Standort) %>%
      summarise(
        Erste_Beobachtung  = format(min(date, na.rm = TRUE), "%d.%m.%Y"),
        Letzte_Beobachtung = format(max(date, na.rm = TRUE), "%d.%m.%Y"),
        Anzahl_Datensaetze = n(),
        .groups = "drop"
      ) %>%
      arrange(Standort) %>%
      rename(
        `Erste Beobachtung`  = Erste_Beobachtung,
        `Letzte Beobachtung` = Letzte_Beobachtung,
        `Anzahl Datensätze`  = Anzahl_Datensaetze
      )

    datatable(
      df_summary, rownames = FALSE,
      options = list(pageLength = 25, dom = "ft",
                     language = list(search = "Suchen"), scrollX = TRUE),
      class = "cell-border stripe compact"
    ) %>% formatStyle("Standort", fontWeight = "bold")
  })

  # ---- Karte ----
  output$site_map <- renderLeaflet({
    leaflet(data = sites_unique) %>%
      addTiles() %>%
      addMarkers(~longitude, ~latitude, label = ~name,
                 clusterOptions = markerClusterOptions())
  })

  # ---- Helper plot functions (base R) ----
  plot_yearly_bars <- function(df, title) {
    yearly_sums <- aggregate(counter ~ year, data = df, sum, na.rm = TRUE)
    par(mar = c(5, 6, 4, 2) + 0.1)
    barplot(
      height = yearly_sums$counter, names.arg = yearly_sums$year,
      xlab = "Jahr", ylab = "", main = title, col = COL_PRIMARY, yaxt = "n"
    )
    yticks <- axTicks(2)
    axis(2, at = yticks,
         labels = format(yticks, scientific = FALSE, big.mark = ".", decimal.mark = ","),
         las = 1)
  }

  plot_quarter_ts <- function(df, title) {
    df$quarter <- lubridate::quarter(df$date)
    q_avg <- aggregate(counter ~ year + quarter, data = df, FUN = sum, na.rm = TRUE)
    q_avg$quarter_start <- as.Date(paste0(q_avg$year, "-", (q_avg$quarter - 1) * 3 + 1, "-01"))
    q_avg$date_mid <- q_avg$quarter_start + 45
    q_avg <- q_avg[order(q_avg$date_mid), ]
    plot(q_avg$date_mid, q_avg$counter, type = "b",
         xlab = "Quartal", ylab = "Radfahrer pro Quartal",
         main = title, col = "darkred", pch = 19, xaxt = "n")
    axis(1, at = q_avg$date_mid,
         labels = paste0(q_avg$year, " Q", q_avg$quarter), las = 2, cex.axis = 0.8)
  }

  plot_month_ts <- function(df, title) {
    current_year  <- lubridate::year(Sys.Date())
    current_month <- lubridate::month(Sys.Date())
    df <- df[!(df$year == current_year & df$month == current_month), ]
    m_sum <- aggregate(counter ~ year + month, data = df, FUN = sum, na.rm = TRUE)
    m_sum$month_start <- as.Date(sprintf("%d-%02d-01", m_sum$year, m_sum$month))
    m_sum$date_mid <- m_sum$month_start + 14
    m_sum <- m_sum[order(m_sum$date_mid), ]
    par(mar = c(5, 6, 4, 2) + 0.1)
    plot(m_sum$date_mid, m_sum$counter, type = "l",
         xlab = "Monat", ylab = "", main = title, col = "darkgreen", lwd = 2, yaxt = "n")
    grid(nx = NA, ny = NULL, lty = 2, col = "lightgray")
    yticks <- axTicks(2)
    axis(2, at = yticks,
         labels = format(yticks, scientific = FALSE, big.mark = ".", decimal.mark = ","),
         las = 1)
  }

  # ---- Standort Analysis ----
  output$analysis_barplot <- renderPlot({
    req(input$analysis_standort)
    plot_yearly_bars(get_standort(input$analysis_standort),
                     paste("Anzahl Radfahrende -", input$analysis_standort))
  })
  output$analysis_quarter_ts <- renderPlot({
    req(input$analysis_standort)
    plot_quarter_ts(get_standort(input$analysis_standort),
                    paste("Quartalsverlauf -", input$analysis_standort))
  })
  output$analysis_month_ts <- renderPlot({
    req(input$analysis_standort)
    plot_month_ts(get_standort(input$analysis_standort),
                  paste("Monatsverlauf -", input$analysis_standort))
  })

  # ════════════════════════════════════════════════════════════
  # 1. ARCHITECTURE — single helper renders ALL cumulative line
  # charts. Collapses ~150 duplicated lines into one function.
  # ════════════════════════════════════════════════════════════
  render_cumulative_lines <- function(df, max_doy = Inf, title, x_range = NULL,
                                      tickvals = MONTH_STARTS, ticktext = MONTH_ABBR_DE) {
    daily <- compute_cumulative(df, max_doy)
    if (is.null(daily)) return(NULL)

    # order legend by final cumulative total (descending)
    totals <- tapply(daily$cumulative, daily$year, max)
    ordered_years <- names(sort(totals, decreasing = TRUE))

    fig <- plot_ly()
    for (yr in ordered_years) {
      yd <- daily[daily$year == as.integer(yr), ]
      yd <- yd[order(yd$day_of_year), ]
      if (nrow(yd) == 0) next
      fig <- add_trace(
        fig, data = yd, x = ~day_of_year, y = ~cumulative,
        type = "scatter", mode = "lines", name = yr, line = list(width = 2),
        hovertemplate = paste0("Jahr: ", yr,
                               "<br>Tag: %{x}<br>Kumuliert: %{y:,.0f}<extra></extra>")
      )
    }

    if (is.null(x_range)) x_range <- c(1, 366)
    fig %>%
      plotly::layout(
        title = title,
        xaxis = list(title = "Tag des Jahres", tickmode = "array",
                     tickvals = tickvals, ticktext = ticktext, range = x_range),
        yaxis = list(title = "Kumulierte Summe der Radfahrer", separatethousands = TRUE),
        hovermode = "closest",
        legend = list(title = list(text = "Jahr"), orientation = "v",
                      x = 0.02, y = 0.98, bgcolor = "rgba(255,255,255,0.8)",
                      bordercolor = "gray", borderwidth = 1)
      ) %>%
      style_plot()
  }

  output$cumulative_plot <- renderPlotly({
    req(input$cumulative_standort)
    render_cumulative_lines(
      get_standort(input$cumulative_standort),
      title = paste("Jahresvergleich kumulativ (gesamtes Jahr) -", input$cumulative_standort)
    )
  })

  output$cumulative_partial_plot <- renderPlotly({
    req(input$cumulative_partial_standort)
    df <- get_standort(input$cumulative_partial_standort)
    if (nrow(df) == 0) return(NULL)
    max_doy <- max(lubridate::yday(max(df$date, na.rm = TRUE)) - 1L, 1L)
    ref_date_str <- format(max(df$date, na.rm = TRUE) - 1, "%d.%m.")
    vis <- MONTH_STARTS <= max_doy
    render_cumulative_lines(
      df, max_doy = max_doy,
      title = paste0("Jahresvergleich kumulativ (bis Tag ", max_doy, " / ",
                     ref_date_str, ") - ", input$cumulative_partial_standort),
      x_range = c(1, max_doy),
      tickvals = MONTH_STARTS[vis], ticktext = MONTH_ABBR_DE[vis]
    )
  })

  output$cumulative_bar_plot <- renderPlotly({
    req(input$cumulative_bar_standort)
    df <- get_standort(input$cumulative_bar_standort)
    df <- df[!is.na(df$counter), ]
    if (nrow(df) == 0) return(NULL)

    max_date <- max(df$date, na.rm = TRUE)
    max_doy  <- max(lubridate::yday(max_date) - 1L, 1L)
    ref_date_str <- format(max_date - 1, "%d.%m.")

    year_sums <- aggregate(counter ~ year,
                           data = df[df$day_of_year <= max_doy, ], FUN = sum, na.rm = TRUE)
    year_sums <- year_sums[order(year_sums$year), ]
    year_sums$year <- as.character(year_sums$year)

    current_year <- as.character(lubridate::year(max_date))
    bar_colors <- ifelse(year_sums$year == current_year, COL_HIGHLIGHT, COL_PRIMARY)

    plot_ly(
      data = year_sums, x = ~year, y = ~counter, type = "bar",
      marker = list(color = bar_colors),
      hovertemplate = paste0("Jahr: %{x}<br>Kumuliert (bis Tag ", max_doy, " / ",
                             ref_date_str, "): %{y:,.0f}<extra></extra>")
    ) %>%
      plotly::layout(
        title = paste0("Jahresvergleich kumulativ (bis Tag ", max_doy, " / ",
                       ref_date_str, ") - ", input$cumulative_bar_standort),
        xaxis = list(title = "Jahr", type = "category"),
        yaxis = list(title = "Kumulierte Summe der Radfahrer", separatethousands = TRUE),
        hovermode = "closest"
      ) %>%
      style_plot()
  })

  # ---- Raw Data Explorer ----
  observeEvent(input$raw_standort, {
    df <- get_standort(input$raw_standort)
    if (nrow(df) > 0) {
      min_date <- min(df$date, na.rm = TRUE)
      max_date <- max(df$date, na.rm = TRUE)
      updateDateRangeInput(session, "raw_daterange",
                           start = min_date, end = max_date,
                           min = min_date, max = max_date)
    }
  })

  output$raw_timeseries <- renderPlot({
    req(input$raw_standort, input$raw_daterange)
    df <- get_standort(input$raw_standort)
    df <- df[df$date >= input$raw_daterange[1] & df$date <= input$raw_daterange[2], ]

    if (nrow(df) == 0) {
      plot.new(); text(0.5, 0.5, "Keine Daten für den ausgewählten Zeitraum", cex = 1.5)
      return()
    }

    df$ts <- as.POSIXct(df$date)
    df <- df[order(df$ts), ]
    plot(df$ts, df$counter, type = "l",
         xlab = "Jahr", ylab = "Anzahl Radfahrer (pro Stunde)",
         main = paste("Rohdaten Zeitreihe -", input$raw_standort),
         col = COL_PRIMARY, lwd = 1.5, xaxt = "n")
    grid(nx = NA, ny = NULL)
    years <- seq(lubridate::year(min(df$ts)), lubridate::year(max(df$ts)), by = 1)
    abline(v = as.POSIXct(paste0(years, "-01-01")), col = "gray40", lwd = 1, lty = 2)
    axis(1, at = as.POSIXct(paste0(years, "-01-01")), labels = years)
  })

  # ════════════════════════════════════════════════════════════
  # 1. ARCHITECTURE — build the 24-month wide table ONCE (reactive)
  # and reuse it for both the DT output and the CSV download, so
  # they can never disagree.
  # ════════════════════════════════════════════════════════════
  monthly_wide <- reactive({ build_monthly_wide(bike_counter, 24) })

  output$monthlyStationTable <- renderDT({
    wide_table <- monthly_wide()
    datatable(
      wide_table,
      extensions = c("Buttons", "FixedColumns"),   # 2. BUGFIX: dom "B" + frozen col need these
      options = list(
        pageLength = 25, scrollX = TRUE, scrollY = "600px",
        fixedColumns = list(leftColumns = 1),
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        language = list(
          search = "Suchen:", lengthMenu = "Zeige _MENU_ Einträge",
          info = "Zeige _START_ bis _END_ von _TOTAL_ Standorten",
          paginate = list(first = "Erste", last = "Letzte",
                          `next` = "Nächste", previous = "Vorherige")
        )
      ),
      rownames = FALSE, class = "cell-border stripe compact"
    ) %>%
      formatStyle(columns = names(wide_table), fontSize = "12px") %>%
      formatStyle("Standort", fontWeight = "bold", backgroundColor = "#f5f5f5")
  })

  output$downloadMonthlyTable <- downloadHandler(
    filename = function() paste0("monatsübersicht_radfahrer_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(monthly_wide(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )

  # ---- Monthly Barchart (Monatsbalken) ----
  output$monthly_bar_plot <- renderPlotly({
    req(input$monthly_bar_standort, input$monthly_bar_month)

    selected_month <- as.integer(input$monthly_bar_month)
    selected_month_label <- MONTH_FULL_DE[selected_month]

    df <- get_standort(input$monthly_bar_standort)
    if (nrow(df) == 0)
      return(plotly_empty() %>% plotly::layout(title = "Keine Daten für diesen Standort"))
    df$dom <- as.integer(format(df$date, "%d"))

    max_date      <- max(df$date, na.rm = TRUE)
    current_month <- as.integer(format(max_date, "%m"))
    cutoff_day    <- as.integer(format(max_date, "%d")) - 1L
    subtitle_txt  <- ""

    if (selected_month == current_month) {
      if (cutoff_day < 1)
        return(plotly_empty() %>% plotly::layout(
          title = paste0("Keine vollständigen Daten für ",
                         selected_month_label, " - ", input$monthly_bar_standort)))
      df <- df[df$dom <= cutoff_day, ]
      subtitle_txt <- paste0(" (nur bis zum ", cutoff_day, ". des Monats)")
    }

    df_plot <- df %>%
      filter(month == selected_month) %>%
      group_by(year) %>%
      summarise(total = sum(counter, na.rm = TRUE), .groups = "drop") %>%
      arrange(year)

    if (nrow(df_plot) == 0)
      return(plotly_empty() %>% plotly::layout(
        title = paste0("Keine Daten für ", selected_month_label,
                       " - ", input$monthly_bar_standort)))

    df_plot$year <- as.character(df_plot$year)
    plot_ly(data = df_plot, x = ~year, y = ~total, type = "bar",
            marker = list(color = COL_PRIMARY),
            hovertemplate = paste0("Jahr: %{x}<br>Monat: ", selected_month_label,
                                   "<br>Summe: %{y:,.0f}<extra></extra>")) %>%
      plotly::layout(
        title = paste0("Monatssummen - ", input$monthly_bar_standort,
                       " - ", selected_month_label, subtitle_txt),
        xaxis = list(title = "Jahr", type = "category"),
        yaxis = list(title = "Summe Radfahrende", separatethousands = TRUE)
      ) %>%
      style_plot()
  })

  # ---- Standortvergleich Monat ----
  monthly_compare_df <- reactive({
    req(input$monthly_standort_compare_ym)
    bike_counter %>%
      mutate(year_month = sprintf("%04d-%02d", year, month)) %>%
      filter(year_month == input$monthly_standort_compare_ym) %>%
      group_by(Standort) %>%
      summarise(total = sum(counter, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total))
  })

  output$monthly_standort_compare_plot <- renderPlotly({
    df <- monthly_compare_df()
    if (nrow(df) == 0)
      return(plotly_empty() %>% plotly::layout(title = "Keine Daten für diesen Monat"))
    df$Standort <- factor(df$Standort, levels = df$Standort)
    plot_ly(data = df, x = ~Standort, y = ~total, type = "bar",
            marker = list(color = COL_PRIMARY),
            hovertemplate = "Standort: %{x}<br>Summe: %{y:,.0f}<extra></extra>") %>%
      plotly::layout(
        title = paste0("Standortvergleich - ", input$monthly_standort_compare_ym),
        xaxis = list(title = "Standort", tickangle = -40),
        yaxis = list(title = "Summe Radfahrende", separatethousands = TRUE)
      ) %>%
      style_plot()
  })

  output$monthly_standort_compare_table <- renderDT({
    df <- monthly_compare_df() %>%
      transmute(Standort, `Summe Radfahrende` = round(total, -3)) %>%
      arrange(desc(`Summe Radfahrende`))
    datatable(
      df, rownames = FALSE,
      options = list(pageLength = 25, dom = "ft",
                     language = list(search = "Suchen:"), scrollX = TRUE),
      class = "cell-border stripe compact"
    ) %>%
      formatStyle("Standort", fontWeight = "bold") %>%
      formatCurrency("Summe Radfahrende", currency = "", interval = 3,
                     mark = ".", digits = 0)
  })

  # ---- Last 14 Days ----
  output$last14_plot <- renderPlot({
    req(input$last14_station)
    df_station <- get_standort(input$last14_station)
    if (nrow(df_station) == 0) return(NULL)

    daily_all <- aggregate(counter ~ year + date + day_of_year,
                           data = df_station, FUN = sum, na.rm = TRUE)
    max_date_all <- max(daily_all$date, na.rm = TRUE)
    window_dates <- seq(max_date_all - 13L, max_date_all, by = "day")
    window_md    <- format(window_dates, "%m-%d")

    years <- sort(unique(daily_all$year), decreasing = TRUE)
    if (length(years) == 0) return(NULL)

    plot_list <- list()
    for (y in years) {
      yr_data <- daily_all[daily_all$year == y, ]
      if (nrow(yr_data) == 0) next
      yr_data$md <- format(yr_data$date, "%m-%d")
      win_y <- yr_data[yr_data$md %in% window_md, ]
      if (nrow(win_y) == 0) next
      win_y$day_index <- match(win_y$md, window_md)
      win_y <- win_y[!is.na(win_y$day_index), ]
      if (nrow(win_y) > 0) plot_list[[as.character(y)]] <- win_y[order(win_y$day_index), ]
    }
    if (length(plot_list) == 0) return(NULL)

    year_stats <- do.call(rbind, lapply(names(plot_list), function(y) {
      d <- plot_list[[y]]
      last_row <- d[d$day_index == max(d$day_index), ]
      data.frame(year = y, last_value = max(last_row$counter, na.rm = TRUE))
    }))
    ordered_years <- year_stats[order(-year_stats$last_value), ]$year

    max_y <- max(vapply(plot_list, function(d) max(d$counter, na.rm = TRUE), numeric(1)))
    x_labels <- paste0(1:14, "\n", format(window_dates, "%d.%m."))

    par(mar = c(6, 6, 4, 2) + 0.1)
    plot(NA, NA, xlim = c(1, 14), ylim = c(0, max_y),
         xlab = "", ylab = "Summe Radfahrende pro Tag",
         main = paste("Letzte verfügbare 14 Tage -", input$last14_station), xaxt = "n")
    axis(1, at = 1:14, labels = x_labels, padj = 0.5)
    mtext("Tag im 14-Tage-Fenster (mit Datum des akt. Jahres)", side = 1, line = 4)

    cols <- rainbow(length(ordered_years))
    for (i in seq_along(ordered_years)) {
      d <- plot_list[[ordered_years[i]]]
      lines(d$day_index, d$counter, type = "b", col = cols[i], pch = 19)
    }
    legend("topleft", legend = ordered_years, col = cols, lty = 1, pch = 19,
           title = "Jahr", bg = "white")
  })

  # ---- Databricks Job Status ----
  job_status_data <- reactive({
    fetch_last_job_run_log()
  }) |> bindCache(as.integer(Sys.time()) %/% 300L)

  output$job_status_table <- renderUI({
    job <- job_status_data()
    if (is.null(job) || (!is.null(job$error) && nchar(job$error) > 0)) {
      return(tags$div(
        tags$p("Kein Job-Status verfügbar (Fehler):"),
        tags$pre(if (is.null(job)) "job == NULL" else job$error)
      ))
    }
    status_val  <- paste(job$life_state, job$result_state)
    ok          <- isTRUE(job$result_state == "SUCCESS")
    status_col  <- if (ok) "#2e7d32" else "#c62828"
    status_icon <- if (ok) icon("check-circle") else icon("times-circle")
    safe_val <- function(x) if (is.null(x) || x == "") "—" else x

    rows <- list(
      list("Job", safe_val(job$job_name)),
      list("Status", tags$span(style = paste0("color:", status_col, "; font-weight:bold;"),
                               status_icon, " ", status_val)),
      list("Start", job$start_time),
      list("Ende", job$end_time),
      list("Dauer (s)", safe_val(job$duration_s)),
      list("Max Data TS", job$max_data_ts),
      list("Rows added (last run)", safe_val(job$rows_added)),
      list("Rows total", safe_val(job$rows_total)),
      list("Written at", job$written_at),
      list("Error", if (!is.null(job$error_msg) && nchar(job$error_msg) > 0) job$error_msg else "—")
    )
    tbl_rows <- lapply(rows, function(r) {
      tags$tr(
        tags$td(style = "font-weight:bold; padding: 6px 16px 6px 8px; white-space:nowrap;", r[[1]]),
        tags$td(r[[2]])
      )
    })
    tags$table(style = "border-collapse: collapse;", do.call(tagList, tbl_rows))
  })

  # ---- Stationsvergleich ----
  output$station_compare_plot <- renderPlotly({
    req(input$compare_daterange)
    df <- bike_counter[bike_counter$date >= input$compare_daterange[1] &
                         bike_counter$date <= input$compare_daterange[2], ]
    if (nrow(df) == 0)
      return(plotly_empty() %>% plotly::layout(title = "Keine Daten für den ausgewählten Zeitraum"))

    date_label <- paste0(format(input$compare_daterange[1], "%d.%m.%Y"), " – ",
                         format(input$compare_daterange[2], "%d.%m.%Y"))
    agg <- input$compare_aggregation

    if (agg == "total") {
      df_agg <- aggregate(counter ~ Standort, data = df, FUN = sum, na.rm = TRUE)
      df_agg <- df_agg[order(-df_agg$counter), ]
      df_agg$Standort <- factor(df_agg$Standort, levels = df_agg$Standort)
      fig <- plot_ly(data = df_agg, x = ~Standort, y = ~counter, type = "bar",
                     marker = list(color = COL_PRIMARY),
                     hovertemplate = "Standort: %{x}<br>Summe: %{y:,.0f}<extra></extra>") %>%
        plotly::layout(title = paste0("Stationsvergleich: ", date_label),
                       xaxis = list(title = "Standort", tickangle = -40),
                       yaxis = list(title = "Summe Radfahrende", separatethousands = TRUE))
    } else if (agg == "year") {
      df$yr <- as.character(df$year)
      df_agg <- aggregate(counter ~ Standort + yr, data = df, FUN = sum, na.rm = TRUE)
      fig <- plot_ly(data = df_agg, x = ~Standort, y = ~counter, color = ~yr, type = "bar",
                     hovertemplate = "Standort: %{x}<br>Summe: %{y:,.0f}<extra></extra>") %>%
        plotly::layout(barmode = "group",
                       title = paste0("Stationsvergleich nach Jahr: ", date_label),
                       xaxis = list(title = "Standort", tickangle = -40),
                       yaxis = list(title = "Summe Radfahrende", separatethousands = TRUE),
                       legend = list(title = list(text = "Jahr")))
    } else {
      df$period <- format(df$date, "%Y-%m")
      df_agg <- aggregate(counter ~ Standort + period, data = df, FUN = sum, na.rm = TRUE)
      df_agg <- df_agg[order(df_agg$period), ]
      fig <- plot_ly(data = df_agg, x = ~Standort, y = ~counter, color = ~period, type = "bar",
                     hovertemplate = "Standort: %{x}<br>Monat: %{legendgroup}<br>Summe: %{y:,.0f}<extra></extra>") %>%
        plotly::layout(barmode = "group",
                       title = paste0("Stationsvergleich nach Monat: ", date_label),
                       xaxis = list(title = "Standort", tickangle = -40),
                       yaxis = list(title = "Summe Radfahrende", separatethousands = TRUE),
                       legend = list(title = list(text = "Monat")))
    }
    style_plot(fig)
  })
}

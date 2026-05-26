server <- function(input, output, session) {
  
  # ---- Übersicht ----
  
  output$overview_n_standorte <- renderValueBox({
    n <- length(unique(bike_counter$Standort))
    valueBox(
      value = n,
      subtitle = "Anzahl Standorte",
      icon = icon("map-marker"),
      color = "blue"
    )
  })
  
  output$overview_latest_obs <- renderValueBox({
    latest <- max(as.Date(bike_counter$date), na.rm = TRUE)
    valueBox(
      value = format(latest, "%d.%m.%Y"),
      subtitle = "Letzte Beobachtung",
      icon = icon("calendar-check"),
      color = "green"
    )
  })
  
  output$overview_total_obs <- renderValueBox({
    n <- format(nrow(bike_counter), big.mark = ".", decimal.mark = ",")
    valueBox(
      value = n,
      subtitle = "Gesamte Datensätze",
      icon = icon("database"),
      color = "purple"
    )
  })
  
  output$overview_standort_table <- renderDT({
    df_summary <- bike_counter %>%
      group_by(Standort) %>%
      summarise(
        Erste_Beobachtung = format(min(as.Date(date), na.rm = TRUE), "%d.%m.%Y"),
        Letzte_Beobachtung = format(max(as.Date(date), na.rm = TRUE), "%d.%m.%Y"),
        Anzahl_Datensaetze = n(),
        .groups = "drop"
      ) %>%
      arrange(Standort) %>%
      rename(
        `Erste Beobachtung` = Erste_Beobachtung,
        `Letzte Beobachtung` = Letzte_Beobachtung,
        `Anzahl Datensätze` = Anzahl_Datensaetze
      )
    
    datatable(
      df_summary,
      rownames = FALSE,
      options = list(
        pageLength = 25,
        dom = "ft",
        language = list(search = "Suchen"),
        scrollX = TRUE
      ),
      class = "cell-border stripe compact"
    ) %>%
      formatStyle("Standort", fontWeight = "bold")
  })
  
  
  # ---- Karte ----
  output$site_map <- renderLeaflet({
    leaflet(data = sites_unique) %>%
      addTiles() %>%
      addMarkers(
        ~longitude, ~latitude,
        label = ~name,
        clusterOptions = markerClusterOptions()
      )
  })
  
  # ---- Helper plot functions (base R) ----
  plot_yearly_bars <- function(df, title) {
    df$year <- lubridate::year(df$date)
    df$month <- lubridate::month(df$date)
    df_sub <- df
    yearly_sums <- aggregate(counter ~ year, data = df_sub, sum, na.rm = TRUE)
    par(mar = c(5, 6, 4, 2) + 0.1)
    barplot(
      height = yearly_sums$counter,
      names.arg = yearly_sums$year,
      xlab = "Jahr",
      ylab = "",
      main = title,
      col = "steelblue",
      yaxt = "n"
    )
    yticks <- axTicks(2)
    axis(
      2,
      at = yticks,
      labels = format(yticks, scientific = FALSE, big.mark = ".", decimal.mark = ","),
      las = 1
    )
  }
  
  plot_quarter_ts <- function(df, title) {
    df$date <- as.POSIXct(df$date)
    df$year <- lubridate::year(df$date)
    df$quarter <- lubridate::quarter(df$date)
    q_avg <- aggregate(counter ~ year + quarter, data = df, FUN = sum, na.rm = TRUE)
    q_avg$quarter_start <- as.Date(paste0(q_avg$year, "-", (q_avg$quarter - 1) * 3 + 1, "-01"))
    q_avg$date_mid <- q_avg$quarter_start + 45
    q_avg <- q_avg[order(q_avg$date_mid), ]
    plot(
      q_avg$date_mid, q_avg$counter,
      type = "b",
      xlab = "Quartal",
      ylab = "Radfahrer pro Quartal",
      main = title,
      col = "darkred",
      pch = 19,
      xaxt = "n"
    )
    quarter_labels <- paste0(q_avg$year, " Q", q_avg$quarter)
    axis(1, at = q_avg$date_mid, labels = quarter_labels, las = 2, cex.axis = 0.8)
  }
  
  plot_month_ts <- function(df, title) {
    df$date <- as.POSIXct(df$date)
    df$year <- lubridate::year(df$date)
    df$month <- lubridate::month(df$date)
    current_year <- lubridate::year(Sys.Date())
    current_month <- lubridate::month(Sys.Date())
    df <- df[!(df$year == current_year & df$month == current_month), ]
    m_sum <- aggregate(counter ~ year + month, data = df, FUN = sum, na.rm = TRUE)
    m_sum$month_start <- as.Date(sprintf("%d-%02d-01", m_sum$year, m_sum$month))
    m_sum$date_mid <- m_sum$month_start + 14
    m_sum <- m_sum[order(m_sum$date_mid), ]
    par(mar = c(5, 6, 4, 2) + 0.1)
    plot(
      m_sum$date_mid, m_sum$counter,
      type = "l",
      xlab = "Monat",
      ylab = "",
      main = title,
      col = "darkgreen",
      lwd = 2,
      yaxt = "n"
    )
    grid(nx = NA, ny = NULL, lty = 2, col = "lightgray")
    yticks <- axTicks(2)
    axis(
      2,
      at = yticks,
      labels = format(yticks, scientific = FALSE, big.mark = ".", decimal.mark = ","),
      las = 1
    )
  }
  
  # ---- Standort Analysis ----
  observe({
    standort_choices <- unique(bike_counter$Standort)
    updateSelectInput(session, "analysis_standort", choices = standort_choices)
  })
  
  output$analysis_barplot <- renderPlot({
    req(input$analysis_standort)
    df_selected <- subset(bike_counter, Standort == input$analysis_standort)
    plot_yearly_bars(df_selected, paste("Anzahl Radfahrende -", input$analysis_standort))
  })
  
  output$analysis_quarter_ts <- renderPlot({
    req(input$analysis_standort)
    df_selected <- subset(bike_counter, Standort == input$analysis_standort)
    plot_quarter_ts(df_selected, paste("Quartalsverlauf -", input$analysis_standort))
  })
  
  output$analysis_month_ts <- renderPlot({
    req(input$analysis_standort)
    df_selected <- subset(bike_counter, Standort == input$analysis_standort)
    plot_month_ts(df_selected, paste("Monatsverlauf -", input$analysis_standort))
  })
  
  # ---- Cumulative Year Comparison (full year, interactive Plotly) ----
  observe({
    standort_choices <- unique(bike_counter$Standort)
    updateSelectInput(session, "cumulative_standort", choices = standort_choices)
  })
  
  observe({
    standort_choices <- unique(bike_counter$Standort)
    updateSelectInput(session, "last14_station", choices = standort_choices)
  })
  
  output$cumulative_plot <- renderPlotly({
    req(input$cumulative_standort)
    
    df_standort <- subset(bike_counter, Standort == input$cumulative_standort)
    df_standort$date <- as.POSIXct(df_standort$date)
    df_standort$year <- lubridate::year(df_standort$date)
    df_standort$day_of_year <- lubridate::yday(df_standort$date)
    
    if (nrow(df_standort) == 0) return(NULL)
    df_standort <- df_standort[!is.na(df_standort$counter), ]
    
    daily_sums <- aggregate(counter ~ year + day_of_year, data = df_standort, FUN = sum, na.rm = TRUE)
    years <- sort(unique(daily_sums$year))
    if (length(years) == 0) return(NULL)
    
    year_totals <- numeric(length(years))
    for (i in seq_along(years)) {
      year_data <- daily_sums[daily_sums$year == years[i], ]
      year_data <- year_data[order(year_data$day_of_year), ]
      daily_sums$cumulative[daily_sums$year == years[i]] <- cumsum(year_data$counter)
      year_totals[i] <- max(cumsum(year_data$counter))
    }
    
    ordered_years <- years[order(-year_totals)]
    
    fig <- plot_ly()
    for (yr in ordered_years) {
      year_data <- daily_sums[daily_sums$year == yr, ]
      year_data <- year_data[order(year_data$day_of_year), ]
      if (nrow(year_data) > 0) {
        fig <- add_trace(
          fig,
          data = year_data,
          x = ~day_of_year,
          y = ~cumulative,
          type = "scatter",
          mode = "lines",
          name = as.character(yr),
          line = list(width = 2),
          hovertemplate = paste0(
            "Jahr: ", yr, " ",
            "Tag: %{x} ",
            "Kumuliert: %{y:,.0f} ",
            ""
          )
        )
      }
    }
    
    month_starts <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
    month_names <- c("Jan", "Feb", "Mär", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")
    
    fig <- plotly::layout(
      fig,
      title = paste("Jahresvergleich kumulativ (gesamtes Jahr) -", input$cumulative_standort),
      xaxis = list(
        title = "Tag des Jahres",
        tickmode = "array",
        tickvals = month_starts,
        ticktext = month_names,
        range = c(1, 366)
      ),
      yaxis = list(
        title = "Kumulierte Summe der Radfahrer",
        separatethousands = TRUE
      ),
      hovermode = "closest",
      legend = list(
        title = list(text = "Jahr"),
        orientation = "v",
        x = 0.02,
        y = 0.98,
        bgcolor = "rgba(255,255,255,0.8)",
        bordercolor = "gray",
        borderwidth = 1
      ),
      plot_bgcolor = "#f5f5f5",
      paper_bgcolor = "white"
    )
    
    fig <- plotly::config(
      fig,
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
      displaylogo = FALSE
    )
    
    fig
  })
  
  # ---- Cumulative Year Comparison (Partial - up to most recent day - 1) ----
  observe({
    standort_choices <- unique(bike_counter$Standort)
    updateSelectInput(session, "cumulative_partial_standort", choices = standort_choices)
  })
  
  output$cumulative_partial_plot <- renderPlotly({
    req(input$cumulative_partial_standort)
    
    df_standort <- subset(bike_counter, Standort == input$cumulative_partial_standort)
    df_standort$date <- as.POSIXct(df_standort$date)
    df_standort$year <- lubridate::year(df_standort$date)
    df_standort$day_of_year <- lubridate::yday(df_standort$date)
    
    if (nrow(df_standort) == 0) return(NULL)
    df_standort <- df_standort[!is.na(df_standort$counter), ]
    
    max_date <- max(df_standort$date, na.rm = TRUE)
    max_doy <- lubridate::yday(max_date) - 1L
    if (max_doy < 1) max_doy <- 1
    
    ref_date_str <- format(as.Date(max_date) - 1, "%d.%m.")
    
    df_filtered <- df_standort[df_standort$day_of_year <= max_doy, ]
    
    daily_sums <- aggregate(counter ~ year + day_of_year, data = df_filtered, FUN = sum, na.rm = TRUE)
    years <- sort(unique(daily_sums$year))
    if (length(years) == 0) return(NULL)
    
    year_totals <- numeric(length(years))
    for (i in seq_along(years)) {
      year_data <- daily_sums[daily_sums$year == years[i], ]
      year_data <- year_data[order(year_data$day_of_year), ]
      daily_sums$cumulative[daily_sums$year == years[i]] <- cumsum(year_data$counter)
      year_totals[i] <- max(cumsum(year_data$counter))
    }
    
    ordered_years <- years[order(-year_totals)]
    
    fig <- plot_ly()
    for (yr in ordered_years) {
      year_data <- daily_sums[daily_sums$year == yr, ]
      year_data <- year_data[order(year_data$day_of_year), ]
      if (nrow(year_data) > 0) {
        fig <- add_trace(
          fig,
          data = year_data,
          x = ~day_of_year,
          y = ~cumulative,
          type = "scatter",
          mode = "lines",
          name = as.character(yr),
          line = list(width = 2),
          hovertemplate = paste0(
            "Jahr: ", yr, " ",
            "Tag: %{x} ",
            "Kumuliert: %{y:,.0f} ",
            ""
          )
        )
      }
    }
    
    month_starts <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
    month_names <- c("Jan", "Feb", "Mär", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")
    visible_months <- month_starts[month_starts <= max_doy]
    visible_names <- month_names[month_starts <= max_doy]
    
    fig <- plotly::layout(
      fig,
      title = paste0(
        "Jahresvergleich kumulativ (bis Tag ",
        max_doy, " / ", ref_date_str, ") - ",
        input$cumulative_partial_standort
      ),
      xaxis = list(
        title = "Tag des Jahres",
        tickmode = "array",
        tickvals = visible_months,
        ticktext = visible_names,
        range = c(1, max_doy)
      ),
      yaxis = list(
        title = "Kumulierte Summe der Radfahrer",
        separatethousands = TRUE
      ),
      hovermode = "closest",
      legend = list(
        title = list(text = "Jahr"),
        orientation = "v",
        x = 0.02,
        y = 0.98,
        bgcolor = "rgba(255,255,255,0.8)",
        bordercolor = "gray",
        borderwidth = 1
      ),
      plot_bgcolor = "#f5f5f5",
      paper_bgcolor = "white"
    )
    
    fig <- plotly::config(
      fig,
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
      displaylogo = FALSE
    )
    
    fig
  })
  
  # ---- Cumulative Year Comparison (Partial - Bar Chart) ----
  observe({
    standort_choices <- unique(bike_counter$Standort)
    updateSelectInput(session, "cumulative_bar_standort", choices = standort_choices)
  })
  
  output$cumulative_bar_plot <- renderPlotly({
    req(input$cumulative_bar_standort)
    
    df_standort <- subset(bike_counter, Standort == input$cumulative_bar_standort)
    df_standort$date <- as.POSIXct(df_standort$date)
    df_standort$year <- lubridate::year(df_standort$date)
    df_standort$day_of_year <- lubridate::yday(df_standort$date)
    
    if (nrow(df_standort) == 0) return(NULL)
    df_standort <- df_standort[!is.na(df_standort$counter), ]
    
    max_date <- max(df_standort$date, na.rm = TRUE)
    max_doy <- lubridate::yday(max_date) - 1L
    if (max_doy < 1) max_doy <- 1
    
    ref_date_str <- format(as.Date(max_date) - 1, "%d.%m.")
    df_filtered <- df_standort[df_standort$day_of_year <= max_doy, ]
    
    year_sums <- aggregate(counter ~ year, data = df_filtered, FUN = sum, na.rm = TRUE)
    year_sums <- year_sums[order(year_sums$year), ]
    year_sums$year <- as.character(year_sums$year)
    
    current_year <- as.character(lubridate::year(max_date))
    bar_colors <- ifelse(year_sums$year == current_year, "#e07b00", "steelblue")
    
    fig <- plot_ly(
      data = year_sums,
      x = ~year,
      y = ~counter,
      type = "bar",
      marker = list(color = bar_colors),
      hovertemplate = paste0(
        "Jahr: %{x} ",
        "Kumuliert (bis Tag ", max_doy, " / ", ref_date_str, "): %{y:,.0f} ",
        ""
      )
    )
    
    fig <- plotly::layout(
      fig,
      title = paste0(
        "Jahresvergleich kumulativ (bis Tag ",
        max_doy, " / ", ref_date_str, ") - ",
        input$cumulative_bar_standort
      ),
      xaxis = list(title = "Jahr", type = "category"),
      yaxis = list(title = "Kumulierte Summe der Radfahrer", separatethousands = TRUE),
      hovermode = "closest",
      plot_bgcolor = "#f5f5f5",
      paper_bgcolor = "white"
    )
    
    fig <- plotly::config(
      fig,
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
      displaylogo = FALSE
    )
    
    fig
  })
  
  # ---- Raw Data Explorer ----
  observe({
    standort_choices <- unique(bike_counter$Standort)
    updateSelectInput(session, "raw_standort", choices = standort_choices)
  })
  
  observe({
    if (is.null(input$raw_standort)) return()
    df_standort <- subset(bike_counter, Standort == input$raw_standort)
    if (nrow(df_standort) > 0) {
      df_standort$date <- as.POSIXct(df_standort$date)
      min_date <- as.Date(min(df_standort$date, na.rm = TRUE))
      max_date <- as.Date(max(df_standort$date, na.rm = TRUE))
      updateDateRangeInput(
        session, "raw_daterange",
        start = min_date, end = max_date,
        min = min_date, max = max_date
      )
    }
  })
  
  output$raw_timeseries <- renderPlot({
    req(input$raw_standort, input$raw_daterange)
    df_filtered <- subset(bike_counter, Standort == input$raw_standort)
    df_filtered$date <- as.POSIXct(df_filtered$date)
    df_filtered <- subset(
      df_filtered,
      as.Date(date) >= input$raw_daterange[1] &
        as.Date(date) <= input$raw_daterange[2]
    )
    
    if (nrow(df_filtered) == 0) {
      plot.new()
      text(0.5, 0.5, "Keine Daten für den ausgewählten Zeitraum", cex = 1.5)
      return()
    }
    
    df_filtered <- df_filtered[order(df_filtered$date), ]
    plot(
      df_filtered$date, df_filtered$counter,
      type = "l",
      xlab = "Jahr",
      ylab = "Anzahl Radfahrer (pro Stunde)",
      main = paste("Rohdaten Zeitreihe -", input$raw_standort),
      col = "steelblue",
      lwd = 1.5,
      xaxt = "n"
    )
    grid(nx = NA, ny = NULL)
    
    years <- seq(
      from = lubridate::year(min(df_filtered$date)),
      to = lubridate::year(max(df_filtered$date)),
      by = 1
    )
    for (year in years) {
      year_start <- as.POSIXct(paste0(year, "-01-01"))
      abline(v = year_start, col = "gray40", lwd = 1, lty = 2)
    }
    year_positions <- as.POSIXct(paste0(years, "-01-01"))
    axis(1, at = year_positions, labels = years)
  })
  
  # ---- Monthly Station Summary Table (24 months) ----
  output$monthlyStationTable <- renderDT({
    cutoff_date <- Sys.Date() %m-% months(24)
    
    df_recent <- bike_counter %>%
      filter(date >= cutoff_date) %>%
      mutate(
        year = year(date),
        month = month(date),
        year_month = sprintf("%04d-%02d", year, month)
      )
    
    monthly_summary <- df_recent %>%
      group_by(Standort, year_month) %>%
      summarise(total = round(sum(counter, na.rm = TRUE), -3), .groups = "drop")
    
    wide_table <- monthly_summary %>%
      pivot_wider(names_from = year_month, values_from = total, values_fill = 0)
    
    month_cols <- setdiff(names(wide_table), "Standort")
    month_cols_sorted <- sort(month_cols, decreasing = TRUE)
    wide_table <- wide_table %>% select(Standort, all_of(month_cols_sorted))
    
    datatable(
      wide_table,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "600px",
        fixedColumns = list(leftColumns = 1),
        dom = "Bfrtip",
        language = list(
          search = "Suchen:",
          lengthMenu = "Zeige _MENU_ Einträge",
          info = "Zeige _START_ bis _END_ von _TOTAL_ Standorten",
          paginate = list(
            first = "Erste",
            last = "Letzte",
            `next` = "Nächste",
            previous = "Vorherige"
          )
        )
      ),
      rownames = FALSE,
      class = "cell-border stripe compact"
    ) %>%
      formatStyle(columns = names(wide_table), fontSize = "12px") %>%
      formatStyle("Standort", fontWeight = "bold", backgroundColor = "#f5f5f5")
  })
  
  output$downloadMonthlyTable <- downloadHandler(
    filename = function() {
      paste0("monatsübersicht_radfahrer_", Sys.Date(), ".csv")
    },
    content = function(file) {
      cutoff_date <- Sys.Date() %m-% months(24)
      
      df_recent <- bike_counter %>%
        filter(date >= cutoff_date) %>%
        mutate(
          year = year(date),
          month = month(date),
          year_month = sprintf("%04d-%02d", year, month)
        )
      
      monthly_summary <- df_recent %>%
        group_by(Standort, year_month) %>%
        summarise(total = round(sum(counter, na.rm = TRUE), -3), .groups = "drop")
      
      wide_table <- monthly_summary %>%
        pivot_wider(names_from = year_month, values_from = total, values_fill = 0)
      
      month_cols <- setdiff(names(wide_table), "Standort")
      month_cols_sorted <- sort(month_cols, decreasing = TRUE)
      wide_table <- wide_table %>% select(Standort, all_of(month_cols_sorted))
      
      write.csv(wide_table, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # ---- Monthly Barchart (Monatsbalken) ----
  observe({
    standort_choices <- sort(unique(bike_counter$Standort))
    updateSelectInput(session, "monthly_bar_standort", choices = standort_choices)
  })
  
  output$monthly_bar_plot <- renderPlotly({
    req(input$monthly_bar_standort, input$monthly_bar_month)
    
    month_labels_de <- c(
      "Januar", "Februar", "März", "April", "Mai", "Juni",
      "Juli", "August", "September", "Oktober", "November", "Dezember"
    )
    
    selected_month <- as.integer(input$monthly_bar_month)
    selected_month_label <- month_labels_de[selected_month]
    
    df <- bike_counter %>%
      filter(Standort == input$monthly_bar_standort) %>%
      mutate(
        date = as.Date(date),
        year = lubridate::year(date),
        month = lubridate::month(date),
        dom = as.integer(format(date, "%d"))
      )
    
    if (nrow(df) == 0) {
      return(
        plotly_empty() %>%
          plotly::layout(title = "Keine Daten für diesen Standort")
      )
    }
    
    max_date <- max(df$date, na.rm = TRUE)
    current_month <- as.integer(format(max_date, "%m"))
    cutoff_day <- as.integer(format(max_date, "%d")) - 1L
    
    subtitle_txt <- ""
    
    if (selected_month == current_month) {
      if (cutoff_day < 1) {
        return(
          plotly_empty() %>%
            plotly::layout(
              title = paste0(
                "Keine vollständigen Daten für ",
                selected_month_label, " - ",
                input$monthly_bar_standort
              )
            )
        )
      }
      df <- df %>% filter(dom <= cutoff_day)
      subtitle_txt <- paste0(" (nur bis zum ", cutoff_day, ". des Monats)")
    }
    
    df_plot <- df %>%
      filter(month == selected_month) %>%
      group_by(year) %>%
      summarise(total = sum(counter, na.rm = TRUE), .groups = "drop") %>%
      arrange(year)
    
    if (nrow(df_plot) == 0) {
      return(
        plotly_empty() %>%
          plotly::layout(
            title = paste0(
              "Keine Daten für ",
              selected_month_label, " - ",
              input$monthly_bar_standort
            )
          )
      )
    }
    
    df_plot$year <- as.character(df_plot$year)
    
    fig <- plot_ly(
      data = df_plot,
      x = ~year,
      y = ~total,
      type = "bar",
      marker = list(color = "steelblue"),
      hovertemplate = paste0(
        "Jahr: %{x}<br>",
        "Monat: ", selected_month_label, "<br>",
        "Summe: %{y:,.0f}<extra></extra>"
      )
    )
    
    fig <- plotly::layout(
      fig,
      title = paste0(
        "Monatssummen - ", input$monthly_bar_standort,
        " - ", selected_month_label, subtitle_txt
      ),
      xaxis = list(title = "Jahr", type = "category"),
      yaxis = list(title = "Summe Radfahrende", separatethousands = TRUE),
      plot_bgcolor = "#f5f5f5",
      paper_bgcolor = "white"
    )
    
    fig <- plotly::config(
      fig,
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
      displaylogo = FALSE
    )
    
    fig
  })
  
  # ---- Standortvergleich Monat ----
  observe({
    ym_choices <- bike_counter %>%
      mutate(
        year_month = sprintf(
          "%04d-%02d",
          lubridate::year(date),
          lubridate::month(date)
        )
      ) %>%
      pull(year_month) %>%
      unique() %>%
      sort(decreasing = TRUE)
    
    updateSelectInput(session, "monthly_standort_compare_ym", choices = ym_choices)
  })
  
  output$monthly_standort_compare_plot <- renderPlotly({
    req(input$monthly_standort_compare_ym)
    
    df <- bike_counter %>%
      mutate(
        year_month = sprintf(
          "%04d-%02d",
          lubridate::year(date),
          lubridate::month(date)
        )
      ) %>%
      filter(year_month == input$monthly_standort_compare_ym) %>%
      group_by(Standort) %>%
      summarise(total = sum(counter, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total))
    
    if (nrow(df) == 0) {
      return(plotly_empty() %>% plotly::layout(title = "Keine Daten für diesen Monat"))
    }
    
    df$Standort <- factor(df$Standort, levels = df$Standort)
    
    fig <- plot_ly(
      data = df,
      x = ~Standort,
      y = ~total,
      type = "bar",
      marker = list(color = "steelblue"),
      hovertemplate = "Standort: %{x}<br>Summe: %{y:,.0f}<extra></extra>"
    )
    
    fig <- plotly::layout(
      fig,
      title = paste0("Standortvergleich - ", input$monthly_standort_compare_ym),
      xaxis = list(title = "Standort", tickangle = -40),
      yaxis = list(title = "Summe Radfahrende", separatethousands = TRUE),
      plot_bgcolor = "#f5f5f5",
      paper_bgcolor = "white"
    )
    
    fig <- plotly::config(
      fig,
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
      displaylogo = FALSE
    )
    
    fig
  })
  
  output$monthly_standort_compare_table <- renderDT({
    req(input$monthly_standort_compare_ym)
    
    df <- bike_counter %>%
      mutate(
        year_month = sprintf(
          "%04d-%02d",
          lubridate::year(date),
          lubridate::month(date)
        )
      ) %>%
      filter(year_month == input$monthly_standort_compare_ym) %>%
      group_by(Standort) %>%
      summarise(
        `Summe Radfahrende` = round(sum(counter, na.rm = TRUE), -3),
        .groups = "drop"
      ) %>%
      arrange(desc(`Summe Radfahrende`))
    
    datatable(
      df,
      rownames = FALSE,
      options = list(
        pageLength = 25,
        dom = "ft",
        language = list(search = "Suchen:"),
        scrollX = TRUE
      ),
      class = "cell-border stripe compact"
    ) %>%
      formatStyle("Standort", fontWeight = "bold") %>%
      formatCurrency(
        "Summe Radfahrende",
        currency = "",
        interval = 3,
        mark = ".",
        digits = 0
      )
  })
  
  # ---- Last 14 Days ----
  output$last14_plot <- renderPlot({
    req(input$last14_station)
    
    df_station <- subset(bike_counter, Standort == input$last14_station)
    if (nrow(df_station) == 0) return(NULL)
    
    df_station$date <- as.Date(df_station$date)
    df_station$year <- lubridate::year(df_station$date)
    df_station$day_of_year <- lubridate::yday(df_station$date)
    
    daily_all <- aggregate(counter ~ year + date + day_of_year, data = df_station, FUN = sum, na.rm = TRUE)
    
    max_date_all <- max(daily_all$date, na.rm = TRUE)
    window_dates <- seq(max_date_all - 13L, max_date_all, by = "day")
    window_md <- format(window_dates, "%m-%d")
    
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
      if (nrow(win_y) > 0) {
        win_y <- win_y[order(win_y$day_index), ]
        plot_list[[as.character(y)]] <- win_y
      }
    }
    
    if (length(plot_list) == 0) return(NULL)
    
    year_stats <- lapply(names(plot_list), function(y) {
      d <- plot_list[[y]]
      d <- d[order(d$day_index), ]
      last_row <- d[d$day_index == max(d$day_index), ]
      data.frame(year = y, last_value = max(last_row$counter, na.rm = TRUE))
    })
    
    year_stats <- do.call(rbind, year_stats)
    year_stats <- year_stats[order(-year_stats$last_value), ]
    ordered_years <- year_stats$year
    
    max_y <- max(vapply(plot_list, function(d) max(d$counter, na.rm = TRUE), numeric(1)))
    x_labels <- paste0(1:14, "\n", format(window_dates, "%d.%m."))
    
    par(mar = c(6, 6, 4, 2) + 0.1)
    plot(
      NA, NA,
      xlim = c(1, 14), ylim = c(0, max_y),
      xlab = "",
      ylab = "Summe Radfahrende pro Tag",
      main = paste("Letzte verfügbare 14 Tage -", input$last14_station),
      xaxt = "n"
    )
    
    axis(1, at = 1:14, labels = x_labels, padj = 0.5)
    mtext("Tag im 14-Tage-Fenster (mit Datum des akt. Jahres)", side = 1, line = 4)
    
    cols <- rainbow(length(ordered_years))
    for (i in seq_along(ordered_years)) {
      y <- ordered_years[i]
      d <- plot_list[[y]][order(plot_list[[y]]$day_index), ]
      lines(d$day_index, d$counter, type = "b", col = cols[i], pch = 19)
    }
    
    legend(
      "topleft",
      legend = ordered_years,
      col = cols,
      lty = 1,
      pch = 19,
      title = "Jahr",
      bg = "white"
    )
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
    
    status_val <- paste(job$life_state, job$result_state)
    ok <- isTRUE(job$result_state == "SUCCESS")
    
    status_col  <- if (ok) "#2e7d32" else "#c62828"
    status_icon <- if (ok) icon("check-circle") else icon("times-circle")
    
    safe_val <- function(x) { if (is.null(x) || x == "") "—" else x }
    
    rows <- list(
      list("Job",                   safe_val(job$job_name)),
      list("Status",                tags$span(style = paste0("color:", status_col, "; font-weight:bold;"), status_icon, " ", status_val)),
      list("Start",                 job$start_time),
      list("Ende",                  job$end_time),
      list("Dauer (s)",             safe_val(job$duration_s)),
      list("Max Data TS",           job$max_data_ts),
      list("Rows added (last run)", safe_val(job$rows_added)),
      list("Rows total",            safe_val(job$rows_total)),
      list("Written at",            job$written_at),
      list("Error",                 if (!is.null(job$error_msg) && nchar(job$error_msg) > 0) job$error_msg else "—")
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
    
    df <- bike_counter
    df$date <- as.Date(df$date)
    
    df_filtered <- df[
      df$date >= input$compare_daterange[1] &
        df$date <= input$compare_daterange[2], ]
    
    if (nrow(df_filtered) == 0) {
      return(
        plotly_empty() %>%
          plotly::layout(title = "Keine Daten für den ausgewählten Zeitraum")
      )
    }
    
    date_label <- paste0(
      format(input$compare_daterange[1], "%d.%m.%Y"),
      " – ",
      format(input$compare_daterange[2], "%d.%m.%Y")
    )
    
    agg <- input$compare_aggregation
    
    if (agg == "total") {
      
      df_agg <- aggregate(counter ~ Standort, data = df_filtered, FUN = sum, na.rm = TRUE)
      df_agg <- df_agg[order(-df_agg$counter), ]
      df_agg$Standort <- factor(df_agg$Standort, levels = df_agg$Standort)
      
      fig <- plot_ly(
        data = df_agg,
        x = ~Standort,
        y = ~counter,
        type = "bar",
        marker = list(color = "steelblue"),
        hovertemplate = "Standort: %{x}<br>Summe: %{y:,.0f}<extra></extra>"
      )
      
      fig <- plotly::layout(
        fig,
        title = paste0("Stationsvergleich: ", date_label),
        xaxis = list(title = "Standort", tickangle = -40),
        yaxis = list(title = "Summe Radfahrende", separatethousands = TRUE),
        plot_bgcolor = "#f5f5f5",
        paper_bgcolor = "white"
      )
      
    } else if (agg == "year") {
      
      df_filtered$year <- as.character(lubridate::year(df_filtered$date))
      df_agg <- aggregate(counter ~ Standort + year, data = df_filtered, FUN = sum, na.rm = TRUE)
      
      fig <- plot_ly(
        data = df_agg,
        x = ~Standort,
        y = ~counter,
        color = ~year,
        type = "bar",
        hovertemplate = "Standort: %{x}<br>Summe: %{y:,.0f}<extra></extra>"
      )
      
      fig <- plotly::layout(
        fig,
        barmode = "group",
        title = paste0("Stationsvergleich nach Jahr: ", date_label),
        xaxis = list(title = "Standort", tickangle = -40),
        yaxis = list(title = "Summe Radfahrende", separatethousands = TRUE),
        legend = list(title = list(text = "Jahr")),
        plot_bgcolor = "#f5f5f5",
        paper_bgcolor = "white"
      )
      
    } else {
      
      df_filtered$period <- format(df_filtered$date, "%Y-%m")
      df_agg <- aggregate(counter ~ Standort + period, data = df_filtered, FUN = sum, na.rm = TRUE)
      df_agg <- df_agg[order(df_agg$period), ]
      
      fig <- plot_ly(
        data = df_agg,
        x = ~Standort,
        y = ~counter,
        color = ~period,
        type = "bar",
        hovertemplate = "Standort: %{x}<br>Monat: %{legendgroup}<br>Summe: %{y:,.0f}<extra></extra>"
      )
      
      fig <- plotly::layout(
        fig,
        barmode = "group",
        title = paste0("Stationsvergleich nach Monat: ", date_label),
        xaxis = list(title = "Standort", tickangle = -40),
        yaxis = list(title = "Summe Radfahrende", separatethousands = TRUE),
        legend = list(title = list(text = "Monat")),
        plot_bgcolor = "#f5f5f5",
        paper_bgcolor = "white"
      )
    }
    
    fig <- plotly::config(
      fig,
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
      displaylogo = FALSE
    )
    
    fig
  })
}

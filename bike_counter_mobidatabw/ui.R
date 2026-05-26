ui <- dashboardPage(
  dashboardHeader(title = "BikeCounter Mannheim"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Radz\u00e4hlstellen", tabName = "map", icon = icon("bicycle")),
      menuItem("Standort-Analyse", tabName = "standort_analysis", icon = icon("chart-column")),
      menuItem("Letzte 14 Tage", tabName = "last14days", icon = icon("calendar-day")),
      menuItem("Jahresvergleich Kumulativ", tabName = "cumulative", icon = icon("line-chart")),
      menuItem("Rohdaten Explorer", tabName = "raw_data", icon = icon("line-chart")),
      menuItem("Monats\u00fcbersicht", tabName = "monthly_table", icon = icon("table")),
      menuItem("Stationsvergleich", tabName = "station_compare", icon = icon("chart-bar")),
      menuItem("\u00dcbersicht", tabName = "overview", icon = icon("info-circle")),
      menuItem("Job Status", tabName = "job_status", icon = icon("server"))
    )
  ),
  dashboardBody(
    tabItems(
      
      # ---- Übersicht ----
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("overview_n_standorte", width = 4),
          valueBoxOutput("overview_latest_obs", width = 4),
          valueBoxOutput("overview_total_obs", width = 4)
        ),
        fluidRow(
          column(
            width = 12,
            h3("Standorte im Datensatz"),
            DTOutput("overview_standort_table")
          )
        )
      ),
      
      # Karte
      tabItem(
        tabName = "map",
        leafletOutput("site_map", height = 700)
      ),
      
      # Standort Analysis
      tabItem(
        tabName = "standort_analysis",
        fluidRow(
          column(
            width = 4,
            selectInput(
              "analysis_standort",
              "Standort ausw\u00e4hlen:",
              choices = NULL,
              selected = NULL
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            tabsetPanel(
              tabPanel(
                "Jahreswerte",
                plotOutput("analysis_barplot", height = 500)
              ),
              tabPanel(
                "Quartalsverlauf",
                plotOutput("analysis_quarter_ts", height = 500)
              ),
              tabPanel(
                "Monatsverlauf",
                plotOutput("analysis_month_ts", height = 500)
              )
            )
          )
        )
      ),
      
      tabItem(
        tabName = "last14days",
        fluidRow(
          column(
            width = 4,
            selectInput(
              "last14_station",
              "Standort ausw\u00e4hlen",
              choices = NULL,
              selected = NULL
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            plotOutput("last14_plot", height = 600)
          )
        )
      ),
      
      # Cumulative Comparison
      tabItem(
        tabName = "cumulative",
        tabsetPanel(
          tabPanel(
            "Gesamtes Jahr",
            fluidRow(
              column(
                width = 4,
                selectInput(
                  "cumulative_standort",
                  "Standort ausw\u00e4hlen:",
                  choices = NULL,
                  selected = NULL
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                plotlyOutput("cumulative_plot", height = 700)
              )
            )
          ),
          tabPanel(
            "Bis aktueller Tag",
            fluidRow(
              column(
                width = 4,
                selectInput(
                  "cumulative_partial_standort",
                  "Standort ausw\u00e4hlen:",
                  choices = NULL,
                  selected = NULL
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                plotlyOutput("cumulative_partial_plot", height = 700)
              )
            )
          ),
          tabPanel(
            "Jahresvergleich Balken",
            fluidRow(
              column(
                width = 4,
                selectInput(
                  "cumulative_bar_standort",
                  "Standort ausw\u00e4hlen:",
                  choices = NULL,
                  selected = NULL
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                plotlyOutput("cumulative_bar_plot", height = 700)
              )
            )
          )
        )
      ),
      
      # Raw Data Explorer
      tabItem(
        tabName = "raw_data",
        fluidRow(
          column(
            width = 4,
            selectInput(
              "raw_standort",
              "Standort ausw\u00e4hlen:",
              choices = NULL,
              selected = NULL
            )
          ),
          column(
            width = 4,
            dateRangeInput(
              "raw_daterange",
              "Zeitraum ausw\u00e4hlen:",
              start = NULL,
              end = NULL,
              language = "de",
              separator = "bis"
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            plotOutput("raw_timeseries", height = 600)
          )
        )
      ),
      
      # Stationsvergleich
      tabItem(
        tabName = "station_compare",
        fluidRow(
          column(
            width = 4,
            dateRangeInput(
              "compare_daterange",
              "Zeitraum ausw\u00e4hlen:",
              start = Sys.Date() - 365,
              end = Sys.Date() - 1,
              language = "de",
              separator = "bis"
            )
          ),
          column(
            width = 4,
            radioButtons(
              "compare_aggregation",
              "Aggregation:",
              choices = c(
                "Gesamt" = "total",
                "Nach Jahr" = "year",
                "Nach Monat" = "month"
              ),
              selected = "total",
              inline = TRUE
            )
          )
        ),
        fluidRow(
          column(width = 12, plotlyOutput("station_compare_plot", height = 620))
        )
      ),
      
      # Monthly Overview Table + Barchart
      tabItem(
        tabName = "monthly_table",
        tabsetPanel(
          tabPanel(
            "Tabelle",
            fluidRow(
              column(
                width = 12,
                h3("Summe der Radfahrenden nach Standort und Monat (letzte 24 Monate)"),
                downloadButton("downloadMonthlyTable", "Download als CSV"),
                br(), br(),
                DTOutput("monthlyStationTable")
              )
            )
          ),
          tabPanel(
            "Monatsbalken",
            fluidRow(
              column(
                width = 4,
                selectInput(
                  "monthly_bar_standort",
                  "Standort ausw\u00e4hlen:",
                  choices = NULL,
                  selected = NULL
                )
              ),
              column(
                width = 4,
                selectInput(
                  "monthly_bar_month",
                  "Monat ausw\u00e4hlen:",
                  choices = c(
                    "Januar" = "1",
                    "Februar" = "2",
                    "M\u00e4rz" = "3",
                    "April" = "4",
                    "Mai" = "5",
                    "Juni" = "6",
                    "Juli" = "7",
                    "August" = "8",
                    "September" = "9",
                    "Oktober" = "10",
                    "November" = "11",
                    "Dezember" = "12"
                  ),
                  selected = as.character(as.integer(format(Sys.Date(), "%m")))
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                plotlyOutput("monthly_bar_plot", height = 600)
              )
            )
          ),
          tabPanel(
            "Standortvergleich Monat",
            fluidRow(
              column(
                width = 4,
                selectInput(
                  inputId = "monthly_standort_compare_ym",
                  label = "Jahr-Monat ausw\u00e4hlen:",
                  choices = NULL,
                  selected = NULL
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                plotlyOutput("monthly_standort_compare_plot", height = 500)
              )
            ),
            fluidRow(
              column(
                width = 12,
                br(),
                DTOutput("monthly_standort_compare_table")
              )
            )
          )
        )
      ),
      
      # Or as a standalone tab:
      tabItem(
        tabName = "job_status",
        fluidRow(
          box(
            title = "Databricks Job \u2013 letzter Lauf",
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            uiOutput("job_status_table")
          )
        )
      )
    )
  )
)

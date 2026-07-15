library(bslib)

adfc_theme <- bs_theme(
  version = 5,
  bg = "#ffffff",             # Weißer Hintergrund
  fg = "#004B7C",             # Dunkelblaue Schrift
  primary = "#EE7400",        # ADFC-Orange (für Buttons, Tabs und aktive Felder)
  secondary = "#7FC600",      # ADFC-Blau
  base_font = font_google("Roboto") # Moderne Google-Schriftart
)

ui <- fluidPage(
  theme = adfc_theme,
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.png"),
    tags$style(HTML("
      #accidentMap, #heatMap {
        height: calc(100vh - 120px) !important;
      }
      .leaflet-container {
        height: 100%;
      }
    "))
  ),
  
  # ADFC-Logo oben rechts
  tags$div(
    style = "position: absolute; top: 10px; right: 20px; z-index: 1000;",
    tags$img(
      src = "https://upload.wikimedia.org/wikipedia/commons/a/a4/ADFC-Logo_2009_1.svg",
      height = "50px",
      alt = "ADFC Logo"
    )
  ),
  
  titlePanel("Unfälle mit Radfahrern und Fußgängern in Mannheim (jetzt inkl. 2025)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("zielgruppe", "Betroffene Verkehrsteilnehmer", 
                  choices = c("Alle", "Rad", "Fuß"), 
                  selected = "Alle"),
      selectInput("UJAHR", "Jahr", choices = NULL, selected = "Alle"),
      selectInput("UKATEGORIE", "Unfallschwere", choices = NULL, selected = "Alle"),
      selectInput("UTYP1", "Unfalltyp", choices = NULL, selected = "Alle"),
      selectInput("involved", "Beteiligte (leer: Alle)", choices = NULL, multiple = TRUE),
      selectInput("art", "Unfallart", choices = NULL, selected = "Alle")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Punktkarte", 
          leaflet::leafletOutput("accidentMap", height = "80vh"),  # <--- HIER
          textOutput("sourceInfo")
        ),
        tabPanel(
          "Heatmap",
          leaflet::leafletOutput("heatMap", height = "80vh"),      # <--- HIER
          textOutput("heatmapSourceInfo")
        ),
        tabPanel(
          "Rasterkarte",
          leaflet::leafletOutput("gridMap", height = "80vh"),      # <--- HIER
          textOutput("gridMapSourceInfo")
        ),
        tabPanel(
          "Unfallschwerpunktanalyse",
          fluidPage(
            br(),
            fluidRow(
              column(
                width = 4,
                sliderInput("hotspot_radius", "Radius (Meter)",
                            min = 10, max = 100, value = 25, step = 5)
              ),
              column(
                width = 4,
                sliderInput("hotspot_minpts", "Mindestanzahl Unfälle pro Schwerpunkt",
                            min = 2, max = 20, value = 3, step = 1)
              ),
              column(
                width = 4,
                br(),
                downloadButton("downloadHotspots", "Schwerpunkte als CSV")
              )
            ),
            leaflet::leafletOutput("hotspotMap", height = "55vh"),
            textOutput("hotspotSourceInfo"),
            hr(),
            h4("Unfallschwerpunkte (sortiert nach Anzahl)"),
            DT::DTOutput("hotspotTable")
          )
        ),
        tabPanel(
          "Kontakt",
          fluidPage(
            titlePanel("Kontakt und Informationen"),
            fluidRow(
              column(
                width = 6,
                h3("Dr. Arne Warnke"),
                p("ADFC Mannheim (Daten & Digitalisierung)"),
                p("Email: arne.warnke [at] adfc-bw.de")
              ),
              column(
                width = 6,
                h3("Robert Hofmann"),
                p("ADFC Mannheim (Sprecher)"),
                p("Email: robert.hofmann [at] adfc-bw.de")
              )
            ),
            hr(),
            h3("Über dieses Projekt"),
            p("Diese interaktive Karte zeigt Unfalldaten mit Fußgänger- und Radfahrerbeteiligung in Mannheim."),
            p("Ziel ist es, mit Hilfe von offenen Daten die Transparenz zu fördern, städtische Planungen zu unterstützen und zur Verkehrssicherheit im Sinne von Vision Zero beizutragen."),
            p("Die Anwendung wurde vom ADFC Mannheim entwickelt – auf ehrenamtlicher Basis."),
            p("Quelle ist die Unfallstatistik des Statistischen Bundesamtes."),
            p("Weitere Informationen zur Unfallverhütung durch den ADFC Mannheim finden Sie hier:"),
            tags$a(
              href = "https://mannheim.adfc.de/artikel/unfallverhuetung-als-zentrale-aufgabe-des-adfc-mannheim",
              "Unfallverhütung als zentrale Aufgabe des ADFC Mannheim",
              target = "_blank"
            ),
            br(), br(),
            p("Der Quellcode ist öffentlich zugänglich auf GitHub:"),
            tags$a(
              href = "https://github.com/AJWarnke/adfc",
              "https://github.com/AJWarnke/adfc",
              target = "_blank"
            )
          )
        ),
        tabPanel(
          "Liste tödlicher Radunfälle",
          fluidPage(
            h3("Tödliche Unfälle - Übersicht"),
            downloadButton("downloadDeadlyAccidents", "Download als CSV"),
            br(), br(),
            DT::DTOutput("deadlyAccidentsTable")  # <--- Auch hier Namespace
          )
        )
      )
    )
  )
)

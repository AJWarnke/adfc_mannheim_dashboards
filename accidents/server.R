server <- function(input, output, session) {
  
  # Load the data
  accidentData <- read.csv("Unfaelle_mit_Fuss.csv")
  
  # Codierung für UTYP1 gemäß PDF - nur Text ohne Nummern
  utyp1_labels <- c(
    "Fahrunfall (F)",
    "Abbiege-Unfall (AB)",
    "Einbiegen/Kreuzen-Unfall (EK)",
    "Überschreiten-Unfall (ÜS)",
    "Unfall durch ruhenden Verkehr (RV)",
    "Unfall im Längsverkehr (LV)",
    "Sonstiger Unfall (SO)"
  )
  names(utyp1_labels) <- 1:7
  
  # Update selectInput choices dynamically
  observe({
    updateSelectInput(
      session, "UJAHR",
      choices = c("Alle", sort(unique(accidentData$UJAHR[!(accidentData$UJAHR %in% c(2025,2026))]), decreasing = TRUE))
    )
    updateSelectInput(
      session, "UKATEGORIE",
      choices = c("Alle", unique(accidentData$UKATEGORIE))
    )
    
    # UTYP1 mit beschreibenden Labels (nur Text, keine Nummern)
    utyp1_values <- sort(unique(accidentData$UTYP1))
    utyp1_choices <- c("Alle" = "Alle", setNames(as.character(utyp1_values), utyp1_labels[as.character(utyp1_values)]))
    
    updateSelectInput(
      session,
      "UTYP1",
      choices = utyp1_choices
    )
    
    # involved Filter - Multiple Choice ohne "Alle" (leer = alle anzeigen)
    updateSelectInput(
      session,
      "involved",
      choices = sort(unique(accidentData$involved)),
      selected = character(0) # <--- HIER: Verhindert die automatische Vorauswahl
    )
    
    
    # art Filter
    updateSelectInput(
      session,
      "art",
      choices = c("Alle", sort(unique(accidentData$art)))
    )
  })
  
  # Reactive expression to filter data based on input
  getMapData <- reactive({
    filteredData <- accidentData
    
    # NEU: Filter für die Zielgruppe
    if (input$zielgruppe == "Rad") {
      filteredData <- dplyr::filter(filteredData, IstRad == 1)
    } else if (input$zielgruppe == "Fuß") {
      filteredData <- dplyr::filter(filteredData, IstFuss == 1)
    }
    
    if (input$UJAHR != "Alle") {
      filteredData <- dplyr::filter(filteredData, UJAHR == input$UJAHR)
    }
    
    if (input$UJAHR != "Alle") {
      filteredData <- dplyr::filter(filteredData, UJAHR == input$UJAHR)
    }
    
    if (input$UKATEGORIE != "Alle") {
      filteredData <- dplyr::filter(filteredData, UKATEGORIE == input$UKATEGORIE)
    }
    
    # UTYP1 Filter
    if (input$UTYP1 != "Alle") {
      selected_utyp1 <- as.integer(input$UTYP1)
      filteredData <- dplyr::filter(filteredData, UTYP1 == selected_utyp1)
    }
    
    # involved Filter - Multiple Choice (leer = keine Filterung)
    if (!is.null(input$involved) && length(input$involved) > 0) {
      filteredData <- dplyr::filter(filteredData, involved %in% input$involved)
    }
    
    # art Filter
    if (input$art != "Alle") {
      filteredData <- dplyr::filter(filteredData, art == input$art)
    }
    
    if (nrow(filteredData) == 0) {
      print("No data found for the selected filters.")
    }
    
    filteredData
  })
  
  # Mit Namespace-Präfix
  category_colors <- leaflet::colorFactor(
    rainbow(length(unique(accidentData$UKATEGORIE))), 
    unique(accidentData$UKATEGORIE)
  )
  
  output$accidentMap <- leaflet::renderLeaflet({
    data <- getMapData()
    if (nrow(data) == 0) {
      m <- leaflet::leaflet()
      m <- leaflet::addTiles(m)
      m <- leaflet::setView(m, lng = 8.4660, lat = 49.4875, zoom = 12)
      return(m)
    }
    
    m <- leaflet::leaflet(data)
    m <- leaflet::addTiles(m)
    m <- leaflet::addCircleMarkers(
      m,
      lng = ~Longitude, lat = ~Latitude, 
      color = ~category_colors(UKATEGORIE),
      radius = 3,
      popup = ~paste("Category:", UKATEGORIE),
      label = ~lapply(seq_len(nrow(data)), function(i) {
        HTML(sprintf(
          "<div style='width: 500px;'>Datum (Monat/Jahr): %s.%s um %s:00 Uhr<br>Category: %s<br>Beteiligte: %s<br>Typ: %s<br>Art: %s<br>%s</div>",
          data$UMONAT[i], data$UJAHR[i], data$USTUNDE[i], data$UKATEGORIE[i], data$involved[i], data$typ[i], data$art[i], data$Kommentar[i]
        ))
      }),
      labelOptions = leaflet::labelOptions(
        style = list(
          "font-weight" = "normal", 
          padding = "3px 8px",
          "white-space" = "normal",
          "word-wrap" = "break-word"
        ),
        textsize = "15px",
        direction = "auto"
      )
    )
    m <- leaflet::addLegend(
      m,
      "bottomright", 
      pal = category_colors, 
      values = data$UKATEGORIE,
      title = "Unfallkategorie",
      opacity = 1
    )
    m
  })
  
  # Render the Leaflet heatmap
  output$heatMap <- leaflet::renderLeaflet({
    data <- getMapData()
    m <- leaflet::leaflet()
    m <- leaflet::addTiles(m)
    m <- leaflet.extras::addHeatmap(
      m,
      data = data, 
      lng = ~Longitude, lat = ~Latitude,
      blur = 20, max = 0.05, radius = 15
    )
    m
  })
  
  output$sourceInfo <- renderText({
    "Quelle: ADFC Mannheim / Unfallstatistik Statistisches Bundesamt"
  })
  
  output$heatmapSourceInfo <- renderText({
    "Quelle: ADFC Mannheim / Unfallstatistik Statistisches Bundesamt"
  })
  
  output$deadlyAccidentsTable <- DT::renderDT({
    deadlyAccidents()
  }, escape = FALSE, options = list(pageLength = 10))
  
  deadlyAccidents <- reactive({
    data <- accidentData
    data <- dplyr::filter(data, trimws(UKATEGORIE) == "Unfall mit Getöteten" & IstRad == 1)
    data <- dplyr::mutate(
      data,
      Datum = paste0(sprintf("%02d", UMONAT), ".", UJAHR),
      Beschreibung = Kommentar,
      Ort = paste0("<a href='https://www.openstreetmap.org/?mlat=", 
                   Latitude, "&mlon=", Longitude, "#map=18/", Latitude, 
                   "/", Longitude,
                   "' target='_blank'>OpenStreetMap</a>")
    )
    data <- dplyr::select(data, Jahr = UJAHR, Datum, Beschreibung, Ort)
    data
  })
  
  # Render the Grid Density Map
  output$gridMap <- leaflet::renderLeaflet({
    data <- getMapData()
    
    if (nrow(data) == 0) {
      m <- leaflet::leaflet()
      m <- leaflet::addTiles(m)
      m <- leaflet::setView(m, lng = 8.4660, lat = 49.4875, zoom = 12)
      return(m)
    }
    
    GRID_SIZE <- 0.001
    
    data <- dplyr::mutate(
      data,
      lat_bin = floor(Latitude / GRID_SIZE) * GRID_SIZE,
      lon_bin = floor(Longitude / GRID_SIZE) * GRID_SIZE
    )
    
    grid_counts <- dplyr::group_by(data, lat_bin, lon_bin)
    grid_counts <- dplyr::summarise(grid_counts, count = dplyr::n(), .groups = 'drop')
    
    pal <- leaflet::colorNumeric(palette = "YlOrRd", domain = grid_counts$count)
    
    m <- leaflet::leaflet()
    m <- leaflet::addTiles(m)
    
    for (i in 1:nrow(grid_counts)) {
      lat <- grid_counts$lat_bin[i]
      lon <- grid_counts$lon_bin[i]
      count <- grid_counts$count[i]
      
      m <- leaflet::addRectangles(
        m,
        lng1 = lon, lat1 = lat,
        lng2 = lon + GRID_SIZE, lat2 = lat + GRID_SIZE,
        fillColor = pal(count),
        fillOpacity = 0.7,
        color = "#777",
        weight = 0.5,
        label = paste(count, "Unfälle")
      )
    }
    
    m <- leaflet::addLegend(m, "bottomright", pal = pal, values = grid_counts$count, 
                            title = "Unfälle pro Rasterzelle")
    m
  })
  
  output$gridMapSourceInfo <- renderText({
    "Quelle: ADFC Mannheim / Unfallstatistik Statistisches Bundesamt"
  })
  
  output$downloadDeadlyAccidents <- downloadHandler(
    filename = function() {
      paste0("Tödliche_Unfälle_Mannheim_Fahrrad_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(deadlyAccidents(), file, row.names = FALSE)
    }
  )
  
}

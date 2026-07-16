server <- function(input, output, session) {
  
  # Load the data
  accidentData <- read.csv("Unfaelle_mit_Fuss.csv")

  accidentData$unfall_id <- seq_len(nrow(accidentData))

  strassenZuordnung <- read.csv("strassen_zuordnung.csv")
  strassenInfo      <- read.csv("strassen_info.csv")

  typ_labels <- c(
    primary   = "Hauptverkehrsstraße (B-Straße)",
    secondary = "Hauptverkehrsstraße",
    tertiary  = "Sammelstraße"
  )
  strassenInfo$strassentyp <- dplyr::coalesce(
    typ_labels[strassenInfo$strassentyp],
    strassenInfo$strassentyp
  )

  strassenGeom      <- sf::st_read("strassen_geom.geojson", quiet = TRUE)
  
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
      choices = c("Alle", sort(unique(accidentData$UJAHR[!(accidentData$UJAHR %in% c(2026))]), decreasing = TRUE))
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

  # ---- Unfallschwerpunktanalyse (DBSCAN) ----
# Cluster benachbarter Unfälle: jeder Schwerpunkt erscheint genau EINMAL,
# egal wie viele Unfälle er enthält.

hotspotData <- reactive({
  data <- getMapData()
  req(nrow(data) > 0)

  # Lokale Umrechnung Grad -> Meter (für Mannheim völlig ausreichend genau)
  lat0 <- mean(data$Latitude, na.rm = TRUE)
  x <- data$Longitude * cos(lat0 * pi / 180) * 111320
  y <- data$Latitude * 111320

  cl <- dbscan::dbscan(cbind(x, y),
                       eps = input$hotspot_radius,
                       minPts = input$hotspot_minpts)
  data$cluster <- cl$cluster   # 0 = kein Schwerpunkt
  data
})

hotspotSummary <- reactive({
  data <- dplyr::filter(hotspotData(), cluster > 0)
  if (nrow(data) == 0) return(data.frame())

  hs <- dplyr::group_by(data, cluster)
  hs <- dplyr::summarise(
    hs,
    Unfaelle       = dplyr::n(),
    Latitude       = mean(Latitude),
    Longitude      = mean(Longitude),
    Rad            = sum(IstRad == 1, na.rm = TRUE),
    Fuss           = sum(IstFuss == 1, na.rm = TRUE),
    Haeufigster_Typ = names(sort(table(typ), decreasing = TRUE))[1],
    Jahre          = paste(sort(unique(UJAHR)), collapse = ", "),
    .groups = "drop"
  )
  hs <- dplyr::arrange(hs, dplyr::desc(Unfaelle))
  hs$Rang <- seq_len(nrow(hs))
  hs
})

output$hotspotMap <- leaflet::renderLeaflet({
  data <- hotspotData()
  hs   <- hotspotSummary()

  m <- leaflet::leaflet()
  m <- leaflet::addTiles(m)

  if (nrow(hs) == 0) {
    m <- leaflet::setView(m, lng = 8.4660, lat = 49.4875, zoom = 12)
    return(m)
  }

  # Einzelunfälle dezent grau im Hintergrund
  noise <- dplyr::filter(data, cluster == 0)
  if (nrow(noise) > 0) {
    m <- leaflet::addCircleMarkers(
      m, data = noise, lng = ~Longitude, lat = ~Latitude,
      radius = 2, color = "#999999", stroke = FALSE, fillOpacity = 0.4
    )
  }

  pal <- leaflet::colorNumeric("YlOrRd", domain = hs$Unfaelle)

  m <- leaflet::addCircleMarkers(
    m, data = hs, lng = ~Longitude, lat = ~Latitude,
    radius = ~pmin(3 + Unfaelle * 0.6, 12),
    color = "#333333", weight = 1,
    fillColor = ~pal(Unfaelle), fillOpacity = 0.85,
    label = ~lapply(seq_len(nrow(hs)), function(i) {
      HTML(sprintf(
        "<b>Schwerpunkt %d</b><br>%d Unfälle<br>davon Rad: %d, Fuß: %d<br>Häufigster Typ: %s<br>Jahre: %s",
        hs$Rang[i], hs$Unfaelle[i], hs$Rad[i], hs$Fuss[i],
        hs$Haeufigster_Typ[i], hs$Jahre[i]
      ))
    })
  )

  m <- leaflet::addLegend(
    m, "bottomright", pal = pal, values = hs$Unfaelle,
    title = "Unfälle pro<br>Schwerpunkt", opacity = 1
  )
  m
})

  output$hotspotTable <- DT::renderDT({
    hs <- hotspotSummary()
    if (nrow(hs) == 0) {
      return(DT::datatable(
        data.frame(Hinweis = "Keine Schwerpunkte mit den aktuellen Einstellungen gefunden."),
        options = list(dom = "t"), rownames = FALSE
      ))
    }
    hs$Ort <- sprintf(
      '<a href="https://www.openstreetmap.org/?mlat=%f&mlon=%f#map=18/%f/%f" target="_blank">Karte</a>',
      hs$Latitude, hs$Longitude, hs$Latitude, hs$Longitude
    )
    DT::datatable(
      dplyr::select(hs, Rang, Unfaelle, Rad, Fuss, Haeufigster_Typ, Jahre, Ort),
      escape = FALSE, rownames = FALSE,
      options = list(pageLength = 10),
      colnames = c("Rang", "Unfälle", "Rad", "Fuß", "Häufigster Typ", "Jahre", "Ort")
    )
  })

  output$hotspotSourceInfo <- renderText({
    "Schwerpunkte: Benachbarte Unfälle innerhalb des gewählten Radius werden zu genau einem Schwerpunkt zusammengefasst. Quelle: ADFC Mannheim / Unfallstatistik Statistisches Bundesamt"
  })

  output$downloadHotspots <- downloadHandler(
    filename = function() {
      paste0("Unfallschwerpunkte_Mannheim_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(hotspotSummary(), file, row.names = FALSE)
    }
  )
  
  output$deadlyAccidentsTable <- DT::renderDT({
    deadlyAccidents()
  }, escape = FALSE, options = list(pageLength = 10))
  
  deadlyAccidents <- reactive({
    data <- accidentData
    data <- dplyr::filter(data, trimws(UKATEGORIE) == "Unfall mit Getöteten" & IstRadBetroffen == 1)
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

    MIN_UNFAELLE_TOPSTRASSEN <- 10  # fest statt Slider

    topStrassen <- reactive({
      getMapData() |>
        dplyr::inner_join(strassenZuordnung, by = "unfall_id") |>
        dplyr::count(strasse, name = "anzahl") |>
        dplyr::inner_join(strassenInfo, by = "strasse") |>
        dplyr::mutate(pro_km = round(anzahl / laenge_km, 1)) |>
        dplyr::filter(anzahl >= MIN_UNFAELLE_TOPSTRASSEN) |>
        dplyr::arrange(dplyr::desc(pro_km)) |>
        head(input$topn_strassen)
    })

output$topStrassenMap <- leaflet::renderLeaflet({
    df <- topStrassen()
    req(nrow(df) > 0)
    geom <- dplyr::inner_join(strassenGeom, df, by = c("name" = "strasse"))
    pal <- leaflet::colorNumeric("YlOrRd", domain = geom$pro_km)

    leaflet::leaflet(geom) |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
      leaflet::addPolylines(
        color = ~pal(pro_km), weight = 6, opacity = 0.9,
        label = ~paste0(name, ": ", anzahl, " Unfälle (", pro_km, " pro km)"),
        popup = ~paste0("<b>", name, "</b><br>",
                        strassentyp, "<br>",
                        "Stadtbezirke: ", stadtbezirke, "<br>",
                        anzahl, " Unfälle auf ", laenge_km, " km<br>",
                        "<b>", pro_km, " Unfälle pro km</b>")
      ) |>
      leaflet::addLegend(pal = pal, values = ~pro_km,
                        title = "Unfälle pro km", position = "bottomright")
  })

  output$topStrassenTable <- DT::renderDT({
    topStrassen() |>
      dplyr::select(Straße = strasse, Typ = strassentyp,
                    Stadtbezirke = stadtbezirke, Unfälle = anzahl,
                    `Länge (km)` = laenge_km, `Unfälle pro km` = pro_km)
  }, options = list(pageLength = 25), rownames = FALSE)

  output$topStrassenInfo <- renderText({
    paste0("Zuordnung: Unfälle im Umkreis von 10 m um Hauptverkehrs- und Sammelstraßen ",
          "(OpenStreetMap). Unfälle abseits dieser Straßen sind nicht enthalten. ",
          "Filter in der Seitenleiste wirken auch auf diese Auswertung.")
  })

  output$downloadTopStrassen <- downloadHandler(
    filename = function() "top_unfallstrassen.csv",
    content = function(file) write.csv(topStrassen(), file, row.names = FALSE)
  )
  
  output$downloadDeadlyAccidents <- downloadHandler(
    filename = function() {
      paste0("Tödliche_Unfälle_Mannheim_Fahrrad_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(deadlyAccidents(), file, row.names = FALSE)
    }
  )
  
}

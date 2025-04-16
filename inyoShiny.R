# Libraries----
library(shiny)
library(tidyverse)
library(here)
library(crosstalk)
library(plotly)
library(janitor)
library(sf)
library(leaflet)
library(DT)
library(waiter)
library(bslib)  # for theming

# Read functions and data----
source("code/functions.R")

# 1) Load and Prep Data ------------------------------------------------------

# DTW data
dtw_pfix <- readRDS(file = here('data','hydro','dtw_2024.rds')) %>%
  filter(Year > 1985)

# LANDSAT
rs_pfix <- readRDS(file = here('data','hydro','rs_2024.rds')) %>%
  filter(Year > 1985)

# Parcel attributes
parcels_augment <- read_csv(here("data","parcel_augment2.csv")) %>% clean_names()

# LPT data
lpt_datax <- read_csv(here("data","lpt_MASTER_2024_pfix.csv"))
lpt_data <- mult_to_single_parcel_name(lpt_datax) %>% arrange(Parcel, Year)

# Summaries for species hits
transects_per_parcel <- lpt_data %>%
  distinct(Parcel, Year, Transect) %>%
  group_by(Parcel, Year) %>%
  summarise(nTransect = n())

species_hits_parcel <- lpt_data %>%
  filter(Cover != 0) %>%
  group_by(Parcel, Year, Species, Code, CommonName) %>%
  summarise(TotalSpHits = sum(Cover))

dd <- species_hits_parcel %>%
  left_join(transects_per_parcel, by = c("Parcel", "Year")) %>%
  mutate(parcelMeanCover = round(TotalSpHits / nTransect, 2))

upar <- dd %>%
  group_by(Parcel) %>%
  summarise(count = n()) %>%
  select(Parcel)

parlist <- upar$Parcel

# Spatial Features
wells <- st_read(here("data","wells.geojson")) %>% st_transform(crs = 4326)
monsitesgeo_wgs84 <- st_read(here("data","monsites.geojson")) %>% st_transform(crs = 4326)

parcelsgeox <- st_read(here("data","parcels.geojson")) %>% st_transform(crs = 4326)
parcelsgeo_sub <- parcelsgeox %>% filter(PCL %in% parlist)

dd10ft <- st_read(here("data","DrawdownContour10ftwellfieldDesig"))
dd10ft_w84 <- st_transform(dd10ft, crs = st_crs(4326))

# Merge polygons with attribute table
parcelsgeo_merged <- parcelsgeox %>%
  left_join(parcels_augment, by = c("PCL" = "pcl"))

# Filter numeric data
dropdownlist <- unique(parlist)
parcels <- readRDS(file = here('data','hydro','parcels_2024.rds')) %>%
  filter(Parcel %in% dropdownlist)

# 2) UI ----------------------------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),

  # Constrain max page width & center, plus top/bottom margin
  style = "max-width: 1400px; margin: 40px auto;",

  # 1) HEADER ROW
  fluidRow(
    column(
      width = 12,
      div(
        style = "display: flex; justify-content: flex-end; align-items: center;",
        tags$h5("Green Book Vegetation Monitoring",
                style = "font-style: italic; margin-right: 10px;"),
        tags$img(src = "inyo_logo.png", height = "60px", style="margin-top: 0px;")
      )
    )
  ),

  # 2) MAIN ROW: Left column (filters + app info), Right column (parcel info + map)
  fluidRow(
    # Left column
    column(
      width = 4,
      wellPanel(
        selectInput("parcel", "Parcel", choices = NULL, width = "100%"),
        checkboxGroupInput("gb_type", "Greenbook Type:",
                           choices = c("A","B","C","D","E"),
                           selected = c("A","B","C","D","E"),
                           inline = TRUE)
      ),
      wellPanel(
        HTML("
          <strong>App Info:</strong>
          <ul>
            <li><strong>Parcel Selector (above):</strong> Select a parcel from the dropdown or click on the map polygon.</li>
            <li><strong>Greenbook Type (above):</strong> Filter dropdown and map parcels by Greenbook type A–E.</li>
            <li><strong>Map (right):</strong> Vegetation parcels, monitoring sites, and wells.</li>
            <li><strong>DTW Plot:</strong> Depth to water over time, in feet below ground surface (bgs).</li>
            <li><strong>Cover Plot:</strong> Stacked bars for annual & perennial cover.</li>
            <li><strong>Proportion Grass:</strong> Perennial grass fraction of total perennial cover.</li>
            <li><strong>NDVI:</strong> Seasonal normalized vegetation index from Landsat 5/7/8/9.</li>
            <li><strong>PPT:</strong> Annual water year precipitation in inches.</li>
            <li><strong>Species Table:</strong> Species-level cover for select year 1986-current.</li>
          </ul>
        ")
      )
    ),

    # Right column
    column(
      width = 8,
      wellPanel(
        htmlOutput("parcel_info")
      ),
      leafletOutput("leaflet_map", height = "500px")
    )
  ),

  # 3) TIME-SERIES PLOTS
  fluidRow(
    column(
      width = 12,
      plotlyOutput("dtw_plot", height = "180px"),
      plotlyOutput("cover_plot", height = "180px"),
      plotlyOutput("cover_plot_ratios", height = "180px"),
      plotlyOutput("ndvi_plot", height = "180px"),
      plotlyOutput("precipitation_plot", height = "180px")
    )
  ),

  # 4) SPECIES TABLE
  fluidRow(
    column(
      width = 12,
      dataTableOutput("species_table_years", height = "600px")
    )
  )
)

# 3) SERVER ------------------------------------------------------------------
server <- function(input, output, session) {

  # OPTIONAL: Waiter splash
  w <- waiter::Waiter$new(
    id = "app-content",
    html = div(
      style = paste(
        "position: fixed;",
        "top: 20px;",
        "left: 50%;",
        "transform: translateX(-50%);",
        "background: rgba(255, 255, 255, 0.9);",
        "padding: 20px;",
        "border-radius: 5px;",
        "z-index: 9999;",
        "width: 600px;",
        "min-height: 200px;",
        "color: black;",
        "pointer-events: auto;"
      ),
      tags$h2("Welcome to Green Book Vegetation Monitoring"),
      tags$p("This app displays vegetation monitoring data. Please read these instructions carefully before proceeding."),
      tags$button("Close", id = "closeSplash", onclick = "Shiny.setInputValue('closeSplash', Math.random())")
    ),
    color = "transparent"
  )

  observeEvent(input$closeSplash, {
    cat("Close button clicked\n")
    w$hide()
  })

  # A) Reactive filtering for numeric & polygons
  filtered_parcels_data <- reactive({
    req(input$gb_type)
    df_parc <- parcels_augment %>%
      filter(type %in% input$gb_type)

    parcels %>% filter(Parcel %in% df_parc$pcl)
  })

  filtered_polygons <- reactive({
    req(input$gb_type)
    df_poly <- parcelsgeo_merged %>%
      filter(TYPE %in% input$gb_type)

    valid_pcls <- filtered_parcels_data()$Parcel
    df_poly %>% filter(PCL %in% valid_pcls)
  })

  # B) Update the Parcel dropdown
  observe({
    df_parc <- filtered_parcels_data()
    new_choices <- sort(unique(df_parc$Parcel))
    old_sel <- isolate(input$parcel)
    if(!is.null(old_sel) && old_sel %in% new_choices) {
      updateSelectInput(session, "parcel", choices = new_choices, selected = old_sel)
    } else {
      if(length(new_choices)>0) {
        updateSelectInput(session, "parcel", choices = new_choices, selected = new_choices[1])
      } else {
        updateSelectInput(session, "parcel", choices = new_choices, selected = NULL)
      }
    }
  })

  # C) Current Parcel
  currentParcel <- reactiveVal()
  observeEvent(input$parcel, {
    currentParcel(input$parcel)
  })

  # D) shared_data
  shared_data <- reactive({
    sp <- currentParcel()
    req(sp)

    dtw_data <- dtw_pfix %>% filter(Parcel == sp)
    numeric_parcels <- filtered_parcels_data()
    cover_data <- numeric_parcels %>%
      filter(Parcel == sp) %>%
      mutate(
        # If TLC is less than Cover, override TLC to equal Cover
        TLC = if_else(TLC < Cover, Cover, TLC),
        annual = round(TLC - Cover, 1),
        Grass  = round(Grass, 1),
        Shrub  = round(Shrub, 1),
        Herb   = round(Herb, 1)
      ) %>%
      select(NominalYear, Grass, Shrub, Herb, annual) %>%
      pivot_longer(cols = -NominalYear, names_to = "Cover_Type", values_to = "Cover_Value") %>%
      mutate(Cover_Type = factor(Cover_Type, levels = c("annual", "Shrub", "Grass", "Herb")))

    ratio_data <- numeric_parcels %>%
      filter(Parcel == sp) %>%
      select(Parcel, NominalYear, pShrub, pGrass, pHerb)

    ndvi_data <- rs_pfix %>% filter(Parcel == sp)

    list(
      dtw_plot = dtw_data,
      cover_plot = cover_data,
      ratio_data = ratio_data,
      ndvi_plot = ndvi_data,
      precipitation_plot = ndvi_data
    )
  })

  # E) MAP - well color logic
  # If wells have TYPE == "W" => red, else => green or blue
  well_color <- function(t) {
    dplyr::case_when(
      t == "W" ~ "red",   # Production wells
      t == "T" ~ "blue",  # Monitoring wells
      TRUE     ~ "green"
    )
  }

  output$leaflet_map <- renderLeaflet({
    df_map <- filtered_polygons()
    req(nrow(df_map) > 0)

    leaflet() %>%
      # Use an Esri imagery tile for a more aerial background
      addProviderTiles("Esri.WorldImagery") %>%
      addPolygons(
        data = dd10ft_w84,
        fillColor = "red",
        fillOpacity = 0.01,
        color = "red",
        weight = 1
      ) %>%
      # Parcels with always-visible labels
      addPolygons(
        data = df_map,
        layerId = ~PCL,
        label = ~PCL,
        labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
        popup = ~paste0(
          "<strong>Parcel ID:</strong> ", PCL, "<br/>",
          "<strong>Greenbook Type:</strong> ", TYPE
        ),
        fillColor = ~case_when(
          TYPE == "A" ~ "ivory",
          TYPE == "B" ~ "yellow",
          TYPE == "C" ~ "darkgreen",
          TYPE == "D" ~ "#8470FF",
          TYPE == "E" ~ "dodgerblue",
          TRUE        ~ "lightgray"
        ),
        fillOpacity = 0.9,
        color = "black",
        weight = 0.5
      ) %>%
      # Monitoring sites (optional)
      addMarkers(
        data = monsitesgeo_wgs84,
        layerId = ~SITE,
        popup = ~paste("Monitoring Site:", SITE),
        label = ~SITE,
        labelOptions = labelOptions(textsize = "12px", noHide = FALSE, direction = "auto")
      ) %>%
      # Wells with color logic
      addCircleMarkers(
        data = wells,
        layerId = ~STAID,
        radius = 6,
        color = ~well_color(TYPE),
        stroke = FALSE, fillOpacity = 0.6,
        popup = ~paste("Well ID:", STAID, "<br/>Type:", TYPE),
        label = ~STAID,
        labelOptions = labelOptions(textsize = "12px", noHide = FALSE, direction = "auto")
      ) %>%
      setView(
        lng = mean(st_coordinates(df_map)[, "X"]),
        lat = mean(st_coordinates(df_map)[, "Y"]),
        zoom = 10
      )
  })

  # Clicking a polygon sets the Parcel
  observeEvent(input$leaflet_map_shape_click, {
    evt <- input$leaflet_map_shape_click
    if(!is.null(evt$id)) {
      valid_ids <- isolate(filtered_polygons()$PCL)
      if(evt$id %in% valid_ids) {
        updateSelectInput(session, "parcel", selected = evt$id)
      }
    }
  })

  # Zoom to selected parcel
  observeEvent(currentParcel(), {
    req(currentParcel())
    df_map <- filtered_polygons()
    if(nrow(df_map)==0) return(NULL)
    coords <- df_map %>% filter(PCL == currentParcel()) %>% st_coordinates()
    if(nrow(coords)>0) {
      lng <- mean(coords[, "X"])
      lat <- mean(coords[, "Y"])
      leafletProxy("leaflet_map") %>%
        setView(lng=lng, lat=lat, zoom=14)
    }
  })

  # F) PARCEL INFO
  output$parcel_info <- renderUI({
    sp <- currentParcel()
    req(sp)
    info <- parcels_augment %>% filter(pcl == sp)
    if(nrow(info)==0) return("No info available.")
    HTML(paste(
      "<p><strong>Ecological Site:</strong>", info$ecologic_3,
      "<strong>Greenbook Type:</strong>", info$type,
      "<strong>Holland Type:</strong>", info$comm_name,
      "<strong>Soils:</strong>", info$taxorder, "-", info$taxclname, "</p>",
      "<p><strong>Wellfield (I=in,O=out):</strong>", info$wellfield, "</p>"
    ))
  })

  # G) SPECIES TABLE
  output$species_table_years <- renderDataTable({
    sp <- currentParcel()
    req(sp)

    dd_ydt <- dd %>%
      filter(Parcel == sp) %>%
      filter(Year %in% c("1985","1986","1987","1992","2020","2024")) %>%
      select(Parcel, Year, CommonName, parcelMeanCover) %>%
      pivot_wider(names_from = Year, values_from = parcelMeanCover)

    DT::datatable(
      dd_ydt,
      caption = "Parcel average cover by species 1986–2024",
      extensions = "Buttons",            # Load the Buttons extension
      options = list(
        dom = "Bfrtip",                  # Show buttons (B), plus search/filter/etc.
        buttons = c("copy", "csv", "excel", "pdf"),  # Export options
        searching = TRUE,
        filter = "top",
        # Show all rows by default:
        pageLength = -1,
        # Let user pick from "All", 10, 25, 50, 100
        lengthMenu = list(c(-1, 10, 25, 50, 100),
                          c("All", "10", "25", "50", "100")),
        scrollX = TRUE
      )
    )
  })


  # H) TIME-SERIES PLOTS
  # 1) DTW
  output$dtw_plot <- renderPlotly({
    dtw_data <- shared_data()$dtw_plot
    req(nrow(dtw_data) > 0)
    p <- ggplot(dtw_data, aes(x=Year, y=DTW)) +
      geom_line(color="darkturquoise") +
      theme_minimal() +
      scale_y_reverse() +
      labs(y="DTW (ft bgs)", x=NULL) +
      scale_x_continuous(breaks=seq(1986,2026,2), limits = c(1985.5, 2026))
    ggplotly(p)
  })

## 2) Cover
  output$cover_plot <- renderPlotly({
    cdata <- shared_data()$cover_plot
    req(nrow(cdata) > 0)

    cdata <- cdata %>%
      mutate(Cover_Type = factor(
        Cover_Type,
        levels = c("annual","Shrub","Herb","Grass")
      ))

    base_1986 <- cdata %>%
      filter(NominalYear == 1986, Cover_Type %in% c("Grass","Shrub","Herb")) %>%
      pull(Cover_Value) %>%
      sum(na.rm = TRUE)

    base_grass_1986 <- cdata %>%
      filter(NominalYear == 1986, Cover_Type == "Grass") %>%
      pull(Cover_Value) %>%
      sum(na.rm = TRUE)


#     cat("Check cdata for year 1986:
# ")
#     print(cdata %>% filter(NominalYear == 1986))
#     cat("
# ")

    p <- ggplot(cdata, aes(x = NominalYear, y = Cover_Value, fill = Cover_Type)) +
      geom_bar(stat = "identity", position = "stack", alpha = 0.7) +
      # Add dashed lines at the baseline values
      geom_hline(yintercept = base_1986, color = "black",size = 0.1, linetype = "dashed") +
      geom_hline(yintercept = base_grass_1986, color = "green4", size = 0.1,linetype = "dashed") +
      labs(y = "Cover", x = NULL) +
      scale_fill_manual(values = c("Grass" = "chartreuse2", "Shrub" = "burlywood","Herb" = "chartreuse4", "annual" = "deeppink2")) +
      theme_minimal() +
      theme(legend.position = "none") +
      coord_cartesian(ylim = c(-.5, NA)) +
      scale_x_continuous(breaks = seq(1986, 2026, 2),limits = c(1985.5, 2026))




    ggplotly(p)

  })

  # 3) Proportion Grass
  output$cover_plot_ratios <- renderPlotly({
    rdata <- shared_data()$ratio_data
    req(nrow(rdata)>0)
    p <- ggplot(rdata, aes(x=NominalYear, y=pGrass)) +
      geom_bar(stat="identity") +
      labs(y="Proportion Grass", x=NULL) +
      theme_minimal() +
      scale_x_continuous(breaks=seq(1986,2026,2), limits = c(1985.5, 2026))
    ggplotly(p)
  })

  # 4) NDVI
  output$ndvi_plot <- renderPlotly({
    ndata <- shared_data()$ndvi_plot
    req(nrow(ndata)>0)
    p <- ggplot(ndata, aes(x=Year, y=NDVI_SUR)) +
      geom_bar(stat="identity", fill="darkseagreen") +
      theme_minimal() +
      labs(y="NDVI (Jul-Sep Avg)", x=NULL) +
      scale_x_continuous(breaks=seq(1986,2026,2), limits = c(1985.5, 2026))
    ggplotly(p)
  })

  # 5) Precip
  output$precipitation_plot <- renderPlotly({
    ndata <- shared_data()$precipitation_plot
    req(nrow(ndata)>0)
    p <- ggplot(ndata, aes(x=Year, y=PPT*0.0393701)) +
      geom_bar(stat="identity", fill="cadetblue") +
      theme_minimal() +
      labs(y="Precip (in)", x=NULL) +
      scale_x_continuous(breaks=seq(1986,2026,2),limits = c(1985.5, 2026))
    ggplotly(p)
  })

  # I) Initialize the parcel dropdown on app start
  observeEvent(TRUE, {
    df_parc <- filtered_parcels_data()
    new_choices <- sort(unique(df_parc$Parcel))
    if(length(new_choices)>0) {
      updateSelectInput(session, "parcel", choices=new_choices, selected="BLK094")
    } else {
      updateSelectInput(session, "parcel", choices=new_choices, selected=NULL)
    }
  }, once=TRUE)
}

shinyApp(ui, server)

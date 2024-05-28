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

# Read functions and data----
source("code/functions.R")

# DTW data ----
# depth to water kriged to grid and averaged across parcel
dtw_pfix <- readRDS(file = here('data','hydro','dtw_pfix.RDS')) %>% filter(Year > 1985)
# LANDSAT
rs_pfix <- readRDS(file = here('data','hydro','rs_pfix.RDS')) %>% filter(Year > 1985)
# Parcel attributes----
# - this could be replaced and is redundant with the parcelsgeo attributes
parcels_augment <- read_csv(here("data","parcel_augment2.csv")) %>% clean_names()

# LPT data----
lpt_datax <- read_csv(here("data","lpt_MASTER.csv"))
lpt_data <- mult_to_single_parcel_name(lpt_datax) %>% arrange(Parcel, Year)

# LPT summary----
transects_per_parcel <- lpt_data %>% select(Parcel,Year,Transect) %>%
  distinct(Parcel,Year,Transect) %>%
  arrange(Parcel,Year,Transect) %>%
  group_by(Parcel, Year) %>%
  summarise(nTransect= n())

species_hits_parcel <- lpt_data %>% select(Parcel,Year,Species,Code,CommonName,Cover) %>% filter(Cover != 0) %>%
  group_by(Parcel,Year,Species,Code,CommonName) %>%
  summarise(TotalSpHits = sum(Cover))

dd <- species_hits_parcel %>%
  left_join(transects_per_parcel, by = c("Parcel","Year")) %>%
  mutate(parcelMeanCover = round(TotalSpHits/nTransect,2))

upar <- dd %>% group_by(Parcel) %>% summarise(count = n()) %>% select(Parcel)
parlist <- upar$Parcel


# data from ESRI GDB

wells <- st_read(here("data","wells.geojson")) %>%  st_transform(crs = 4326)

monsitesgeo_wgs84 <- st_read(here("data","monsites.geojson")) %>% st_transform(crs = 4326)

parcelsgeox<- st_read(here("data","parcels.geojson")) %>% st_transform(crs = 4326)

parcelsgeo <- parcelsgeox %>%
  # filter(TYPE %in% c("E","D","C","B","A","A/C") | GB_TYPE %in% c("E","D","C","B","A/C","A")) %>%
  filter(PCL %in% parlist) #parcels having field data

dd10ft <- st_read(here("data","DrawdownContour10ftwellfieldDesig"))

# Reproject to WGS84 (EPSG:4326)
dd10ft_w84 <- st_transform(dd10ft, crs = st_crs(4326))

# parcel list
dropdownlist <- c("ABD012",
      "BGP031",
      "BGP047",
      "BGP088",
      "BGP094",
      "BGP154",
      "BGP157",
      "BGP162",
      "BIS055",
      "BIS060",
      "BIS085",
      "BLK002",
      "BLK006",
      "BLK008",
      "BLK009",
      "BLK011",
      "BLK016",
      "BLK021",
      "BLK024",
      "BLK029",
      "BLK033",
      "BLK039",
      "BLK044",
      "BLK069",
      "BLK074",
      "BLK075",
      "BLK077",
      "BLK093",
      "BLK094",
      "BLK095",
      "BLK096",
      "BLK099",
      "BLK115",
      "BLK142",
      "BLK143",
      "FSL044",
      "FSL051",
      "FSL053",
      "FSL054",
      "FSL064",
      "FSL065",
      "FSL116",
      "FSL118",
      "FSL120",
      "FSL123",
      "FSL124",
      "FSL128",
      "FSL129",
      "FSL130",
      "FSL138",
      "FSL158",
      "FSL161",
      "FSL166",
      "FSL172",
      "FSL187",
      "FSP004",
      "FSP006",
      "FSP015",
      "FSP019",
      "FSP020",
      "IND011",
      "IND019",
      "IND021",
      "IND024",
      "IND026",
      "IND029",
      "IND035",
      "IND064",
      "IND067",
      "IND086",
      "IND087",
      "IND096",
      "IND106",
      "IND111",
      "IND119",
      "IND122",
      "IND124",
      "IND132",
      "IND133",
      "IND139",
      "IND151",
      "IND163",
      "IND205",
      "IND231",
      "LAW030",
      "LAW035",
      "LAW043",
      "LAW052",
      "LAW062",
      "LAW063",
      "LAW065",
      "LAW070",
      "LAW072",
      "LAW078",
      "LAW082",
      "LAW085",
      "LAW105",
      "LAW107",
      "LAW108",
      "LAW109",
      "LAW112",
      "LAW120",
      "LAW122",
      "LAW137",
      "LNP018",
      "LNP019",
      "LNP045",
      "LNP050",
      "LNP095",
      "MAN006",
      "MAN007",
      "MAN014",
      "MAN034",
      "MAN037",
      "MAN038",
      "MAN042",
      "MAN060",
      "PLC007",
      "PLC024",
      "PLC028",
      "PLC056",
      "PLC059",
      "PLC070",
      "PLC072",
      "PLC088",
      "PLC092",
      "PLC097",
      "PLC106",
      "PLC107",
      "PLC121",
      "PLC136",
      "PLC137",
      "PLC144",
      "PLC220",
      "PLC223",
      "TIN006",
      "TIN028",
      "TIN030",
      "TIN050",
      "TIN053",
      "TIN061",
      "TIN064",
      "TIN067",
      "TIN068",
      "UNW029",
      "UNW031",
      "UNW039")


# filter parcels----
parcels <- readRDS(file = here('data','hydro','parcels.RDS'))%>%
  filter(Parcel %in% dropdownlist)

# p <- parcels %>% distinct(Parcel)
num_parcels <- length(unique(parcels$Parcel))

parcelsgeo <- parcelsgeox %>%
  filter(PCL %in% dropdownlist) %>%
  select(PCL,TYPE,COMM_NAME,FIELD_NAME,LEASE,Ranch_Plan)


# UI----
# add canals, wells, mitigation projects, greenbook types, baseline data, ndvi percentiles.
# Type E (irrag, RSM, ponds, NNP), C (meadow, shrub meadow), D (marsh, riparian woodland, shrub willow), B (saltbush, greasewood, rabbitbrush), A (upland, greasewood where ET < ppt), A/C (Alkali meadow where ET < ppt)

ui <- fluidPage(
  titlePanel("Green Book Vegetation Monitoring"),
  fluidRow(
    column(2, selectInput("parcel", "Choose Parcel", choices = unique(parcels$Parcel))),
    column(3, htmlOutput("parcel_info")),  # Display parcel information in 70% width column
    column(7, leafletOutput("leaflet_map", height ="400px")),
    style = "height: 400px;"  # Set height for the row
    # Render Leaflet map in the UI
  ),
  fluidRow(
    # plotlyOutput("pumping_plot",height = "180px"),
    plotlyOutput("dtw_plot", height = "180px"),
    plotlyOutput("cover_plot", height = "180px"),
    plotlyOutput("cover_plot_ratios", height = "180px"),
    plotlyOutput("ndvi_plot", height = "180px"),
    plotlyOutput("precipitation_plot", height = "180px"),
    plotlyOutput("ndvi_cover_regression_plot", height = "500px"),
    plotlyOutput("new_dtw_cover_regression_plot", height = "500px"),
    dataTableOutput("species_table_years", height ="600px")
  )
)


# SERVER----
server <- function(input, output, session) {

  # REACTIVE values----
  # current parcel index
  currentParcelIndex <- reactiveVal(1)

  # Function to update parcel based on index----
  updateParcel <- function(index) {
    selected_parcel <- parcels$Parcel[index]
    updateSelectInput(session, "parcel", selected = selected_parcel)
  }# updateParcel is the current parcel plotted

  # # Update dropdown selection and current index when selecting from dropdown----
  observeEvent(input$parcel, {
    currentParcelIndex(which(parcels$Parcel == input$parcel))
  })#updates currentParcelIndex in with dropdown parcel selection

  # Update plots based on selected parcel index
  observe({
    index <- currentParcelIndex()  # Added: Get the selected parcel index
    # Update plots here using the selected index
  })

  # SHARED DATA - shared dataframe----

  shared_data <- reactive({
    selected_parcel <- parcels$Parcel[currentParcelIndex()]
    list(
      dtw_plot = dtw_pfix %>% filter(Parcel == selected_parcel),
      cover_plot = parcels %>%
        filter(Parcel == selected_parcel) %>%
        mutate(annual = round(TLC - Cover,1),
               Grass = round(Grass,1),
               Shrub = round(Shrub,1),
               Herb = round(Herb, 1)) %>%
        select(NominalYear, Grass, Shrub, Herb, annual) %>%
        pivot_longer(cols = -NominalYear, names_to = "Cover_Type", values_to = "Cover_Value") %>%
        mutate(Cover_Type = factor(Cover_Type, levels = c("annual", "Shrub", "Grass","Herb"))),
      ratio_data = parcels %>% select(Parcel, NominalYear, pShrub, pGrass, pHerb) %>%
        filter(Parcel == selected_parcel),
      ndvi_plot = rs_pfix %>% filter(Parcel == selected_parcel),
      precipitation_plot = rs_pfix %>% filter(Year == 1986, Parcel == selected_parcel)
    )
  })


  # MAP - Leaflet map----
  # Observer for updating map zoom when parcel changes in dropdown
  observeEvent(input$parcel, {
    selected_parcel <- input$parcel
    # Get coordinates of the selected parcel
    parcel_coords <- parcelsgeo %>%
      filter(PCL == selected_parcel) %>%
      st_coordinates()
    # Calculate the average coordinates to focus the map view
    lng <- mean(parcel_coords[, "X"])
    lat <- mean(parcel_coords[, "Y"])
    # Update the map view
    leafletProxy("leaflet_map") %>%
      setView(lng = lng, lat = lat, zoom = 14) # Adjust zoom level as needed
  })


# type_colors <- c("A/C" = "antiquewhite", "B" = "yellow4", "C" = "chartreuse4", "D" = "lightslateblue", "E" = "cadetblue")
type_colors <- c("A" = "lightyellow","A/C" = "darkolivegreen1", "B" = "yellowgreen", "C" = "#7FFF00", "D" = "#8470FF", "E" = "dodgerblue")

  output$leaflet_map <- renderLeaflet({

    pal <- colorFactor(c("green", "red","blue"), domain = c("T", "W", "V"))

    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      addPolygons(data = dd10ft_w84,
                  fillColor = "red",
                  fillOpacity = .01,
                  color = "red",
                  weight = 1) %>%
      addPolygons(data = parcelsgeo,
                  popup = ~paste("Parcel ID:", ~PCL, "<br/>",
                                "Site:", ~SITE, "<br/>",
                                "GB TYPE:", ~TYPE, "<br/>",
                                "Holland:", ~COMM_NAME,"<br/>",
                                "Field Name:" ~FIELD_NAME, "</br>"
                                ),
                  fillColor = ~factor(TYPE, levels = names(type_colors), labels = type_colors),  # Use factor with predefined colors
                  fillOpacity = .9,  # Adjust fill opacity as needed
                  color = "black",    # Adjust border color as needed
                  weight = .5,
                  label = ~PCL,       # Add parcel ID labels
                  labelOptions = labelOptions(noHide = FALSE, direction = "auto")
                  ) %>%
      addMarkers(data = monsitesgeo_wgs84,
                 popup = ~paste("Monitoring Site:",~SITE,"STAID: ", ~STAID),
                 label = ~SITE,
                 labelOptions = labelOptions(textsize = 2,noHide = TRUE, direction = "auto",
                 style = list("background-color" = "lightblue",  # Change background color
                              "font-size" = "12px",  # Change font size
                              "color" = "black"  # Change font color
                              ))
      ) %>%

      addCircleMarkers(data = wells %>% filter(TYPE %in% c("W")),
        radius = 10,
        color = "red",
        stroke = FALSE, fillOpacity = 0.5,
        popup = ~paste("ID: ", ~STAID),
        label = ~STAID,
        labelOptions = labelOptions(textsize = 2,noHide =TRUE, direction = "auto")) %>%

      setView(lng = mean(st_coordinates(parcelsgeo)[, "X"]),
              lat = mean(st_coordinates(parcelsgeo)[, "Y"]),
              zoom = 11) # Adjust initial view
  })

  observeEvent(input$leaflet_map_shape_click, {
    event_data <- input$leaflet_map_shape_click
    if (!is.null(event_data)) {
      selected_parcel <- event_data$PCL
      updateSelectInput(session, "Parcel", selected = selected_parcel)
    }
  })


  # Update plots based on selected parcel index--
  observe({
    index <- currentParcelIndex()  # Added: Get the selected parcel index
  })


  # DTW - dtw_plot based on the current selected parcel----
  output$dtw_plot <- renderPlotly({
    dtw_plot <- shared_data()$dtw_plot
    hline_value <- dtw_plot %>% filter(Year == 1986) %>% pull(DTW)

    p <- ggplot(dtw_plot, aes(x = Year, y = DTW )) +#text = paste("Flag:", flag)
      geom_line(color = "darkturquoise") +
      geom_hline(yintercept = hline_value, color = "black", size = 0.1, linetype = "dashed") +
      geom_segment(aes(x = Year, xend = Year, y = DTW, yend = DTW-.5), color = ifelse(dtw_plot$DTW < hline_value, "cornflowerblue", "transparent"), size = 1.5) +

      labs(y = "DTW (ft bgs)", x = NULL) +
      theme_minimal() +
      scale_y_reverse() +
      scale_x_continuous(breaks = seq(from = 1986, to = 2026, by = 2))

    return(plotly::ggplotly(p))
  })

  # NDVI - ndvi_plot----
  output$ndvi_plot <- renderPlotly({
    selected_parcel <- parcels$Parcel[currentParcelIndex()]

    ndvi_data <- rs_pfix %>% filter(Parcel == selected_parcel)

    hline_value <- ndvi_data %>% filter(Year == 1986) %>% pull(NDVI_SUR)

    # Count the number of times NDVI value is above the hline
    flagged_count <- sum(ndvi_data$NDVI_SUR > hline_value - 0.01)

    # Calculate fraction of years with flagged NDVI value
    fraction_flagged <- flagged_count * 100 / nrow(ndvi_data)

    p <- ggplot(ndvi_data, aes(x = Year, y = NDVI_SUR)) +
      geom_bar(stat = "identity", fill = "darkseagreen", alpha = 0.7) +
      geom_hline(yintercept = hline_value, color = "black", size = 0.1, linetype = "dashed") +
      geom_segment(aes(x = Year, xend = Year, y = NDVI_SUR, yend = NDVI_SUR + 0.01), color = ifelse(ndvi_data$NDVI_SUR > hline_value - 0.01, "cyan4", "transparent"), size = 1.5) +  # Draw red line segments above the hline
      annotate("text", x = 2000, y = max(ndvi_data$NDVI_SUR), label = paste("Years reaching 1986:", flagged_count -1, "/", nrow(ndvi_data) -1, "=", round(fraction_flagged, 0), "%"), color = "blue") +  # Annotation for flagged count as fraction of total years
      labs(y = "NDVI", x = NULL) +
      theme_minimal() +
      scale_y_continuous(labels = function(x) round(x, 1)) +  # Round to one decimal place
      coord_cartesian(ylim = c(0, NA)) +  # Setting y-axis limits to start from 0 and auto-adjust the upper limit
      scale_x_continuous(breaks = seq(from = 1986, to = 2026, by = 2))

    return(plotly::ggplotly(p))
  })
  # PARCEL INFO  - output parcel_info----
  output$parcel_info <- renderUI({
    selected_parcel <- input$parcel
    # parcel_geo_info <- parcelsgeo %>% filter(PCL == selected_parcel)
    parcel_info <- parcels_augment %>% filter(pcl == selected_parcel)
    HTML(paste("<p><strong>Ecological Site:</strong>", parcel_info$ecologic_3,
               "<strong>Greenbook Type:</strong> ", parcel_info$type, " ",
               "<strong>Holland Type:</strong> ", parcel_info$comm_name, " ",
               "<strong>Soils:</strong> ",parcel_info$taxorder, "- ", parcel_info$taxclname, "-", "</p>",
               "<p><strong>Wellfield (I=in,O=out):</strong>", parcel_info$wellfield, "</p>"), sep = "")
  })


  # LPT - cover_plot----
  output$cover_plot <- renderPlotly({
    cover_plot <- shared_data()$cover_plot

    hline_cover <- cover_plot %>%
      filter(NominalYear == 1986, Cover_Type %in% c("Grass", "Shrub","Herb")) %>%
      pull(Cover_Value) %>%
      sum()

    hline_grass <- cover_plot %>%
      filter(NominalYear == 1986, Cover_Type %in% c("Grass")) %>%
      pull(Cover_Value) %>%
      sum()

    p <- ggplot(cover_plot, aes(x = NominalYear, y = Cover_Value, fill = Cover_Type)) +
      geom_bar(stat = "identity", position = "stack", alpha = 0.7) +
      geom_hline(yintercept = hline_cover, color = "black", size = 0.1, linetype = "dashed") +
      geom_hline(yintercept = hline_grass, color = "green4", size = 0.1, linetype = "dashed") +
      labs(y = "Cover", x = NULL) +
      scale_fill_manual(values = c("Grass" = "chartreuse2", "Shrub" = "burlywood","Herb" = "chartreuse4", "annual" = "deeppink2")) +
      theme_minimal() +
      theme(legend.position = "none") +
      coord_cartesian(ylim = c(0, NA)) +
      scale_x_continuous(breaks = seq(from = 1986, to = 2026, by = 2))

    return(plotly::ggplotly(p))
  })



  output$cover_plot_ratios <- renderPlotly({
    ratio_plot <- shared_data()$ratio_data


    p <- ggplot(ratio_plot, aes(x = NominalYear, y = pGrass)) +
      geom_bar(stat = "identity", position = "stack", alpha = 0.7) +
      labs(y = "Proportion Grass", x = NULL) +
      theme_minimal() +
      theme(legend.position = "none") +
      coord_cartesian(ylim = c(0, NA)) +
      scale_x_continuous(breaks = seq(from = 1986, to = 2026, by = 2))

    return(plotly::ggplotly(p))
  })

  # PPT - precipitation_plot----
  output$precipitation_plot <- renderPlotly({
    selected_parcel <- parcels$Parcel[currentParcelIndex()]

    p <- ggplot(rs_pfix %>% filter(Parcel == selected_parcel), aes(x = Year, y = PPT * 0.0393701)) +
      geom_bar(stat = "identity", fill = "cadetblue", alpha = 0.7) +
      geom_hline(yintercept = rs_pfix %>% filter(Year == 1986, Parcel == selected_parcel) %>% pull(PPT) * 0.0393701, color = "black", size = 0.1, linetype = "dashed") +
      labs(x = "Year", y = "PPT (inches)") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(from = 1986, to = 2026, by = 2))

    return(plotly::ggplotly(p))
  })
  # NDVIxCOVER - ndvi_cover_regression_plot----
  output$ndvi_cover_regression_plot <- renderPlotly({
    veg_data <- parcels %>%
      filter(Parcel == input$parcel) %>%
      select(Year, TLC, Cover, Grass, Shrub)

    ndvi_data <- rs_pfix %>%
      filter(Parcel == input$parcel) %>%
      select(Year, NDVI_SUR)

    combined_data <- inner_join(veg_data, ndvi_data, by = "Year")

    lm_model <- lm(Cover ~ NDVI_SUR, data = combined_data)

    # Calculate the midpoint of the x-axis range
    x_midpoint <- (min(combined_data$NDVI_SUR) + max(combined_data$NDVI_SUR)) / 2

    # Create the plot
    p <- ggplot(combined_data, aes(x = NDVI_SUR, y = Cover)) +
      geom_point(size = 1) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "NDVI", y = "Cover") +
      theme_minimal() +
      annotate("text", x = x_midpoint, y = max(combined_data$Cover),
               label = paste("R^2 =", round(summary(lm_model)$r.squared, 2), " | Cover =",
                             round(coef(lm_model)[1], 2), "+", round(coef(lm_model)[2], 2), "* NDVI"),
               hjust = 0.5, vjust = 1, size = 3)

    # Add labels for 1986 and the most recent year
    p <- p +
      geom_text(data = filter(combined_data, Year %in% c(1986, 1992, 2000, 2020, max(combined_data$Year))),
                aes(label = Year), vjust = 0, hjust = 0, color = "red4", nudge_x = .008, nudge_y = 0)

    plotly::ggplotly(p)


  })

  # DTWxCOVER - new_dtw_cover_regression_plot----------------
  output$new_dtw_cover_regression_plot <- renderPlotly({

    dtw_data <- dtw_pfix %>%
      filter(Parcel == input$parcel) %>%
      select(Year, DTW)

    ppt_data <- rs_pfix %>% filter(Parcel == input$parcel)%>%
      select(Year, PPT)

    cover_data <- parcels %>%
      filter(Parcel == input$parcel) %>%
      select(Year, Cover, Grass)

    combined_data <- inner_join(dtw_data, cover_data, by = "Year")
    x_midpoint <- (min(combined_data$DTW) + max(combined_data$DTW)) / 2
    lm_model <- lm(Cover ~ DTW, data = combined_data)

    # Create the plot
    p <- ggplot(combined_data, aes(x = DTW, y = Cover)) +
      geom_point(size = 1) +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(x = "DTW (ft bgs)", y = "Cover") +
      theme_minimal() +
      annotate("text", x = x_midpoint, y = max(combined_data$Cover),
               label = paste("R^2 =", round(summary(lm_model)$r.squared, 2), " | Cover =",
                             round(coef(lm_model)[1], 2),"+", round(coef(lm_model)[2], 2), "* DTW"),
               hjust = 0.5, vjust = 1, size = 3)

    # Add labels for 1986 and the most recent year
    p <- p +
      geom_text(data = filter(combined_data, Year %in% c(1986, 1992, 2000, 2020, max(combined_data$Year))),
                aes(label = Year), vjust = 0, hjust = 0, color = "red4", nudge_x = .5, nudge_y = 0)

    plotly::ggplotly(p)


  })


  # TABLE - Generate species interannual comparison----
  output$species_table_years <- renderDataTable({

    dd_ydt <- dd %>% filter(Parcel == input$parcel) %>%
      filter(Year %in% c("1984","1985","1986","1987","1991","1992","1993","2022","2023")) %>%
      select(Parcel, Year, Species, Code, CommonName, parcelMeanCover) %>%
      pivot_wider(names_from = Year, values_from = parcelMeanCover)

    datatable(dd_ydt,
              options = list(
                searching = TRUE,  # Enable search functionality
                filter = "top",    # Place filter boxes at the top
                pageLength = 10,   # Set default number of rows per page
                lengthMenu = c(10, 25, 50, 100),  # Customize the rows per page dropdown menu
                dom = 'lfrtip',    # Define the layout of the datatable (l - length changing input, f - filtering input, r - processing display element, t - table, i - information summary, p - pagination control, < and > - div elements)
                buttons = c('copy', 'csv', 'excel', 'pdf'),  # Add export buttons for copying, CSV, Excel, and PDF
                scrollX = TRUE     # Enable horizontal scrolling if needed
              )
    )
  })
}


# Run the app
shinyApp(ui = ui, server = server)





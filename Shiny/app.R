
# Shiny App for Wind River Elk and Mule deer real-time monitoring
# Written by G. Rozman: May 18th, 2021; updated by L. Wilde: Sep 10, 2021

#NEED: when change bmap, need to keep same entries for individuals; learn how to extract values of rater to points, learn how to 

source("G:/My Drive/Wind_River_visproj/WindRiver_vis_proj/MiscFunctions/standard_functions.R")
#remotes::install_github("wmgeolab/rgeoboundaries")
setUp(c("sf","dplyr","leaftime","htmltools","shiny","leaflet","RColorBrewer","shinyWidgets","MODIStsp", "tidyverse", "lme4", "rgeoboundaries", "ggplot2", "maps", "remotes", "devtools", "tigris", "raster", "here", "viridis", "viridisLite", "RColorBrewer"))

setwd("G:/My Drive/Wind_River_visproj/WindRiver_vis_proj/")

# Import data sets
Elks.df = read.csv("./Shiny/Elks.df.csv", header = TRUE)
MuleDeer.df = read.csv("./Shiny/MuleDeer.df.csv", header = TRUE)

#Elk_status.df = read.csv("Elk_status.df.csv", header = TRUE)
#MuleDeer_status.df = read.csv("MuleDeer_status.df.csv", header = TRUE)

# add species column
Elks.df$Species = "Elk"
MuleDeer.df$Species = "Mule Deer"

# select relevant columns
Elks.df = Elks.df[c("AID" , "GPS.Latitude",  "GPS.Longitude", "DateTime", "Species", "Status")]
MuleDeer.df = MuleDeer.df[c("AID" , "GPS.Latitude",  "GPS.Longitude", "DateTime", "Species", "Status")]

# Back up in order to merge and get lon and lat columns after getting geometries
Elks.Mov = Elks.df
MuleDeer.Mov = MuleDeer.df

# convert data to an sf object
projcrs <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 +no_defs"

Elks.df <- st_as_sf(x = Elks.df,                         
                    coords = c("GPS.Longitude", "GPS.Latitude"),
                    crs = projcrs)

MuleDeer.df <- st_as_sf(x = MuleDeer.df,                         
                        coords = c("GPS.Longitude", "GPS.Latitude"),
                        crs = projcrs)

#Elk_status.df <- st_as_sf(x = Elk_status.df,                         
#                    coords = c("GPS.Longitude", "GPS.Latitude"),
#                    crs = projcrs)

#MuleDeer_status.df <- st_as_sf(x = MuleDeer_status.df,                         
#                        coords = c("GPS.Longitude", "GPS.Latitude"),
#                        crs = projcrs)
#names = sample(unique(Elks.df$Serial_Number), 15)
#Elks.df.sub = Elks.df[which(Elks.df$Serial_Number %in% names),]
Elks.df.sub = Elks.df
Elks.df.sub = cbind(Elks.df.sub,
                    Elks.Mov[c("GPS.Latitude","GPS.Longitude")])

Elks.df.sub$Date = as.Date(Elks.df.sub$DateTime)

MuleDeer.df = cbind(MuleDeer.df,
                    MuleDeer.Mov[c("GPS.Latitude","GPS.Longitude")])

MuleDeer.df$Date = as.Date(MuleDeer.df$DateTime)

# Bind both species data sets
All.Species = rbind(Elks.df.sub, MuleDeer.df)

#All.Status = rbind(Elk_status.df, MuleDeer_status.df)
All.Status = All.Species[which(!(is.na(All.Species$Status))),]


#how to guide of downloading modis layers at https://rspatialdata.github.io/land_cover.html

MODIStsp_get_prodlayers("MCD12Q1")
# USA <- states(year = 2000, resolution = '20m', class="sf")
# 
# WY <- USA[USA$NAME == "Wyoming", ]
# #plot(WY)
# 
# spatial_filepath <- "G:/My Drive/Wind_River_visproj/Wind River Elk and Mule Deer/Wyoming_Poly/WY_state.shp"
# st_write(WY, paste0(spatial_filepath))

#bit of a delay, use current year -2 data (2019 most current)

#cord works, but doesnt need to run - load raster instead
# MODIStsp(gui             = FALSE,
#          out_folder      = "G:/My Drive/Wind_River_visproj/Wind River Elk and Mule Deer/LandCoverData",
#          out_folder_mod  = "LandCoverData",
#          selprod         = "LandCover_Type_Yearly_500m (MCD12Q1)",
#          bandsel         = "LC1", 
#          user            = "mstp_test" ,
#          password        = "MSTP_test_01",
#          start_date      = "2019.01.01", 
#          end_date        = "2019.12.31", 
#          verbose         = FALSE,
#          spatmeth        = "file",
#          spafile         = "G:/My Drive/Wind_River_visproj/Wind River Elk and Mule Deer/Wyoming_Poly/WY_state.shp",
#          out_format      = "GTiff")

#original path
IGBP_raster <- raster(here::here("G:/My Drive/Wind_River_visproj/Wind River Elk and Mule Deer/LandCoverData/WY_state/LandCover_Type_Yearly_500m_v6/LC1/MCD12Q1_LC1_2019_001.tif"))

#new path
IGBP_raster <- raster(here::here("G:/My Drive/Wind_River_visproj/Wind River Elk and Mule Deer/WindRiver/MCD12Q1_LC1_2019_001.tif"))

WY <- st_transform(st_read("G:/My Drive/Wind_River_visproj/WindRiver_vis_proj/Wyoming_Poly/WY_state.shp"), crs = crs(Elks.df))

# Transforming data
IGBP_raster <- projectRaster(IGBP_raster, crs = crs(Elks.df))

# Cropping data
IGBP_raster <- raster::mask(IGBP_raster, as_Spatial(WY))

# Converting the raster object into a dataframe and converting the IGBP classification into a factor
IGBP_df <- as.data.frame(IGBP_raster, xy = TRUE, na.rm = TRUE) %>%
  mutate(MCD12Q1_LC1_2019_001 = as.factor(round(MCD12Q1_LC1_2019_001)))
rownames(IGBP_df) <- c()

IGBP_raster$MCD12Q1_LC1_2019_001 <- unique(as.factor(round(IGBP_raster$MCD12Q1_LC1_2019_001)))


# Renaming IGBP classification levels
levels(IGBP_df$MCD12Q1_LC1_2019_001) <- c( "Evergreen needleleaf forests",
                                           "Evergreen broadleaf forests",
                                           "Deciduous needleleaf forests",
                                           "Deciduous broadleaf forests",
                                           "Mixed forests",
                                           "Closed shrublands",
                                           "Open shrublands",
                                           "Woody savannas",
                                           "Savannas",
                                           "Grasslands",
                                           "Permanent wetlands",
                                           "Croplands",
                                           "Urban and built-up lands",
                                           "Cropland/natural vegetation mosaics",
                                           "Snow and ice",
                                           "Barren",
                                           "Water bodies")
# Visualising using ggplot2
ggplot() + 
  geom_raster(data = IGBP_df,
              aes(x = x, y = y, fill = MCD12Q1_LC1_2019_001)) +
  geom_sf(data = WY, inherit.aes = FALSE, fill = NA) +
  scale_fill_viridis(name = "Land Cover Type", discrete = TRUE) +
  labs(title = "Land Cover classification in Wyoming",
       subtitle = "Year: 2019",
       x = "Longitude",
       y = "Latitude")



#solutions from https://stackoverflow.com/questions/46575204/leaflet-custom-bin-legend-for-raster and https://stackoverflow.com/questions/47410833/how-to-customize-legend-labels-in-r-leaflet

#need to fix colors. unclear what is happening, but they are jumbled up in the legend

dispal  <- c(  "forestgreen",
               "green3",
               "olivedrab2",
               "palegreen",
               "darkolivegreen3",
               "rosybrown",
               "navajowhite1",
               "yellowgreen",
               "gold2",
               "goldenrod1",
               "skyblue4",
               "khaki",
               "red",
               "lightgoldenrod3",
               "snow",
               "snow3",
               "royalblue4")

at <- c(unique(as.factor(round(IGBP_raster$MCD12Q1_LC1_2019_001))),18)
cb <- colorBin(palette = dispal, bins = at, domain = at, na.color = "transparent")


labels = c( "Evergreen needleleaf forests",
            "Evergreen broadleaf forests",
            "Deciduous needleleaf forests",
            "Deciduous broadleaf forests",
            "Mixed forests",
            "Closed shrublands",
            "Open shrublands",
            "Woody savannas",
            "Savannas",
            "Grasslands",
            "Permanent wetlands",
            "Croplands",
            "Urban and built-up lands",
            "Cropland/natural vegetation mosaics",
            "Snow and ice",
            "Barren",
            "Water bodies")

leaflet() %>% addProviderTiles("Esri.WorldTerrain") %>% addRasterImage(IGBP_raster, opacity = 0.5, colors = cb) %>%  addLegend(pal=cb, title = "Land Cover Type", values = at, labFormat = function(type, cuts, p) {  # Here's the trick
  paste0(labels)
})

#https://lpdaac.usgs.gov/documents/101/MCD12_User_Guide_V6.pdf p 7 legend

raster

#----------------------------------------------------------------------------------------
#tabsetPanel(tabPanel("Explore the Data"), tabPanel("Analyze"), tabPanel("Compare"), tabPanel("Learn")) 
#ui <- shinyUI(fluidPage(headerPanel(title=HTML(""), windowTitle = ""), tabPanel("Explore the Data")

#--------------------new UI --------------------------------------
ui2 <- navbarPage("Home", setBackgroundColor(color = c("#E0E0E0", "#F0F8FF"), gradient  = "radial", direction = c("bottom", "right"), shinydashboard  = FALSE ),
                  titlePanel(fluidRow(
                    align = "left",
                    column(
                      1,
                      img(src="https://upload.wikimedia.org/wikipedia/commons/4/40/Seal_of_the_United_States_Fish_and_Wildlife_Service.svg", 
                          height="90",width="70")
  ),
  column(
    1,
    img(src="https://sheridanclt.org/wp-content/uploads/2020/06/WC-Fish-Wildlife-logo_black-1024x556.png", 
        height="90",width="160")
  ), h1(strong("Wind River Ungulate Study"), align = "center"))),
  
  
                  tabPanel("Explore the Map",# Side Bar Panel
                           sidebarLayout(sidebarPanel(
                             
                             # Map selector
                             selectInput("bmap", "Select Base map:", choices = c("World Map" = "Esri.NatGeoWorldMap",
                                                                                 "Roads and Intersections" = "OpenStreetMap", "Wold Imagery" = "Esri.WorldImagery",
                                                                                 "Terrain" = "Stamen.TerrainBackground", "Wold Imagery Night" = "NASAGIBS.ViirsEarthAtNight2012"),
                                         selected = "Esri.NatGeoWorldMap", width = 500),
                             # Species selector
                             selectInput("Species", "Select Species:", choices = unique(All.Species$Species), multiple = T, selected = "Elk", width = 500),
                             
                             # Date Slider
                             sliderInput("range", "Select Date Range:", min(Elks.df.sub$Date), max(Elks.df.sub$Date),
                                         value = range(Elks.df.sub$Date), step = 1, width = 500),
                             br(),                      
                             # Individual selector
                             pickerInput(inputId = "AID",
                                         label = "Individual:", 
                                         choices = c(unique(Elks.df.sub$AID)),
                                         options = list(`actions-box` = TRUE,
                                                        size = 10,
                                                        `selected-text-format` = "count > 3",
                                                        width = 500),
                                         multiple = TRUE,
                                         width = 500)), 
                             
                             
                             
                             
                             # Main Panel
                             mainPanel(leafletOutput("map", width = "100%", height = 600))
                             
                           )),
                  
                  tabPanel("Analyze"),
                  
                  tabPanel("Compare",
                           sidebarLayout(
                    sidebarPanel(
                      radioButtons("comparison_metric", h3("Select comparison:"),
                                   c("Distance Travelled" = "NSD",
                                     "Distance to Road" = "DTR",
                                     "Habitat Use" = "HS",
                                     "Migration Date" = "MD")),
                      textOutput("epi_notes_1"),
                      textOutput("epi_notes_2"),
                      textOutput("epi_notes_3")
                    ),
                    
                    mainPanel()
                  )),
                  
                  tabPanel("Learn"),
  fluidRow(column(10,
                  h6(div("Gabriel Rozman and Luke Wilde*", style = 'color:black')),
                  h6(div("Wyoming Cooperative Fish And Wildlife Research Unit", style = 'color:black')),
                  #h6(div("Department of Zoology and Physiology", style = 'color:white')),
                  #h6(div("University of Wyoming", style = 'color:white')),
                  h6(div("*corresponding email: lwilde2@uwyo.edu", style = 'color:black')))))
                        


#---------------------------original UI -----------------------------
ui <- fluidPage(headerPanel(title=HTML(""), windowTitle = ""),
                        # Title Panel
                        # setBackgroundImage(
                        #     src = "https://www.publicdomainpictures.net/pictures/200000/velka/plain-olive-background.jpg"
                        #    
                        # ),
                        setBackgroundColor(color = c("#E0E0E0", "#F0F8FF"), gradient  = "radial", direction = c("bottom", "right"), shinydashboard  = FALSE ),
                        
                        titlePanel(fluidRow(
                          align = "left",
                          column(
                            1,
                            img(src="https://upload.wikimedia.org/wikipedia/commons/4/40/Seal_of_the_United_States_Fish_and_Wildlife_Service.svg", 
                                height="90",width="70")
                          ),
                          column(
                            1,
                            img(src="https://sheridanclt.org/wp-content/uploads/2020/06/WC-Fish-Wildlife-logo_black-1024x556.png", 
                                height="90",width="160")
                          ), h1(strong("Wind River Ungulate Study"), align = "center"))),
                        
                        # Side Bar Panel
                        sidebarLayout(sidebarPanel(
                          
                          # Map selector
                          selectInput("bmap", "Select Base map:", choices = c("World Map" = "Esri.NatGeoWorldMap",
                                                                       "Roads and Intersections" = "OpenStreetMap", "Wold Imagery" = "Esri.WorldImagery",
 "Terrain" = "Stamen.TerrainBackground", "Wold Imagery Night" = "NASAGIBS.ViirsEarthAtNight2012"),
                                      selected = "Esri.NatGeoWorldMap", width = 500),
                          # Species selector
                          selectInput("Species", "Select Species:", choices = unique(All.Species$Species), multiple = T, selected = "Elk", width = 500),
                          
                          # Date Slider
                          sliderInput("range", "Select Date Range:", min(Elks.df.sub$Date), max(Elks.df.sub$Date),
                                      value = range(Elks.df.sub$Date), step = 1, width = 500),
                          br(),                      
                          # Individual selector
                          pickerInput(inputId = "AID",
                                      label = "Individual:", 
                                      choices = c(unique(Elks.df.sub$AID)),
                                      options = list(`actions-box` = TRUE,
                                                     size = 10,
                                                     `selected-text-format` = "count > 3",
                                                     width = 500),
                                      multiple = TRUE,
                                      width = 500)), 
                          
                          
                          
                          
                          # Main Panel
                          mainPanel(leafletOutput("map", width = "100%", height = 700))
                          
                        ),
                        fluidRow(column(10,
                                        h6(div("Gabriel Rozman and Luke Wilde*", style = 'color:black')),
                                        h6(div("Wyoming Cooperative Fish And Wildlife Research Unit", style = 'color:black')),
                                        #h6(div("Department of Zoology and Physiology", style = 'color:white')),
                                        #h6(div("University of Wyoming", style = 'color:white')),
                                        h6(div("*corresponding email: lwilde2@uwyo.edu", style = 'color:black')))),
)

#-----------------------------server------------------------------------------------------------

#if individuals selected >23, change legend size as it blocks the rest of the map features

server <- function(input, output, session) {
  
  # Change slider and individual selector depending on which species was selected
  Species.df <- reactive({
    All.Species[which(All.Species$Species==input$Species),]
  })
  observeEvent(Species.df(), {
    updateSliderInput(session = session, inputId = "range", min = min(Species.df()$Date), max = max(Species.df()$Date),
                      value = range(Species.df()$Date), step = 1)
    
    updatePickerInput(session = session, inputId = "AID",
                      label = paste0(input$Species," ", "Individual:"), 
                      choices = c(unique(Species.df()$AID)),
                      options = list(`actions-box` = TRUE,
                                     size = 10,
                                     `selected-text-format` = "count > 3"))
    
  })
  SpeciesFiltered <- reactive({
    Species.df()[Species.df()$Date >= input$range[1] & Species.df()$Date <= input$range[2] & Species.df()$AID %in% input$AID,]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  # colorpal <- reactive({
  #     colorNumeric(input$colors, Elks.df.sub$Serial_Number)
  # })
  if(nrow(All.Status)>0){
    CollarStatus <- reactive({
      All.Status[which(All.Status$AID %in% SpeciesFiltered()$AID & All.Status$Date >= input$range[1] & All.Status$Date <= input$range[2] ),]
    })
    
    output$map <- renderLeaflet({
      # Use leaflet() here, and only include aspects of the map that
      # won't need to change dynamically (at least, not unless the
      # entire map is being torn down and recreated).
      leaflet(Species.df()) %>% addProviderTiles(input$bmap) %>% addMeasure(primaryAreaUnit = "sqmiles",activeColor = "#0bd3d3",completedColor = "#f890e7") %>% addScaleBar(position="bottomleft") %>% addMiniMap() %>%
        fitBounds(~min(GPS.Longitude), ~min(GPS.Latitude), ~max(GPS.Longitude), ~max(GPS.Latitude)) 
    })
    
    # Incremental changes to the map (in this case, replacing the
    # circles when a new color is chosen) should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    observe({
      pal <- colorFactor(palette = 'Dark2', domain = SpeciesFiltered()$AID)
      leafletProxy("map", data = SpeciesFiltered()) %>% addMeasure(primaryAreaUnit = "sqmiles",activeColor = "#0bd3d3",completedColor = "#f890e7") %>% addScaleBar(position="bottomleft") %>% addMiniMap() %>% 
        clearMarkers() %>% clearControls() %>%
        addLegend(position = "bottomright", pal = pal , values = input$AID, title = paste0(input$Species," ", "ID:")) %>%
        addCircleMarkers(data = SpeciesFiltered(), group = "Date",color = "black",
                         fillColor = ~pal(AID), fillOpacity = 1, weight = 2, radius = 4,
                         popup = ~paste("Date: ", Date, " , ", paste0(unique(SpeciesFiltered()$Species), ", ID: ", AID))
        )%>%
        #addLegend(position = "bottomleft", pal = pal , values = CollarStatus()$Serial_Number, title = paste0(input$Species," ","Mortalities or Collar release")) %>%
        addCircleMarkers(data = CollarStatus(), group = "Date", color = "red",
                         label =  ~paste("",'<strong>', unique(CollarStatus()$Status, '</strong>'))%>% 
                           lapply(htmltools::HTML),
                         labelOptions = labelOptions(noHide = T, direction = "bottom"), 
                         fillColor = ~pal(AID), fillOpacity = 1, weight = 4, radius = 5,
                         popup = ~paste("Date: ", Date, " , ", paste0(unique(CollarStatus()$Species), " ID: ", AID)))  
    })
    
    
  }else{
    output$map <- renderLeaflet({
      # Use leaflet() here, and only include aspects of the map that
      # won't need to change dynamically (at least, not unless the
      # entire map is being torn down and recreated).
      leaflet(Species.df()) %>% addProviderTiles(input$bmap) %>% addMeasure() %>% addScaleBar(position="bottomleft") %>% addMiniMap() %>%
        fitBounds(~min(GPS.Longitude), ~min(GPS.Latitude), ~max(GPS.Longitude), ~max(GPS.Latitude)) 
    })
    
    # Incremental changes to the map (in this case, replacing the
    # circles when a new color is chosen) should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    observe({
      pal <- colorFactor(palette = 'Dark2', domain = SpeciesFiltered()$AID)
      leafletProxy("map", data = SpeciesFiltered()) %>% addMeasure() %>% addScaleBar(position="bottomleft") %>% addMiniMap() %>% 
        clearMarkers() %>% clearControls() %>%
        addLegend(position = "bottomright", pal = pal , values = input$AID, title = paste0(input$Species," ", "ID:")) %>%
        addCircleMarkers(data = SpeciesFiltered(), group = "Date",color = "black",
                         fillColor = ~pal(AID), fillOpacity = 1, weight = 2, radius = 4,
                         popup = ~paste("Date: ", Date, " , ", paste0(unique(SpeciesFiltered()$Species), ", ID: ", AID)) %>% addTiles() %>% addMeasure(position = "topright",primaryLenghUnit="feet", secondaryLengthUnit = "miles", primaryAreaUnit = "sqmiles",activeColor = "#0bd3d3",completedColor = "#f890e7"  ) 
        )
    })
  }
  
  
  
  
}


shinyApp(ui2, server)

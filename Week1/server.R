

library(choroplethr)
library(acs)
api.key.install('72cfeef68568abee0cb7bcd33d284748d4dc0e37')
library(choroplethrMaps)
library(tidyverse)
library(magrittr)
library(viridis)
library(maps)
library(ggmap)
library(leaflet)
data(county.map)
data(county.regions)
options(scipen = 100)
setwd("~/datasciencecoursera/DataProducts/Week1/")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    violent_crime <- readRDS("~/future/Capital_Crime/data/Violent_Crime_wLocs.RDS")
    bourbon <- readRDS("~/future/Bourbon/distilleries.RDS")
    sub_crime <- eventReactive({input$crime_update}, {
        filter(violent_crime,
               OFFENSE %in% input$offense,
               METHOD %in% input$weapon)
    })
    
    offense_pal <- colorFactor("Dark2", domain = violent_crime$OFFENSE)
    
    method_icons <- iconList(
        "GUN" = makeIcon("www/gun.png", iconWidth = 24, iconHeight = 24),
        "KNIFE" = makeIcon("www/knife.png", iconWidth = 24, iconHeight = 24),
        "OTHER" = makeIcon("www/other.png", iconWidth = 24, iconHeight = 24)
    )
    
    output$crime_raw <- renderLeaflet({
        withProgress(message = "Mapping...",{
            leaflet(sub_crime()) %>% addProviderTiles("CartoDB.Positron") %>% 
                addCircles(color = ~offense_pal(OFFENSE),
                           popup = ~as.character(date),
                           highlightOptions = highlightOptions(bringToFront = T)) %>%
                    #radius = 300, color = ~pal(METHOD), popup = ~as.character(METHOD), fillOpacity = .5) %>%
                addLegend("bottomright", pal = pal, values = ~METHOD, title = "Murder Weapon")
        })
    })
    
    output$crime_cluster <- renderLeaflet({
        withProgress(message = "Mapping...",{
            leaflet(sub_crime()) %>% addProviderTiles("CartoDB.Positron") %>% 
                addMarkers(clusterOptions = markerClusterOptions(),
                           icon = ~method_icons[METHOD],
                           #color = ~offense_pal(OFFENSE),
                           popup = ~as.character(date)) %>%
                #radius = 300, color = ~pal(METHOD), popup = ~as.character(METHOD), fillOpacity = .5) %>%
                addLegend("bottomright", pal = pal, values = ~METHOD, title = "Murder Weapon")
        })
    })
    
    output$crime_icon <- renderLeaflet({
        withProgress(message = "Mapping...",{
            leaflet(sub_crime()) %>% addProviderTiles("CartoDB.Positron") %>% 
                addMarkers(clusterOptions = markerClusterOptions(),
                           icon = ~method_icons[METHOD],
                           #color = ~offense_pal(OFFENSE),
                           popup = ~as.character(date)) %>%
                #radius = 300, color = ~pal(METHOD), popup = ~as.character(METHOD), fillOpacity = .5) %>%
                addLegend("bottomright", pal = pal, values = ~METHOD, title = "Murder Weapon")
        })
    })
    
    sub_bourbon <- eventReactive({input$bourbon_update}, {
        filter(bourbon, distillery %in% input$distilleries)
    })
    # pal <- colorFactor(palette = "Set1", domain = homicides$METHOD)
    output$bourbon_map <- renderLeaflet({
        withProgress(message = "Mapping...",{
            leaflet(sub_bourbon()) %>% addProviderTiles("CartoDB.Positron") %>% 
                addCircles(radius = 900,
                           #color = ~pal(METHOD),
                           popup = ~as.character(distillery), fillOpacity = .5) # %>%
                #addLegend("bottomright", pal = pal, values = ~METHOD, title = "Murder Weapon")
        })
    })
    
  })


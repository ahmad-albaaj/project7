
library(shiny)
library(leaflet)
library(RColorBrewer)

shinyServer(function(input, output) {
  colorpal <- reactive({
    colorNumeric(brewer.pal(5, "YlOrBr"), quakes$mag)
  })
  
  filteredData <- reactive({
    quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  })
  
  output$map <- renderLeaflet({
    leaflet(quakes) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))  %>%
      addMarkers(lng=174.821029, lat=-41.309602, popup="Epi-interactive") 
    
  })
  
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
                 fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
      )
  })
  
  # Add the new observe and leaflet proxy here #
  observe({
    proxy <- leafletProxy("map", data = filteredData())
    proxy %>% 
      clearControls()
    if (input$legend) {
      pal <- colorpal() 
      proxy %>% 
        addLegend(position = "bottomright",pal = pal, values = ~mag )
    }
  })
  
  
})

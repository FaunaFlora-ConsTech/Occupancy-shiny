# Serengeti Data Visualization App


# Server -----------------------------------------

server <- function(input, output, session) { 
  
  # SINGLE SPECIES ---------------------------------
  
  records_subset <- reactive({dat})
  
  # calculate basic occupancy model
  om <- reactive({
    om.calculate(records_subset(), camera_operation, input$date_range[1], input$date_range[2], 
                 input$om_cov, input$detection_window, camera_metadata)
  })
  
  # combine psi and metadata
  om_metadata <- reactive({
    left_join(om(), camera_metadata)
  })
  
  # Map outputs --------------------------------
  
  # merge grid with OM
  grid_om <- reactive({
    full_join(grid, om())
  })
  
  # make color palette for map
  pal.om <- reactive({  
    colorNumeric(palette = "magma", domain = grid_om()$Predicted)  
  })
  
  # create map labels
  map_labels_om <- reactive({
    sprintf(
      "<strong>Camera: %s</strong><br/>Predicted OM: %g<br/>SE: %g<br/>Covariate value: %g",
      grid_om()$site, grid_om()$Predicted, grid_om()$SE, grid_om()$siteCovs, 0) %>% 
      lapply(htmltools::HTML)
  }) 
  
  # generate leaflet map
  output$om_map <- renderLeaflet({
    
    leaflet(grid_om(), options=leafletOptions(zoomSnap=0.25)) %>%
      
      setView(34.9253, -2.4978, 10.5) %>% # centre coordinates of the study area (might have try a couple of times before setting these values)
      addTiles() %>% # or satellite image: addProviderTiles(providers$Esri.WorldImagery)
      
      addPolygons(
        data = grid_om(),
        fillColor = ~pal.om()(grid_om()$Predicted),
        fillOpacity = 1, 
        weight = 1, # stroke weight of lines
        color = "gray", # color of lines
        label = map_labels_om(),
        highlight = highlightOptions(
          weight = 2,
          color = "white",
          fillOpacity = 1,
          bringToFront = TRUE)
      ) %>% 
      
      addLegend_decreasing(pal = pal.om(), 
                           values = ~Predicted,
                           opacity = 1, 
                           title = "Predicted Occupancy",
                           position = "topleft",
                           decreasing = TRUE)
    
  }) 
  
  # render a reactive graph with Psi against other variable
  output$om_metadata <- renderPlotly({
    ggplotly(ggplot(data = om_metadata(),
                    aes_string(x = input$metadata_select, y = "Predicted")) +
               geom_line() +
               ylab("Predicted occupancy") +
               #geom_ribbon(aes(ymin=lower, ymax=upper)) +
               theme_bw()
    )
  }) 
  
  
}

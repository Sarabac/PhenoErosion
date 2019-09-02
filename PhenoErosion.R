#https://moderndata.plot.ly/plotly-4-7-0-now-on-cran/
#https://plotly-r.com/
#W.DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
W.DIR = "~/Dropbox/Kuhn/phenology/PhenoErosion"
setwd(W.DIR)

source("Functions_Pheno.R")
source("Database.R")
source("GraphErosion.R")
source("UI.R")
library(tidyverse)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(tictoc)

# to upload large geojson files
options(shiny.maxRequestSize=1000*1024^2)
addResourcePath("_Images", "_Images")

# load the border of Germany
# shape_init = load4leaflet(GERMANY, "Germany")
# prevent selecton on the entier Germany
#shape_init$selected = NA


############### SERVER ############################

server = function(input, output, session){
  # the connection object to store all the data
  session$userData$conn = Init_database(dbConnect(RSQLite::SQLite(), ":memory:"))
  # when the user disconnect, remove the database connection
  session$onSessionEnded(function() {
    dbDisconnect(session$userData$conn)
  })
  # when the user add a new geojson file
  session$userData$currentGeo = list()
  # last field clicked
  session$userData$currentShape = 0
  
  observeEvent(input$compute, {
    # extract the informations from the geotif
    # when the user click on the "compute" button
    showModal(modalDialog(
      helpText("Loading"),
      footer =NULL))
    Import_Phase(session$userData$conn)
    progress <- shiny::Progress$new() # the progress bar
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Extracting data", value = 0)
    Import_Measure(session$userData$conn, progress=progress$set)
    
    removeModal()
  })
  
  observeEvent(input$geofile, {
    # when the user upload a Geojson file
    # remember its path and name
    session$userData$currentGeo["name"] = input$geofile$name
    session$userData$currentGeo["path"] = input$geofile$datapath
    #take the potential IDs for the layer. The user will choose one.
    idvar = input$geofile$datapath %>% sf::read_sf() %>%
      sf::st_drop_geometry() %>% colnames()
    #There is also the possibility to create IDs based on row number
    Name = setNames( c("", idvar), c("Auto Generated", idvar))
    Crop = setNames( c("", idvar), c("None", idvar))
    Declaration = setNames( c("", idvar), c("None", idvar))
    Erosion = setNames( c("", idvar), c("None", idvar))
    showModal(modalDialog(
      radioButtons("varname", "ID Variable", choices=Name),
      radioButtons("varcrop", "Crop Variable", choices=Crop),
      radioButtons("varerosion", "Erosion Date variable", choices=Erosion),
      footer = tagList(actionButton("ok", "OK")),
      title = "Import Geojson")
    )
  })
  
  observeEvent(input$ok,{
    # When the choice of the IDs, crop, erosion_event is made
    infos = session$userData$currentGeo #retrive the file data
    Zone_ID = load4leaflet(session$userData$conn,
                            infos$path,
                            str_remove(infos$name, "\\..*$"),
                            input$varname, input$varcrop,
                           input$varerosion)
    newShape = sf_database(session$userData$conn) # take all the shape
    leafletProxy("map") %>% create_layer(session$userData$conn) %>%
      create_layerControl(unique(newShape$Zone_Name))
    removeModal() # remove the form to choose ID, crop, Erosion event date
  })
  
  observeEvent(input$map_draw_new_feature, {
    # when the user use the drawing tool
    drawing_infos = input$map_draw_new_feature
    newF = create_feature(drawing_infos) # geometry object
    Zone_ID = load4leaflet(session$userData$conn,
                           newF, "Custom")
    newShape = sf_database(session$userData$conn)
    leafletProxy("map") %>% create_layer(session$userData$conn) %>%
      create_layerControl(unique(newShape$Zone_Name))
  })
  
  observe({on_click(input$map_shape_click[["id"]])})   #for polygons   
  observe({on_click(input$map_marker_click[["id"]])})  #for points
  on_click = function(clickID){
    # function called when a feature is clicked
    if(is.null(clickID)){return(NULL)}
    session$userData$currentShape = clickID
    dbExecute( # update the "selected" attribut of the clicked field
      session$userData$conn,
      "UPDATE Field set selected=NOT(selected) where Field_ID=?",
      params = list(clickID)
      )
    leafletProxy("map") %>% 
      create_layer(session$userData$conn, clickID)
    # update the field editor with the current field
    removeUI("#currentField")
    editField(session$userData$conn, clickID)
  }
  
  observe({ # when the user edit the field
    Name = input$editName
    conn = session$userData$conn
    Field_ID = session$userData$currentShape
    if(Field_ID==0){return(NULL)}
    # update field name
    dbExecute(conn, "UPDATE Field set Name=? where Field_ID=?",
              param = list(Name, Field_ID))
    # remove selected erosion events
    erodelete = input$deleteErosion
    lapply(erodelete, function(eroID){
      dbExecute(conn, "DELETE FROM ErosionEvent where Event_ID = ?",
                param = list(eroID))
    })
    # add created erosion event
    erodate = input$newErosion
    if(length(erodate)){
      dbWriteTable(conn, "ErosionEvent",tibble(
        Field_ID = Field_ID,
        Event_Date = as.character(erodate)
      ), append = TRUE)
    }
    # remove selected culture
    culturedelete = input$deleteCulture
    lapply(culturedelete, function(culID){
      dbExecute(conn, "DELETE FROM Culture where Culture_ID = ?",
                param = list(culID))
    })
    # add created culture
    cuturedate = input$newDeclaration
    cuturecrop = isolate(input$CropSelect) 
    if(length(cuturedate)){
      dbWriteTable(conn, "Culture",tibble(
        Field_ID = Field_ID,
        Declaration = as.character(cuturedate),
        Crop = cuturecrop
      ), append = TRUE)
    }
    # refresh edit panel
    leafletProxy("map") %>%
      create_layer(session$userData$conn, Field_ID)
    removeUI("#currentField")
    editField(session$userData$conn, Field_ID)
  })
  
  observeEvent(input$deleteField, {
    # if the user click on the deleteField button
    # in the field panel
    conn = session$userData$conn
    Field_ID = session$userData$currentShape
    dbExecute(conn, "DELETE from Field where Field_ID = ?",
              params = list(Field_ID))
    leafletProxy("map") %>% removeShape(Field_ID)
    removeUI("#currentField")
  })
  
  output$downloadData <- downloadHandler(
    # when the user download th database
    filename = "PhenoErosion.sqlite",
    content = function(file) {
      export = dbConnect(RSQLite::SQLite(), file)
      RSQLite::sqliteCopyDatabase(session$userData$conn, export)
    })
  
  observeEvent(input$openData, {
    # when the user upload a sqlite database
    datapath = input$openData$datapath
    newbase = dbConnect(RSQLite::SQLite(), datapath)
    dbDisconnect(session$userData$conn)
    session$userData$conn = dbConnect(RSQLite::SQLite(), ":memory:")
    RSQLite::sqliteCopyDatabase(newbase, session$userData$conn)
    dbDisconnect(newbase)
    Init_database(session$userData$conn)
    # redraw the map
    newShape = sf_database(session$userData$conn)
    leafletProxy("map") %>% clearMarkers() %>% clearShapes() %>% 
      create_layer(session$userData$conn) %>%
      create_layerControl(unique(newShape$Zone_Name))
  })
  
  output$map = renderLeaflet({
    #initialize the map
    map = create_map() %>% create_layerControl()
    return(map)
  })
  
  graphData = eventReactive(input$drawGraph, {
    return(
      getGraphData(session$userData$conn, input$threshold)
      )
  })
  observe({
    output$DOY_GRAPH = renderPlot({
      gdata = graphData()
      lim = input$DatesMerge
      dat = gdata
      #dat = lapply(gdata, function(x){ filter(x, Date>=lim[1] & Date <= lim[2])})
      if(is.null(dat)){return(NULL)}
      graph = drawErosion(
        dat$culture, dat$erosion,
        filter(dat$NDVI, input$NDVIchoice), 
        filter(dat$precipitation, Variable==input$preciChoice),
        date_limits =c(lim[1], lim[2])
      )
      return(graph)
    }, height = input$plotHeight)
  })

  
}

shinyApp(ui = ui, server = server)
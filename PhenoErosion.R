#https://moderndata.plot.ly/plotly-4-7-0-now-on-cran/
#https://plotly-r.com/
#W.DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
#u = rgdal::readOGR("field_test/qgis2", "bb")
# rgdal::writeOGR(as_Spatial(m), "field_test/qgis2", "bb", "SQLite", overwrite_layer = TRUE)

W.DIR = "~/Dropbox/Kuhn/phenology/PhenoErosion"
setwd(W.DIR)

source("Functions_Pheno.R")
source("Database.R")
source("GraphErosion.R")
source("UI.R")
library(tidyverse)
library(shiny)
library(shinyWidgets)
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
  session$userData$conn = Init_database(dbConnect(RSQLite::SQLite(), ":memory:",
                                                  loadable.extensions = TRUE))
  conn <<- session$userData$conn
  # define the content of the variables picker
  variables <<- tbl(session$userData$conn, "variableCrop") %>% 
    collect() %>% group_by(SourceName) %>%
    summarise(Source = list(setNames(Var_ID, VarName)))
  Vchoice = setNames(variables$Source, variables$SourceName)
  updatePickerInput(session, "selectVar", choices = Vchoice)
  # when the user disconnect, remove the database connection
  #session$onSessionEnded(function() {dbDisconnect(session$userData$conn)})
  # when the user add a new geojson file
  session$userData$currentGeo = list()
  # last field clicked
  session$userData$currentShape = reactiveVal(0)
  
  observeEvent(input$selectAll, {select_deselect(1)})
  observeEvent(input$deselectAll, {select_deselect(0)})
  select_deselect = function(selection){
    dbExecute(session$userData$conn,
              paste("UPDATE Field set selected=", selection, sep="") )
    field_id = tbl(session$userData$conn, "Field") %>% pull(Field_ID)
    leafletProxy("map") %>% 
      create_layer(session$userData$conn,field_id)
  }
  
  observeEvent(input$compute, {
    # extract the informations from the geotif
    # when the user click on the "compute" button
    var_ID <<- input$selectVar
    sliderYears <<- input$selectYear
    Years <<- seq(sliderYears[1], sliderYears[2])
    field_id = tbl(conn, "Field") %>% 
      filter(selected) %>% pull(Field_ID)
    Source_ids = tbl(conn, "Variable") %>% 
      inner_join(tbl(conn, "DataSource"), by="Source_ID") %>% 
      filter(Var_ID%in%var_ID) %>% 
      pull(Source_ID) %>% unique()
    # if no field or no variable selected
    if(length(field_id)==0|length(var_ID)==0){return(NULL)}
    
    showModal(modalDialog(
      helpText("Loading"),
      footer =NULL))
    for(sid in Source_ids){
      Import_Weight(conn, sid, field_id)
    }
    progress <- Progress$new() # the progress bar
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    for (vid in var_ID){
      progress$set(value = 0,
                   message = tbl(conn, "Variable") %>%
                     filter(Var_ID==vid) %>% pull(VarName)
                   )
      Import_Measure(session$userData$conn, vid, field_id,
                    Years, progresfunc = progress$set)
    }
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
    showModal(MODALIMPORT(Name, Crop, Erosion))
  })
  
  observeEvent(input$ok,{
    # When the choice of the IDs, crop, erosion_event is made
    infos = session$userData$currentGeo #retrive the file data
    load4leaflet(session$userData$conn,
                            infos$path,
                            str_remove(infos$name, "\\..*$"),
                            input$varname, input$varcrop,
                           input$varerosion)
    newShape = sf_database(session$userData$conn) # take all the shape
    leafletProxy("map") %>% create_layer(session$userData$conn) %>%
      create_layerControl(unique(newShape$GroupName))
    removeModal() # remove the form to choose ID, crop, Erosion event date
  })
  
  observeEvent(input$map_draw_new_feature, {
    # when the user use the drawing tool
    drawing_infos = input$map_draw_new_feature
    newF = create_feature(drawing_infos) # geometry object
    load4leaflet(session$userData$conn,  newF, "Custom")
    newShape = sf_database(session$userData$conn)
    leafletProxy("map") %>% create_layer(session$userData$conn) %>%
      create_layerControl(unique(newShape$GroupName))
  })
  
  observe({on_click(input$map_shape_click[["id"]])})   #for polygons   
  observe({on_click(input$map_marker_click[["id"]])})  #for points
  on_click = function(clickID){
    # function called when a feature is clicked
    if(is.null(clickID)){return(NULL)}
    session$userData$currentShape(as.integer(clickID))
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
    Field_ID = isolate(as.integer(session$userData$currentShape()))
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
    Field_ID = session$userData$currentShape()
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
      create_layerControl(unique(newShape$GroupName))
  })
  
  output$map = renderLeaflet({
    #initialize the map
    map = create_map() %>% create_layerControl()
    return(map)
  })
  
  graphData = eventReactive(input$drawGraph, {
    return(TRUE)
    #return(getGraphData(session$userData$conn, input$threshold))
  })
  SgetCulture = reactive({
    graphData()
    return( getCulture(session$userData$conn) )
  })
  SgetPhase = reactive({
    graphData()
    return( getPhase(session$userData$conn) )
    })
  SgetErosion = reactive({
    graphData()
    return( getErosion(session$userData$conn) )
    })
  SgetPrecipitation = reactive({
    graphData()
    choiceprecis <<- input$preciChoice
    return( getPrecipitation(session$userData$conn,
            varName = input$preciChoice ))
    })
  SgetNDVI = reactive({
    graphData()
    return( getNDVI(session$userData$conn) )
    })
  
  observe({
    output$DOY_GRAPH = renderPlot({
      lim = input$DatesMerge
      #dat = lapply(gdata, function(x){ filter(x, Date>=lim[1] & Date <= lim[2])})
      #if(is.null(dat)){return(NULL)}
      field_corress = dbGetQuery(
        session$userData$conn,
        "select Field_ID, GroupName ||'\n'|| Name as name from Field where selected"
        )
      if(!nrow(field_corress)){
        return(ggplot() + geom_text(aes(x=0, y=0, label="No field selected")))
      }
      field_list = setNames(field_corress$name, field_corress$Field_ID)
      graph = drawErosion(
        SgetCulture(),
        SgetPhase(),
        SgetErosion(),
        filter(SgetNDVI(), input$NDVIchoice), 
        SgetPrecipitation(),
        date_limits =c(lim[1], lim[2]),
        field_list
      )
      return(graph)
    }, height = input$plotHeight)
  })
  
  output$plot_extracted_data = renderPlot({
    field_id = session$userData$currentShape()
    print(field_id)
    graph = plot_extracted_data(
      session$userData$conn,
      field_id
      )
    return(graph)
  })

  
}

shinyApp(ui = ui, server = server)
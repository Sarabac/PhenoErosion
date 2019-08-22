#https://moderndata.plot.ly/plotly-4-7-0-now-on-cran/
#https://plotly-r.com/
W.DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(W.DIR)

source("Functions_Pheno.R")
source("Database.R")
library(tidyverse)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(tictoc)

# to upload large geojson files
options(shiny.maxRequestSize=30*1024^2)
addResourcePath("_Images", "_Images")

# load the border of Germany
# shape_init = load4leaflet(GERMANY, "Germany")
# prevent selecton on the entier Germany
#shape_init$selected = NA

# the minimal value of the time scale
minDate = as.Date("1993-01-01")
# the maximale value of the time scale
maxDate = as.Date(today() + years(1))

# widgets of the shiny app
title_div = div(img(src="_Images/EMRA_Logo.svg", width="30px"),
                "PhenoWin: Visualisation of phenological windows in Germany" )
ui = fluidPage(
  tags$head(# css styles
    tags$style(HTML("
                    #compute{background-color:GreenYellow }
                    *{font: bold 12px/30px Arial, serif}
                    ")),
    tags$title("PhenoWin"),
    tags$link(rel="shortcut icon", href="_Images/EMRA_Logo.ico")
  ),
  fluidRow(
    titlePanel(title_div),
    column(2,
           actionButton("compute", "Compute", icon = icon("play")),
           actionButton("deselectAll", "Deselect All")),
    column(2, radioButtons("clickSelect", "Mode:", inline=TRUE,
                        choices=c("Select"="select", "Delete"="delete"),
                        selected="select")),
    column(2, fileInput("geofile", "Import Geojson", accept = c(".geojson"))),
    column(2, downloadButton("downloadData", "Save"),
           fileInput("openData", "Open") ),
    column(2, downloadButton("downloadPhase", "Download Phase Code") )),
  fluidRow(column(8,leafletOutput("map")),column(4,div(id="fieldedit"))),
  fluidRow(
             sliderInput("DatesMerge", "Time Periode",
                         min = minDate,
                         max = maxDate,
                         value=c(as.Date("2015-01-01"),as.Date("2018-01-01") ),
                         timeFormat="%Y-%m-%d", width = "100%")),
  fluidRow(plotOutput("DOY_GRAPH"))
  )

editField = function(conn, Field_ID){
  fbase = tbl(conn, "editField") %>% filter(Field_ID==!!Field_ID)
  ### Field ###
  Zone_Name = fbase %>% pull(Zone_Name) %>% unique()
  Name = fbase %>% pull(Name) %>% unique()
  saveChoice = setNames(c(Field_ID), c("SAVE") )
  field = h3(
    Zone_Name,
    textInput("editName", NULL, value=Name, width = "25%"),
    actionButton("editSave", "SAVE", icon = icon("save"))
  )
  ### Erosion ###
  erosion = fbase %>% dplyr::select(Event_ID, Event_Date) %>%
    distinct() %>%  collect()
  newErosion = dateInput("newErosion", NULL, value = "0000-00-00")
  if(nrow(drop_na(erosion))){
    deleteErosion = checkboxGroupInput(
      "deleteErosion", NULL,
      choices = setNames(erosion$Event_ID, erosion$Event_Date)
    )
    erodiv = div(newErosion, deleteErosion) 
  }else{
    erodiv = div(newErosion) 
  }
  ### Culture ###
  culture = fbase %>% dplyr::select(Culture_ID, Declaration, Crop) %>%
    distinct() %>%  collect() %>% 
    mutate(display = paste(Crop," (", Declaration, ")", sep=""))
  newCulture = div(
    selectInput("CropSelect", "Select Crop", choices = CROPS_CORRESPONDANCE),
    dateInput("newDeclaration", NULL, value = "0000-00-00")
    )
  if(nrow(drop_na(culture))){
    deleteCulture = checkboxGroupInput(
      "deleteCulture", NULL,
      choices = setNames(culture$Culture_ID, culture$display)
    )
    culturediv = div(newCulture, deleteCulture) 
  }else{
    culturediv = div(newCulture) 
  }
  
  
  insertUI(
    selector = "#fieldedit", where = "afterBegin",
    ui = div(field, erodiv, culturediv, id = "currentField")
  )
  
}

server = function(input, output, session){
  
  session$userData$conn = Init_database(":memory:")
  session$userData$currentGeo = list()
  session$userData$currentShape = 0
  
  observe({
    # when the user upload a Geojson file
    req(input$geofile)
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
      radioButtons("vardeclaration", "Date Variable", choices=Declaration),
      radioButtons("varerosion", "Erosion Date variable", choices=Erosion),
      footer = tagList(actionButton("ok", "OK")))
    )
  })
  
  observeEvent(input$ok,{
    #When the choice of the IDs is made
    infos = session$userData$currentGeo #retrive the file data
    Zone_ID = load4leaflet(session$userData$conn,
                            infos$path, #remove the file extension
                            str_remove(infos$name, "\\..*$"),
                            input$varname, input$varcrop,
                            input$vardeclaration, input$varerosion)
    newShape = sf_database(session$userData$conn)
    leafletProxy("map") %>% create_layer(newShape) %>%
      create_layerControl(unique(newShape$Zone_Name))
    removeModal()
  })
  
  observe({on_click(input$map_shape_click[["id"]])})   #for polygons   
  observe({on_click(input$map_marker_click[["id"]])})  #for points
  on_click = function(clickID){
    # function called when a feature is clicked
    if(is.null(clickID)){return(NULL)}
    session$userData$currentShape = clickID
    dbExecute(
      session$userData$conn,
      "UPDATE Field set selected=NOT(selected) where Field_ID=?",
      params = list(clickID)
      )
    leafletProxy("map") %>% 
      create_layer(filter(sf_database(session$userData$conn),
                          Field_ID == clickID))
    removeUI("#currentField")
    editField(session$userData$conn, clickID)
    
  }
  
  observeEvent(input$editSave, {
    conn = session$userData$conn
    Field_ID = session$userData$currentShape
    print(Field_ID)
    if(is.null(Field_ID)){return(NULL)}
    Name = input$editName
    dbExecute(conn, "UPDATE Field set Name=? where Field_ID=?",
              param = list(Name, Field_ID))
    erodelete = input$deleteErosion
    lapply(erodelete, function(eroID){
      dbExecute(conn, "DELETE FROM ErosionEvent where Event_ID = ?",
                param = list(eroID))
    })
    erodate = input$newErosion
    if(length(erodate)){
      dbWriteTable(conn, "ErosionEvent",tibble(
        Field_ID = Field_ID,
        Event_Date = as.character(erodate)
      ), append = TRUE)
    }
    culturedelete = input$deleteCulture
    lapply(culturedelete, function(culID){
      dbExecute(conn, "DELETE FROM Culture where Culture_ID = ?",
                param = list(culID))
    })
    cuturedate = input$newDeclaration
    cuturecrop = input$CropSelect
    if(length(cuturedate)){
      dbWriteTable(conn, "Culture",tibble(
        Field_ID = Field_ID,
        Declaration = as.character(cuturedate),
        Crop = cuturecrop
      ), append = TRUE)
    }
    
    removeUI("#currentField")
    editField(session$userData$conn, Field_ID)
    
  })
  
  output$downloadData <- downloadHandler(
    filename = "PhenoErosion.sqlite",
    content = function(file) {
      export = dbConnect(RSQLite::SQLite(), file)
      RSQLite::sqliteCopyDatabase(session$userData$conn, export)
    })
  observe({
    # when the user upload a sqlite database
    req(input$openData)
    # remember its path and name
    datapath = input$openData$datapath
    newbase = dbConnect(RSQLite::SQLite(), datapath)
    dbDisconnect(session$userData$conn)
    session$userData$conn = dbConnect(RSQLite::SQLite(), ":memory:")
    RSQLite::sqliteCopyDatabase(newbase, session$userData$conn)
    newShape = sf_database(session$userData$conn)
    leafletProxy("map") %>% clearMarkers() %>% clearShapes() %>% 
      create_layer(newShape) %>%
      create_layerControl(unique(newShape$Zone_Name))
  })
  
  output$map = renderLeaflet({
    #initialize the map
    map = create_map() %>% 
      create_layerControl()
    return(map)
  })
  
}

shinyApp(ui = ui, server = server)
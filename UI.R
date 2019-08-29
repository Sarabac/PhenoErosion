

library(shiny)
library(leaflet)

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
           actionButton("drawGraph", "Draw Graph", icon = icon("draw")),
           actionButton("deselectAll", "Deselect All")),
    column(2, fileInput("geofile", "Import Geojson", accept = c(".geojson"))),
    column(2, downloadButton("downloadData", "Save"),
           fileInput("openData", "Open") ),
    column(2, downloadButton("downloadPhase", "Download Phase Code") ),
    column(2, sliderInput("plotHeight", "Height of the plot",
                          min = 100, max = 2000, value = 500),
           sliderInput("threshold", "Threshold",
                       min = 0, max = 1, value = 0.40, step = 0.01),
           radioButtons("preciChoice", "Precipitation Variable", choices= PRECI.VARIABLES),
           checkboxInput("NDVIchoice", "Include NDVI", value = TRUE))
  ),
  fluidRow(column(8,leafletOutput("map")),column(4,div(id="fieldedit"))),
  fluidRow(
    sliderInput("DatesMerge", "Time Periode",
                min = minDate,
                max = maxDate,
                value=c(as.Date("2015-01-01"),as.Date("2018-01-01") ),
                timeFormat="%Y-%m-%d", width = "100%")),
  fluidRow(plotOutput("DOY_GRAPH"))
    )

# the right panel to edit date of erosion events and type of crops
editField = function(conn, Field_ID){
  fbase = tbl(conn, "editField") %>% filter(Field_ID==!!Field_ID)
  ### Field ###
  Zone_Name = fbase %>% pull(Zone_Name) %>% unique()
  Name = fbase %>% pull(Name) %>% unique()
  field = h3( # zone containing name aof the field
    Zone_Name,
    textInput("editName", NULL, value=Name, width = "25%")
  )
  ### Erosion ###
  erosion = fbase %>% dplyr::select(Event_ID, Event_Date) %>%
    distinct() %>%  collect()
  newErosion = dateInput("newErosion", NULL, value = "0000-00-00")
  # if the field already contain erosion events
  if(nrow(drop_na(erosion))){
    # when checked, deleteErosion remove the corresponding erosion event
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
    # when checked, deleteErosion remove the corresponding erosion crop
    deleteCulture = checkboxGroupInput(
      "deleteCulture", NULL,
      choices = setNames(culture$Culture_ID, culture$display)
    )
    culturediv = div(newCulture, deleteCulture) 
  }else{
    culturediv = div(newCulture) 
  }
  deleteField = actionButton("deleteField", "DELETE", icon = icon("trash"))
  # drae the panel
  insertUI(
    selector = "#fieldedit", where = "afterBegin",
    ui = div(field, erodiv, culturediv, deleteField, id = "currentField")
  )
  
}

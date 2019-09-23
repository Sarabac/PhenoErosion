library(shiny)
library(shinyWidgets)
library(leaflet)
library(lubridate)

# the minimal value of the time scale
minDate = as.Date("1993-01-01")
# the maximale value of the time scale
maxDate = as.Date(today() + years(1))

#downloadButton("downloadPhase", "Download Phase Code")
helpMenu = fluidPage(
  downloadButton("downloadPhase", "Download Phase Code")
)

MAP = fluidPage(
  fluidRow(
    fluidRow(
      fluidRow(
        actionButton("deselectAll", "Deselect All"),
        actionButton("selectAll", "select All")
      ),
      fileInput("geofile", "Import Geojson", accept = c(".geojson")),
      fileInput("openData", "Open")
    ),
    fluidRow(
      pickerInput(
        inputId = "selectVar",
        label = "Select Variable",
        choices=list("var"=1),
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ),
      sliderInput("selectYear", "Select Year",
                  min = 2000, max = 2020, value = c(2010, 2012)),
      actionButton("compute", "Compute", icon = icon("play"))
    ),
    width = "100%", class = "tabheader"),
  fluidRow( sidebarLayout(
    sidebarPanel(div(id="fieldedit")),
    mainPanel(leafletOutput("map", height = "600px")),
    position = "right"),
    class="content"
  ),
  fluidRow(plotOutput("plot_extracted_data"))
  
)

GRAPH = fluidPage(
  fluidRow(
    fluidRow(
      actionButton("drawGraph", "Draw Graph", icon = icon("draw")),
      sliderInput("plotHeight", "Height of the plot",
                  min = 100, max = 2000, value = 500),
      sliderInput("threshold", "Threshold",
                  min = 0, max = 1, value = 0.40, step = 0.01),
      #checkboxInput("NDVIchoice", "Include NDVI", value = TRUE),
      radioButtons("preciChoice", "Precipitation Variable", choices= PRECI.VARIABLES)
    ),
    fluidRow(sliderInput("DatesMerge",NULL,
                       min = minDate,
                       max = maxDate,
                       value=c(as.Date("2015-01-01"),as.Date("2018-01-01") ),
                       timeFormat="%Y-%m-%d", width = "100%")),
  width = "100%", class = "tabheader"),
  fluidRow(plotOutput("DOY_GRAPH", height = "auto"), class="content")
  
)

SAVE = fluidPage(
  fluidRow(
    downloadButton("downloadData", "Save")
  )
)

ui = navbarPage(
  "PhenoErosion", selected = "MAP",
  tags$title("PhenoWin"),
# tags$link(rel="shortcut icon", href="www/EMRA_Logo.ico"),
  includeCSS("www/PhenoErosion.css"),
#  includeScript("www/PhenoErosion.js"),
  tabPanel("MAP", MAP),
  tabPanel("GRAPH", GRAPH),
  tabPanel("SAVE", SAVE),
  tabPanel("HELP", helpMenu)
  
)

server = function(input, output){}
shinyApp(ui, server)




# widgets of the shiny app
title_div = div(img(src="_Images/EMRA_Logo.svg", width="30px"),
                "PhenoWin: Visualisation of phenological windows in Germany" )


# the right panel to edit date of erosion events and type of crops
editField = function(conn, Field_ID){
  fbase = tbl(conn, "editField") %>% filter(Field_ID==!!Field_ID)
  ### Field ###
  Zone_Name = fbase %>% pull(GroupName) %>% unique()
  Name = fbase %>% pull(Name) %>% unique()
  field = h3( # zone containing name aof the field
    actionButton("deleteField", "DELETE", icon = icon("trash")),
    Zone_Name,
    textInput("editName", NULL, value=Name, width = "25%"),
    class = "field"
  )
  ### Erosion ###
  erosion = fbase %>% dplyr::select(Event_ID, Event_Date) %>%
    distinct() %>%  collect()
  newErosion = dateInput("newErosion", "Create erosion event", value = "0000-00-00")
  # if the field already contain erosion events
  if(nrow(drop_na(erosion))){
    # when checked, deleteErosion remove the corresponding erosion event
    deleteErosion = checkboxGroupInput(
      "deleteErosion", "Delete erosion event",
      choices = setNames(erosion$Event_ID, erosion$Event_Date)
    )
    erodiv = div(newErosion, deleteErosion, class="erosion") 
  }else{
    erodiv = div(newErosion) 
  }
  ### Culture ###
  culture = fbase %>% dplyr::select(Culture_ID, Declaration, Crop) %>%
    distinct() %>%
    inner_join(tbl(conn, "Crop"), by = "Crop") %>% 
    collect() %>% 
    mutate(display = paste(Crop_name," (", Declaration, ")", sep=""))
  newCulture = div(
    selectInput("CropSelect", "Create Culture", choices = CROPS_CORRESPONDANCE),
    dateInput("newDeclaration", NULL, value = "0000-00-00")
  )
  if(nrow(drop_na(culture))){
    # when checked, deleteErosion remove the corresponding erosion crop
    deleteCulture = checkboxGroupInput(
      "deleteCulture", "Delete Culture",
      choices = setNames(culture$Culture_ID, culture$display)
    )
    culturediv = div(newCulture, deleteCulture, class="culture") 
  }else{
    culturediv = div(newCulture) 
  }
   
  # draw the panel
  insertUI(
    selector = "#fieldedit", where = "afterBegin",
    ui = div(field, erodiv, culturediv, id = "currentField")
  )
  
}

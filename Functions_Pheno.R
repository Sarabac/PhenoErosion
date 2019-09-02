
list.of.packages = c("tidyverse", "lubridate", "sf",
                     "raster", "velox", "shiny", "leaflet", "scales", "leaflet.extras", "rgdal")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){install.packages(new.packages)}

# functions used to extract the data and build the graphs
library(tidyverse)
library(lubridate)
library(leaflet.extras)

# variables
GERMANY = file.path("Zones/DEU_adm0.shp") #border of germany

# Access the phenological data of the Crop "n", ready for the graph
LEAFLET_CRS = sf::st_crs(4326)

# separation of the rows in the attribut table of the fields
ROW_SEPARATION = ";"
CULTURE_SEPARATION = "[|]"

# Corespondance between the name of the crop and its number
CROPS_CORRESPONDANCE_FRAME = read.csv("crop_correspondance.csv")
CROPS_CORRESPONDANCE = setNames(CROPS_CORRESPONDANCE_FRAME$Crop,
                                CROPS_CORRESPONDANCE_FRAME$Crop_name)

extract_n = function(dat, n){
  # extract a number of length n from a character vector
  as.integer(str_extract(dat, paste("(?:(?<!\\d)\\d{",n,"}(?!\\d))", sep="")))
}

extract_code = function(dat){
  tibble(
    Crop = extract_n(dat, 3),
    Year = extract_n(dat, 4),
    # Phenology have a lenght 1 or 2
    P = coalesce(extract_n(dat, 2),extract_n(dat, 1))
  )
}

extract_tif_info = function(directory){
  # extract the path of each geotif in a directory with
  # the Crop number, the year and the phenological stage
  tibble(dir = list.files(directory, full.names = T)) %>%
    filter(str_detect(.$dir, ".tif$")) %>% # take all the .tif files
    bind_cols(extract_code(.$dir))
}

extract_date = function(dat){
  mutate(dat, DOY = round(DOY)) %>%
    mutate(Date = as.Date(paste(Year,"01-01",sep="-")) + days(DOY - 1))
}


create_feature = function(feature){
  # extract the coordinate of points returned by leaflet
  co = feature[["geometry"]][["coordinates"]]
  if(feature[["geometry"]][["type"]] == "Polygon"){
    coor = co[[1]]
    xy = matrix(nrow = length(coor), ncol = 2)
    for(a in 1:length(coor)){
      for(b in 1:length(coor[[a]])){
        xy[a,b] = coor[[a]][[b]]
      }
    }
    sps = sf::st_polygon(list(xy))
  }else{
    sps = sf::st_point(c(co[[1]],co[[2]]))
  }
  # assign the WG84 projection
  geom_set = sf::st_sf(geometry = st_sfc(sps), crs=LEAFLET_CRS)
  return(geom_set)
}


is.point = function(geometry){
  sf::st_geometry_type(geometry) %in% c("POINT","MULTIPOINT")
}
sf_database = function(conn){
  st_read(conn, "Field") %>% 
    inner_join(collect(tbl(conn, "Zone")), by="Zone_ID")
}
create_map = function(){
  # origin is a shapefile which extent is the default
  # map extent
  map = leaflet() %>%
    fitBounds(4.80,55.53, 16.38,46.87) %>% 
    addDrawToolbar( targetGroup = "created",
                    polylineOptions = FALSE,
                    circleOptions = FALSE,
                    rectangleOptions = FALSE,
                    circleMarkerOptions = FALSE,
                    markerOptions = FALSE,
                    singleFeature = TRUE
    ) %>% 
    addSearchOSM() %>% addResetMapButton() %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles("Esri.WorldImagery", group = "Orthos") %>%
    addProviderTiles("OpenTopoMap", group = "OpenTopoMap")
  return(map)
}

fill_field = tibble(
  erosion = c(TRUE, TRUE, FALSE, FALSE),
  culture = c(TRUE, FALSE, TRUE, FALSE),
  fill_color = c("blue", "orange", "green", "black")
)

create_layer = function(map, conn, Field_ID=NULL){
  if(is.null(Field_ID)){
    Field_ID = tbl(conn, "Field") %>% pull(Field_ID)
  } # no change if nothing to add
  for(Li in Field_ID){
    sh = st_read(conn, "Field") %>% filter(Field_ID==Li)
    # the color is depending if the field contain erosion or cultures
    haveErosion = tbl(conn, "ErosionEvent") %>% 
      filter(Field_ID==Li) %>% collect() %>% nrow()
    haveCulture = tbl(conn, "Culture") %>% 
      filter(Field_ID==Li) %>% collect() %>% nrow()
    fill_color = fill_field %>%
      filter(erosion==(haveErosion>0)&culture==(haveCulture>0)) %>% 
      pull(fill_color)
    # border color is depending if selected
    color = ifelse(sh$selected, "red", "black")
    map = map %>% removeShape(Li) %>% 
        addPolygons(color = color, weight = 1, smoothFactor = 0.5,
                    fillColor = fill_color,
                    opacity = 1.0, fillOpacity = 0.4,
                    layerId = as.character(sh$Field_ID),
                    group = as.character(sh$Zone_Name),
                    data = sh,
                    label = as.character(sh$Name),
                    labelOptions = labelOptions(
                      noHide = T,
                      style = list(color = color)
                      ),
                    highlightOptions = highlightOptions(
                      color = "orange", weight = 3, bringToFront = TRUE)
                    )
  }
  return(map)
}

create_layerControl = function(map, groupNames = c()){
  return(addLayersControl(map,
    baseGroups = c("OpenStreetMap", "OpenTopoMap","Orthos"),
    overlayGroups=groupNames,
    options = layersControlOptions(collapsed = FALSE)
  ))
}

load4leaflet = function(conn, path, name, varname="",
                        varcrop="", varerosion=""){
  # put the fields in the database
  # use the data in the field attributs
  if(is.character(path)){
    polyg = sf::st_transform(sf::read_sf(path),LEAFLET_CRS)
  }else{ # if the path is already a sf object
    polyg = sf::st_transform(path,LEAFLET_CRS)
  }
  if(varname==""){ # create a name with the row number
    result = mutate(polyg, Name = row_number())
  }else{
    result = rename(polyg, Name = !!varname)
  }
  
  Zone_ID = Import_Zone(conn, dplyr::select(result, Name),
                        Zone_name = name)
  field = result %>% st_drop_geometry()
  if(varerosion!=""){
    eroField = dplyr::select(field, Name, Event_Date = !!varerosion) %>% 
      separate_rows(Event_Date, sep = ROW_SEPARATION)
    Import_Erosion(conn, Zone_ID, eroField)
  }
  if(varcrop!=""){
    
  culture = field %>% 
    dplyr::select(Name, cultures=!!varcrop) %>% 
    separate_rows(cultures, sep=ROW_SEPARATION) %>% 
    separate(cultures, c("Crop", "Declaration"), sep=CULTURE_SEPARATION)
    
  Import_Culture(conn, Zone_ID, culture)
  }
  return(Zone_ID)
  }

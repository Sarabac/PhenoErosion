
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

### Colors corresponding to each phenological stage
phasesCode = read.csv("Phases.csv") %>% arrange(Code)

# Corespondance between the name of the crop and its number
CROPS_CORRESPONDANCE = list(
  "Permanent Grassland" = 201,
  "Winter Wheat" = 202,
  "Winter Barley" = 204,
  "Winter Rape" = 205,
  "Oat" = 208,
  "Maize" = 215
)

CROPS_CORRESPONDANCE_FRAME = tibble(
  Crop = unlist(CROPS_CORRESPONDANCE),
  Crop_name = names(CROPS_CORRESPONDANCE)
)


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
  geom_set = sf::st_sfc(sps, crs=LEAFLET_CRS)
  # assign the WG84 projection
  return(geom_set)
}

build_DOY_graph = function(dat, date_breaks=waiver(),
                           user_facet=facet_grid(Area~Crop)){
  # create the Phenological graph from the data frame "dat" with attributs:
  #     "Date": class Date
  #     "sum_weight": between 0 and 1, proportion of pixels in phase P
  #     "P": phenological stage
  #     "Area": spatial entities ID
  #     "Crop": the crop ID
  # date_breaks: character
  fphasesCode = filter(phasesCode, Code%in%dat$P)
  color_fill_rule = as.vector(fphasesCode$Color)
  names(color_fill_rule) <- fphasesCode$Code
  graph =  ggplot(dat, aes(x = Date, y=1, alpha = sum_weight,
                           fill = as.factor(P)))+
    geom_tile() + 
    user_facet+
    scale_fill_manual(values = color_fill_rule) + # color fill scall of the phenological stages
    geom_vline(aes(
          xintercept = as.Date(paste(year(Date),"01", "01", sep="-")),
          linetype = "Year"), size = 2)+
    geom_vline(aes(
          xintercept = as.Date(paste(year(Date), month(Date), "01", sep="-")),
                   linetype = "Month"))  +
    labs(fill = "Phenology", alpha = "Weight") +
    
    scale_x_date(name="DOY", date_breaks=date_breaks,
                 labels=scales::date_format("%j"),
                 sec.axis=dup_axis(
                   name="Date",labels = scales::date_format("%d %b %Y"))) +
    scale_linetype_manual("Breaks", 
                          values = c("Month"="dotted", "Year"="dashed")) +
    theme(axis.text.x=element_text(angle=30, hjust=1),
          axis.text.x.top = element_text(angle = 30, vjust=0, hjust=0))
  return(graph)
}

period_labelling = function(from, to){
  # define the date break of the graph
  # depending of the lenght of the time period
  dif = ymd(to) - ymd(from)
  label_period = case_when(
    dif > 2100 ~ "1 month",
    dif> 1800 ~ "4 week",
    dif > 1000 ~ "3 week",
    dif > 500 ~ "2 week",
    dif > 200 ~ "1 week",
    dif > 60 ~ "2 day",
    TRUE ~ "1 day"
  )
  return(label_period)
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
                    polygonOptions = TRUE,
                    markerOptions = TRUE,
                    singleFeature = TRUE
    ) %>% 
    addSearchOSM() %>% addResetMapButton() %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles("Esri.WorldImagery", group = "Orthos") %>%
    addProviderTiles("OpenTopoMap", group = "OpenTopoMap")
  return(map)
}

create_layer = function(map, shape){
  if(!nrow(shape)){return(map)} # no change if nothing to add
  for(Li in shape$Field_ID){
    sh = shape %>% filter(Field_ID==Li)
    color = ifelse(sh$selected, "red", "blue")
    if (is.point(sh)){
      map = map %>% removeMarker(sh$Field_ID) %>% 
        addAwesomeMarkers(icon = awesomeIcons(markerColor=color),
                          layerId = as.character(sh$Field_ID),
                          label = as.character(sh$Name),
                          labelOptions = labelOptions(noHide = T),
                          group = sh$Zone_Name,
                          data = sh)
    }else{
      map = map %>% removeShape(sh$Field_ID) %>% 
        addPolygons(color = color, weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.3,
                    layerId = as.character(sh$Field_ID),
                    group = as.character(sh$Zone_Name),
                    data = sh,
                    label = as.character(sh$Name),
                    labelOptions = labelOptions(noHide = T),
                    highlightOptions = highlightOptions(
                      color = "orange", weight = 3, bringToFront = TRUE))
    }
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
                        varcrop="", vardeclaration="", varerosion=""){
  polyg=sf::st_transform(sf::read_sf(path),LEAFLET_CRS)
  if(varname==""){
    result = mutate(polyg, Name = row_number())
  }else{
    result = rename(polyg, Name = !!varname)
  }
  Zone_ID = Import_Zone(conn, dplyr::select(result, Name),
                        Zone_name = name)
  field = result %>% st_drop_geometry()
  if(varerosion!=""){
    eroField = dplyr::select(field, Name, Event_Date = !!varerosion)
    Import_Erosion(conn, Zone_ID, eroField)
  }
  if(varcrop!=""&vardeclaration!=""){
    
  culture = field %>% 
    dplyr::select(Name, Crop=!!varcrop,
           Declaration=!!vardeclaration) %>% 
    separate_rows(Crop, Declaration, sep="(;|,)")
  Import_Culture(conn, Zone_ID, culture)
  }
  return(Zone_ID)
  }

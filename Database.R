library(DBI)
library(tidyverse)
library(raster)
library(lubridate)
library(velox)
library(sf)
library(tictoc)
source("Utils.R")
source("Functions_Pheno.R")

extract_n = function(dat, n){
  # extract a number of length n from a character vector
  as.integer(str_extract(dat, paste("(?:(?<!\\d)\\d{",n,"}(?!\\d))", sep="")))
}

PRECI.CRS=CRS("+init=epsg:31467")
P_limits = c(10, 24)

SOURCES.CRS = tibble(
  SourceName = c("PHASE", "MODIS", "PRECIPITATION"),
  # crs = NULL : use the crs in the raster data
  crs = list(NULL, NULL, CRS("+init=epsg:31467"))
)

PHASE.INFO = tibble(
  files = list.files("/home/luxis/Dropbox/Kuhn/phenology/PhenoWin1/_DOY",
                     "\\.tif$", full.names = TRUE)
  ) %>% mutate(name = basename(files),
  # Varname format Crop_xxx with xxx the crop code
         VarName = paste("Crop", extract_n(name, 3), sep="_")) %>% 
  group_by(VarName) %>% 
  summarise(sources = list(files)) %>% 
  mutate(SourceName = "PHASE")
  
RASTERINFO = tibble(
  VarName = c(
    #"NDVI", 
    "RADOLANGT10MM", "RADOLANMAX", "RADOLANSUM"
  ),
  sources = list(
    #c(list.files("/home/luxis/Dropbox/MODIS","_NDVI_.*\\.tif$", full.names = TRUE)),
    c(list.files("/home/luxis/Dropbox/RadolanIndex",
                 "RADOLANGT10MM.*\\.asc$", full.names = TRUE)),
    c(list.files("/home/luxis/Dropbox/RadolanIndex",
                 "RADOLANMAX.*\\.asc$", full.names = TRUE)),
    c(list.files("/home/luxis/Dropbox/RadolanIndex",
                 "RADOLANSUM.*\\.asc$", full.names = TRUE))
  ),
  SourceName = c(#"MODIS", 
                 "PRECIPITATION", "PRECIPITATION", "PRECIPITATION")
) %>% bind_rows(PHASE.INFO) 

### automatique

DATARASTER = unnest(RASTERINFO) %>% 
  mutate(name = basename(sources),
         Year = extract_n(name, 4),
         S_ID = row_number()) %>% 
  inner_join(SOURCES.CRS, by="SourceName")

RASTERSOURCES = RASTERINFO %>% 
  group_by(SourceName) %>% 
  summarise(model = sources[[1]][1]) %>% 
  inner_join(SOURCES.CRS, by="SourceName")

# generate the table DataSource
DATASOURCES = NULL
for(i in 1:nrow(RASTERSOURCES)){
  ModelRaster = raster(RASTERSOURCES$model[[i]])
  if(!is.null(RASTERSOURCES$crs[[i]])){
    crs(ModelRaster) = RASTERSOURCES$crs[[i]]
  }
  Rext = extent(ModelRaster)
  Rtable = tibble(
    SourceName = RASTERSOURCES$SourceName[[i]],
    xmin = Rext@xmin, xmax = Rext@xmax,
    ymin = Rext@ymin, ymax = Rext@ymax,
    nrow = nrow(ModelRaster),
    ncol = ncol(ModelRaster),
    CRS = crs(ModelRaster, asText=T)
  )
  if(is.null(DATASOURCES)){
    DATASOURCES = Rtable
  }else{
    DATASOURCES = rbind(DATASOURCES, Rtable)
  }
}

Init_database = function(conn, init_sql = "Init_Database.sql"){
  cc <<- conn
  sql_init = read_file(init_sql)
  sql_list = str_split(sql_init, ";", simplify=TRUE)
  for(i in 1:(length(sql_list)-1)){#last instruction is just a space
    dbExecute(conn, sql_list[i])
  }
  #if (!("geometry" %in% dbGetQuery(conn, "PRAGMA table_info(Field)")$name))
  #{dbExecute(conn, "SELECT AddGeometryColumn('Field', 'geometry',4326, 'POLYGON', 'XY')")}
  # add the definition of empty raster data source
  dbWriteTable(conn, "DataSource", DATASOURCES, append=TRUE)
  # write the names of the corresponding variables
  Variable4database = RASTERINFO %>% 
    inner_join(collect(tbl(conn, "DataSource")), by = "SourceName") %>% 
    dplyr::select(VarName, Source_ID)
  dbWriteTable(conn, "Variable", Variable4database, append=TRUE)
  return(conn)
}

Import_Weight = function(conn, Source_ID, Field_id, precision=10){
  # Source_ID: integer
  # Field_id: list of intergers
  # precision: precision of the calculation of the coverage
  RasterID = Load_RasterID(conn, Source_ID)
  field = st_read(conn, "Field") %>% 
    filter(Field_ID %in% Field_id) %>% 
    mutate(Field_NR = row_number()) %>% 
    st_transform(st_crs(RasterID))
  VrasterID = velox(RasterID)
  VrasterID$crop(field)
  Rcropped = VrasterID$as.RasterLayer()
  # split each cell into 100 pieces to calculate the weight
  Rdisa = raster::disaggregate(Rcropped, precision)
  Vdisa = velox(Rdisa)
  extractCoord = Vdisa$extract(field, df=TRUE, small=TRUE) %>% 
    rename(Field_NR = 1, Coord = 2) 
  field_size = extractCoord %>% group_by(Field_NR) %>% 
    summarise(size = n())
  weighting = extractCoord %>% 
    group_by(Field_NR, Coord) %>% #count proportion of each pixel
    summarise(intersection = n()) %>%
    inner_join(field_size, by = "Field_NR") %>% 
    mutate(Pcover = intersection/(precision^2),
           Fcover = intersection/size) %>% 
    ungroup() %>% 
    inner_join(st_drop_geometry(field), by = "Field_NR")
  #### Save Pixel Position
  Position4database = transmute(weighting, Coord, Source_ID = !!Source_ID)
  dbWriteTable(conn, "Position", Position4database, append=TRUE)
  #### Retrive IDs in the database
  weighting4database = weighting %>%
    inner_join(
      dbGetQuery(conn, # retrive the Position ID automatically created
                 "Select Position_ID, Coord from Position where Source_ID=?",
                 param=Source_ID),
      by="Coord") %>% 
    dplyr::select(Field_ID, Position_ID, Pcover, Fcover) # remove joining columns
  #### Save weightings data
  dbWriteTable(conn, "Weighting", weighting4database, append=TRUE)
}

Import_Measure = function(conn, Var_ID, Field_ID, Years, 
                          progresfunc = function(x){}){
  # Var_ID: unique integer
  # Field_ID: list of integer
  # Years: list of integers format yyyy
  variableONfield = tbl(conn,  "variableONfield") %>% 
    filter(Var_ID==!!Var_ID & Field_ID%in%!!Field_ID) %>% 
    collect()
  Lraster = DATARASTER %>%
    filter(Year %in% Years & VarName %in% variableONfield$VarName)

  tic(paste("extract", nrow(variableONfield), "pixels on",
            nrow(Lraster), "rasters"))
  lenLraster = nrow(Lraster)
  result = NULL
  for( i in 1:lenLraster){
    R = raster(Lraster$sources[i])
    extr = tibble(
      Value = R[variableONfield$Coord],
      Position_ID = variableONfield$Position_ID,
      name = Lraster$name[i],
      Year = Lraster$Year[i],
      Var_ID = Var_ID
    )
    if(is.null(result)){
      result = extr
    }else{
      result = bind_rows(result, extr)
    }
    progresfunc(i/lenLraster)
  }
  toc()
  
  if(unique(variableONfield$SourceName)=="PHASE"){
      measure = result %>% 
        mutate(DOY = as.integer(Value),
               # the phase code is the value
               Value = coalesce(extract_n(name, 2), extract_n(name, 1)))
  }else{
    measure = result %>% 
      mutate(DOY = extract_n(name, 3))
  }
  M4database = measure %>% 
    transmute(
      Position_ID, Var_ID, Value,
      Date = as.character(as.Date(paste(Year,"01-01",sep="-")) + days(DOY - 1))
      ) %>% 
    drop_na()
  dbWriteTable(conn, "Measure", M4database, append=TRUE)
}

### TEST ###
if(!interactive()){
  W.DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(W.DIR)
  field = st_read("field_test/field_test.geojson", quiet = TRUE) %>% 
    transmute(Name, GroupName = "test", selected = FALSE)
  conn = dbConnect(RSQLite::SQLite(), ":memory:")
  Init_database(conn)
  dbWriteTable(conn, "Field", field, append=TRUE)
  field_id = tbl(conn, "Field") %>% pull(Field_ID)
  for(i in tbl(conn, "DataSource") %>% pull(Source_ID)){
    Import_Weight(conn, i, field_id)
  }
  Import_Measure(conn, 8, 2, c(2012))
  
}
library(DBI)
library(tidyverse)
library(raster)
library(lubridate)
library(velox)
library(sf)
source("Utils.R")

MODIS.MODEL = "/home/luxis/Dropbox/MODIS/MOD09Q1_NDVI_2010_001.tif"
PHASE.FILES = list.files("/home/luxis/Dropbox/Kuhn/phenology/PhenoWin1/_DOY",
                         "\\.tif$", full.names = TRUE)
MODIS.FILES = list.files("/home/luxis/Dropbox/MODIS",
                         "_NDVI_.*\\.tif$", full.names = TRUE)
PRECI.FILES = list.files("/home/luxis/Dropbox/RadolanIndex",
                         "\\.asc$", full.names = TRUE)

PRECI.CRS=CRS("+init=epsg:31467")
P_limits = c(10, 24)

Init_database = function(conn, init_sql = "Init_Database.sql"){
  
  sql_init = read_file(init_sql)
  sql_list = str_split(sql_init, ";", simplify=TRUE)
  for(i in 1:(length(sql_list)-1)){#last instruction is just a space
    dbExecute(conn, sql_list[i])
  }
  return(conn)
}

Import_Zone = function(conn, field, Zone_name = "",
                       precision = 10){
  # field attr:
  # Name, Crop, Declaration
  
  #### Create the Zone
  MRaster = raster(MODIS.MODEL)
  names(MRaster) = Zone_name
  FieldID = mutate(field,
           Field_NR = row_number(),
           Name = as.character(Name),
           selected=FALSE)
  reproField = st_transform(FieldID, st_crs(MRaster))
  ModelRaster = Crop_sf(MRaster, reproField)
  Zone_ID = Save_RasterID(conn, ModelRaster)
  FieldID$Zone_ID = Zone_ID
  #### Create weightings
  RasterID = Load_RasterID(conn, Zone_ID)
  # split each cell into 100 pieces to calculate the weight
  Rdisa = raster::disaggregate(RasterID, precision)
  Vdisa = velox(Rdisa)
  weighting = Vdisa$extract(reproField, df=TRUE, small=TRUE) %>% 
    rename(Field_NR = 1, Coord = 2) %>%
    group_by(Field_NR, Coord) %>% #count proportion of each pixel
    summarise(weight = n()/(precision^2)) %>%
    ungroup() %>% # link weightings to the fields
    inner_join(st_drop_geometry(FieldID), by = "Field_NR")
  #### Save Pixel Position
  Position4database = dplyr::select(weighting, Coord, Zone_ID)
  dbWriteTable(conn, "Position", Position4database, append=TRUE)
  #### Save Fields data
  Field4database = FieldID %>% dplyr::select(Name, selected, Zone_ID) 
  dbWriteTable(conn, "Field", Field4database, append=TRUE)
  #### Retrive IDs in the database
  weighting4database = weighting %>%
    inner_join(
      dbGetQuery(conn, # retrive the Position ID automatically created
                 "Select Position_ID, Coord from Position where Zone_ID=?",
                 param=Zone_ID),
      by="Coord") %>% 
    inner_join(
      dbGetQuery(conn, # retrive the Field ID automatically created
                 "Select Field_ID, Name from Field where Zone_ID=?",
                 param=Zone_ID), by="Name") %>%
    dplyr::select(Field_ID, Position_ID, weight) # remove joining columns
  #### Save weightings data
  dbWriteTable(conn, "Weighting", weighting4database, append=TRUE)
  return(Zone_ID)
}

Import_Culture = function(conn, Zone_ID, culture){
  # culture : Name, Declaration, Crop
  # Name: name of the field
  # Declaration: declaration date of the culture (yyyy-mm-dd)
  # Crop code
  culture = mutate(culture, Name = as.character(Name))
  Plimits = c(10,24)
  print(culture)
  culture4database = tbl(conn, "Field") %>% 
    filter(Zone_ID==!!Zone_ID) %>% 
    collect() %>% 
    transmute(Field_ID, Name = as.character(Name)) %>% 
    inner_join(culture, by="Name") %>% 
    dplyr::select(Field_ID, Declaration, Crop)
  print("sdsdsd")
  dbAppendTable(conn, "Culture", culture4database)
}

Import_Phase = function(conn, Field_id = NA){
  culposquery = tbl(conn, "CulturePosition")
  if(!is.na(Field_id)){
    culposquery = filter(culposquery, Field_ID%in%Field_id)
  }
  CulturePosition = culposquery %>% 
    collect() %>% 
    dplyr::select(-Field_ID) %>% 
    gather("limit", "exist", -Position_ID,
           -Crop, -Declaration, -Culture_ID) %>% 
    filter(is.na(exist)) %>% 
    mutate(
      P = case_when(
        limit == "Beginning" ~ 10,
        limit == "Ending" ~ 24
      ),
      direct = case_when(
        limit == "Beginning" ~ -1,
        limit == "Ending" ~ 1
      ),
      year1 = year(Declaration),
      year2 = year(Declaration) + direct
      )
  print(CulturePosition)
  if(!nrow(CulturePosition)){return(NULL)}
  print("NULLL")
  Crops = CulturePosition %>% 
    dplyr::select( Crop, year1, year2) %>% 
    gather("name", "Year", -Crop) %>%
    distinct(Crop, Year)
  PInfo = extract_PhaseDir(PHASE.FILES) %>% 
    inner_join(Crops, by = c("Crop", "Year"))
  # mask for extraction
  posiMask = maskFromPosition(conn,CulturePosition$Position_ID)
  # extract values of the phase rasters
  Phase = ExtractRaster(PInfo$dir, PInfo$IDfile, posiMask) %>% 
    gather("IDfile","DOY", -Position_ID)%>% 
    inner_join(PInfo, by="IDfile") %>%
    extract_date() %>% 
    dplyr::select(name, Position_ID, Crop, Phase_Date = Date, P)
  Limits4database = Phase %>%
    inner_join(CulturePosition, by = c("Crop", "P", "Position_ID")) %>%
    filter(year(Phase_Date)==year1|year(Phase_Date)==year2) %>% 
    mutate(DDistance = difftime(Phase_Date, Declaration, units="days")) %>% 
    filter(DDistance*direct >=0) %>%
    mutate(absDistance = abs(DDistance)) %>% 
    group_by(Culture_ID, P, Declaration, direct) %>% 
    summarise(distances = quantile(absDistance, 0.25, na.rm=TRUE)) %>% 
    mutate(Transition = as.Date(Declaration) + direct*distances) %>%
    ungroup() %>% 
    dplyr::transmute(P, Culture_ID, Transition =as.character(Transition))
  dbWriteTable(conn, "Phase", Limits4database, append=TRUE)
  
  # Load the other phases
  Phase4database = tbl(conn, "CulturePosition") %>% 
    dplyr::select(-Field_ID) %>% 
    collect() %>% drop_na() %>% 
    inner_join(Phase, by=c("Crop", "Position_ID")) %>%
    filter(!P%in%P_limits) %>% 
    filter(as.Date(Phase_Date)>=as.Date(Beginning)&
             as.Date(Phase_Date)<= as.Date(Ending)) %>% 
    group_by(Culture_ID, P) %>% 
    summarise(Transition = as.Date(Phase_Date) %>%
                as.integer() %>% quantile(0.25) %>% 
                as.Date(origin = "1970-01-01") %>%
                as.character())
  dbWriteTable(conn, "Phase", Phase4database, append=TRUE)
}

Import_Measure = function(conn, field_id = NA, progress=NA){
  # progress : function with number between 0 and 1 
  
  culQuery = tbl(conn, "CulturePosition")
  if(!is.na(field_id)){culQuery = filter(culQuery, Field_ID==field_id)}
  cultures = collect(culQuery)
  
  modis = tibble(dir=MODIS.FILES) %>% 
    mutate(Variable = "NDVI", name = basename(dir))
  preci = tibble(dir=PRECI.FILES) %>% 
    mutate(name = basename(dir)) %>% 
    mutate(Variable = str_split(name, "_", simplify = TRUE)[,1])
  Minfo = bind_rows(modis, preci)%>% 
    mutate(Year=extract_n(name,4), DOY=extract_n(name,3)) %>%
    extract_date() %>% 
    mutate(IDfile = paste("X", row_number(), sep=""))
  Ldates = distinct(Minfo, Date) %>% pull(Date)
  pb <- txtProgressBar(min=0, max=length(Ldates), style=3) 
  for(CurDate in Ldates){
    prog = getTxtProgressBar(pb)+1
    progress(prog/length(Ldates))
    setTxtProgressBar(pb, prog)
    cul = cultures %>% 
      filter((Beginning <= CurDate) & (Ending >= CurDate))
    if(!nrow(cul)){next}
    Dinfo = filter(Minfo, Date == CurDate)
    maskRaster = maskFromPosition(conn, cul$Position_ID) %>% trim()
    
    names(maskRaster) = "Position_ID"
    StackR = stack(maskRaster)
    for(i in 1:nrow(Dinfo)){
      loadedR = raster(Dinfo$dir[i])
      if(is.na(crs(loadedR))){crs(loadedR) = PRECI.CRS}
      newR = projectRaster(loadedR, maskRaster)
      names(newR) = Dinfo$IDfile[i]
      StackR = stack(newR, StackR)
    }
    M4database = as.data.frame(StackR, na.rm=TRUE) %>% 
      gather("IDfile", "Value", -Position_ID) %>% 
      inner_join(Dinfo, by="IDfile") %>% 
      dplyr::select(Position_ID, Date, Variable, Value)
    dbWriteTable(conn,"Measure", M4database, append=TRUE)
    }
}

Import_Erosion = function(conn, Zone_ID, erosion){
  # erosion : Name, Event_Date
  erosion = mutate_all(erosion, as.character) %>% drop_na()
  erosion4database = tbl(conn, "Field") %>% 
    filter(Zone_ID==!!Zone_ID) %>% 
    collect() %>% 
    transmute(Field_ID, Name=as.character(Name)) %>% 
    left_join(erosion, by="Name") %>% 
    dplyr::select(Field_ID, Event_Date)
  dbAppendTable(conn, "ErosionEvent", erosion4database)
}

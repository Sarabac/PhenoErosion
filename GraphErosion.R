
### Colors corresponding to each phenological stage
phasesCode = read.csv("Phases.csv") %>% arrange(Code)

### Precipitation Name
PRECI.VARIABLES = c("RADOLANGT10MM", "RADOLANMAX", "RADOLANSUM")
names(PRECI.VARIABLES) = PRECI.VARIABLES

normalize = function(x){
  (x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))
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

selected_fields = function(conn){tbl(conn, "Field") %>% filter(selected) %>% pull(Field_ID)}

conn  = dbConnect(RSQLite::SQLite(), "/home/luxis/Dropbox/Kuhn/phenology/PhenoErosion/field_test/PhenoErosion(6).sqlite")

getPhase = function(conn, TPcover = 0, TFcover = 0){
  
  Ph = tbl(conn, "Phase") %>% collect() %>% 
    filter(Field_ID%in%selected_fields(conn)) %>%
    rename(Date = Transition) %>% 
    filter(Pcover>=TPcover) %>% 
    group_by(Field_ID, Culture_ID, P) %>% 
    summarise(Date = max(Date)) %>% 
    ungroup()
    
  phase = Ph %>% 
    group_by(Culture_ID) %>% 
    expand(Date = seq(min(as.Date(Date)), max(as.Date(Date)), by = "days")) %>% 
    ungroup() %>% 
    mutate(Date = as.character(Date)) %>% 
    left_join(Ph, by = c("Culture_ID", "Date")) %>% 
    fill(Field_ID, P) %>% 
    mutate(NormValue = 0.5)
  
  return(phase)
}

getPrecipitation = function(conn, varName = "RADOLANGT10MM",
                            TPcover = 0, TFcover = 0){
  precipitation = tbl(conn, "WeightMeasure") %>%
    filter(VarName == varName) %>%
    collect() %>% 
    filter(Field_ID %in% selected_fields(conn)) %>% 
    filter(Pcover>=TPcover) %>% 
    group_by(Field_ID, Date) %>% 
    summarise(Value = max(Value)) %>% 
    mutate(NormValue = normalize(Value))
  return(precipitation)
}

getNDVI = function(conn, TPcover = 0, TFcover = 0){
  NDVI = tbl(conn, "WeightMeasure") %>%
    filter(VarName == "NDVI") %>%
    collect() %>% 
    filter(Field_ID %in% selected_fields(conn)) %>% 
    filter(Pcover>=TPcover) %>% 
    mutate(NormValue = normalize(Value))
  return(NDVI)
}

getErosion = function(conn){
  erosion = tbl(conn, "ErosionEvent") %>% 
    collect() %>% 
    filter(Field_ID %in% selected_fields(conn)) %>% 
    transmute(Field_ID, Date = as.Date(Event_Date))
  return(erosion)
}

getCulture = function(conn){
  culture =  tbl(conn, "Culture") %>% 
    collect() %>% 
    filter(Field_ID %in% selected_fields(conn)) %>% 
    transmute(Field_ID,
              Crop = cropName(Crop),
              Date = as.Date(Declaration),
              NormValue = 1)
}

drawErosion = function(culture, phase, erosion, NDVI, precipitation, date_limits = NULL){
  # create the Phenological graph from the data frame "dat" with attributs:
  #     "Date": class Date
  #     "sum_weight": between 0 and 1, proportion of pixels in phase P
  #     "P": phenological stage
  #     "Area": spatial entities ID
  #     "Crop": the crop ID
  # date_breaks: character
  culture <<- culture
  phase <<- phase
  erosion <<- erosion
  NDVI <<- NDVI
  precipitation <<- precipitation
  date_limits <<- date_limits

  if(!is.null(date_limits)){
    date_limits = as.Date(date_limits)
    date_breaks = period_labelling(date_limits[1], date_limits[2])
  }else{
    date_breaks = waiver()
  }
  
  fphasesCode = filter(phasesCode, Code%in%phase$P)
  color_fill_rule = as.vector(fphasesCode$Color)
  names(color_fill_rule) <- fphasesCode$Code
  #color_fill_rule = c(color_fill_rule, c("Precipitation"="darkblue"))
  
  graph =  ggplot( mapping = aes(x = as.Date(Date), y=NormValue))+
    geom_tile(data = phase, mapping =  aes(fill = as.factor(P))) +
    geom_label(data = culture, aes(label = Crop), vjust = -0.1, size = 4) +
    geom_boxplot( aes(group=Date), data = NDVI, outlier.alpha=0) +
    geom_bar(data = precipitation, fill = "darkblue", stat = "identity", color="black") +
    geom_vline(data= erosion, mapping =  aes(xintercept = Date, color = "Erosion"),
               linetype="dotted", size = 1) +
    facet_grid(Field_ID~.) +
    # color fill scall of the phenological stages
    scale_fill_manual(values = color_fill_rule) + 
    scale_color_manual(values = c("Erosion"="red")) + 
    labs(fill = "Phenology") +
    scale_x_date(name="DOY",
                 date_breaks=date_breaks,
                 labels=scales::date_format("%j"),
                 limits = date_limits,
                 sec.axis=dup_axis(
                   name="Date",labels = scales::date_format("%d %b %Y"))) +
    scale_y_continuous(limits = c(0, 1.25)) +
    theme(axis.text.x=element_text(angle=30, hjust=1),
          axis.text.x.top = element_text(angle = 30, vjust=0, hjust=0))

  return(graph)
}

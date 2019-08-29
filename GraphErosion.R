
### Colors corresponding to each phenological stage
phasesCode = read.csv("Phases.csv") %>% arrange(Code)

### Precipitation Name
PRECI.VARIABLES = c("RADOLANGT10MM", "RADOLANMAX", "RADOLANSUM")
names(PRECI.VARIABLES) = PRECI.VARIABLES

normalize = function(x){(x-min(x))/(max(x)-min(x))}

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

conn  = dbConnect(RSQLite::SQLite(), "/home/luxis/Dropbox/Kuhn/phenology/PhenoErosion/field_test/PhenoErosion.sqlite")

getGraphData = function(conn, threshold){
  fields = tbl(conn, "Field") %>% filter(selected) %>% pull(Field_ID)
  if(!length(fields)){return(NULL)}
  
  Measure = collect(tbl(conn, "Measure")) %>%
    inner_join(collect(tbl(conn, "Weighting")), by="Position_ID") %>%
    filter(Field_ID%in%fields) %>% 
    collect()
  
  FMeasure = Measure %>% 
    filter(weight>=threshold) %>% 
    dplyr::select(-M_ID, -W_ID) 
  
  Preci = filter(FMeasure, Variable!="NDVI")%>% 
    group_by(Variable) %>% 
    mutate(NormValue = normalize(Value)) %>% 
    group_by(Field_ID, Date, Variable) %>% 
    summarise(NormValue = median(NormValue)) %>% 
    ungroup()
  NDVI = filter(FMeasure, Variable=="NDVI") %>% 
    mutate(NormValue = normalize(Value))
  
  CultureData = tbl(conn, "CultureDate") %>% collect() %>% 
    filter(Field_ID%in%fields) %>% drop_na()
  if(!nrow(CultureData)){return(NULL)}
  CultureDate = CultureData %>% 
    group_by(Field_ID, Culture_ID) %>% 
    expand(Date = seq(as.Date(Beginning), as.Date(Ending), by="days")) %>% 
    ungroup() %>% 
    mutate(Date = as.character(Date)) %>% 
    # phases
    left_join(collect(tbl(conn, "Phase")), 
              by = c("Culture_ID"="Culture_ID", "Date"="Transition")) %>% 
    dplyr::select(-Phase_ID) %>% 
    group_by(Field_ID) %>% 
    arrange(Date, .by_group = TRUE) %>% 
    fill(P, .direction = "down") %>% 
    mutate(NormValue = 0.5) %>% # to put the tile in the middle of the graph
    ungroup()
  
  erosion = collect(tbl(conn, "ErosionEvent")) %>% 
    filter(Field_ID%in%fields) %>% 
    transmute(Field_ID, Date = as.Date(Event_Date))
  return(list(
    culture = CultureDate,
    erosion = erosion,
    NDVI = NDVI,
    precipitation = Preci
  ))
}

drawErosion = function(culture, erosion, NDVI, precipitation, date_limits = NULL){
  # create the Phenological graph from the data frame "dat" with attributs:
  #     "Date": class Date
  #     "sum_weight": between 0 and 1, proportion of pixels in phase P
  #     "P": phenological stage
  #     "Area": spatial entities ID
  #     "Crop": the crop ID
  # date_breaks: character

  if(!is.null(date_limits)){
    date_limits = as.Date(date_limits)
    date_breaks = period_labelling(date_limits[1], date_limits[2])
  }else{
    date_breaks = waiver()
  }
  
  fphasesCode = filter(phasesCode, Code%in%culture$P)
  color_fill_rule = as.vector(fphasesCode$Color)
  names(color_fill_rule) <- fphasesCode$Code
  #color_fill_rule = c(color_fill_rule, c("Precipitation"="darkblue"))
  
  graph =  ggplot(data = culture, mapping = aes(x = as.Date(Date), y=NormValue))+
    geom_tile(mapping =  aes(fill = as.factor(P))) +
    geom_vline(data= erosion, mapping =  aes(xintercept = Date, color = "Erosion")) +
    geom_boxplot( aes(group=Date), data = NDVI, outlier.alpha=0) +
    geom_bar(data = precipitation, fill = "darkblue", stat = "identity") +
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
    theme(axis.text.x=element_text(angle=30, hjust=1),
          axis.text.x.top = element_text(angle = 30, vjust=0, hjust=0))

  return(graph)
}

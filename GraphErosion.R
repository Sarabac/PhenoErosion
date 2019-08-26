
### Colors corresponding to each phenological stage
phasesCode = read.csv("Phases.csv") %>% arrange(Code)

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

conn  = dbConnect(RSQLite::SQLite(), "/home/luxis/Dropbox/Kuhn/phenology/PhenoErosion/field_test/test4.sqlite")

drawErosion = function(conn, date_limits = NULL){
  # create the Phenological graph from the data frame "dat" with attributs:
  #     "Date": class Date
  #     "sum_weight": between 0 and 1, proportion of pixels in phase P
  #     "P": phenological stage
  #     "Area": spatial entities ID
  #     "Crop": the crop ID
  # date_breaks: character

  if(is.null(date_limits)){
    date_limits = as.Date(date_limits)
  }
  
  fields = tbl(conn, "Field") %>% filter(selected) %>% pull(Field_ID)
  if(!length(fields)){return(NULL)}

  Measure = collect(tbl(conn, "Measure")) %>%
    inner_join(collect(tbl(conn, "Weighting")), by="Position_ID") %>%
    filter(Field_ID%in%fields) %>% 
    collect()
  
  FMeasure = filter(Measure, weight==1) %>% 
    dplyr::select(-M_ID, -W_ID) %>% 
    group_by(Variable) %>% 
    mutate(NormValue = normalize(Value)) %>% 
    ungroup() 
  
  Preci = filter(FMeasure, Variable!="NDVI")%>% 
    group_by(Field_ID, Date, Variable) %>% 
    summarise(NormValue = median(NormValue)) %>% 
    ungroup()
  NDVI = filter(FMeasure, Variable=="NDVI")
  
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
    mutate(NormValue = 0.5) %>% 
    ungroup()
  
  erosion = collect(tbl(conn, "ErosionEvent")) %>% 
    filter(Field_ID%in%fields) %>% 
    transmute(Field_ID, Date = as.Date(Event_Date))
  
  fphasesCode = filter(phasesCode, Code%in%CultureDate$P)
  color_fill_rule = as.vector(fphasesCode$Color)
  names(color_fill_rule) <- fphasesCode$Code
  
  graph =  ggplot(data = CultureDate, mapping = aes(x = as.Date(Date), y=NormValue))+
    geom_tile(mapping =  aes(fill = as.factor(P))) +
    geom_vline(data= erosion, mapping =  aes(xintercept = Date)) +
    geom_boxplot( aes(group=Date), data = NDVI, outlier.alpha=0) +
    geom_line(data = Preci, mapping = aes(color=Variable)) +
    facet_grid(Field_ID~.) +
    # color fill scall of the phenological stages
    scale_fill_manual(values = color_fill_rule) + 
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

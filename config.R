
#### Have to be changed if you want to include new data sources ###

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
             VarName = as.character(extract_n(name, 3))) %>% 
  group_by(VarName) %>% 
  summarise(sources = list(files)) %>% 
  mutate(SourceName = "PHASE")

RASTERINFO = tibble(
  VarName = c(
    #"NDVI", 
    "RADOLANGT10MM"#, "RADOLANMAX", "RADOLANSUM"
  ),
  sources = list(
    #c(list.files("/home/luxis/Dropbox/MODIS","_NDVI_.*\\.tif$", full.names = TRUE)),
    c(list.files("/home/luxis/Dropbox/RadolanIndex", "RADOLANGT10MM.*\\.asc$", full.names = TRUE))#,
    #c(list.files("/home/luxis/Dropbox/RadolanIndex", "RADOLANMAX.*\\.asc$", full.names = TRUE)),
    #c(list.files("/home/luxis/Dropbox/RadolanIndex", "RADOLANSUM.*\\.asc$", full.names = TRUE))
  ),
  SourceName = c(#"MODIS",  "PRECIPITATION", "PRECIPITATION", 
    "PRECIPITATION")
) %>% 
  # the column are the same with Phase Info
  bind_rows(PHASE.INFO) 

# 0. set up ----

## 0.1. librarys ----

if(!require('tidyverse')) {
  install.packages('tidyverse', dep = TRUE)
}
if(!require('sf')) {
  install.packages('sf', dep = TRUE)
}
if(!require('xtable')) {
  install.packages('xtable', dependencies = TRUE)
}
if(!require('animation')) {
  install.packages('animation', dep = TRUE)
}
if(!require('terra')) {
  install.packages('terra', dep = TRUE)
}
if(!require('exactextractr')) {
  install.packages('exactextractr', dep = TRUE)
}
if(!require('animation')) {
  install.packages('animation', dep = TRUE)
}

## 0.2. directories ---- 

# retrieve directories
## project
dir.path <- rstudioapi::getActiveDocumentContext()$path
dir.home <- sub('(englishSuicides).*', '\\1', dir.path)
dir.code <- paste0(dir.home, '/code')
dir.data <- paste0(dir.home, '/data')
dir.res <- paste0(dir.home, '/results')
dir.data.organised <- paste0(dir.data, '/organised')
dir.res.exploration <- paste0(dir.res, '/01_covariateData')
## data
dir.masterData <- paste0(sub('(OneDrive - Imperial College London).*', '\\1', dir.path), '/00_masterData')
dir.masterData.spatial <- paste0(dir.masterData, '/onsSpatial')
dir.masterData.ndvi <- paste0(dir.masterData, '/ndvi')

## 0.3. functions ----

setwd(dir.code)
source('00_functions.R')

## 0.4. shape file ----

setwd(dir.masterData.spatial)

### 0.4.1. polygon ----

poly.lsoa <- sf::st_read(dsn = dir.masterData.spatial, layer = 'ONS11_LSOA')
poly.msoa <- sf::st_read(dsn = dir.masterData.spatial, layer = 'ONS11_MSOA')
poly.lad <- sf::st_read(dsn = dir.masterData.spatial, layer = 'ONS11_LAD')

poly.lsoa.england <- poly.lsoa %>%  dplyr::filter(str_detect(LSOA11CD, '^E'))
poly.msoa.england <- poly.msoa %>%  dplyr::filter(str_detect(MSOA11CD, '^E'))
poly.lad.england <- poly.lad %>%  dplyr::filter(str_detect(LAD11CD, '^E'))

lsoa.nbs.england <- spdep::poly2nb(pl = as(poly.lsoa.england, 'Spatial'))
lsoa.nbs.england.order.2 <- spdep::nblag_cumul(nblags = spdep::nblag(neighbours = lsoa.nbs.england, maxlag = 2))

msoa.nbs.england <- spdep::poly2nb(pl = as(poly.msoa.england, 'Spatial'))
msoa.nbs.england.order.2 <- spdep::nblag_cumul(nblags = spdep::nblag(neighbours = msoa.nbs.england, maxlag = 2))

lad.nbs.england <- spdep::poly2nb(pl = as(poly.lad.england, 'Spatial'))
lad.nbs.england.order.2 <- spdep::nblag_cumul(nblags = spdep::nblag(neighbours = lad.nbs.england, maxlag = 2))

### 0.4.2. lookup file ----

lookup.11 <- 
  read.csv('LSOA11_MSOA11_LAD11_lookUp.csv') %>% 
  dplyr::select(-OA11CD, -ObjectId) %>% 
  dplyr::distinct()

## 0.5. plot arguments ----

# plot arguments
text.size <- 20
width <- height <- 10

# colour schemes
colours.diverging.full <- c('#00768D', '#7CB5C0', '#E9E0C8', '#FB8171', '#C13E2E')
colours.diverging <- c('#00768D', '#C13E2E')
colours.exceedance <- c('#A8E6CF', '#5CDB95', '#004D3A')
colours.sequential <- c('#E9E0C8', '#4B0082')

## 0.6. import ----

### 0.6.1. ndvi (import or load) ----

setwd(paste0(dir.masterData.ndvi, '/tif_16day_250m'))


if(length(list.files(pattern = '^MOD13Q1')) == 0){
  
  # directory ----
  
  dir.masterData.ndvi.hdf <- paste0(data.ndvi.dir, '/HDF Files')
  
  # availiable datasets ----
  
  MODIStsp::MODIStsp_get_prodlayers('M*D13Q1')
  
  # data downlaod ----
  
  ## define bbox ----
  
  crs.nat.england <-  sf::st_crs(poly.nat.england)$proj4string
  bbox.nat.england <- sf::st_bbox(poly.nat.england)
  
  ## download nasa data ----
  
  options(timeout = 10000)
  MODIStsp::MODIStsp(gui = FALSE,
                     out_folder = data.ndvi.dir,
                     out_folder_mod = dir.masterData.ndvi.hdf,
                     # define the vegitation index
                     selprod = 'Vegetation Indexes_16Days_250m (M*D13Q1)',
                     bandsel = c('EVI', 'NDVI'),
                     quality_bandsel = 'QA_usef',
                     indexes_bandsel = 'SR',
                     # username and password for nasaearthdata
                     user = 'USERNAME' ,
                     password = 'PASSWORD',
                     start_date = '2000.01.01',
                     end_date = '2022.12.31',
                     # define bounding box and project for UK
                     spatmeth = 'bbox',
                     bbox = bbox.nat.england,
                     output_proj = crs.nat.england,
                     delete_hdf = TRUE,
                     # reprocess = TRUE, # only do if changing something
                     scale_val = TRUE, # to have NDVI between -1 and 1
                     ts_format = 'R RasterStack',
                     out_format = 'GTiff',
                     verbose = TRUE,
                     parallel = TRUE)
  
}

# list all files
ndvi.files <- list.files(pattern = '^MOD13Q1')
# import all files
ndvi.rasters <- lapply(X = ndvi.files, FUN = terra::rast)

# 1. ndvi ----

## 1.0. function ----

# function for averaging
aggregate.ndvi <- function(raster, poly, code){
  
  # 0. function arguments ----
  
  # raster <- ndvi.rasters[[1]]
  # poly <- poly.msoa.england
  # code <- 'MSOA11CD'
  
  # 1. average over polygon ----
  
  # average over polygon
  average.poly <- 
    exactextractr::exact_extract(x = raster, 
                                 y = poly, 
                                 # weighted mean by area
                                 fun = 'weighted_mean', 
                                 weights = 'area',
                                 # add the columns of poly to new values
                                 append_cols = TRUE)
  
  data <- 
    average.poly %>% 
    dplyr::select(eval(code), 'weighted_mean') %>% 
    dplyr::rename(ndvi = weighted_mean) %>% 
    dplyr::mutate(DATE = sub('.*NDVI_', '', names(raster)) %>%  as.Date(., format = '%Y_%j'),
                  ndvi = dplyr::if_else(ndvi == 'NaN', NA, ndvi))
  
  # 2. return ----
  
  return(data)
  
}

impute.ndvi <- function(date, data, neighbours){
  
  # 0. function arguments ----
  
  # date <- dates.lsoa[1]
  # data <- ndvi.lsoa.temp
  # neighbours <- lsoa.nbs.england.order.2
  
  # 1. filter date ----
  
  # make crossection for this to work
  data.temp <-
    data %>%
    # filter out the year month
    dplyr::filter(DATE == date)
  
  # 2. impute ----
  
  data.temp$ndvi[is.na(data.temp$ndvi)] <-
    sapply(neighbours[which(is.na(data.temp$ndvi))],
           FUN = function(x) { mean(data.temp$ndvi[x], na.rm = TRUE)}) %>% 
    as.numeric()
  
  # 3. return ----
  
  return(data.temp)
  
}

## 1.1. aggregate ----

ndvi.list.lsoa <- 
  lapply(X = ndvi.rasters, 
         FUN = aggregate.ndvi, 
         poly = poly.lsoa.england, 
         code = 'LSOA11CD') %>%
  dplyr::bind_rows()

ndvi.list.msoa <- 
  lapply(X = ndvi.rasters, 
         FUN = aggregate.ndvi, 
         poly = poly.msoa.england, 
         code = 'MSOA11CD') %>%
  dplyr::bind_rows()

ndvi.list.lad <- 
  lapply(X = ndvi.rasters, 
         FUN = aggregate.ndvi, 
         poly = poly.lad.england, 
         code = 'LAD11CD') %>%
  dplyr::bind_rows()

# setwd(dir.data.organised)
# save(ndvi.list.lsoa, file = 'LSOA11_NDVI_LIST_TEMP.rda')
# save(ndvi.list.msoa, file = 'MSOA11_NDVI_LIST_TEMP.rda')
# save(ndvi.list.lad, file = 'LAD11_NDVI_LIST_TEMP.rda')

## 1.2. impute ----

### 1.2.1. lsoa ----

# unique dates
dates.lsoa <- ndvi.list.lsoa$DATE %>% unique() %>% sort()

# all the data together
ndvi.lsoa.temp <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              DATE = dates.lsoa) %>%
  dplyr::left_join(., ndvi.list.lsoa, by = c('LSOA11CD', 'DATE')) %>%
  dplyr::arrange(DATE, LSOA11CD) %>% 
  # join spatial polygon to impute missing data
  dplyr::left_join(., poly.lsoa.england, by = 'LSOA11CD') %>%
  sf::st_as_sf()

# any missing lsoa
ndvi.missing.lsoa <- 
  ndvi.lsoa.temp %>% 
  dplyr::filter(is.na(ndvi)) %>%
  sf::st_drop_geometry() %>% 
  dplyr::select(LSOA11CD, LSOA11NM, DATE) %>% 
  dplyr::distinct()

if(nrow(ndvi.missing.lsoa) > 0){
  
  ndvi.lsoa.16day.imputed <- 
    lapply(X = dates.lsoa,
           FUN = function (x) {
             impute.ndvi(date = x, data = ndvi.lsoa.temp, neighbours = lsoa.nbs.england.order.2)
           }) %>% 
    dplyr::bind_rows() %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(LSOA11CD, DATE, ndvi)
  
  setwd(dir.res.exploration)
  print(x = xtable::xtable(x = ndvi.missing.lsoa),
        include.rownames = FALSE,
        file = 'txt/lsoa/LSOA11_NDVI_MISSING.txt')
  
} else {
  
  ndvi.lsoa.16day.imputed <- 
    ndvi.lsoa.temp %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(LSOA11CD, DATE, ndvi)
  
  
}

ndvi.lsoa <- 
  ndvi.lsoa.16day.imputed %>% 
  dplyr::mutate(YEAR = lubridate::year(DATE)) %>% 
  dplyr::summarise(ndvi = mean(ndvi),
                   .by = c('LSOA11CD', 'YEAR'))

setwd(dir.data.organised)
save(ndvi.lsoa, file = 'lsoa/rda/LSOA11_NDVI.rda')
write.csv(ndvi.lsoa, file = 'lsoa/csv/LSOA11_NDVI.csv', row.names = FALSE)
save(ndvi.lsoa.16day.imputed, file = 'lsoa/rda/LSOA11_NDVI_16DAY.rda')
write.csv(ndvi.lsoa.16day.imputed, file = 'lsoa/csv/LSOA11_NDVI_16DAY.csv', row.names = FALSE)

### 1.2.2. msoa ----

# unique dates
dates.msoa <- ndvi.list.msoa$DATE %>% unique() %>% sort()

# all the data together
ndvi.msoa.temp <-
  expand.grid(MSOA11CD = poly.msoa.england$MSOA11CD,
              DATE = dates.msoa) %>%
  dplyr::left_join(., ndvi.list.msoa, by = c('MSOA11CD', 'DATE')) %>%
  dplyr::arrange(DATE, MSOA11CD) %>% 
  # join spatial polygon to impute missing data
  dplyr::left_join(., poly.msoa.england, by = 'MSOA11CD') %>%
  sf::st_as_sf()

# any missing msoa
ndvi.missing.msoa <- 
  ndvi.msoa.temp %>% 
  dplyr::filter(is.na(ndvi)) %>%
  sf::st_drop_geometry() %>% 
  dplyr::select(MSOA11CD, MSOA11NM, DATE) %>% 
  dplyr::distinct()

if(nrow(ndvi.missing.msoa) > 0){
  
  ndvi.msoa.16day.imputed <- 
    lapply(X = dates.msoa,
           FUN = function (x) {
             impute.ndvi(date = x, data = ndvi.msoa.temp, neighbours = msoa.nbs.england)
           }) %>% 
    dplyr::bind_rows() %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(MSOA11CD, DATE, ndvi)
  
  setwd(dir.res.exploration)
  print(x = xtable::xtable(x = ndvi.missing.msoa),
        include.rownames = FALSE,
        file = 'txt/msoa/MSOA11_NDVI_MISSING.txt')
  
} else {
  
  ndvi.msoa.16day.imputed <- 
    ndvi.msoa.temp %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(MSOA11CD, DATE, ndvi)
  
  
}

ndvi.msoa <- 
  ndvi.msoa.16day.imputed %>% 
  dplyr::mutate(YEAR = lubridate::year(DATE)) %>% 
  dplyr::summarise(ndvi = mean(ndvi),
                   .by = c('MSOA11CD', 'YEAR'))

setwd(dir.data.organised)
save(ndvi.msoa, file = 'msoa/rda/MSOA11_NDVI.rda')
write.csv(ndvi.msoa, file = 'msoa/csv/MSOA11_NDVI.csv', row.names = FALSE)
save(ndvi.msoa.16day.imputed, file = 'msoa/rda/MSOA11_NDVI_16DAY.rda')
write.csv(ndvi.msoa.16day.imputed, file = 'msoa/csv/MSOA11_NDVI_16DAY.csv', row.names = FALSE)

### 1.2.3. lad ----

# unique dates
dates.lad <- ndvi.list.lad$DATE %>% unique() %>% sort()

# all the data together
ndvi.lad.temp <-
  expand.grid(LAD11CD = poly.lad.england$LAD11CD,
              DATE = dates.lad) %>%
  dplyr::left_join(., ndvi.list.lad, by = c('LAD11CD', 'DATE')) %>%
  dplyr::arrange(DATE, LAD11CD) %>% 
  # join spatial polygon to impute missing data
  dplyr::left_join(., poly.lad.england, by = 'LAD11CD') %>%
  sf::st_as_sf()

# any missing lad
ndvi.missing.lad <- 
  ndvi.lad.temp %>% 
  dplyr::filter(is.na(ndvi)) %>%
  sf::st_drop_geometry() %>% 
  dplyr::select(LAD11CD, LAD11NM, DATE) %>% 
  dplyr::distinct()

if(nrow(ndvi.missing.lad) > 0){
  
  ndvi.lad.16day.imputed <- 
    lapply(X = dates.lad,
           FUN = function (x) {
             impute.ndvi(date = x, data = ndvi.lad.temp, neighbours = lad.nbs.england)
           }) %>% 
    dplyr::bind_rows() %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(LAD11CD, DATE, ndvi)
  
  setwd(dir.res.exploration)
  print(x = xtable::xtable(x = ndvi.missing.lad),
        include.rownames = FALSE,
        file = 'txt/lad/LAD11_NDVI_MISSING.txt')
  
} else {
  
  ndvi.lad.16day.imputed <- 
    ndvi.lad.temp %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(LAD11CD, DATE, ndvi)
  
  
}

ndvi.lad <- 
  ndvi.lad.16day.imputed %>% 
  dplyr::mutate(YEAR = lubridate::year(DATE)) %>% 
  dplyr::summarise(ndvi = mean(ndvi),
                   .by = c('LAD11CD', 'YEAR'))

setwd(dir.data.organised)
save(ndvi.lad, file = 'lad/rda/LAD11_NDVI.rda')
write.csv(ndvi.lad, file = 'lad/csv/LAD11_NDVI.csv', row.names = FALSE)
save(ndvi.lad.16day.imputed, file = 'lad/rda/LAD11_NDVI_16DAY.rda')
write.csv(ndvi.lad.16day.imputed, file = 'lad/csv/LAD11_NDVI_16DAY.csv', row.names = FALSE)

# 2. figures ----

## 2.0. function ----

make.plot.ndvi <- function(year = NULL, 
                           data, 
                           spatial.level = NULL,
                           poly, 
                           lookup = NULL, 
                           title.legend = 'Normalized Difference Vegetation Index (NDVI)'){
  
  # 0. funciton arguments ----
  
  # year <- 2002
  # data <- ndvi.msoa
  # spatial.level <- c('LSOA11CD', 'MSOA11CD', 'LAD11CD')[2]
  # title.legend = 'Normalized Difference Vegetation Index (NDVI)'
  # poly <- poly.msoa.england
  # lookup <- lookup.11 %>% dplyr::select(MSOA11CD, LAD11CD) %>% unique()
  
  # 1. filter and limits ----
  
  limits.spatial <- c(0, 1)
  breaks.spatial <- seq(from = limits.spatial[1] , to = limits.spatial[2], by = limits.spatial[2]/4)
  
  # 2. data sort ----
  
  plot.data <-
    data %>% 
    # filter by year IF needed
    {
      if (!is.null(year)) {
        dplyr::filter(., YEAR == year)
      } else {
        .
      }
    } %>% 
    # add lad11 codes IF needed
    {
      if (spatial.level != 'LAD11CD') {
        dplyr::left_join(., lookup %>% dplyr::select({{spatial.level}}, LAD11CD), by = spatial.level)
      } else {
        .
      }
    } %>%
    dplyr::left_join(., poly, by = spatial.level) %>%
    sf::st_as_sf()
  
  plot.data.london <-
    plot.data %>% 
    dplyr::filter(str_detect(LAD11CD, '^E09'))
  
  # 3. plot ----
  
  plot.london <-
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = plot.data.london, aes(fill = ndvi), colour = NA) +
    ggplot2::scale_fill_gradient(name = '',
                                 low = colours.sequential[1], high = colours.sequential[2],
                                 limits = limits.spatial, breaks = breaks.spatial) +
    my.map.theme(legend.position = 'none')
  
  plot <- 
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = plot.data, aes(fill = ndvi), colour = NA) +
    ggplot2::scale_fill_gradient(name = title.legend,
                                 guide = guide_colourbar(title.position = 'top'),
                                 low = colours.sequential[1], high = colours.sequential[2],
                                 limits = limits.spatial, breaks = breaks.spatial) +
    # month/year label in top right
    ggplot2::annotate('text', x = Inf, y = Inf, label = year, vjust = 1, hjust = 1, color = 'grey', size = 7.5) +
    # add london in seperately
    ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
    ggplot2::annotation_custom(ggplot2::ggplotGrob(plot.london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
    my.map.theme(text = element_text(size = 20),
                 legend.position = 'bottom',
                 legend.key.width = unit(2, 'cm'),
                 legend.title = element_text(hjust = 0.5))
  
  # 4. return ----
  
  return(plot)
  
}

## 2.1. gif ----

### 2.1.1. lsoa ----

ndvi.lsoa.plots <- lapply(2002:2022, 
                          make.plot.ndvi, 
                          data = ndvi.lsoa,
                          spatial.level = 'LSOA11CD',
                          poly = poly.lsoa.england, 
                          lookup = lookup.11 %>% dplyr::select(LSOA11CD, LAD11CD) %>% unique())


# save the ggplots as a gif
setwd(dir.res.exploration)
animation::saveGIF(
  expr = {for (i in 1:length(ndvi.lsoa.plots)) { print(ndvi.lsoa.plots[[i]]) } }, 
  movie.name = paste0(dir.res.exploration, '/gif/lsoa/LSOA11_NDVI.gif'), 
  interval = 0.5,
  ani.width = width * 100,
  ani.height = height * 100)

### 2.1.2. msoa ----

ndvi.msoa.plots <- lapply(2002:2022, 
                          make.plot.ndvi, 
                          data = ndvi.msoa,
                          spatial.level = 'MSOA11CD',
                          poly = poly.msoa.england, 
                          lookup = lookup.11 %>% dplyr::select(MSOA11CD, LAD11CD) %>% unique())


# save the ggplots as a gif
setwd(dir.res.exploration)
animation::saveGIF(
  expr = {for (i in 1:length(ndvi.msoa.plots)) { print(ndvi.msoa.plots[[i]]) } }, 
  movie.name = paste0(dir.res.exploration, '/gif/msoa/MSOA11_NDVI.gif'), 
  interval = 0.5,
  ani.width = width * 100,
  ani.height = height * 100)

### 2.1.3. lad ----

ndvi.lad.plots <- lapply(2002:2022, 
                         make.plot.ndvi, 
                         data = ndvi.lad,
                         spatial.level = 'LAD11CD',
                         poly = poly.lad.england)


# save the ggplots as a gif
setwd(dir.res.exploration)
animation::saveGIF(
  expr = {for (i in 1:length(ndvi.lad.plots)) { print(ndvi.lad.plots[[i]]) } }, 
  movie.name = paste0(dir.res.exploration, '/gif/lad/LAD11_NDVI.gif'), 
  interval = 0.5,
  ani.width = width * 100,
  ani.height = height * 100)


## 2.2. plot (average) ----

### 2.2.1. lsoa ----

ndvi.average.plot.lsoa <- 
  ndvi.lsoa %>%
  # filter for study period
  dplyr::filter(YEAR %in% 2002:2022) %>% 
  # average
  dplyr::summarise(ndvi = ndvi %>% mean(),
                   .by = 'LSOA11CD') %>% 
  make.plot.ndvi(data = .,
                 spatial.level = 'LSOA11CD',
                 poly = poly.lsoa.england,
                 lookup = lookup.11 %>% dplyr::select(LSOA11CD, LAD11CD) %>% unique(),
                 title.legend = '')

setwd(dir.res.exploration)
# png
ggplot2::ggsave(plot = ndvi.average.plot.lsoa,
                filename = 'png/lsoa/LSOA11_NDVI_AVERAGE.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = ndvi.average.plot.lsoa,
                filename = 'eps/lsoa/LSOA11_NDVI_AVERAGE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)

### 2.2.2. msoa ----

ndvi.average.plot.msoa <- 
  ndvi.msoa %>% 
  # filter for study period
  dplyr::filter(YEAR %in% 2002:2022) %>% 
  # average
  dplyr::summarise(ndvi = ndvi %>% mean(),
                   .by = 'MSOA11CD') %>% 
  make.plot.ndvi(data = .,
                 spatial.level = 'MSOA11CD',
                 poly = poly.msoa.england,
                 lookup = lookup.11 %>% dplyr::select(MSOA11CD, LAD11CD) %>% unique(),
                 title.legend = '')

setwd(dir.res.exploration)
# png
ggplot2::ggsave(plot = ndvi.average.plot.msoa,
                filename = 'png/msoa/MSOA11_NDVI_AVERAGE.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = ndvi.average.plot.msoa,
                filename = 'eps/msoa/MSOA11_NDVI_AVERAGE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)

### 2.2.3. lad ----

ndvi.average.plot.lad <- 
  ndvi.lad %>% 
  # filter for study period
  dplyr::filter(YEAR %in% 2002:2022) %>% 
  # average
  dplyr::summarise(ndvi = ndvi %>% mean(),
                   .by = 'LAD11CD') %>% 
  make.plot.ndvi(data = .,
                 spatial.level = 'LAD11CD',
                 poly = poly.lad.england,
                 title.legend = '')

setwd(dir.res.exploration)
# png
ggplot2::ggsave(plot = ndvi.average.plot.lad,
                filename = 'png/lad/LAD11_NDVI_AVERAGE.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = ndvi.average.plot.lad,
                filename = 'eps/lad/LAD11_NDVI_AVERAGE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)

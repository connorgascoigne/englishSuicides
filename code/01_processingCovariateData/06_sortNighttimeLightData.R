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
if(!require('future')) {
  install.packages('future', dep = TRUE)
}
if(!require('future.apply')) {
  install.packages('future.apply', dep = TRUE)
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
dir.masterData.ntl <- paste0(dir.masterData, '/nighttimeLight')

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

### 0.6.1. nighttime light (import or load)

setwd(dir.masterData.ntl)

# all nightime lights 
website.call <- rfigshare::fs_details('9828827', mine = F, session = NULL)
website.details.all <- jsonlite::fromJSON(jsonlite::toJSON(website.call$files), flatten = T)

# download files
ntl.urls <- 
  website.details.all %>% 
  dplyr::mutate(download = paste0(download_url, '/', name),
                year = name %>% stringr::str_replace_all(., '[^0-9]', '')) %>% 
  dplyr::filter(name != 'DN_NTL_2013_simVIIRS.tif') %>% 
  dplyr::arrange(year) %>% 
  dplyr::pull(download)

ntl.file.names.website <- basename(ntl.urls)
ntl.file.names.local <- list.files(pattern = '.tif')

if(!dplyr::setequal(ntl.file.names.website, ntl.file.names.local)){
  # download
  lapply(ntl.urls, 
         function(x){ tryCatch({ download.file(url = x, destfile = basename(x), mode = 'wb') }, error = function(e) { result <- NA }) })
}

# import all files
ntl.rasters <- lapply(X = ntl.file.names.local, FUN = terra::rast)
year.min <- 1992
year.max <- 2022

# 1. nighttime lights ----

## 1.0. function ----

# function for averaging
aggregate.ntl <- function(raster, poly, code){
  
  # 0. function arguments ----
  
  # raster <- ntl.rasters[[1]]
  # poly <- poly.msoa.england
  # code <- 'MSOA11CD'
  
  # 1. reproject raster ----
  
  # reproject raster
  poly.crs <- sf::st_crs(poly)$proj4string
  rast.crs <- terra::crs(raster)
  
  # quicker to crop and then reproject
  ## put poly on raster crs
  poly.temp <- 
    poly %>% 
    st_transform(., rast.crs)
  ## crop raster to UK
  raster.crop <- terra::crop(x = raster, y = poly.temp)
  ## reproject raster to ONS proj
  raster.crop.reproj <- terra::project(x = raster.crop, y = poly.crs)
  
  # 2. average over polygon ----
  
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
    dplyr::rename(nighttimeLight = weighted_mean) %>% 
    dplyr::mutate(YEAR = gsub('[^0-9]', '', names(raster)) %>% as.numeric())
  
  # 3. return ----
  
  return(data)
  
}

# function for imputing
impute.ntl <- function(year, data, neighbours){
  
  # 0. function arguments ----
  
  # X <- 2000
  # data <- ntl.lsoa
  
  # 1. filter year ----
  
  # make crossection for this to work
  data.temp <-
    data %>%
    # filter out the year month
    dplyr::filter(YEAR == year)
  
  # 2. impute missing values ----
  
  data.temp$nighttimeLight[is.na(data.temp$nighttimeLight)] <-
    sapply(neighbours[which(is.na(data.temp$nighttimeLight))],
           FUN = function(x) { mean(data.temp$nighttimeLight[x], na.rm = TRUE)}) %>% 
    as.numeric()
  
  # 3. return ----
  
  # return imputed cross sectional data
  return(data.temp)
  
}

## 1.1. aggregate ----

ntl.list.lsoa <- 
  lapply(X = ntl.rasters,
         FUN = function(x){
           aggregate.ntl(raster = x, poly = poly.lsoa.england, code = 'LSOA11CD')
         }) %>%
  dplyr::bind_rows()

ntl.list.msoa <- 
  lapply(X = ntl.rasters,
         FUN = function(x){
           aggregate.ntl(raster = x, poly = poly.msoa.england, code = 'MSOA11CD')
         }) %>%
  dplyr::bind_rows()

ntl.list.lad <- 
  lapply(X = ntl.rasters,
         FUN = function(x){
           aggregate.ntl(raster = x, poly = poly.lad.england, code = 'LAD11CD')
         }) %>%
  dplyr::bind_rows()

## 1.2. impute ----

### 1.2.1. lsoa ----

ntl.lsoa.temp <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              YEAR = 1992:2022) %>% 
  dplyr::left_join(., ntl.list.lsoa, by = c('LSOA11CD', 'YEAR')) %>% 
  # join spatial polygon to impute missing data
  dplyr::left_join(., poly.lsoa.england, by = 'LSOA11CD') %>%
  sf::st_as_sf()

# any missing lsoa
ntl.missing.lsoa <- 
  ntl.lsoa.temp %>% 
  dplyr::filter(is.na(nighttimeLight)) %>%
  sf::st_drop_geometry() %>% 
  dplyr::select(LSOA11CD, LSOA11NM, YEAR)

if(nrow(ntl.missing.lsoa) > 0){
  
  lsoa.nbs.england <- spdep::poly2nb(pl = as(poly.lsoa.england, 'Spatial'))
  
  ntl.lsoa <- 
    lapply(X = year.min:year.max,
           FUN = function (x) {
             impute.ntl(year = x, data = ntl.lsoa.temp, neighbours = lsoa.nbs.england)
           }) %>% 
    dplyr::bind_rows() %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(LSOA11CD, YEAR, nighttimeLight)
  
  setwd(dir.res.exploration)
  print(x = xtable::xtable(x = ntl.missing.lsoa),
        include.rownames = FALSE,
        file = 'txt/lsoa/LSOA11_NIGHTTIME_LIGHTS_MISSING.txt')
  
} else {
  
  ntl.lsoa <- 
    ntl.lsoa.temp %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(LSOA11CD, YEAR, nighttimeLight)
  
}

setwd(dir.data.organised)
save(ntl.lsoa, file = 'lsoa/rda/LSOA11_NIGHTTIME_LIGHTS.rda')
write.csv(ntl.lsoa, file = 'lsoa/csv/LSOA11_NIGHTTIME_LIGHTS.csv', row.names = FALSE)

### 1.2.1. msoa ----

ntl.msoa.temp <-
  expand.grid(MSOA11CD = poly.msoa.england$MSOA11CD,
              YEAR = 1992:2022) %>% 
  dplyr::left_join(., ntl.list.msoa, by = c('MSOA11CD', 'YEAR')) %>% 
  # join spatial polygon to impute missing data
  dplyr::left_join(., poly.msoa.england, by = 'MSOA11CD') %>%
  sf::st_as_sf()

# any missing msoa
ntl.missing.msoa <- 
  ntl.msoa.temp %>% 
  dplyr::filter(is.na(nighttimeLight)) %>%
  sf::st_drop_geometry() %>% 
  dplyr::select(MSOA11CD, MSOA11NM, YEAR)

if(nrow(ntl.missing.msoa) > 0){
  
  msoa.nbs.england <- spdep::poly2nb(pl = as(poly.msoa.england, 'Spatial'))
  
  ntl.msoa <- 
    lapply(X = year.min:year.max,
           FUN = function (x) {
             impute.ntl(year = x, data = ntl.msoa.temp, neighbours = msoa.nbs.england)
           }) %>% 
    dplyr::bind_rows() %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(MSOA11CD, YEAR, nighttimeLight)
  
  setwd(dir.res.exploration)
  print(x = xtable::xtable(x = ntl.missing.msoa),
        include.rownames = FALSE,
        file = 'txt/msoa/MSOA11_NIGHTTIME_LIGHTS_MISSING.txt')
  
} else {
  
  ntl.msoa <- 
    ntl.msoa.temp %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(MSOA11CD, YEAR, nighttimeLight)
  
}

setwd(dir.data.organised)
save(ntl.msoa, file = 'msoa/rda/MSOA11_NIGHTTIME_LIGHTS.rda')
write.csv(ntl.msoa, file = 'msoa/csv/MSOA11_NIGHTTIME_LIGHTS.csv', row.names = FALSE)

### 1.2.1. lad ----

ntl.lad.temp <-
  expand.grid(LAD11CD = poly.lad.england$LAD11CD,
              YEAR = 1992:2022) %>% 
  dplyr::left_join(., ntl.list.lad, by = c('LAD11CD', 'YEAR')) %>% 
  # join spatial polygon to impute missing data
  dplyr::left_join(., poly.lad.england, by = 'LAD11CD') %>%
  sf::st_as_sf()

# any missing lad
ntl.missing.lad <- 
  ntl.lad.temp %>% 
  dplyr::filter(is.na(nighttimeLight)) %>%
  sf::st_drop_geometry() %>% 
  dplyr::select(LAD11CD, LAD11NM, YEAR)

if(nrow(ntl.missing.lad) > 0){
  
  lad.nbs.england <- spdep::poly2nb(pl = as(poly.lad.england, 'Spatial'))
  
  ntl.lad <- 
    lapply(X = year.min:year.max,
           FUN = function (x) {
             impute.ntl(year = x, data = ntl.lad.temp, neighbours = lad.nbs.england)
           }) %>% 
    dplyr::bind_rows() %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(LAD11CD, YEAR, nighttimeLight)
  
  setwd(dir.res.exploration)
  print(x = xtable::xtable(x = ntl.missing.lad),
        include.rownames = FALSE,
        file = 'txt/lad/LAD11_NIGHTTIME_LIGHTS_MISSING.txt')
  
} else {
  
  ntl.lad <- 
    ntl.lad.temp %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(LAD11CD, YEAR, nighttimeLight)
  
}

setwd(dir.data.organised)
save(ntl.lad, file = 'lad/rda/LAD11_NIGHTTIME_LIGHTS.rda')
write.csv(ntl.lad, file = 'lad/csv/LAD11_NIGHTTIME_LIGHTS.csv', row.names = FALSE)

# 2. plots ----

## 2.0 function ----

make.plot.nighttimeLight <- function(year = NULL, 
                                     data, 
                                     spatial.level = NULL, 
                                     poly, 
                                     lookup = NULL,
                                     title.legend = 'Light pollution (DN values)'){
  
  # 0. function arguments ----
  
  # year <- 1992
  # data <- ntl.msoa
  # spatial.level <- c('LSOA11CD', 'MSOA11CD', 'LAD11CD')[2]
  # poly <- poly.msoa.england
  # lookup <- lookup.11 %>% dplyr::select(MSOA11CD, LAD11CD) %>% unique()
  
  # 1. filter and limits ----
  
  limits.spatial <- c(0, 64)
  breaks.spatial <- seq(from = limits.spatial[1], to = limits.spatial[2], by = limits.spatial[2]/4)
  
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
    ggplot2::geom_sf(data = plot.data.london, aes(fill = nighttimeLight), colour = NA) +
    ggplot2::scale_fill_gradient(name = '',
                                 low = colours.sequential[1], high = colours.sequential[2],
                                 limits = limits.spatial, breaks = breaks.spatial) +
    my.map.theme(legend.position = 'none')
  
  plot <- 
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = plot.data, aes(fill = nighttimeLight), colour = NA) +
    ggplot2::scale_fill_gradient(name = title.legend,
                                 guide = guide_colourbar(title.position = 'top'),
                                 low = colours.sequential[1], high = colours.sequential[2],
                                 limits = limits.spatial, breaks = breaks.spatial) +
    # year label in top right
    ggplot2::annotate('text', x = Inf, y = Inf, label = year, vjust = 1, hjust = 1, color = 'grey', size = 7.5) +
    # add london in seperately
    ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
    ggplot2::annotation_custom(ggplot2::ggplotGrob(plot.london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
    my.map.theme(text = element_text(size = 20),
                 legend.position = 'bottom',
                 legend.key.width = unit(2, 'cm'),
                 legend.title = element_text(hjust = 0.5))
  
  # return ----
  
  return(plot)
  
}

## 2.1 gif ----

### 2.1.1. lsoa ----

ntl.lsoa.plots <- lapply(2002:2022, 
                         make.plot.nighttimeLight, 
                         data = ntl.lsoa,
                         spatial.level = 'LSOA11CD',
                         poly = poly.lsoa.england, 
                         lookup = lookup.11 %>% dplyr::select(LSOA11CD, LAD11CD) %>% unique())

# Save the ggplots as a gif
setwd(dir.res.exploration)
animation::saveGIF(
  expr = {for (i in 1:length(ntl.lsoa.plots)) { print(ntl.lsoa.plots[[i]]) } }, 
  movie.name = paste0(dir.res.exploration, '/gif/lsoa/LSOA11_NIGHTTIME_LIGHT.gif'), 
  interval = 0.5,
  ani.width = width * 100,
  ani.height = height * 100)

### 2.1.2. msoa ----

ntl.msoa.plots <- lapply(2002:2022, 
                         make.plot.nighttimeLight, 
                         data = ntl.msoa,
                         spatial.level = 'MSOA11CD',
                         poly = poly.msoa.england, 
                         lookup = lookup.11 %>% dplyr::select(MSOA11CD, LAD11CD) %>% unique())

# Save the ggplots as a gif
setwd(dir.res.exploration)
animation::saveGIF(
  expr = {for (i in 1:length(ntl.msoa.plots)) { print(ntl.msoa.plots[[i]]) } }, 
  movie.name = paste0(dir.res.exploration, '/gif/msoa/MSOA11_NIGHTTIME_LIGHT.gif'), 
  interval = 0.5,
  ani.width = width * 100,
  ani.height = height * 100)

### 2.1.3. lad ----

ntl.lad.plots <- lapply(2002:2022, 
                        make.plot.nighttimeLight, 
                        data = ntl.lad,
                        spatial.level = 'LAD11CD',
                        poly = poly.lad.england)

# Save the ggplots as a gif
setwd(dir.res.exploration)
animation::saveGIF(
  expr = {for (i in 1:length(ntl.lad.plots)) { print(ntl.lad.plots[[i]]) } }, 
  movie.name = paste0(dir.res.exploration, '/gif/lad/LAD11_NIGHTTIME_LIGHT.gif'), 
  interval = 0.5,
  ani.width = width * 100,
  ani.height = height * 100)

## 2.2. plot (average) ----

### 2.2.1. lsoa ----

ntl.average.plot.lsoa <- 
  ntl.lsoa %>% 
  # filter for study period
  dplyr::filter(YEAR %in% 2002:2022) %>% 
  # average
  dplyr::summarise(nighttimeLight = nighttimeLight %>% mean(),
                   .by = 'LSOA11CD') %>% 
  make.plot.nighttimeLight(data = .,
                           spatial.level = 'LSOA11CD',
                           poly = poly.lsoa.england,
                           lookup = lookup.11 %>% dplyr::select(LSOA11CD, LAD11CD) %>% unique(),
                           title.legend = '')

setwd(dir.res.exploration)
# png
ggplot2::ggsave(plot = ntl.average.plot.lsoa,
                filename = 'png/lsoa/LSOA11_NIGHTTIME_LIGHT_AVERAGE.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = ntl.average.plot.lsoa,
                filename = 'eps/lsoa/LSOA11_NIGHTTIME_LIGHT_AVERAGE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)

### 2.2.2. msoa ----

ntl.average.plot.msoa <- 
  ntl.msoa %>% 
  # filter for study period
  dplyr::filter(YEAR %in% 2002:2022) %>% 
  # average
  dplyr::summarise(nighttimeLight = nighttimeLight %>% mean(),
                   .by = 'MSOA11CD') %>% 
  make.plot.nighttimeLight(data = .,
                           spatial.level = 'MSOA11CD',
                           poly = poly.msoa.england,
                           lookup = lookup.11 %>% dplyr::select(MSOA11CD, LAD11CD) %>% unique(),
                           title.legend = '')

setwd(dir.res.exploration)
# png
ggplot2::ggsave(plot = ntl.average.plot.msoa,
                filename = 'png/msoa/MSOA11_NIGHTTIME_LIGHT_AVERAGE.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = ntl.average.plot.msoa,
                filename = 'eps/msoa/MSOA11_NIGHTTIME_LIGHT_AVERAGE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)


### 2.2.3. lad ----

ntl.average.plot.lad <- 
  ntl.lad %>% 
  # filter for study period
  dplyr::filter(YEAR %in% 2002:2022) %>% 
  # average
  dplyr::summarise(nighttimeLight = nighttimeLight %>% mean(),
                   .by = 'LAD11CD') %>% 
  make.plot.nighttimeLight(data = .,
                           spatial.level = 'LAD11CD',
                           poly = poly.lad.england,
                           title.legend = '')

setwd(dir.res.exploration)
# png
ggplot2::ggsave(plot = ntl.average.plot.lad,
                filename = 'png/lad/LAD11_NIGHTTIME_LIGHT_AVERAGE.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = ntl.average.plot.lad,
                filename = 'eps/lad/LAD11_NIGHTTIME_LIGHT_AVERAGE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)

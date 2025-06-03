# 0. set up ----

## 0.1. packages ----

if(!require('tidyverse')) {
  install.packages('tidyverse', dep = TRUE)
}
if(!require('sf')) {
  install.packages('sf', dep = TRUE)
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
dir.masterData.railway <- paste0(dir.masterData, '/osmRailway')

## 0.3. functions ----

setwd(dir.code)
source('00_functions.R')

## 0.4. shape file ----

setwd(dir.masterData.spatial)

### 0.4.1. polygon ----

poly.lsoa <- sf::st_read(dsn = dir.masterData.spatial, layer = 'ONS11_LSOA')
poly.msoa <- sf::st_read(dsn = dir.masterData.spatial, layer = 'ONS11_MSOA')
poly.lad <- sf::st_read(dsn = dir.masterData.spatial, layer = 'ONS11_LAD')
poly.nat <- sf::st_read(dsn = dir.masterData.spatial, layer = 'ONS21_NAT')

poly.lsoa.england <- poly.lsoa %>%  dplyr::filter(str_detect(LSOA11CD, '^E'))
poly.msoa.england <- poly.msoa %>%  dplyr::filter(str_detect(MSOA11CD, '^E'))
poly.lad.england <- poly.lad %>%  dplyr::filter(str_detect(LAD11CD, '^E'))
poly.nat.england <- poly.nat %>%  dplyr::filter(str_detect(CTRY21CD, '^E'))

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

### 0.6.1. railways (download or load) ----

setwd(dir.masterData.railway)

# all railways
website.details.all <- read.csv('https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv')
railway.urls <- paste0('https://biogeo.ucdavis.edu/data/diva/rrd/', website.details.all$alpha.3, '_rrd.zip')

if(length(list.files(pattern = 'GBR_rails', full.names = TRUE)) == 0){
  
  # download files
  lapply(railway.urls, 
         function(x){ tryCatch({ download.file(url = x, destfile = basename(x)) }, error = function(e) { result <- NA }) })
  
  # unzip files
  railways.zipped <- list.files(pattern = '.zip')
  lapply(railways.zipped, 
         function(x){unzip(x); files.remove(x)})
  
}

railways.all <- 
  sf::st_read(dsn = dir.masterData.railway, layer = 'GBR_rails') %>%
  # transform CRS to that from ONS (load ONS librarys first)
  sf::st_transform(., crs = sf::st_crs(poly.nat)) %>% 
  dplyr::mutate(railType = dplyr::if_else(EXS_DESCRI == 'Operational', 'Operational', 'Unexamined'))

railways.england <- 
  sf::st_intersection(railways.all, poly.nat.england)

# 1. railway lines ----

## 1.1. lsoa ----

rail.length.lsoa <- 
  # intersection between rails and lsoa
  sf::st_intersection(railways.all, poly.lsoa.england) %>% 
  # length of the rails in each lsoa
  dplyr::mutate(Length_M = sf::st_length(geometry) %>% as.numeric(),
                Length_KM = sf::st_length(geometry) %>% units::set_units(., 'km') %>% as.numeric()) %>% 
  sf::st_drop_geometry() %>%
  group_by(LSOA11CD, railType) %>%
  summarise(
    Length_M = sum(Length_M),
    Length_KM = sum(Length_KM),
    .groups = 'drop') %>%
  bind_rows(.,
            group_by(., LSOA11CD) %>%
              summarise(Length_M = sum(Length_M),
                        Length_KM = sum(Length_KM),
                        .groups = 'drop') %>%
              mutate(railType = 'Total'))

area.lsoa <- 
  poly.lsoa.england %>% 
  dplyr::mutate(Area_M2 = sf::st_area(geometry) %>% as.numeric(),
                Area_KM2 = sf::st_area(geometry) %>% units::set_units(., 'km^2') %>% as.numeric()) %>% 
  dplyr::select(LSOA11CD, Area_M2, Area_KM2) %>% 
  sf::st_drop_geometry()

rail.density.lsoa <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              railType = c('Operational', 'Unexamined', 'Total')) %>% 
  dplyr::left_join(., area.lsoa, by = c('LSOA11CD')) %>% 
  dplyr::left_join(., rail.length.lsoa, by = c('LSOA11CD', 'railType')) %>% 
  dplyr::mutate(Length_M = dplyr::if_else(is.na(Length_M), 0, Length_M),
                Length_KM = dplyr::if_else(is.na(Length_KM), 0, Length_KM),
                railDensity_M = Length_M/Area_M2,
                railDensity_KM = Length_KM/Area_KM2) %>% 
  tidyr::pivot_wider(names_from = 'railType',
                     values_from = c('Area_M2', 'Area_KM2', 'Length_M', 'Length_KM', 'railDensity_M', 'railDensity_KM')) %>% 
  dplyr::mutate(operationalRail = railDensity_KM_Operational %>% as.numeric(),
                unexaminedRail = railDensity_KM_Unexamined %>% as.numeric(),
                totalRail = railDensity_KM_Total %>% as.numeric())

setwd(dir.data.organised)
save(rail.density.lsoa, file = 'lsoa/rda/LSOA11_RAIL_DENSITY.rda')
write.csv(rail.density.lsoa, file = 'lsoa/csv/LSOA11_RAIL_DENSITY.csv', row.names = FALSE)

## 1.2. msoa ----

rail.length.msoa <- 
  # intersection between rails and msoa
  sf::st_intersection(railways.all, poly.msoa.england) %>% 
  # length of the rails in each msoa
  dplyr::mutate(Length_M = sf::st_length(geometry) %>% as.numeric(),
                Length_KM = sf::st_length(geometry) %>% units::set_units(., 'km') %>% as.numeric()) %>% 
  sf::st_drop_geometry() %>%
  group_by(MSOA11CD, railType) %>%
  summarise(
    Length_M = sum(Length_M),
    Length_KM = sum(Length_KM),
    .groups = 'drop') %>%
  bind_rows(.,
            group_by(., MSOA11CD) %>%
              summarise(Length_M = sum(Length_M),
                        Length_KM = sum(Length_KM),
                        .groups = 'drop') %>%
              mutate(railType = 'Total'))

area.msoa <- 
  poly.msoa.england %>% 
  dplyr::mutate(Area_M2 = sf::st_area(geometry) %>% as.numeric(),
                Area_KM2 = sf::st_area(geometry) %>% units::set_units(., 'km^2') %>% as.numeric()) %>% 
  dplyr::select(MSOA11CD, Area_M2, Area_KM2) %>% 
  sf::st_drop_geometry()

rail.density.msoa <-
  expand.grid(MSOA11CD = poly.msoa.england$MSOA11CD,
              railType = c('Operational', 'Unexamined', 'Total')) %>% 
  dplyr::left_join(., area.msoa, by = c('MSOA11CD')) %>% 
  dplyr::left_join(., rail.length.msoa, by = c('MSOA11CD', 'railType')) %>% 
  dplyr::mutate(Length_M = dplyr::if_else(is.na(Length_M), 0, Length_M),
                Length_KM = dplyr::if_else(is.na(Length_KM), 0, Length_KM),
                railDensity_M = Length_M/Area_M2,
                railDensity_KM = Length_KM/Area_KM2) %>% 
  tidyr::pivot_wider(names_from = 'railType',
                     values_from = c('Area_M2', 'Area_KM2', 'Length_M', 'Length_KM', 'railDensity_M', 'railDensity_KM')) %>% 
  dplyr::mutate(operationalRail = railDensity_KM_Operational %>% as.numeric(),
                unexaminedRail = railDensity_KM_Unexamined %>% as.numeric(),
                totalRail = railDensity_KM_Total %>% as.numeric())

setwd(dir.data.organised)
save(rail.density.msoa, file = 'msoa/rda/MSOA11_RAIL_DENSITY.rda')
write.csv(rail.density.msoa, file = 'msoa/csv/MSOA11_RAIL_DENSITY.csv', row.names = FALSE)

## 1.3. lad ----

rail.length.lad <- 
  # intersection between rails and lad
  sf::st_intersection(railways.all, poly.lad.england) %>% 
  # length of the rails in each lad
  dplyr::mutate(Length_M = sf::st_length(geometry) %>% as.numeric(),
                Length_KM = sf::st_length(geometry) %>% units::set_units(., 'km') %>% as.numeric()) %>% 
  sf::st_drop_geometry() %>%
  group_by(LAD11CD, railType) %>%
  summarise(
    Length_M = sum(Length_M),
    Length_KM = sum(Length_KM),
    .groups = 'drop') %>%
  bind_rows(.,
            group_by(., LAD11CD) %>%
              summarise(Length_M = sum(Length_M),
                        Length_KM = sum(Length_KM),
                        .groups = 'drop') %>%
              mutate(railType = 'Total'))

area.lad <- 
  poly.lad.england %>% 
  dplyr::mutate(Area_M2 = sf::st_area(geometry) %>% as.numeric(),
                Area_KM2 = sf::st_area(geometry) %>% units::set_units(., 'km^2') %>% as.numeric()) %>% 
  dplyr::select(LAD11CD, Area_M2, Area_KM2) %>% 
  sf::st_drop_geometry()

rail.density.lad <-
  expand.grid(LAD11CD = poly.lad.england$LAD11CD,
              railType = c('Operational', 'Unexamined', 'Total')) %>% 
  dplyr::left_join(., area.lad, by = c('LAD11CD')) %>% 
  dplyr::left_join(., rail.length.lad, by = c('LAD11CD', 'railType')) %>% 
  dplyr::mutate(Length_M = dplyr::if_else(is.na(Length_M), 0, Length_M),
                Length_KM = dplyr::if_else(is.na(Length_KM), 0, Length_KM),
                railDensity_M = Length_M/Area_M2,
                railDensity_KM = Length_KM/Area_KM2) %>% 
  tidyr::pivot_wider(names_from = 'railType',
                     values_from = c('Area_M2', 'Area_KM2', 'Length_M', 'Length_KM', 'railDensity_M', 'railDensity_KM')) %>% 
  dplyr::mutate(operationalRail = railDensity_KM_Operational %>% as.numeric(),
                unexaminedRail = railDensity_KM_Unexamined %>% as.numeric(),
                totalRail = railDensity_KM_Total %>% as.numeric())

setwd(dir.data.organised)
save(rail.density.lad, file = 'lad/rda/LAD11_RAIL_DENSITY.rda')
write.csv(rail.density.lad, file = 'lad/csv/LAD11_RAIL_DENSITY.csv', row.names = FALSE)

# 2. figures ----

## 2.0. function ----

make.plot.railway <- function(rail.type, 
                              number.type = 3, 
                              data, 
                              spatial.level = NULL, 
                              poly, 
                              lookup = NULL,
                              title.legend = 'Railway density (per $KM^{2}$)'){
  
  # 0. function arguments ----
  
  # rail.type <- c('operational', 'unexamined', 'total')[3]
  # data <- rail.density.msoa
  # spatial.level <- c('LSOA11CD', 'MSOA11CD', 'LAD11CD')[2]
  # poly <- poly.msoa.england
  # lookup <- lookup.11.21.msoa
  
  # 1. filter and limits ----
  
  rail.type.name <- paste0(rail.type, 'Rail')
  
  if(number.type == 1){rail.type <- NULL}
  
  if(spatial.level == 'LSOA11CD'){
    limits.spatial <- c(0, 10)
  } else if (spatial.level == 'MSOA11CD') {
    limits.spatial <- c(0, 4)
  } else {
    limits.spatial <- c(0, 1.5)
  }
  breaks.spatial <- seq(from = limits.spatial[1] , to = limits.spatial[2], by = limits.spatial[2]/4)
  
  # 2. data sort ----
  
  plot.data <-
    data %>% 
    dplyr::mutate(railType = !!rlang::parse_expr(rail.type.name)) %>% 
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
    ggplot2::geom_sf(data = plot.data.london, aes(fill = railType), colour = NA) +
    ggplot2::scale_fill_gradient(name = '',
                                 low = colours.sequential[1], high = colours.sequential[2],
                                 limits = limits.spatial, breaks = breaks.spatial) +
    my.map.theme(legend.position = 'none')
  
  plot <- 
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = plot.data, aes(fill = railType), colour = NA) +
    ggplot2::scale_fill_gradient(name = unname(latex2exp ::TeX(c(title.legend))),
                                 guide = guide_colourbar(title.position = 'top'),
                                 low = colours.sequential[1], high = colours.sequential[2],
                                 limits = limits.spatial, breaks = breaks.spatial) +
    # rail type label in top right
    ggplot2::annotate('text', x = Inf, y = Inf, label = stringr::str_to_title(rail.type), vjust = 1, hjust = 1, color = 'grey', size = 7.5) +
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

## 2.1 gif ----

### 2.1.1. lsoa ----

railway.lsoa.plots <- lapply(c('operational', 'unexamined', 'total'), 
                             make.plot.railway, 
                             data = rail.density.lsoa,
                             spatial.level = 'LSOA11CD',
                             poly = poly.lsoa.england, 
                             lookup = lookup.11 %>% dplyr::select(LSOA11CD, LAD11CD) %>% unique())

# save the ggplots as a gif
setwd(dir.res.exploration)
animation::saveGIF(
  expr = {for (i in 1:length(railway.lsoa.plots)) { print(railway.lsoa.plots[[i]]) } }, 
  movie.name = paste0(dir.res.exploration, '/gif/lsoa/LSOA11_RAIL_DENSITY.gif'), 
  interval = 0.5,
  ani.width = width * 100,
  ani.height = height * 100)

### 2.1.2. msoa ----

railway.msoa.plots <- lapply(c('operational', 'unexamined', 'total'), 
                             make.plot.railway, 
                             data = rail.density.msoa,
                             spatial.level = 'MSOA11CD',
                             poly = poly.msoa.england, 
                             lookup = lookup.11 %>% dplyr::select(MSOA11CD, LAD11CD) %>% unique())

# save the ggplots as a gif
setwd(dir.res.exploration)
animation::saveGIF(
  expr = {for (i in 1:length(railway.msoa.plots)) { print(railway.msoa.plots[[i]]) } }, 
  movie.name = paste0(dir.res.exploration, '/gif/msoa/MSOA11_RAIL_DENSITY.gif'), 
  interval = 0.5,
  ani.width = width * 100,
  ani.height = height * 100)

### 2.1.2. lad ----

railway.lad.plots <- lapply(c('operational', 'unexamined', 'total'), 
                             make.plot.railway, 
                             data = rail.density.lad,
                             spatial.level = 'LAD11CD',
                             poly = poly.lad.england)

# save the ggplots as a gif
setwd(dir.res.exploration)
animation::saveGIF(
  expr = {for (i in 1:length(railway.lad.plots)) { print(railway.lad.plots[[i]]) } }, 
  movie.name = paste0(dir.res.exploration, '/gif/lad/LAD11_RAIL_DENSITY.gif'), 
  interval = 0.5,
  ani.width = width * 100,
  ani.height = height * 100)

## 2.2. plot (average) ----

### 2.2.1. lsoa ----

railway.average.plot.lsoa <- 
  rail.density.lsoa %>% 
  make.plot.railway(data = .,
                    rail.type = 'total',
                    number.type = 1,
                    spatial.level = 'LSOA11CD',
                    poly = poly.lsoa.england,
                    lookup = lookup.11 %>% dplyr::select(LSOA11CD, LAD11CD) %>% unique(),
                    title.legend = '')

setwd(dir.res.exploration)
# png
ggplot2::ggsave(plot = railway.average.plot.lsoa,
                filename = 'png/lsoa/LSOA11_RAIL_DENSITY_AVERAGE.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = railway.average.plot.lsoa,
                filename = 'eps/lsoa/LSOA11_RAIL_DENSITY_AVERAGE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)

### 2.2.2. msoa ----

railway.average.plot.msoa <- 
  rail.density.msoa %>% 
  make.plot.railway(data = .,
                    rail.type = 'total',
                    number.type = 1,
                    spatial.level = 'MSOA11CD',
                    poly = poly.msoa.england,
                    lookup = lookup.11 %>% dplyr::select(MSOA11CD, LAD11CD) %>% unique(),
                    title.legend = '')

setwd(dir.res.exploration)
# png
ggplot2::ggsave(plot = railway.average.plot.msoa,
                filename = 'png/msoa/MSOA11_RAIL_DENSITY_AVERAGE.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = railway.average.plot.msoa,
                filename = 'eps/msoa/MSOA11_RAIL_DENSITY_AVERAGE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)

### 2.2.3. lad ----

railway.average.plot.lad <- 
  rail.density.lad %>% 
  make.plot.railway(data = .,
                    rail.type = 'total',
                    number.type = 1,
                    spatial.level = 'LAD11CD',
                    poly = poly.lad.england,
                    title.legend = '')

setwd(dir.res.exploration)
# png
ggplot2::ggsave(plot = railway.average.plot.lad,
                filename = 'png/lad/LAD11_RAIL_DENSITY_AVERAGE.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = railway.average.plot.lad,
                filename = 'eps/lad/LAD11_RAIL_DENSITY_AVERAGE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)


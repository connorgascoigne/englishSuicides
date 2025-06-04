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
dir.masterData.road <- paste0(dir.masterData, '/osmRoad')

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

### 0.6.1. roads load ----

setwd(dir.masterData.road)

all.files <- 
  list.files() %>% 
  tools::file_path_sans_ext() %>% 
  unique()

all.files.chunk <- all.files %>% sub(pattern = '_.*', replacement = '', x = .) %>% unique()
all.files.feature <- all.files %>% sub(pattern = '.*_', replacement = '', x = .) %>% unique()

load.road.network <- function(dsn, chunk, feature, crs){
  
  layer <- paste(chunk, feature, sep = '_')
  
  road <- 
    sf::st_read(dsn = dsn, layer = layer) %>% 
    sf::st_transform(., crs = crs)
  
  return(road)
  
}

all.roads <- 
  lapply(X = all.files.chunk, FUN = load.road.network, dsn = dir.masterData.road, feature = 'RoadLink', crs = sf::st_crs(poly.nat)) %>%
  dplyr::bind_rows() %>% 
  dplyr::mutate(roadType = dplyr::if_else(class %in% c('Motorway', 'A Road'), 'Major', 'Minor'))

# 1. road density ----

## 1.1. lsoa ----

road.length.lsoa <- 
  # intersection between roads and lsoa
  sf::st_intersection(all.roads, poly.lsoa.england) %>% 
  # length of the roads in each lsoa
  dplyr::mutate(Length_M = sf::st_length(geometry) %>% as.numeric(),
                Length_KM = sf::st_length(geometry) %>% units::set_units(., 'km') %>% as.numeric()) %>% 
  sf::st_drop_geometry() %>%
  group_by(LSOA11CD, roadType) %>%
  summarise(
    Length_M = sum(Length_M),
    Length_KM = sum(Length_KM),
    .groups = 'drop') %>%
  bind_rows(.,
            group_by(., LSOA11CD) %>%
              summarise(Length_M = sum(Length_M),
                        Length_KM = sum(Length_KM),
                        .groups = 'drop') %>%
              mutate(roadType = 'Total'))

area.lsoa <- 
  poly.lsoa.england %>% 
  dplyr::mutate(Area_M2 = sf::st_area(geometry) %>% as.numeric(),
                Area_KM2 = sf::st_area(geometry) %>% units::set_units(., 'km^2') %>% as.numeric()) %>% 
  dplyr::select(LSOA11CD, Area_M2, Area_KM2) %>% 
  sf::st_drop_geometry()

road.density.lsoa <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              roadType = c('Minor', 'Major', 'Total')) %>% 
  dplyr::left_join(., area.lsoa, by = c('LSOA11CD')) %>% 
  dplyr::left_join(., road.length.lsoa, by = c('LSOA11CD', 'roadType')) %>% 
  dplyr::mutate(Length_M = dplyr::if_else(is.na(Length_M), 0, Length_M),
                Length_KM = dplyr::if_else(is.na(Length_KM), 0, Length_KM),
                roadDensity_M = Length_M/Area_M2,
                roadDensity_KM = Length_KM/Area_KM2) %>% 
  tidyr::pivot_wider(names_from = 'roadType',
                     values_from = c('Area_M2', 'Area_KM2', 'Length_M', 'Length_KM', 'roadDensity_M', 'roadDensity_KM')) %>% 
  dplyr::mutate(minorRoad = roadDensity_KM_Minor %>% as.numeric(),
                majorRoad = roadDensity_KM_Major %>% as.numeric(),
                totalRoad = roadDensity_KM_Total %>% as.numeric())

setwd(dir.data.organised)
save(road.density.lsoa, file = 'lsoa/rda/LSOA11_ROAD_DENSITY.rda')
write.csv(road.density.lsoa, file = 'lsoa/csv/LSOA11_ROAD_DENSITY.csv', row.names = FALSE)

## 1.2. msoa ----

road.length.msoa <- 
  # intersection between roads and msoa
  sf::st_intersection(all.roads, poly.msoa.england) %>% 
  # length of the roads in each msoa
  dplyr::mutate(Length_M = sf::st_length(geometry) %>% as.numeric(),
                Length_KM = sf::st_length(geometry) %>% units::set_units(., 'km') %>% as.numeric()) %>% 
  sf::st_drop_geometry() %>%
  group_by(MSOA11CD, roadType) %>%
  summarise(
    Length_M = sum(Length_M),
    Length_KM = sum(Length_KM),
    .groups = 'drop') %>%
  bind_rows(.,
            group_by(., MSOA11CD) %>%
              summarise(Length_M = sum(Length_M),
                        Length_KM = sum(Length_KM),
                        .groups = 'drop') %>%
              mutate(roadType = 'Total'))

area.msoa <- 
  poly.msoa.england %>% 
  dplyr::mutate(Area_M2 = sf::st_area(geometry) %>% as.numeric(),
                Area_KM2 = sf::st_area(geometry) %>% units::set_units(., 'km^2') %>% as.numeric()) %>% 
  dplyr::select(MSOA11CD, Area_M2, Area_KM2) %>% 
  sf::st_drop_geometry()

road.density.msoa <-
  expand.grid(MSOA11CD = poly.msoa.england$MSOA11CD,
              roadType = c('Minor', 'Major', 'Total')) %>% 
  dplyr::left_join(., area.msoa, by = c('MSOA11CD')) %>% 
  dplyr::left_join(., road.length.msoa, by = c('MSOA11CD', 'roadType')) %>% 
  dplyr::mutate(Length_M = dplyr::if_else(is.na(Length_M), 0, Length_M),
                Length_KM = dplyr::if_else(is.na(Length_KM), 0, Length_KM),
                roadDensity_M = Length_M/Area_M2,
                roadDensity_KM = Length_KM/Area_KM2) %>% 
  tidyr::pivot_wider(names_from = 'roadType',
                     values_from = c('Area_M2', 'Area_KM2', 'Length_M', 'Length_KM', 'roadDensity_M', 'roadDensity_KM')) %>% 
  dplyr::mutate(minorRoad = roadDensity_KM_Minor %>% as.numeric(),
                majorRoad = roadDensity_KM_Major %>% as.numeric(),
                totalRoad = roadDensity_KM_Total %>% as.numeric())

setwd(dir.data.organised)
save(road.density.msoa, file = 'msoa/rda/MSOA11_ROAD_DENSITY.rda')
write.csv(road.density.msoa, file = 'msoa/csv/MSOA11_ROAD_DENSITY.csv', row.names = FALSE)

## 1.3. lad ----

road.length.lad <- 
  # intersection between roads and lad
  sf::st_intersection(all.roads, poly.lad.england) %>% 
  # length of the roads in each lad
  dplyr::mutate(Length_M = sf::st_length(geometry) %>% as.numeric(),
                Length_KM = sf::st_length(geometry) %>% units::set_units(., 'km') %>% as.numeric()) %>% 
  sf::st_drop_geometry() %>%
  group_by(LAD11CD, roadType) %>%
  summarise(
    Length_M = sum(Length_M),
    Length_KM = sum(Length_KM),
    .groups = 'drop') %>%
  bind_rows(.,
            group_by(., LAD11CD) %>%
              summarise(Length_M = sum(Length_M),
                        Length_KM = sum(Length_KM),
                        .groups = 'drop') %>%
              mutate(roadType = 'Total'))

area.lad <- 
  poly.lad.england %>% 
  dplyr::mutate(Area_M2 = sf::st_area(geometry) %>% as.numeric(),
                Area_KM2 = sf::st_area(geometry) %>% units::set_units(., 'km^2') %>% as.numeric()) %>% 
  dplyr::select(LAD11CD, Area_M2, Area_KM2) %>% 
  sf::st_drop_geometry()

road.density.lad <-
  expand.grid(LAD11CD = poly.lad.england$LAD11CD,
              roadType = c('Minor', 'Major', 'Total')) %>% 
  dplyr::left_join(., area.lad, by = c('LAD11CD')) %>% 
  dplyr::left_join(., road.length.lad, by = c('LAD11CD', 'roadType')) %>% 
  dplyr::mutate(Length_M = dplyr::if_else(is.na(Length_M), 0, Length_M),
                Length_KM = dplyr::if_else(is.na(Length_KM), 0, Length_KM),
                roadDensity_M = Length_M/Area_M2,
                roadDensity_KM = Length_KM/Area_KM2) %>% 
  tidyr::pivot_wider(names_from = 'roadType',
                     values_from = c('Area_M2', 'Area_KM2', 'Length_M', 'Length_KM', 'roadDensity_M', 'roadDensity_KM')) %>% 
  dplyr::mutate(minorRoad = roadDensity_KM_Minor %>% as.numeric(),
                majorRoad = roadDensity_KM_Major %>% as.numeric(),
                totalRoad = roadDensity_KM_Total %>% as.numeric())

setwd(dir.data.organised)
save(road.density.lad, file = 'lad/rda/LAD11_ROAD_DENSITY.rda')
write.csv(road.density.lad, file = 'lad/csv/LAD11_ROAD_DENSITY.csv', row.names = FALSE)

# 2. figures ----

## 2.0. function ----

make.plot.road <- function(road.type, 
                           number.type = 3, 
                           data, 
                           spatial.level = NULL, 
                           poly, 
                           lookup = NULL,
                           title.legend = 'Road density (per $KM^{2}$)'){
  
  # 0. function arguments ----
  
  # road.type <- c('minor', 'major', 'total')[3]
  # data <- road.density.msoa
  # spatial.level <- c('LSOA11CD', 'MSOA11CD', 'LAD11CD')[2]
  # poly <- poly.msoa.england
  # lookup <- lookup.11.21.msoa
  
  # 1. filter and limits ----
  
  road.type.name <- paste0(road.type, 'Road')
  if(number.type == 1){road.type <- NULL}
  
  if(spatial.level == 'LSOA11CD'){
    limits.spatial <- c(0, 60)
  } else if (spatial.level == 'MSOA11CD') {
    limits.spatial <- c(0, 40)
  } else {
    limits.spatial <- c(0, 20)
  }
  breaks.spatial <- seq(from = limits.spatial[1] , to = limits.spatial[2], by = limits.spatial[2]/4)
  
  # 2. data sort ----
  
  plot.data <-
    data %>% 
    dplyr::mutate(roadType = !!rlang::parse_expr(road.type.name)) %>% 
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
  
  # 3. plots ----
  
  plot.london <-
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = plot.data.london, aes(fill = roadType), colour = NA) +
    ggplot2::scale_fill_gradient(name = '',
                                 low = colours.sequential[1], high = colours.sequential[2],
                                 limits = limits.spatial, breaks = breaks.spatial) +
    my.map.theme(legend.position = 'none')
  
  plot <- 
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = plot.data, aes(fill = roadType), colour = NA) +
    ggplot2::scale_fill_gradient(name = unname(latex2exp ::TeX(c(title.legend))),
                                 guide = guide_colourbar(title.position = 'top'),
                                 low = colours.sequential[1], high = colours.sequential[2],
                                 limits = limits.spatial, breaks = breaks.spatial) +
    # road type label in top right
    ggplot2::annotate('text', x = Inf, y = Inf, label = stringr::str_to_title(road.type), vjust = 1, hjust = 1, color = 'grey', size = 7.5) +
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

## 2.1. gif ----

### 2.1.1. lsoa ----

road.lsoa.plots <- lapply(c('minor', 'major', 'total'), 
                          make.plot.road, 
                          data = road.density.lsoa,
                          spatial.level = 'LSOA11CD',
                          poly = poly.lsoa.england, 
                          lookup = lookup.11 %>% dplyr::select(LSOA11CD, LAD11CD) %>% unique())

# save the ggplots as a gif
setwd(dir.res.exploration)
animation::saveGIF(
  expr = {for (i in 1:length(road.lsoa.plots)) { print(road.lsoa.plots[[i]]) } }, 
  movie.name = paste0(dir.res.exploration, '/gif/lsoa/LSOA11_ROAD_DENSITY.gif'), 
  interval = 0.5,
  ani.width = width * 100,
  ani.height = height * 100)

### 2.1.2. msoa ----

road.msoa.plots <- lapply(c('minor', 'major', 'total'), 
                          make.plot.road, 
                          data = road.density.msoa,
                          spatial.level = 'MSOA11CD',
                          poly = poly.msoa.england, 
                          lookup = lookup.11 %>% dplyr::select(MSOA11CD, LAD11CD) %>% unique())

# save the ggplots as a gif
setwd(dir.res.exploration)
animation::saveGIF(
  expr = {for (i in 1:length(road.msoa.plots)) { print(road.msoa.plots[[i]]) } }, 
  movie.name = paste0(dir.res.exploration, '/gif/msoa/MSOA11_ROAD_DENSITY.gif'), 
  interval = 0.5,
  ani.width = width * 100,
  ani.height = height * 100)

### 2.1.3. lad ----

road.lad.plots <- lapply(c('minor', 'major', 'total'), 
                          make.plot.road, 
                          data = road.density.lad,
                          spatial.level = 'LAD11CD',
                          poly = poly.lad.england)

# save the ggplots as a gif
setwd(dir.res.exploration)
animation::saveGIF(
  expr = {for (i in 1:length(road.lad.plots)) { print(road.lad.plots[[i]]) } }, 
  movie.name = paste0(dir.res.exploration, '/gif/lad/LAD11_ROAD_DENSITY.gif'), 
  interval = 0.5,
  ani.width = width * 100,
  ani.height = height * 100)

## 2.2. plot (average) ----

### 2.2.1. lsoa ----

road.density.average.plot.lsoa <- 
  road.density.lsoa %>% 
  make.plot.road(data = .,
                 road.type = 'total',
                 number.type = 1,
                 spatial.level = 'LSOA11CD',
                 poly = poly.lsoa.england,
                 lookup = lookup.11 %>% dplyr::select(LSOA11CD, LAD11CD) %>% unique(),
                 title.legend = '')

setwd(dir.res.exploration)
# png
ggplot2::ggsave(plot = road.density.average.plot.lsoa,
                filename = 'png/lsoa/LSOA11_ROAD_DENSITY_AVERAGE.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = road.density.average.plot.lsoa,
                filename = 'eps/lsoa/LSOA11_ROAD_DENSITY_AVERAGE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)

### 2.2.2. msoa ----

road.density.average.plot.msoa <- 
  road.density.msoa %>% 
  make.plot.road(data = .,
                 road.type = 'total',
                 number.type = 1,
                 spatial.level = 'MSOA11CD',
                 poly = poly.msoa.england,
                 lookup = lookup.11 %>% dplyr::select(MSOA11CD, LAD11CD) %>% unique(),
                 title.legend = '')

setwd(dir.res.exploration)
# png
ggplot2::ggsave(plot = road.density.average.plot.msoa,
                filename = 'png/msoa/MSOA11_ROAD_DENSITY_AVERAGE.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = road.density.average.plot.msoa,
                filename = 'eps/msoa/MSOA11_ROAD_DENSITY_AVERAGE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)

### 2.2.3. lad ----

road.density.average.plot.lad <- 
  road.density.lad %>% 
  make.plot.road(data = .,
                 road.type = 'total',
                 number.type = 1,
                 spatial.level = 'LAD11CD',
                 poly = poly.lad.england,
                 title.legend = '')

setwd(dir.res.exploration)
# png
ggplot2::ggsave(plot = road.density.average.plot.lad,
                filename = 'png/lad/LAD11_ROAD_DENSITY_AVERAGE.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = road.density.average.plot.lad,
                filename = 'eps/lad/LAD11_ROAD_DENSITY_AVERAGE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)
# 0. set up ----

## 0.1. packages ----

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

## 0.4.2. lookup file ----

lookup.01.11 <- read.csv('LSOA01_LSOA11_LAD11_lookUp.csv')
lookup.11.21 <- read.csv('LSOA11_LSOA21_LAD22_lookUp.csv')
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

### 0.6.1. population totals ----

setwd(dir.data.organised)
population.lsoa <- read.csv(file = 'lsoa/csv/LSOA11_POPULATION_TOTAL.csv')
population.msoa <- read.csv(file = 'msoa/csv/MSOA11_POPULATION_TOTAL.csv')
population.lad <- read.csv(file = 'lad/csv/LAD11_POPULATION_TOTAL.csv')

# 1. population density ----

## 1.1 lsoa ----

area.lsoa <- 
  poly.lsoa.england %>% 
  dplyr::mutate(Area_M2 = sf::st_area(geometry) %>% as.numeric(),
                Area_KM2 = sf::st_area(geometry) %>% units::set_units(., 'km^2') %>% as.numeric()) %>% 
  dplyr::select(LSOA11CD, Area_M2, Area_KM2) %>% 
  sf::st_drop_geometry()

pd.lsoa <-
  population.lsoa %>% 
  dplyr::summarise(POPULATION = sum(POPULATION),
                   .by = c('LSOA11CD', 'YEAR')) %>% 
  dplyr::left_join(., 
                   area.lsoa, 
                   by = 'LSOA11CD') %>% 
  dplyr::mutate(populationDensity = POPULATION/Area_KM2)

setwd(dir.data.organised)
save(pd.lsoa, file = 'lsoa/rda/LSOA11_POPULATION_DENSITY.rda')
write.csv(pd.lsoa, file = 'lsoa/csv/LSOA11_POPULATION_DENSITY.csv', row.names = FALSE)

## 1.2. msoa ----

area.msoa <- 
  poly.msoa.england %>% 
  dplyr::mutate(Area_M2 = sf::st_area(geometry) %>% as.numeric(),
                Area_KM2 = sf::st_area(geometry) %>% units::set_units(., 'km^2') %>% as.numeric()) %>% 
  dplyr::select(MSOA11CD, Area_M2, Area_KM2) %>% 
  sf::st_drop_geometry()

pd.msoa <-
  population.msoa %>%
  dplyr::summarise(POPULATION = sum(POPULATION),
                   .by = c('MSOA11CD', 'YEAR')) %>% 
  dplyr::left_join(., 
                   area.msoa, 
                   by = 'MSOA11CD') %>% 
  dplyr::mutate(populationDensity = POPULATION/Area_KM2) 

setwd(dir.data.organised)
save(pd.msoa, file = 'msoa/rda/MSOA11_POPULATION_DENSITY.rda')
write.csv(pd.msoa, file = 'msoa/csv/MSOA11_POPULATION_DENSITY.csv', row.names = FALSE)

## 1.3. lad ----

area.lad <- 
  poly.lad.england %>% 
  dplyr::mutate(Area_M2 = sf::st_area(geometry) %>% as.numeric(),
                Area_KM2 = sf::st_area(geometry) %>% units::set_units(., 'km^2') %>% as.numeric()) %>% 
  dplyr::select(LAD11CD, Area_M2, Area_KM2) %>% 
  sf::st_drop_geometry()

pd.lad <-
  population.lad %>%
  dplyr::summarise(POPULATION = sum(POPULATION),
                   .by = c('LAD11CD', 'YEAR')) %>% 
  dplyr::left_join(., 
                   area.lad, 
                   by = 'LAD11CD') %>% 
  dplyr::mutate(populationDensity = POPULATION/Area_KM2) 


setwd(dir.data.organised)
save(pd.lad, file = 'lad/rda/LAD11_POPULATION_DENSITY.rda')
write.csv(pd.lad, file = 'lad/csv/LAD11_POPULATION_DENSITY.csv', row.names = FALSE)

# 2. figure ----

## 2.0. function ----

make.plot.populationDensity <- function(year = NULL, 
                                        data, 
                                        spatial.level = NULL, 
                                        poly, 
                                        lookup = NULL,
                                        title.legend = 'Population density (per $KM^{2}$)'){
  
  # 0. function arguments ----
  
  # year <- 2002
  # data <- pd.lsoa
  # spatial.level <- c('LSOA11CD', 'MSOA11CD', 'LAD11CD')[2]
  # poly <- poly.lsoa.england %>% sf::st_simplify(., preserveTopology = TRUE, dTolerance = 1000)
  # lookup <- lookup.11.21.lsoa
  
  # 1. filter and limits ----
  
  if(spatial.level == 'LSOA11CD'){
    limits.spatial <- c(0, 120000)
  } else if (spatial.level == 'MSOA11CD') {
    limits.spatial <- c(0, 32000)
  } else {
    limits.spatial <- c(0, 20000)
  }
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
  
  ## plot
  plot.london <-
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = plot.data.london, aes(fill = populationDensity), colour = NA) +
    ggplot2::scale_fill_gradient(name = '',
                                 low = colours.sequential[1], high = colours.sequential[2],
                                 limits = limits.spatial, breaks = breaks.spatial) +
    my.map.theme(legend.position = 'none')
  
  # final plot
  plot <- 
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = plot.data, aes(fill = populationDensity), colour = NA) +
    ggplot2::scale_fill_gradient(name = unname(latex2exp ::TeX(c(title.legend))),
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

## 2.1 gif ----

### 2.1.1. lsoa ----

pd.lsoa.plots <- lapply(X = 2002:2022, 
                        make.plot.populationDensity, 
                        data = pd.lsoa,
                        spatial.level = 'LSOA11CD',
                        poly = poly.lsoa.england, 
                        lookup = lookup.11 %>% dplyr::select(LSOA11CD, LAD11CD) %>% unique())

# save the ggplots as a gif
setwd(dir.res.exploration)
animation::saveGIF(
  expr = {for (i in 1:length(pd.lsoa.plots)) { print(pd.lsoa.plots[[i]]) } }, 
  movie.name = paste0(dir.res.exploration, '/gif/lsoa/LSOA11_POPULATION_DENSITY.gif'), 
  interval = 0.5,
  ani.width = width * 100,
  ani.height = height * 100)

### 2.1.2. msoa ----

pd.msoa.plots <- lapply(X = 2002:2022, 
                        make.plot.populationDensity, 
                        data = pd.msoa,
                        spatial.level = 'MSOA11CD',
                        poly = poly.msoa.england, 
                        lookup = lookup.11 %>% dplyr::select(MSOA11CD, LAD11CD) %>% unique())

# save the ggplots as a gif
setwd(dir.res.exploration)
animation::saveGIF(
  expr = {for (i in 1:length(pd.msoa.plots)) { print(pd.msoa.plots[[i]]) } }, 
  movie.name = paste0(dir.res.exploration, '/gif/msoa/MSOA11_POPULATION_DENSITY.gif'), 
  interval = 0.5,
  ani.width = width * 100,
  ani.height = height * 100)

### 2.1.3. lad ----

pd.lad.plots <- lapply(X = 2002:2022, 
                       make.plot.populationDensity, 
                       data = pd.lad,
                       spatial.level = 'LAD11CD',
                       poly = poly.lad.england)

# save the ggplots as a gif
setwd(dir.res.exploration)
animation::saveGIF(
  expr = {for (i in 1:length(pd.lad.plots)) { print(pd.lad.plots[[i]]) } }, 
  movie.name = paste0(dir.res.exploration, '/gif/lad/LAD11_POPULATION_DENSITY.gif'), 
  interval = 0.5,
  ani.width = width * 100,
  ani.height = height * 100)

## 2.2. plot (average) ----

### 2.2.1. lsoa ----

pd.average.plot.lsoa <- 
  pd.lsoa %>% 
  # average
  dplyr::summarise(populationDensity = populationDensity %>% mean(),
                   .by = 'LSOA11CD') %>% 
  make.plot.populationDensity(data = .,
                              spatial.level = 'LSOA11CD',
                              poly = poly.lsoa.england,
                              lookup = lookup.11 %>% dplyr::select(LSOA11CD, LAD11CD) %>% unique(),
                              title.legend = '')

setwd(dir.res.exploration)
# png
ggplot2::ggsave(plot = pd.average.plot.lsoa,
                filename = 'png/lsoa/LSOA11_POPULATION_DENSITY_AVERAGE.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = pd.average.plot.lsoa,
                filename = 'eps/lsoa/LSOA11_POPULATION_DENSITY_AVERAGE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)

### 2.2.2. msoa ----

pd.average.plot.msoa <- 
  pd.msoa %>% 
  # average
  dplyr::summarise(populationDensity = populationDensity %>% mean(),
                   .by = 'MSOA11CD') %>% 
  make.plot.populationDensity(data = .,
                              spatial.level = 'MSOA11CD',
                              poly = poly.msoa.england,
                              lookup = lookup.11 %>% dplyr::select(MSOA11CD, LAD11CD) %>% unique(),
                              title.legend = '')

setwd(dir.res.exploration)
# png
ggplot2::ggsave(plot = pd.average.plot.msoa,
                filename = 'png/msoa/MSOA11_POPULATION_DENSITY_AVERAGE.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = pd.average.plot.msoa,
                filename = 'eps/msoa/MSOA11_POPULATION_DENSITY_AVERAGE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)

### 2.2.3. lad ----

pd.average.plot.lad <- 
  pd.lad %>% 
  # average
  dplyr::summarise(populationDensity = populationDensity %>% mean(),
                   .by = 'LAD11CD') %>% 
  make.plot.populationDensity(data = .,
                              spatial.level = 'LAD11CD',
                              poly = poly.lad.england,
                              lookup = lookup.11 %>% dplyr::select(LAD11CD, LAD11CD) %>% unique(),
                              title.legend = '')

setwd(dir.res.exploration)
# png
ggplot2::ggsave(plot = pd.average.plot.lad,
                filename = 'png/lad/LAD11_POPULATION_DENSITY_AVERAGE.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = pd.average.plot.lad,
                filename = 'eps/lad/LAD11_POPULATION_DENSITY_AVERAGE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)
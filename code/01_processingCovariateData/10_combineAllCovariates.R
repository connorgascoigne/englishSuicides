# 0. set up ----

## 0.1. librarys ----

if(!require('tidyverse')) {
  install.packages('tidyverse', dep = TRUE)
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

## 0.6. import ----

### 0.6.1. socio-environmental ----

setwd(dir.data.organised)
files.all <- c(list.files(path = 'lsoa/rda', full.names = TRUE), 
               list.files(path = 'msoa/rda', full.names = TRUE), 
               list.files(path = 'lad/rda', full.names = TRUE))

print(files.all[grepl("_16DAY", files.all)])

invisible(
  lapply(X = files.all[!grepl("_16DAY", files.all)],
       FUN = function(x){ 
         cat(paste0('Opening: ', basename(x), '\n'))
         load(file = x, envir = .GlobalEnv) 
         })
)

# 1. link variables ----

## 1.0. labels ----

min.year <- 2002
max.year <- 2022

## 1.1. lsoa ----

covariates.lsoa <- 
  tidyr::expand_grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
                     YEAR = min.year:max.year) %>% 
  dplyr::left_join(., 
                   imd.lsoa %>% dplyr::select(LSOA11CD, YEAR, imdScore, incomeScore, employmentScore, healthScore, educationScore, housingScore, crimeScore, environmentScore), 
                   by = c('LSOA11CD', 'YEAR')) %>% 
  dplyr::left_join(., 
                   ed.lsoa %>% dplyr::select(LSOA11CD, YEAR, nonWhite), 
                   by = c('LSOA11CD', 'YEAR')) %>% 
  dplyr::left_join(., 
                   pd.lsoa %>% dplyr::select(LSOA11CD, YEAR, populationDensity), 
                   by = c('LSOA11CD', 'YEAR')) %>% 
  dplyr::left_join(., 
                   ntl.lsoa %>% dplyr::select(LSOA11CD, YEAR, nighttimeLight), 
                   by = c('LSOA11CD', 'YEAR')) %>% 
  dplyr::left_join(., 
                   rail.density.lsoa %>% dplyr::select(LSOA11CD, totalRail), 
                   by = c('LSOA11CD')) %>% 
  dplyr::left_join(., 
                   road.density.lsoa %>% dplyr::select(LSOA11CD, totalRoad), 
                   by = c('LSOA11CD')) %>% 
  dplyr::left_join(., 
                   ndvi.lsoa %>% dplyr::select(LSOA11CD, YEAR, ndvi), 
                   by = c('LSOA11CD', 'YEAR'))

covariates.lsoa %>% is.na() %>% sum()

setwd(dir.data.organised)
save(covariates.lsoa, file = 'LSOA11_COVARIATES.rda')
write.csv(covariates.lsoa, file = 'LSOA11_COVARIATES.csv', row.names = FALSE)

## 1.2. msoa ----

covariates.msoa <- 
  tidyr::expand_grid(MSOA11CD = poly.msoa.england$MSOA11CD,
                     YEAR = min.year:max.year) %>% 
  dplyr::left_join(., 
                   imd.msoa %>% dplyr::select(MSOA11CD, YEAR, imdScore, incomeScore, employmentScore, healthScore, educationScore, housingScore, crimeScore, environmentScore),
                   by = c('MSOA11CD', 'YEAR')) %>% 
  dplyr::left_join(., 
                   ed.msoa %>% dplyr::select(MSOA11CD, YEAR, nonWhite), 
                   by = c('MSOA11CD', 'YEAR')) %>% 
  dplyr::left_join(., 
                   pd.msoa %>% dplyr::select(MSOA11CD, YEAR, populationDensity), 
                   by = c('MSOA11CD', 'YEAR')) %>% 
  dplyr::left_join(., 
                   ntl.msoa %>% dplyr::select(MSOA11CD, YEAR, nighttimeLight), 
                   by = c('MSOA11CD', 'YEAR')) %>% 
  dplyr::left_join(., 
                   rail.density.msoa %>% dplyr::select(MSOA11CD, totalRail), 
                   by = c('MSOA11CD')) %>% 
  dplyr::left_join(., 
                   road.density.msoa %>% dplyr::select(MSOA11CD, totalRoad), 
                   by = c('MSOA11CD')) %>% 
  dplyr::left_join(., 
                   ndvi.msoa %>% dplyr::select(MSOA11CD, YEAR, ndvi), 
                   by = c('MSOA11CD', 'YEAR'))

covariates.msoa %>% is.na() %>% sum()

setwd(dir.data.organised)
save(covariates.msoa, file = 'MSOA11_COVARIATES.rda')
write.csv(covariates.msoa, file = 'MSOA11_COVARIATES.csv', row.names = FALSE)

## 1.3. lad ----

covariates.lad <- 
  tidyr::expand_grid(LAD11CD = poly.lad.england$LAD11CD,
                     YEAR = min.year:max.year) %>% 
  dplyr::left_join(., 
                   imd.lad %>% dplyr::select(LAD11CD, YEAR, imdScore, incomeScore, employmentScore, healthScore, educationScore, housingScore, crimeScore, environmentScore),
                   by = c('LAD11CD', 'YEAR')) %>% 
  dplyr::left_join(., 
                   ed.lad %>% dplyr::select(LAD11CD, YEAR, nonWhite), 
                   by = c('LAD11CD', 'YEAR')) %>% 
  dplyr::left_join(., 
                   pd.lad %>% dplyr::select(LAD11CD, YEAR, populationDensity), 
                   by = c('LAD11CD', 'YEAR')) %>% 
  dplyr::left_join(., 
                   ntl.lad %>% dplyr::select(LAD11CD, YEAR, nighttimeLight), 
                   by = c('LAD11CD', 'YEAR')) %>% 
  dplyr::left_join(., 
                   rail.density.lad %>% dplyr::select(LAD11CD, totalRail), 
                   by = c('LAD11CD')) %>% 
  dplyr::left_join(., 
                   road.density.lad %>% dplyr::select(LAD11CD, totalRoad), 
                   by = c('LAD11CD')) %>% 
  dplyr::left_join(., 
                   ndvi.lad %>% dplyr::select(LAD11CD, YEAR, ndvi), 
                   by = c('LAD11CD', 'YEAR'))

covariates.lad %>% is.na() %>% sum()

setwd(dir.data.organised)
save(covariates.lad, file = 'LAD11_COVARIATES.rda')
write.csv(covariates.lad, file = 'LAD11_COVARIATES.csv', row.names = FALSE)

# 0 set up ----

## 0.1. packages ----

if(!require('tidyverse')) {
  install.packages('tidyverse', dependencies = TRUE)
}
if(!require('sf')) {
  install.packages('xtable', dependencies = TRUE)
}
## 0.2. directories ----

# retrieve directories
dir.path <- rstudioapi::getActiveDocumentContext()$path
dir.home <- sub('(Policy_Evaluation/01b_updatedAnalysis).*', '\\1', dir.path)
dir.code <- paste0(dir.home, '/code')
dir.data <- paste0(dir.home, '/data')
dir.data.organised <- paste0(dir.data, '/organised')
dir.data.spatial <- paste0(dir.data, '/spatial')

## 0.3. imports ----

### 0.3.1. functions ----

setwd(dir.code)
source('00_functions.R')

### 0.3.2. shape file ----

#### 0.3.2.1. full polygons ----

poly.lsoa <- sf::st_read(dsn = dir.data.spatial, layer = 'ONS11_LSOA')
poly.msoa <- sf::st_read(dsn = dir.data.spatial, layer = 'ONS11_MSOA')
poly.lad <- sf::st_read(dsn = dir.data.spatial, layer = 'ONS11_LAD')

#### 0.3.2.1. england only ----

poly.lsoa.england <- poly.lsoa %>% dplyr::filter(stringr::str_detect(LSOA11CD, '^E'))
poly.msoa.england <- poly.msoa %>% dplyr::filter(stringr::str_detect(MSOA11CD, '^E'))
poly.lad.england <- poly.lad %>% dplyr::filter(stringr::str_detect(LAD11CD, '^E'))

### 0.3.3. look up ----

setwd(dir.data.spatial)
lookup.msoa.rgn <-
  readr::read_csv('MSOA11_LAD11_RGN11_lookUp.csv') %>% 
  # renaming
  ## RGN21NM - for plotting as cannot find RGN11 poly and nothing has changed
  dplyr::rename(RGN21NM = 'RGN11NM')

# 1. organise spatial files ----

## 1.1. adjacent matrices ----

mat.lsoa.temp <- spdep::poly2nb(as(poly.lsoa.england, 'Spatial'))
mat.lsoa <- spdep::nb2mat(mat.lsoa.temp, zero.policy = TRUE)
colnames(mat.lsoa) <- rownames(mat.lsoa) <- paste0('lsoa_', 1:nrow(poly.lsoa.england))

mat.msoa.temp <- spdep::poly2nb(as(poly.msoa.england, 'Spatial'))
mat.msoa <- spdep::nb2mat(mat.msoa.temp, zero.policy = TRUE)
colnames(mat.msoa) <- rownames(mat.msoa) <- paste0('msoa_', 1:nrow(poly.msoa.england))

mat.lad.temp <- spdep::poly2nb(as(poly.lad.england, 'Spatial'))
mat.lad <- spdep::nb2mat(mat.lad.temp, zero.policy = TRUE)
colnames(mat.lad) <- rownames(mat.lad) <- paste0('lad_', 1:nrow(poly.lad.england))

## 1.2. information data frames ----

info.lsoa <-
  data.frame(LSOA11CD = poly.lsoa.england$LSOA11CD,
             LSOA11NM = poly.lsoa.england$LSOA11NM) %>% 
  dplyr::left_join(., 
                   poly.lsoa.england %>% 
                     dplyr::mutate(Area_M2 = sf::st_area(poly.lsoa.england) %>% as.numeric(),
                                   Area_KM2 = sf::st_area(poly.lsoa.england) %>% units::set_units(., 'km^2') %>% as.numeric()) %>% 
                     dplyr::select(LSOA11CD, Area_M2, Area_KM2) %>% 
                     sf::st_drop_geometry(),
                   by = 'LSOA11CD') %>% 
  dplyr::mutate(Internal = rownames(mat.lsoa))

info.msoa <-
  data.frame(MSOA11CD = poly.msoa.england$MSOA11CD,
             MSOA11NM = poly.msoa.england$MSOA11NM) %>% 
  dplyr::left_join(., 
                   poly.msoa.england %>% 
                     dplyr::mutate(Area_M2 = sf::st_area(poly.msoa.england) %>% as.numeric(),
                                   Area_KM2 = sf::st_area(poly.msoa.england) %>% units::set_units(., 'km^2') %>% as.numeric()) %>% 
                     dplyr::select(MSOA11CD, Area_M2, Area_KM2) %>% 
                     sf::st_drop_geometry(),
                   by = 'MSOA11CD') %>% 
  dplyr::mutate(Internal = rownames(mat.msoa))

info.lad <-
  data.frame(LAD11CD = poly.lad.england$LAD11CD,
             LAD11NM = poly.lad.england$LAD11NM) %>% 
  dplyr::left_join(., 
                   poly.lad.england %>% 
                     dplyr::mutate(Area_M2 = sf::st_area(poly.lad.england) %>% as.numeric(),
                                   Area_KM2 = sf::st_area(poly.lad.england) %>% units::set_units(., 'km^2') %>% as.numeric()) %>% 
                     dplyr::select(LAD11CD, Area_M2, Area_KM2) %>% 
                     sf::st_drop_geometry(),
                   by = 'LAD11CD') %>% 
  dplyr::mutate(Internal = rownames(mat.lad))

## 1.3. save ----

setwd(dir.data.organised)
save(mat.lsoa, mat.msoa, mat.lad, file = 'ENGLAND_AMAT_LSOA_MSOA_LAD.rda')
save(info.lsoa, info.msoa, info.lad, file = 'ENGLAND_SPATIAL_INFORMATION_LSOA_MSOA_LAD.rda')
# 0. set up ----

## 0.1. packages ----

if(!require('tidyverse')) {
  install.packages('tidyverse', dep = TRUE)
}
if(!require('sf')) {
  install.packages('sf', dep = TRUE)
}

## 0.2. directories ---- 

# retrieve directories
## project
dir.path <- rstudioapi::getActiveDocumentContext()$path
dir.home <- sub('(englishSuicides).*', '\\1', dir.path)
dir.code <- paste0(dir.home, '/code')
dir.data <- paste0(dir.home, '/data')
dir.data.organised <- paste0(dir.data, '/organised')
## data
dir.masterData <- paste0(sub('(OneDrive - Imperial College London).*', '\\1', dir.path), '/00_masterData')
dir.masterData.population <- paste0(dir.masterData, '/population')
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

# 1. organise spatial files ----

## 1.1. adjacent matrices ----

lsoa.mat.temp <- spdep::poly2nb(as(poly.lsoa.england, 'Spatial'))
lsoa.mat <- spdep::nb2mat(lsoa.mat.temp, zero.policy = TRUE)
colnames(lsoa.mat) <- rownames(lsoa.mat) <- paste0('lsoa_', 1:dim(lsoa.mat)[1])

msoa.mat.temp <- spdep::poly2nb(as(poly.msoa.england, 'Spatial'))
msoa.mat <- spdep::nb2mat(msoa.mat.temp, zero.policy = TRUE)
colnames(msoa.mat) <- rownames(msoa.mat) <- paste0('msoa_', 1:dim(msoa.mat)[1])

lad.mat.temp <- spdep::poly2nb(as(poly.lad.england, 'Spatial'))
lad.mat <- spdep::nb2mat(lad.mat.temp, zero.policy = TRUE)
colnames(lad.mat) <- rownames(lad.mat) <- paste0('lad_', 1:dim(lad.mat)[1])

## 1.2. information data frames ----

lsoa.info <- 
  data.frame(LSOA11CD = poly.lsoa.england$LSOA11CD,
             LSOA11NM = poly.lsoa.england$LSOA11NM) %>%
  dplyr::left_join(., 
                   poly.lsoa.england %>% 
                     dplyr::mutate(Area_M2 = sf::st_area(poly.lsoa.england) %>% as.numeric(),
                                   Area_KM2 = sf::st_area(poly.lsoa.england) %>% units::set_units(., 'km^2') %>% as.numeric()) %>% 
                     dplyr::select(LSOA11CD, Area_M2, Area_KM2) %>% 
                     sf::st_drop_geometry(),
                   by = 'LSOA11CD') %>% 
  dplyr::mutate(Internal = rownames(lsoa.mat))

msoa.info <- 
  data.frame(MSOA11CD = poly.msoa.england$MSOA11CD,
             MSOA11NM = poly.msoa.england$MSOA11NM) %>%
  dplyr::left_join(., 
                   poly.msoa.england %>% 
                     dplyr::mutate(Area_M2 = sf::st_area(poly.msoa.england) %>% as.numeric(),
                                   Area_KM2 = sf::st_area(poly.msoa.england) %>% units::set_units(., 'km^2') %>% as.numeric()) %>% 
                     dplyr::select(MSOA11CD, Area_M2, Area_KM2) %>% 
                     sf::st_drop_geometry(),
                   by = 'MSOA11CD') %>% 
  dplyr::mutate(Internal = rownames(msoa.mat))

lad.info <- 
  data.frame(LAD11CD = poly.lad.england$LAD11CD,
             LAD11NM = poly.lad.england$LAD11NM) %>%
  dplyr::left_join(., 
                   poly.lad.england %>% 
                     dplyr::mutate(Area_M2 = sf::st_area(poly.lad.england) %>% as.numeric(),
                                   Area_KM2 = sf::st_area(poly.lad.england) %>% units::set_units(., 'km^2') %>% as.numeric()) %>% 
                     dplyr::select(LAD11CD, Area_M2, Area_KM2) %>% 
                     sf::st_drop_geometry(),
                   by = 'LAD11CD') %>% 
  dplyr::mutate(Internal = rownames(lad.mat))

## 1.3. save ----

setwd(dir.data.organised)
save(lsoa.mat, msoa.mat, lad.mat, file = 'ENGLAND_AMAT_LSOA_MSOA_LAD.rda')
save(lsoa.info, msoa.info, lad.info, file = 'ENGLAND_SPATIAL_INFORMATION_LSOA_MSOA_LAD.rda')


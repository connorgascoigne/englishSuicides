# 0 set up ----

## 0.1 packages ----

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
dir.masterData.spatial <- paste0(dir.masterData, '/onsSpatial')

## 0.3. functions ----

setwd(dir.code)
source('00_functions.R')

## 0.4. shape file ----

setwd(dir.masterData.spatial)
poly.msoa <- sf::st_read(dsn = dir.masterData.spatial, layer = 'ONS11_MSOA')
poly.msoa.england <- poly.msoa %>%  dplyr::filter(str_detect(MSOA11CD, '^E'))

# 1. simulate covariate data ----

## 1.1. data labels ----

year.min <- 2002
year.max <- 2022

label.msoa <- poly.msoa.england$MSOA11CD
label.year <- year.min:year.max

## 1.2. simulate covariate ----

covariates.msoa <- 
  tidyr::expand_grid(MSOA11CD = label.msoa,
                     YEAR = label.year) %>%
  dplyr::mutate(
    # normal dist between 0 and 100
    imdScore = round(pmin(100, pmax(0, rnorm(n(), mean = 30, sd = 15))), 1),
    # right-skew dist between 0 and 100
    nonWhite = pmin(0.99, 1 - rbeta(n(), shape1 = 2, shape2 = 0.5)),
    # left-skew between 0 and infinity
    populationDensity = round(rlnorm(n(), meanlog = 6.5, sdlog = 0.5)), 
    # left-skew between 0 and 63
    nighttimeLight = round(pmin(63, rbeta(n(), 5, 2) * 63), 2),
    # zero inflated then slight right-skew between 0 and infinity
    totalRail = round(rexp(n(), rate = 2) * 2, 2),
    # right-skew between 0 and infinity 
    totalRoad = round(pmax(0.5, rnorm(n(), mean = 3.5, sd = 1)), 2),
    # slight right-skew between -1 and 1
    ndvi = round(pmin(0.9, pmax(0.1, rnorm(n(), mean = 0.45, sd = 0.1))), 2))


## 1.3. save ----

setwd(dir.data.organised)
save(covariates.msoa, file = 'MSOA11_COVARIATES_SIMULATED.rda')
write.csv(x = data.final, file = 'MSOA11_COVARIATES_SIMULATED.csv', row.names = FALSE)


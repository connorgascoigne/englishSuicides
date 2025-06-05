# 0 set up ----

## 0.1 packages ----

if(!require('tidyverse')) {
  install.packages('tidyverse', dep = TRUE)
}
if(!require('sf')) {
  install.packages('sf', dep = TRUE)
}
if(!require('data.table')) {
  install.packages('data.table', dep = TRUE)
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
poly.lsoa <- sf::st_read(dsn = dir.masterData.spatial, layer = 'ONS11_LSOA')
poly.lsoa.england <- poly.lsoa %>%  dplyr::filter(str_detect(LSOA11CD, '^E'))

# 1. simulate population data ----

## 1.1. data labels ----

year.min <- 2002
year.max <- 2022

label.lsoa <- poly.lsoa.england$LSOA11CD
label.year <- year.min:year.max
label.ageClass <- c('[0, 5)', '[5, 15)', '[15, 25)', '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)', '[65, 75)', '[75, 85)', '85+')
label.sex <- c('MALES', 'FEMALES')

## 1.2 weighting ----

weights.age <-
  data.table(AGE_CLASS = label.ageClass,
             # representative and these sum to one
             AGE_WEIGHT = c(0.06, 0.12, 0.13, 0.14, 0.13, 0.12, 0.10, 0.08, 0.06, 0.06))

weights.sex <- 
  data.table(SEX = label.sex,
                          # representative and sum to one
                          SEX_WEIGHT = c(0.49, 0.51))

weights.lsoa <-
  data.table(LSOA11CD = label.lsoa,
             # lsoa between 1000-3000 people 
             BASE_POP = round(runif(length(label.lsoa), min = 1000, max = 3000)))

## 1.3. complete data ----

population.msoa <- 
  data.table::CJ(LSOA11CD = label.lsoa,
                 YEAR = label.year,
                 SEX = label.sex,
                 AGE_CLASS = label.ageClass) %>% 
  merge(x = .,
        y = weights.sex,
        by = 'SEX') %>% 
  merge(x = ., 
        y = weights.age,
        by = 'AGE_CLASS') %>% 
  merge(x = ., 
        y = weights.lsoa, 
        by = 'LSOA11CD') %>% 
  # small population growth per year
  .[, YEAR_EFFECT := 1 + 0.005 * (YEAR - year.min)] %>% 
  .[, POPULATION := round(BASE_POP * AGE_WEIGHT * SEX_WEIGHT * YEAR_EFFECT)] %>% 
  # select relavent columns
  .[, .(LSOA11CD, YEAR, SEX, AGE_CLASS, POPULATION)] %>% 
  # to data frame and arrange
  as.data.frame() %>% 
  dplyr::arrange(YEAR, LSOA11CD, SEX, AGE_CLASS)

## 1.5. save ----

setwd(dir.data.organised)
save(population.msoa, file = 'msoa/rda/MSOA11_POPULATION_TOTAL_SIMULATED.rda')
write.csv(population.msoa, file = 'msoa/csv/MSOA11_POPULATION_TOTAL_SIMULATED.csv', row.names = FALSE)
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
dir.masterData.imd <- paste0(dir.masterData, '/imd')
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

lsoa.nbs <- spdep::poly2nb(pl = as(poly.lsoa.england, 'Spatial'))

### 0.4.2. lookup file ----

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

### 0.6.1. imd files ----

setwd(dir.masterData.imd)
# open each sheet (except notes) and join
imd.04.import <-
  lapply(X = readxl::excel_sheets('LSOA01_2004_IMD_SCORES.xls')[-1],
         FUN = function(x) { readxl::read_excel('LSOA01_2004_IMD_SCORES.xls', sheet = x) }) %>% 
  purrr::reduce(.x = .,
                .f = dplyr::left_join,
                by = c('SOA', 'LA CODE', 'LA NAME', 'GOR CODE', 'GOR NAME'))

# open each sheet (except notes) and join
imd.07.import <-
  lapply(X = readxl::excel_sheets('LSOA01_2007_IMD_SCORES.xls')[-1],
         FUN = function(x) { readxl::read_excel('LSOA01_2007_IMD_SCORES.xls', sheet = x) }) %>% 
  purrr::reduce(.x = .,
                .f = dplyr::left_join,
                by = c('LSOA', 'LA CODE', 'LA NAME', 'GOR CODE', 'GOR NAME'))

# open each sheet (except notes) and join
imd.10.import <-
  lapply(X = readxl::excel_sheets('LSOA01_2010_IMD_SCORES.xls')[-1],
         FUN = function(x) { readxl::read_excel('LSOA01_2010_IMD_SCORES.xls', sheet = x) }) %>% 
  purrr::reduce(.x = .,
                .f = dplyr::left_join,
                by = c('LSOA CODE'))

# all scores in one sheet already
imd.15.import <- readxl::read_excel('LSOA11_2015_IMD_SCORES.xlsx', sheet = 'ID2015 Scores')
imd.19.import <- readxl::read_excel('LSOA11_2019_IMD_SCORES.xlsx', sheet = 'IoD2019 Scores')

### 0.6.2. population files ----

setwd(dir.data.organised)
population.import <- read.csv(file = 'lsoa/csv/LSOA11_POPULATION_TOTAL.csv')
population.04 <-
  population.import %>% 
  dplyr::filter(YEAR == 2004) %>% 
  dplyr::summarise(POPULATION = POPULATION %>% sum(),
                   .by = 'LSOA11CD')
population.07 <-
  population.import %>% 
  dplyr::filter(YEAR == 2007) %>% 
  dplyr::summarise(POPULATION = POPULATION %>% sum(),
                   .by = 'LSOA11CD')
population.10 <-
  population.import %>% 
  dplyr::filter(YEAR == 2010) %>% 
  dplyr::summarise(POPULATION = POPULATION %>% sum(),
                   .by = 'LSOA11CD')
population.15 <-
  population.import %>% 
  dplyr::filter(YEAR == 2015) %>% 
  dplyr::summarise(POPULATION = POPULATION %>% sum(),
                   .by = 'LSOA11CD')
population.19 <-
  population.import %>% 
  dplyr::filter(YEAR == 2019) %>% 
  dplyr::summarise(POPULATION = POPULATION %>% sum(),
                   .by = 'LSOA11CD')

# 1. format imd files ----

## 1.1. 2004 imd ----

imd.04.temp <- 
  data.frame(LSOA11CD = poly.lsoa.england$LSOA11CD) %>% 
  dplyr::left_join(., 
                   # sort and join imd 2004
                   imd.04.import %>% 
                     ## Rename
                     dplyr::rename('LSOA01CD' = 'SOA',
                                   'imdScore' = 'IMD SCORE',
                                   'incomeScore' = 'INCOME SCORE',
                                   'employmentScore' = 'EMPLOYMENT SCORE',
                                   'healthScore' = 'HEALTH DEPRIVATION AND DISABILITY SCORE',
                                   'educationScore' = 'EDUCATION SKILLS AND TRAINING SCORE',
                                   'housingScore' = 'BARRIERS TO HOUSING AND SERVICES SCORE',
                                   'crimeScore' = 'CRIME AND DISORDER SCORE',
                                   'environmentScore' = 'LIVING ENVIRONMENT SCORE') %>% 
                     ## join the LSOA11CD
                     dplyr::left_join(., 
                                      lookup.01.11 %>% 
                                        dplyr::select(LSOA01CD, LSOA11CD) %>% 
                                        unique(),
                                      by = 'LSOA01CD',
                                      relationship = 'many-to-many') %>% 
                     ## average over repeated LSOAs because multiple LSOA01CD got to LSOA11CD
                     dplyr::summarise(imdScore = imdScore %>% mean(),
                                      incomeScore = incomeScore %>% mean(),
                                      employmentScore = employmentScore %>% mean(),
                                      healthScore = healthScore %>% mean(),
                                      educationScore = educationScore %>% mean(),
                                      housingScore = housingScore %>% mean(),
                                      crimeScore = crimeScore %>% mean(),
                                      environmentScore = environmentScore %>% mean(),
                                      .by = 'LSOA11CD'),
                   by = 'LSOA11CD') %>% 
  # join population 
  dplyr::left_join(.,
                   population.04,
                   by = 'LSOA11CD') %>% 
  # join spatial polygons to impute missing data
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

imd.04.missing.lsoa <- 
  imd.04.temp %>% 
  dplyr::filter(rowSums(is.na(.)) > 0) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(LSOA11CD, LSOA11NM) %>% 
  dplyr::mutate(imdYear = 2004)

imd.04.sorted <- 
  imd.04.temp %>% 
  dplyr::mutate(imdYear = 2004)

if(nrow(imd.04.missing.lsoa) > 0){
  # full imd score
  imd.04.sorted$imdScore[is.na(imd.04.sorted$imdScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.04.sorted$imdScore))],
           FUN = function(x) { mean(imd.04.sorted$imdScore[x], na.rm = TRUE) %>% round() })
  # subdomain scores
  ## income
  imd.04.sorted$incomeScore[is.na(imd.04.sorted$incomeScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.04.sorted$incomeScore))],
           FUN = function(x) { mean(imd.04.sorted$incomeScore[x], na.rm = TRUE) %>% round() })
  ## employment
  imd.04.sorted$employmentScore[is.na(imd.04.sorted$employmentScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.04.sorted$employmentScore))],
           FUN = function(x) { mean(imd.04.sorted$employmentScore[x], na.rm = TRUE) %>% round() })
  ## health
  imd.04.sorted$healthScore[is.na(imd.04.sorted$healthScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.04.sorted$healthScore))],
           FUN = function(x) { mean(imd.04.sorted$healthScore[x], na.rm = TRUE) %>% round() })
  ## education
  imd.04.sorted$educationScore[is.na(imd.04.sorted$educationScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.04.sorted$educationScore))],
           FUN = function(x) { mean(imd.04.sorted$educationScore[x], na.rm = TRUE) %>% round() })
  ## housing 
  imd.04.sorted$housingScore[is.na(imd.04.sorted$housingScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.04.sorted$housingScore))],
           FUN = function(x) { mean(imd.04.sorted$housingScore[x], na.rm = TRUE) %>% round() })
  ## crime
  imd.04.sorted$crimeScore[is.na(imd.04.sorted$crimeScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.04.sorted$crimeScore))],
           FUN = function(x) { mean(imd.04.sorted$crimeScore[x], na.rm = TRUE) %>% round() })
  ## environment
  imd.04.sorted$environmentScore[is.na(imd.04.sorted$environmentScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.04.sorted$environmentScore))],
           FUN = function(x) { mean(imd.04.sorted$environmentScore[x], na.rm = TRUE) %>% round() })
}

## 1.2. 2007 imd ----

imd.07.temp <- 
  data.frame(LSOA11CD = poly.lsoa.england$LSOA11CD) %>% 
  dplyr::left_join(., 
                   # sort and join imd 2004
                   imd.07.import %>% 
                     ## Rename
                     dplyr::rename('LSOA01CD' = 'LSOA',
                                   'imdScore' = 'IMD SCORE',
                                   'incomeScore' = 'INCOME SCORE',
                                   'employmentScore' = 'EMPLOYMENT SCORE',
                                   'healthScore' = 'HEALTH DEPRIVATION AND DISABILITY SCORE',
                                   'educationScore' = 'EDUCATION SKILLS AND TRAINING SCORE',
                                   'housingScore' = 'BARRIERS TO HOUSING AND SERVICES SCORE',
                                   'crimeScore' = 'CRIME AND DISORDER SCORE',
                                   'environmentScore' = 'LIVING ENVIRONMENT SCORE') %>% 
                     ## join the LSOA11CD
                     dplyr::left_join(., 
                                      lookup.01.11 %>% 
                                        dplyr::select(LSOA01CD, LSOA11CD) %>% 
                                        unique(),
                                      by = 'LSOA01CD',
                                      relationship = 'many-to-many') %>% 
                     ## average over repeated LSOAs because multiple LSOA01CD got to LSOA11CD
                     dplyr::summarise(imdScore = imdScore %>% mean(),
                                      incomeScore = incomeScore %>% mean(),
                                      employmentScore = employmentScore %>% mean(),
                                      healthScore = healthScore %>% mean(),
                                      educationScore = educationScore %>% mean(),
                                      housingScore = housingScore %>% mean(),
                                      crimeScore = crimeScore %>% mean(),
                                      environmentScore = environmentScore %>% mean(),
                                      .by = 'LSOA11CD'),
                   by = 'LSOA11CD') %>%  
  # join population 
  dplyr::left_join(.,
                   population.07,
                   by = 'LSOA11CD') %>% 
  # join spatial polygons to impute missing data
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

imd.07.missing.lsoa <- 
  imd.07.temp %>% 
  dplyr::filter(rowSums(is.na(.)) > 0) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(LSOA11CD, LSOA11NM) %>% 
  dplyr::mutate(imdYear = 2007)

imd.07.sorted <-
  imd.07.temp %>% 
  dplyr::mutate(imdYear = 2007)

if(nrow(imd.07.missing.lsoa) > 0){
  # full imd score
  imd.07.sorted$imdScore[is.na(imd.07.sorted$imdScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.07.sorted$imdScore))],
           FUN = function(x) { mean(imd.07.sorted$imdScore[x], na.rm = TRUE) %>% round() })
  # subdomain scores
  ## income
  imd.07.sorted$incomeScore[is.na(imd.07.sorted$incomeScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.07.sorted$incomeScore))],
           FUN = function(x) { mean(imd.07.sorted$incomeScore[x], na.rm = TRUE) %>% round() })
  ## employment
  imd.07.sorted$employmentScore[is.na(imd.07.sorted$employmentScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.07.sorted$employmentScore))],
           FUN = function(x) { mean(imd.07.sorted$employmentScore[x], na.rm = TRUE) %>% round() })
  ## health
  imd.07.sorted$healthScore[is.na(imd.07.sorted$healthScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.07.sorted$healthScore))],
           FUN = function(x) { mean(imd.07.sorted$healthScore[x], na.rm = TRUE) %>% round() })
  ## education
  imd.07.sorted$educationScore[is.na(imd.07.sorted$educationScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.07.sorted$educationScore))],
           FUN = function(x) { mean(imd.07.sorted$educationScore[x], na.rm = TRUE) %>% round() })
  ## housing 
  imd.07.sorted$housingScore[is.na(imd.07.sorted$housingScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.07.sorted$housingScore))],
           FUN = function(x) { mean(imd.07.sorted$housingScore[x], na.rm = TRUE) %>% round() })
  ## crime
  imd.07.sorted$crimeScore[is.na(imd.07.sorted$crimeScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.07.sorted$crimeScore))],
           FUN = function(x) { mean(imd.07.sorted$crimeScore[x], na.rm = TRUE) %>% round() })
  ## environment
  imd.07.sorted$environmentScore[is.na(imd.07.sorted$environmentScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.07.sorted$environmentScore))],
           FUN = function(x) { mean(imd.07.sorted$environmentScore[x], na.rm = TRUE) %>% round() })
}

## 1.3. 2010 imd ----

imd.10.temp <- 
  data.frame(LSOA11CD = poly.lsoa.england$LSOA11CD) %>% 
  dplyr::left_join(., 
                   # sort and join imd 2004
                   imd.10.import %>% 
                     ## Rename
                     dplyr::rename('LSOA01CD' = 'LSOA CODE',
                                   'imdScore' = 'IMD SCORE',
                                   'incomeScore' = 'INCOME SCORE',
                                   'employmentScore' = 'EMPLOYMENT SCORE',
                                   'healthScore' = 'HEALTH DEPRIVATION AND DISABILITY SCORE',
                                   'educationScore' = 'EDUCATION SKILLS AND TRAINING SCORE',
                                   'housingScore' = 'BARRIERS TO HOUSING AND SERVICES SCORE',
                                   'crimeScore' = 'CRIME AND DISORDER SCORE',
                                   'environmentScore' = 'LIVING ENVIRONMENT SCORE') %>% 
                     ## join the LSOA11CD
                     dplyr::left_join(., 
                                      lookup.01.11 %>% 
                                        dplyr::select(LSOA01CD, LSOA11CD) %>% 
                                        unique(),
                                      by = 'LSOA01CD',
                                      relationship = 'many-to-many') %>% 
                     ## average over repeated LSOAs because multiple LSOA01CD got to LSOA11CD
                     dplyr::summarise(imdScore = imdScore %>% mean(),
                                      incomeScore = incomeScore %>% mean(),
                                      employmentScore = employmentScore %>% mean(),
                                      healthScore = healthScore %>% mean(),
                                      educationScore = educationScore %>% mean(),
                                      housingScore = housingScore %>% mean(),
                                      crimeScore = crimeScore %>% mean(),
                                      environmentScore = environmentScore %>% mean(),
                                      .by = 'LSOA11CD'),
                   by = 'LSOA11CD') %>%  
  # join population 
  dplyr::left_join(.,
                   population.10,
                   by = 'LSOA11CD') %>% 
  # join spatial polygons to impute missing data
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

imd.10.missing.lsoa <- 
  imd.10.temp %>% 
  dplyr::filter(rowSums(is.na(.)) > 0) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(LSOA11CD, LSOA11NM) %>% 
  dplyr::mutate(imdYear = 2010)

imd.10.sorted <- 
  imd.10.temp %>% 
  dplyr::mutate(imdYear = 2010)

if(nrow(imd.10.missing.lsoa) > 0){
  # full imd score
  imd.10.sorted$imdScore[is.na(imd.10.sorted$imdScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.10.sorted$imdScore))],
           FUN = function(x) { mean(imd.10.sorted$imdScore[x], na.rm = TRUE) %>% round() })
  # subdomain scores
  ## income
  imd.10.sorted$incomeScore[is.na(imd.10.sorted$incomeScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.10.sorted$incomeScore))],
           FUN = function(x) { mean(imd.10.sorted$incomeScore[x], na.rm = TRUE) %>% round() })
  ## employment
  imd.10.sorted$employmentScore[is.na(imd.10.sorted$employmentScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.10.sorted$employmentScore))],
           FUN = function(x) { mean(imd.10.sorted$employmentScore[x], na.rm = TRUE) %>% round() })
  ## health
  imd.10.sorted$healthScore[is.na(imd.10.sorted$healthScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.10.sorted$healthScore))],
           FUN = function(x) { mean(imd.10.sorted$healthScore[x], na.rm = TRUE) %>% round() })
  ## education
  imd.10.sorted$educationScore[is.na(imd.10.sorted$educationScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.10.sorted$educationScore))],
           FUN = function(x) { mean(imd.10.sorted$educationScore[x], na.rm = TRUE) %>% round() })
  ## housing 
  imd.10.sorted$housingScore[is.na(imd.10.sorted$housingScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.10.sorted$housingScore))],
           FUN = function(x) { mean(imd.10.sorted$housingScore[x], na.rm = TRUE) %>% round() })
  ## crime
  imd.10.sorted$crimeScore[is.na(imd.10.sorted$crimeScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.10.sorted$crimeScore))],
           FUN = function(x) { mean(imd.10.sorted$crimeScore[x], na.rm = TRUE) %>% round() })
  ## environment
  imd.10.sorted$environmentScore[is.na(imd.10.sorted$environmentScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.10.sorted$environmentScore))],
           FUN = function(x) { mean(imd.10.sorted$environmentScore[x], na.rm = TRUE) %>% round() })
}

## 1.4. 2015 imd ----

imd.15.temp <- 
  data.frame(LSOA11CD = poly.lsoa.england$LSOA11CD) %>% 
  dplyr::left_join(., 
                   # sort and join imd 2004
                   imd.15.import %>% 
                     ## Rename
                     dplyr::rename('LSOA11CD' = 'LSOA code (2011)',
                                   'imdScore' = 'Index of Multiple Deprivation (IMD) Score',
                                   'incomeScore' = 'Income Score (rate)',
                                   'employmentScore' = 'Employment Score (rate)',
                                   'healthScore' = 'Health Deprivation and Disability Score',
                                   'educationScore' = 'Education, Skills and Training Score',
                                   'housingScore' = 'Barriers to Housing and Services Score',
                                   'crimeScore' = 'Crime Score',
                                   'environmentScore' = 'Living Environment Score') %>% 
                     ## average over repeated LSOAs because multiple LSOA01CD got to LSOA11CD (does nothing for 2015+)
                     dplyr::summarise(imdScore = imdScore %>% mean(),
                                      incomeScore = incomeScore %>% mean(),
                                      employmentScore = employmentScore %>% mean(),
                                      healthScore = healthScore %>% mean(),
                                      educationScore = educationScore %>% mean(),
                                      housingScore = housingScore %>% mean(),
                                      crimeScore = crimeScore %>% mean(),
                                      environmentScore = environmentScore %>% mean(),
                                      .by = 'LSOA11CD'),
                   by = 'LSOA11CD') %>%  
  # join population 
  dplyr::left_join(.,
                   population.15,
                   by = 'LSOA11CD') %>% 
  # join spatial polygons to impute missing data
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

imd.15.missing.lsoa <- 
  imd.15.temp %>% 
  dplyr::filter(rowSums(is.na(.)) > 0) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(LSOA11CD, LSOA11NM) %>% 
  dplyr::mutate(imdYear = 2015)

imd.15.sorted <- 
  imd.15.temp %>% 
  dplyr::mutate(imdYear = 2015)

if(nrow(imd.15.missing.lsoa) > 0){
  # full imd score
  imd.15.sorted$imdScore[is.na(imd.15.sorted$imdScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.15.sorted$imdScore))],
           FUN = function(x) { mean(imd.15.sorted$imdScore[x], na.rm = TRUE) %>% round() })
  # subdomain scores
  ## income
  imd.15.sorted$incomeScore[is.na(imd.15.sorted$incomeScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.15.sorted$incomeScore))],
           FUN = function(x) { mean(imd.15.sorted$incomeScore[x], na.rm = TRUE) %>% round() })
  ## employment
  imd.15.sorted$employmentScore[is.na(imd.15.sorted$employmentScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.15.sorted$employmentScore))],
           FUN = function(x) { mean(imd.15.sorted$employmentScore[x], na.rm = TRUE) %>% round() })
  ## health
  imd.15.sorted$healthScore[is.na(imd.15.sorted$healthScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.15.sorted$healthScore))],
           FUN = function(x) { mean(imd.15.sorted$healthScore[x], na.rm = TRUE) %>% round() })
  ## education
  imd.15.sorted$educationScore[is.na(imd.15.sorted$educationScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.15.sorted$educationScore))],
           FUN = function(x) { mean(imd.15.sorted$educationScore[x], na.rm = TRUE) %>% round() })
  ## housing 
  imd.15.sorted$housingScore[is.na(imd.15.sorted$housingScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.15.sorted$housingScore))],
           FUN = function(x) { mean(imd.15.sorted$housingScore[x], na.rm = TRUE) %>% round() })
  ## crime
  imd.15.sorted$crimeScore[is.na(imd.15.sorted$crimeScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.15.sorted$crimeScore))],
           FUN = function(x) { mean(imd.15.sorted$crimeScore[x], na.rm = TRUE) %>% round() })
  ## environment
  imd.15.sorted$environmentScore[is.na(imd.15.sorted$environmentScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.15.sorted$environmentScore))],
           FUN = function(x) { mean(imd.15.sorted$environmentScore[x], na.rm = TRUE) %>% round() })
}

## 1.5. 2019 imd ----

imd.19.temp <- 
  data.frame(LSOA11CD = poly.lsoa.england$LSOA11CD) %>% 
  dplyr::left_join(., 
                   # sort and join imd 2004
                   imd.19.import %>% 
                     ## Rename
                     dplyr::rename('LSOA11CD' = 'LSOA code (2011)',
                                   'imdScore' = 'Index of Multiple Deprivation (IMD) Score',
                                   'incomeScore' = 'Income Score (rate)',
                                   'employmentScore' = 'Employment Score (rate)',
                                   'healthScore' = 'Health Deprivation and Disability Score',
                                   'educationScore' = 'Education, Skills and Training Score',
                                   'housingScore' = 'Barriers to Housing and Services Score',
                                   'crimeScore' = 'Crime Score',
                                   'environmentScore' = 'Living Environment Score') %>% 
                     ## average over repeated LSOAs because multiple LSOA01CD got to LSOA11CD (does nothing for 2015+)
                     dplyr::summarise(imdScore = imdScore %>% mean(),
                                      incomeScore = incomeScore %>% mean(),
                                      employmentScore = employmentScore %>% mean(),
                                      healthScore = healthScore %>% mean(),
                                      educationScore = educationScore %>% mean(),
                                      housingScore = housingScore %>% mean(),
                                      crimeScore = crimeScore %>% mean(),
                                      environmentScore = environmentScore %>% mean(),
                                      .by = 'LSOA11CD'),
                   by = 'LSOA11CD') %>%  
  # join population 
  dplyr::left_join(.,
                   population.19,
                   by = 'LSOA11CD') %>% 
  # join spatial polygons to impute missing data
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

imd.19.missing.lsoa <- 
  imd.19.temp %>% 
  dplyr::filter(rowSums(is.na(.)) > 0) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(LSOA11CD, LSOA11NM) %>% 
  dplyr::mutate(imdYear = 2019)

imd.19.sorted <- 
  imd.19.temp %>% 
  dplyr::mutate(imdYear = 2019)

if(nrow(imd.19.missing.lsoa) > 0){
  # full imd score
  imd.19.sorted$imdScore[is.na(imd.19.sorted$imdScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.19.sorted$imdScore))],
           FUN = function(x) { mean(imd.19.sorted$imdScore[x], na.rm = TRUE) %>% round() })
  # subdomain scores
  ## income
  imd.19.sorted$incomeScore[is.na(imd.19.sorted$incomeScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.19.sorted$incomeScore))],
           FUN = function(x) { mean(imd.19.sorted$incomeScore[x], na.rm = TRUE) %>% round() })
  ## employment
  imd.19.sorted$employmentScore[is.na(imd.19.sorted$employmentScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.19.sorted$employmentScore))],
           FUN = function(x) { mean(imd.19.sorted$employmentScore[x], na.rm = TRUE) %>% round() })
  ## health
  imd.19.sorted$healthScore[is.na(imd.19.sorted$healthScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.19.sorted$healthScore))],
           FUN = function(x) { mean(imd.19.sorted$healthScore[x], na.rm = TRUE) %>% round() })
  ## education
  imd.19.sorted$educationScore[is.na(imd.19.sorted$educationScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.19.sorted$educationScore))],
           FUN = function(x) { mean(imd.19.sorted$educationScore[x], na.rm = TRUE) %>% round() })
  ## housing 
  imd.19.sorted$housingScore[is.na(imd.19.sorted$housingScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.19.sorted$housingScore))],
           FUN = function(x) { mean(imd.19.sorted$housingScore[x], na.rm = TRUE) %>% round() })
  ## crime
  imd.19.sorted$crimeScore[is.na(imd.19.sorted$crimeScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.19.sorted$crimeScore))],
           FUN = function(x) { mean(imd.19.sorted$crimeScore[x], na.rm = TRUE) %>% round() })
  ## environment
  imd.19.sorted$environmentScore[is.na(imd.19.sorted$environmentScore)] <-
    sapply(lsoa.nbs[which(is.na(imd.19.sorted$environmentScore))],
           FUN = function(x) { mean(imd.19.sorted$environmentScore[x], na.rm = TRUE) %>% round() })
}

# 2. combine all ----

## 2.1. missing ----

imd.missing <-
  dplyr::bind_rows(imd.04.missing.lsoa,
                   imd.07.missing.lsoa,
                   imd.10.missing.lsoa,
                   imd.15.missing.lsoa,
                   imd.19.missing.lsoa)

setwd(dir.res.exploration)
print(x = xtable::xtable(x = imd.missing),
      include.rownames = FALSE,
      file = 'txt/lsoa/LSOA11_DEPRVATION_MISSING.txt')

## 2.2. imd score ----

imd.00.06.lsoa <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              YEAR = 2000:2006) %>% 
  dplyr::left_join(.,
                   imd.04.sorted,
                   by = 'LSOA11CD')

imd.07.09.lsoa <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              YEAR = 2007:2009) %>% 
  dplyr::left_join(.,
                   imd.07.sorted,
                   by = 'LSOA11CD')

imd.10.14.lsoa <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              YEAR = 2010:2014) %>% 
  dplyr::left_join(.,
                   imd.10.sorted,
                   by = 'LSOA11CD')

imd.15.18.lsoa <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              YEAR = 2015:2018) %>% 
  dplyr::left_join(.,
                   imd.15.sorted,
                   by = 'LSOA11CD')

imd.19.22.lsoa <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              YEAR = 2019:2022) %>% 
  dplyr::left_join(.,
                   imd.19.sorted,
                   by = 'LSOA11CD')

imd.lsoa.temp <-
  dplyr::bind_rows(imd.00.06.lsoa,
                   imd.07.09.lsoa,
                   imd.10.14.lsoa,
                   imd.15.18.lsoa,
                   imd.19.22.lsoa) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::left_join(.,
                   lookup.11 %>% dplyr::select(LSOA11CD, MSOA11CD, LAD11CD) %>% unique(),
                   by = 'LSOA11CD') %>% 
  dplyr::select(LSOA11CD, MSOA11CD, LAD11CD, imdYear, YEAR, POPULATION, 
                imdScore, 
                incomeScore, employmentScore, healthScore, educationScore, housingScore, crimeScore, environmentScore)

nrow(poly.lsoa.england)*length(2000:2022) == nrow(imd.lsoa.temp)
imd.lsoa.temp %>% is.na() %>% sum()

# 3. organise by spatial level ----

## 3.1. lsoa ----

imd.lsoa <-
  imd.lsoa.temp %>% 
  dplyr::select(LSOA11CD, YEAR, imdYear, 
                imdScore, 
                incomeScore, employmentScore, healthScore, educationScore, housingScore, crimeScore, environmentScore) %>% 
  dplyr::arrange(YEAR, LSOA11CD)

setwd(dir.data.organised)
save(imd.lsoa, file = 'lsoa/rda/LSOA11_IMD.rda')
write.csv(imd.lsoa, file = 'lsoa/csv/LSOA11_IMD.csv', row.names = FALSE)

## 3.2. msoa ----

imd.msoa <-
  imd.lsoa.temp %>% 
  # population weighted average
  dplyr::summarise(imdScore = stats::weighted.mean(x = imdScore, w = POPULATION, na.rm = TRUE),
                   incomeScore = stats::weighted.mean(x = incomeScore, w = POPULATION, na.rm = TRUE),
                   employmentScore = stats::weighted.mean(x = employmentScore, w = POPULATION, na.rm = TRUE),
                   healthScore = stats::weighted.mean(x = healthScore, w = POPULATION, na.rm = TRUE),
                   educationScore = stats::weighted.mean(x = educationScore, w = POPULATION, na.rm = TRUE),
                   housingScore = stats::weighted.mean(x = housingScore, w = POPULATION, na.rm = TRUE),
                   crimeScore = stats::weighted.mean(x = crimeScore, w = POPULATION, na.rm = TRUE),
                   environmentScore = stats::weighted.mean(x = environmentScore, w = POPULATION, na.rm = TRUE),
                   .by = c('MSOA11CD', 'YEAR', 'imdYear')) %>% 
  dplyr::arrange(YEAR, MSOA11CD) %>% 
  dplyr::select(MSOA11CD, YEAR, imdYear, 
                imdScore, 
                incomeScore, employmentScore, healthScore, educationScore, housingScore, crimeScore, environmentScore)

setwd(dir.data.organised)
save(imd.msoa, file = 'msoa/rda/MSOA11_IMD.rda')
write.csv(imd.msoa, file = 'msoa/csv/MSOA11_IMD.csv', row.names = FALSE)

## 3.3. lad ----

imd.lad <-
  imd.lsoa.temp %>% 
  # population weighted average
  dplyr::summarise(imdScore = stats::weighted.mean(x = imdScore, w = POPULATION, na.rm = TRUE),
                   incomeScore = stats::weighted.mean(x = incomeScore, w = POPULATION, na.rm = TRUE),
                   employmentScore = stats::weighted.mean(x = employmentScore, w = POPULATION, na.rm = TRUE),
                   healthScore = stats::weighted.mean(x = healthScore, w = POPULATION, na.rm = TRUE),
                   educationScore = stats::weighted.mean(x = educationScore, w = POPULATION, na.rm = TRUE),
                   housingScore = stats::weighted.mean(x = housingScore, w = POPULATION, na.rm = TRUE),
                   crimeScore = stats::weighted.mean(x = crimeScore, w = POPULATION, na.rm = TRUE),
                   environmentScore = stats::weighted.mean(x = environmentScore, w = POPULATION, na.rm = TRUE),
                   .by = c('LAD11CD', 'YEAR', 'imdYear')) %>% 
  dplyr::arrange(YEAR, LAD11CD) %>% 
  dplyr::select(LAD11CD, YEAR, imdYear, 
                imdScore, 
                incomeScore, employmentScore, healthScore, educationScore, housingScore, crimeScore, environmentScore)

setwd(dir.data.organised)
save(imd.lad, file = 'lad/rda/LAD11_IMD.rda')
write.csv(imd.lad, file = 'lad/csv/LAD11_IMD.csv', row.names = FALSE)

# 3. plots ----

## 3.0. function ----

make.plot.imd <- function(data, 
                          spatial.level = NULL, 
                          poly, 
                          lookup = NULL, 
                          title.legend = 'Index of Multiple Deprivation Score'){
  
  # 0. funtion arguments ----
  
  # data <- imd.04.sorted.lsoa
  # spatial.level <- c('LSOA11CD', 'MSOA11CD', 'LAD11CD')[1]
  # poly <- poly.lsoa.england
  # lookup <- lookup.11.21.lsoa
  
  # 1. filter and set ranges ----
  
  imd.year <- data$imdYear %>% unique()
  limits.spatial <- c(0, 100)
  breaks.spatial <- seq(from = limits.spatial[1] , to = limits.spatial[2], by = limits.spatial[2]/4)
  
  # 2. sort data ----
  
  plot.data <-
    data %>% 
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
    ggplot2::geom_sf(data = plot.data.london, aes(fill = imdScore), colour = NA) +
    ggplot2::scale_fill_gradient(name = '',
                                 low = colours.sequential[1], high = colours.sequential[2],
                                 limits = limits.spatial, breaks = breaks.spatial) +
    my.map.theme(legend.position = 'none')
  
  plot <-
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = plot.data, aes(fill = imdScore), colour = NA) +
    ggplot2::scale_fill_gradient(name = title.legend,
                                 guide = guide_colourbar(title.position = 'top'),
                                 low = colours.sequential[1], high = colours.sequential[2],
                                 limits = limits.spatial, breaks = breaks.spatial) +
    # imd year label label in top right
    ggplot2::annotate('text', x = Inf, y = Inf, label = imd.year, 
                      vjust = 1, hjust = 1, color = 'grey', size = 7.5) +
    # add london in seperately
    ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
    ggplot2::annotation_custom(ggplot2::ggplotGrob(plot.london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
    my.map.theme(text = element_text(size = text.size),
                 legend.position = 'bottom',
                 legend.key.width = unit(2, 'cm'),
                 legend.title = element_text(hjust = 0.5))
  
  # 4. return ----
  
  return(plot)
  
  
}

## 3.1. gif ----

### 3.1.1. lsoa ----

imd.data.list.lsoa <- list(imd.lsoa %>% dplyr::filter(imdYear == 2004),
                           imd.lsoa %>% dplyr::filter(imdYear == 2007),
                           imd.lsoa %>% dplyr::filter(imdYear == 2010),
                           imd.lsoa %>% dplyr::filter(imdYear == 2015),
                           imd.lsoa %>% dplyr::filter(imdYear == 2019))

imd.plots.lsoa <- lapply(X = imd.data.list.lsoa,
                         FUN = make.plot.imd,
                         spatial.level = 'LSOA11CD',
                         poly = poly.lsoa.england,
                         lookup = lookup.11 %>% dplyr::select(LSOA11CD, LAD11CD) %>% unique())

setwd(dir.res.exploration)
animation::saveGIF(
  expr = {for (i in 1:length(imd.plots.lsoa)) { print(imd.plots.lsoa[[i]]) } }, 
  movie.name = paste0(dir.res.exploration, '/gif/lsoa/LSOA11_IMD.gif'), 
  interval = 0.5,
  ani.width = width * 100,
  ani.height = height * 100)

### 3.1.2. msoa ----

imd.data.list.msoa <- list(imd.msoa %>% dplyr::filter(imdYear == 2004),
                           imd.msoa %>% dplyr::filter(imdYear == 2007),
                           imd.msoa %>% dplyr::filter(imdYear == 2010),
                           imd.msoa %>% dplyr::filter(imdYear == 2015),
                           imd.msoa %>% dplyr::filter(imdYear == 2019))

imd.plots.msoa <- lapply(X = imd.data.list.msoa,
                         FUN = make.plot.imd,
                         spatial.level = 'MSOA11CD',
                         poly = poly.msoa.england,
                         lookup = lookup.11 %>% dplyr::select(MSOA11CD, LAD11CD) %>% unique())

setwd(dir.res.exploration)
animation::saveGIF(
  expr = {for (i in 1:length(imd.plots.msoa)) { print(imd.plots.msoa[[i]]) } }, 
  movie.name = paste0(dir.res.exploration, '/gif/msoa/MSOA11_IMD.gif'), 
  interval = 0.5,
  ani.width = width * 100,
  ani.height = height * 100)

### 3.1.2. lad ----

imd.data.list.lad <- list(imd.lad %>% dplyr::filter(imdYear == 2004),
                          imd.lad %>% dplyr::filter(imdYear == 2007),
                          imd.lad %>% dplyr::filter(imdYear == 2010),
                          imd.lad %>% dplyr::filter(imdYear == 2015),
                          imd.lad %>% dplyr::filter(imdYear == 2019))

imd.plots.lad <- lapply(X = imd.data.list.lad,
                        FUN = make.plot.imd,
                        spatial.level = 'LAD11CD',
                        poly = poly.lad.england)

setwd(dir.res.exploration)
animation::saveGIF(
  expr = {for (i in 1:length(imd.plots.lad)) { print(imd.plots.lad[[i]]) } }, 
  movie.name = paste0(dir.res.exploration, '/gif/lad/LAD11_IMD.gif'), 
  interval = 0.5,
  ani.width = width * 100,
  ani.height = height * 100)

## 3.2. plot (average) ----

### 3.2.1. lsoa ----

imd.average.plot.lsoa <- 
  imd.lsoa %>% 
  # average
  dplyr::summarise(imdScore = imdScore %>% mean(),
                   .by = 'LSOA11CD') %>% 
  make.plot.imd(data = .,
                spatial.level = 'LSOA11CD',
                poly = poly.lsoa.england,
                lookup = lookup.11 %>% dplyr::select(LSOA11CD, LAD11CD) %>% unique(),
                title.legend = '')

setwd(dir.res.exploration)
# png
ggplot2::ggsave(plot = imd.average.plot.lsoa,
                filename = 'png/lsoa/LSOA11_IMD_AVERAGE.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = imd.average.plot.lsoa,
                filename = 'eps/lsoa/LSOA11_IMD_AVERAGE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)

### 3.2.2. msoa ----

imd.average.plot.msoa <- 
  imd.msoa %>% 
  # average
  dplyr::summarise(imdScore = imdScore %>% mean(),
                   .by = 'MSOA11CD') %>% 
  make.plot.imd(data = .,
                spatial.level = 'MSOA11CD',
                poly = poly.msoa.england,
                lookup = lookup.11 %>% dplyr::select(MSOA11CD, LAD11CD) %>% unique(),
                title.legend = '')

setwd(dir.res.exploration)
# png
ggplot2::ggsave(plot = imd.average.plot.msoa,
                filename = 'png/msoa/MSOA11_IMD_AVERAGE.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = imd.average.plot.msoa,
                filename = 'eps/msoa/MSOA11_IMD_AVERAGE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)

### 3.2.3. lad ----

imd.average.plot.lad <- 
  imd.lad %>% 
  # average
  dplyr::summarise(imdScore = imdScore %>% mean(),
                   .by = 'LAD11CD') %>% 
  make.plot.imd(data = .,
                spatial.level = 'LAD11CD',
                poly = poly.lad.england,
                title.legend = '')

setwd(dir.res.exploration)
# png
ggplot2::ggsave(plot = imd.average.plot.lad,
                filename = 'png/lad/LAD11_IMD_AVERAGE.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = imd.average.plot.lad,
                filename = 'eps/lad/LAD11_IMD_AVERAGE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)

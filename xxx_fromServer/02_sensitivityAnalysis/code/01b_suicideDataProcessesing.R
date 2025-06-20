# 0 set up ----

## 0.1 packages ----

if(!require('tidyverse')) {
  install.packages('tidyverse', dep = TRUE)
}

## 0.2. directories ----

# retrieve directories
code.path <- rstudioapi::getActiveDocumentContext()$path
dir.home <- sub('(Policy_Evaluation/02_sensitivityAnalysis).*', '\\1', code.path)
dir.code <- paste0(dir.home, '/code')
dir.data <- paste0(dir.home, '/data')
dir.res <- paste0(dir.home, '/results')
dir.data.organised <- paste0(dir.data, '/organised')
dir.data.spatial <- paste0(dir.data, '/spatial')
dir.data.suicide <- paste0(dir.data, '/suicide')

## 0.3. import ----

### 0.3.1. functions ----

setwd(dir.code)
source('00_functions.R')

### 0.3.2. organised data ----

setwd(dir.data.organised)
load('ENGLAND_SPATIAL_INFORMATION_LSOA_MSOA_LAD.rda')
load('MSOA11_COVARIATES.rda')
load('MSOA11_POPULATION_TOTAL.rda')

# suicide data
setwd(dir.data.suicide)
suicide.data.extract <- 
  read.csv('E20241210_DEATHS_CONNOR.csv') %>% 
  # SENSITIVITY ANALYSIS STARTS HERE!!!
  filter(ICD10_X60_X84 == 1)

### 0.3.3. look up ----

setwd(dir.data.spatial)
lookup.lsoa.msoa <- read.csv('OA11_LSOA11_MSOA11_LAD11_lookUp.csv')
lookup.msoa.rgn <-  
  read.csv('MSOA11_LAD11_RGN11_lookUp.csv') %>% 
  # renaming
  ## RGN21NM - for plotting as cannot find RGN11 poly and nothing has changed
  dplyr::rename(RGN21NM = 'RGN11NM')

## 0.4. labelling data frames ----

year.min <- 2002
year.max <- 2022

age.label <- c('[0, 5)', '[5, 15)', '[15, 25)', '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)', '[65, 75)', '[75, 85)', '85+')
sex.label <- c('MALES', 'FEMALES')

# 1. msoa ----

## 1.0. generic spatiotemporal labelling ----

space.label <- 
  data.frame(MSOA11CD = info.msoa$MSOA11CD) %>% 
  dplyr::mutate(space_id = MSOA11CD %>% as.factor() %>% as.numeric())

time.label <-
  data.frame(YEAR = year.min:year.max) %>% 
  dplyr::mutate(year_id = 1:n())

space.year.label <-
  expand.grid(MSOA11CD = info.msoa$MSOA11CD,
              YEAR = year.min:year.max) %>% 
  dplyr::left_join(., space.label, by = c('MSOA11CD')) %>% 
  dplyr::left_join(., time.label, by = c('YEAR')) %>% 
  dplyr::mutate(spaceYear_id = interaction(space_id, year_id) %>% as.numeric())

### 1.1. suicides by msoa ----

suicide.data.msoa <-
  suicide.data.extract %>% 
  # change labelling for age to be nicer
  dplyr::rename(LSOA11CD = LSOA11) %>% 
  dplyr::mutate(AGE_CLASS = dplyr::case_when(AGE_CLASS == '0_4' ~ age.label[1],
                                             AGE_CLASS == '5_14' ~ age.label[2],
                                             AGE_CLASS == '15_24' ~ age.label[3],
                                             AGE_CLASS == '25_34' ~ age.label[4],
                                             AGE_CLASS == '35_44' ~ age.label[5],
                                             AGE_CLASS == '45_54' ~ age.label[6],
                                             AGE_CLASS == '55_64' ~ age.label[7],
                                             AGE_CLASS == '65_74' ~ age.label[8],
                                             AGE_CLASS == '75_84' ~ age.label[9],
                                             AGE_CLASS == '85' ~ age.label[10],
                                             TRUE ~ 'NA'),
                SEX = SEX %>% factor(., labels = sex.label) %>% as.character()) %>%
  # FILTERING
  ## ENGLAND // 15+
  dplyr::filter(stringr::str_detect(LSOA11CD, '^E'),
                AGE_CLASS %in% age.label[-c(1,2)]) %>%
  # joing msoa labels
  dplyr::left_join(., 
                   # need to distinct as this is the lookup including OA
                   lookup.lsoa.msoa %>% dplyr::select(LSOA11CD, MSOA11CD) %>% dplyr::distinct(),
                   # relationship = 'many-to-many',
                   by = 'LSOA11CD') %>% 
  # aggregate over lsoa to msoa
  dplyr::summarise(Y = sum(TOTAL),
                   .by = c('MSOA11CD', 'YEAR', 'SEX', 'AGE_CLASS'))


### 1.2. balanced data ----

# all combinations of MSOA-YEAR-MONTH-AGE-SEX
data.balanced <- 
  tidyr::expand_grid(MSOA11CD = info.msoa$MSOA11CD,
                     YEAR = year.min:year.max,
                     AGE_CLASS = age.label[-c(1,2)],
                     SEX = sex.label)

### 1.3. reference data ----

# want AGE-SEX specific rates per MSOA
data.reference <-
  data.balanced %>%
  # join suicide data to balanced data
  dplyr::left_join(., suicide.data.msoa, 
                   by = c('MSOA11CD', 'YEAR', 'AGE_CLASS', 'SEX')) %>% 
  ## replace na's with 0
  dplyr::mutate(Y = Y %>% tidyr::replace_na(., replace = 0)) %>% 
  # join (yearly) population totals
  dplyr::left_join(., population.msoa,
                   by = c('MSOA11CD', 'YEAR', 'AGE_CLASS', 'SEX')) %>% 
  # sum per MSOA-AGE-SEX
  dplyr::summarise(Y = sum(Y),
                   N = sum(POPULATION),
                   .by = c('AGE_CLASS', 'SEX')) %>% 
  # AGE-SEX specific rates
  dplyr::mutate(R = Y/N)

### 1.4. final data ----

data.final <- 
  data.balanced %>%
  # join suicide data to balanced data
  ## replace na's with 0
  dplyr::left_join(., suicide.data.msoa, 
                   by = c('MSOA11CD', 'YEAR', 'AGE_CLASS', 'SEX')) %>% 
  dplyr::mutate(Y = Y %>% tidyr::replace_na(., replace = 0)) %>%
  # join (monthly) population total
  dplyr::left_join(., population.msoa,
                   by = c('MSOA11CD', 'YEAR', 'AGE_CLASS', 'SEX')) %>% 
  # join age-sex rates
  dplyr::left_join(., data.reference %>% dplyr::select(AGE_CLASS, SEX, R),
                   by = c('AGE_CLASS', 'SEX')) %>% 
  # expected = population * age-sex specific rate
  dplyr::mutate(E = POPULATION * R) %>% 
  # summarise over age-sex as adjusted for
  dplyr::summarise(Y = sum(Y),
                   N = sum(POPULATION),
                   E = sum(E),
                   .by = c('MSOA11CD', 'YEAR')) %>% 
  dplyr::mutate(adjustedRate = Y/E) %>%
  # join additional covariates
  ## region 
  dplyr::left_join(., lookup.msoa.rgn %>% dplyr::select(MSOA11CD, RGN21NM),
                   by = c('MSOA11CD')) %>%
  ## socio-environmental
  dplyr::left_join(., covariates.msoa, 
                   by = c('MSOA11CD', 'YEAR')) %>%   
  ## correct space, time, space-time labels
  dplyr::left_join(., space.year.label, by = c('MSOA11CD', 'YEAR'))

### 1.5. check ----

data.final$Y %>% sum() == data.final$E %>% sum()
data.final %>% is.na() %>% sum()

### 1.6. save ----

setwd(dir.data.organised)
save(data.final, file = 'MSOA11_SUICIDES_FINAL.rda')

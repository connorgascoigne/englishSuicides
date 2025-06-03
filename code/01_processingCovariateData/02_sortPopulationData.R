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

## 0.4. spatial ----

setwd(dir.masterData.spatial)

### 0.4.1. polygon ----

poly.lsoa <- sf::st_read(dsn = dir.masterData.spatial, layer = 'ONS11_LSOA')
poly.msoa <- sf::st_read(dsn = dir.masterData.spatial, layer = 'ONS11_MSOA')
poly.lad <- sf::st_read(dsn = dir.masterData.spatial, layer = 'ONS11_LAD')

poly.lsoa.england <- poly.lsoa %>%  dplyr::filter(str_detect(LSOA11CD, '^E'))
poly.msoa.england <- poly.msoa %>%  dplyr::filter(str_detect(MSOA11CD, '^E'))
poly.lad.england <- poly.lad %>%  dplyr::filter(str_detect(LAD11CD, '^E'))

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

### 0.6.1. males ----

setwd(dir.masterData.population)
totals.02.males <- readxl::read_xls('LSOA11_2002_2006_MALES_POPULATION_TOTAL.xls', sheet = 'Mid-2002')
totals.03.males <- readxl::read_xls('LSOA11_2002_2006_MALES_POPULATION_TOTAL.xls', sheet = 'Mid-2003')
totals.04.males <- readxl::read_xls('LSOA11_2002_2006_MALES_POPULATION_TOTAL.xls', sheet = 'Mid-2004')
totals.05.males <- readxl::read_xls('LSOA11_2002_2006_MALES_POPULATION_TOTAL.xls', sheet = 'Mid-2005')
totals.06.males <- readxl::read_xls('LSOA11_2002_2006_MALES_POPULATION_TOTAL.xls', sheet = 'Mid-2006')
totals.07.males <- readxl::read_xls('LSOA11_2007_2011_MALES_POPULATION_TOTAL.xls', sheet = 'Mid-2007')
totals.08.males <- readxl::read_xls('LSOA11_2007_2011_MALES_POPULATION_TOTAL.xls', sheet = 'Mid-2008')
totals.09.males <- readxl::read_xls('LSOA11_2007_2011_MALES_POPULATION_TOTAL.xls', sheet = 'Mid-2009')
totals.10.males <- readxl::read_xls('LSOA11_2007_2011_MALES_POPULATION_TOTAL.xls', sheet = 'Mid-2010')
totals.11.males <- readxl::read_xls('LSOA11_2007_2011_MALES_POPULATION_TOTAL.xls', sheet = 'Mid-2011')
totals.12.males <- readxl::read_xls('LSOA11_2012_POPULATION_TOTAL.xls', sheet = 'Mid-2012 Males', skip = 3)
totals.13.males <- readxl::read_xls('LSOA11_2013_POPULATION_TOTAL.xls', sheet = 'Mid-2013 Males', skip = 3)
totals.14.males <- readxl::read_xls('LSOA11_2014_POPULATION_TOTAL.xls', sheet = 'Mid-2014 Males', skip = 3)
totals.15.males <- readxl::read_xls('LSOA11_2015_POPULATION_TOTAL.xls', sheet = 'Mid-2015 Males', skip = 3)
totals.16.males <- readxl::read_xls('LSOA11_2016_POPULATION_TOTAL.xls', sheet = 'Mid-2016 Males', skip = 3)
totals.17.males <- readxl::read_xls('LSOA11_2017_POPULATION_TOTAL.xls', sheet = 'Mid-2017 Males', skip = 3)
totals.18.males <- readxl::read_xlsx('LSOA11_2018_POPULATION_TOTAL.xlsx', sheet = 'Mid-2018 Males', skip = 3)
totals.19.males <- readxl::read_xlsx('LSOA11_2019_POPULATION_TOTAL.xlsx', sheet = 'Mid-2019 Males', skip = 3)
totals.20.males <- readxl::read_xlsx('LSOA11_2020_POPULATION_TOTAL.xlsx', sheet = 'Mid-2020 Males', skip = 3)
totals.21.males <- readxl::read_xlsx('LSOA21_2021_2022_POPULATION_TOTAL.xlsx', sheet = 'Mid-2021 LSOA 2021', skip = 3)
totals.22.males <- readxl::read_xlsx('LSOA21_2021_2022_POPULATION_TOTAL.xlsx', sheet = 'Mid-2022 LSOA 2021', skip = 3)

### 0.6.2. females ----

setwd(dir.masterData.population)
totals.02.females <- readxl::read_xls('LSOA11_2002_2006_FEMALES_POPULATION_TOTAL.xls', sheet = 'Mid-2002')
totals.03.females <- readxl::read_xls('LSOA11_2002_2006_FEMALES_POPULATION_TOTAL.xls', sheet = 'Mid-2003')
totals.04.females <- readxl::read_xls('LSOA11_2002_2006_FEMALES_POPULATION_TOTAL.xls', sheet = 'Mid-2004')
totals.05.females <- readxl::read_xls('LSOA11_2002_2006_FEMALES_POPULATION_TOTAL.xls', sheet = 'Mid-2005')
totals.06.females <- readxl::read_xls('LSOA11_2002_2006_FEMALES_POPULATION_TOTAL.xls', sheet = 'Mid-2006')
totals.07.females <- readxl::read_xls('LSOA11_2007_2011_FEMALES_POPULATION_TOTAL.xls', sheet = 'Mid-2007')
totals.08.females <- readxl::read_xls('LSOA11_2007_2011_FEMALES_POPULATION_TOTAL.xls', sheet = 'Mid-2008')
totals.09.females <- readxl::read_xls('LSOA11_2007_2011_FEMALES_POPULATION_TOTAL.xls', sheet = 'Mid-2009')
totals.10.females <- readxl::read_xls('LSOA11_2007_2011_FEMALES_POPULATION_TOTAL.xls', sheet = 'Mid-2010')
totals.11.females <- readxl::read_xls('LSOA11_2007_2011_FEMALES_POPULATION_TOTAL.xls', sheet = 'Mid-2011')
totals.12.females <- readxl::read_xls('LSOA11_2012_POPULATION_TOTAL.xls', sheet = 'Mid-2012 Females', skip = 3)
totals.13.females <- readxl::read_xls('LSOA11_2013_POPULATION_TOTAL.xls', sheet = 'Mid-2013 Females', skip = 3)
totals.14.females <- readxl::read_xls('LSOA11_2014_POPULATION_TOTAL.xls', sheet = 'Mid-2014 Females', skip = 3)
totals.15.females <- readxl::read_xls('LSOA11_2015_POPULATION_TOTAL.xls', sheet = 'Mid-2015 Females', skip = 3)
totals.16.females <- readxl::read_xls('LSOA11_2016_POPULATION_TOTAL.xls', sheet = 'Mid-2016 Females', skip = 3)
totals.17.females <- readxl::read_xls('LSOA11_2017_POPULATION_TOTAL.xls', sheet = 'Mid-2017 Females', skip = 3)
totals.18.females <- readxl::read_xlsx('LSOA11_2018_POPULATION_TOTAL.xlsx', sheet = 'Mid-2018 Females', skip = 3)
totals.19.females <- readxl::read_xlsx('LSOA11_2019_POPULATION_TOTAL.xlsx', sheet = 'Mid-2019 Females', skip = 3)
totals.20.females <- readxl::read_xlsx('LSOA11_2020_POPULATION_TOTAL.xlsx', sheet = 'Mid-2020 Females', skip = 3)
totals.21.females <- readxl::read_xlsx('LSOA21_2021_2022_POPULATION_TOTAL.xlsx', sheet = 'Mid-2021 LSOA 2021', skip = 3)
totals.22.females <- readxl::read_xlsx('LSOA21_2021_2022_POPULATION_TOTAL.xlsx', sheet = 'Mid-2022 LSOA 2021', skip = 3)

# 1. sort population ----

## 1.0.labels ----

year.min <- 2002
year.max <- 2022
label.year <- year.min:year.max
label.age <- c(as.character(0:89), '90plus')
label.ageClass <- c('[0, 5)', '[5, 15)', '[15, 25)', '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)', '[65, 75)', '[75, 85)', '85+')
label.sex <- c('MALES', 'FEMALES')
template.age <-
  data.frame(AGE = label.age) %>% 
  dplyr::mutate(AGE_NUM = dplyr::if_else(AGE == '90plus', 90, as.numeric(AGE)),
                AGE_CLASS = case_when(AGE_NUM < 5   ~ label.ageClass[1],
                                      AGE_NUM < 15  ~ label.ageClass[2],
                                      AGE_NUM < 25  ~ label.ageClass[3],
                                      AGE_NUM < 35  ~ label.ageClass[4],
                                      AGE_NUM < 45  ~ label.ageClass[5],
                                      AGE_NUM < 55  ~ label.ageClass[6],
                                      AGE_NUM < 65  ~ label.ageClass[7],
                                      AGE_NUM < 75  ~ label.ageClass[8],
                                      AGE_NUM < 85  ~ label.ageClass[9],
                                      AGE_NUM >= 85 ~ label.ageClass[10])) %>%
  dplyr::select(-AGE_NUM)

## 1.1. merge population files ----

data.list <-
  data.frame(YEAR = year.min:year.max,
             MALE = paste0('totals.', sprintf('%02d', 2:22), '.males'),
             FEMALE = paste0('totals.', sprintf('%02d', 2:22), '.females'))

population.list <- list()
for(i in seq_along(year.min:year.max)){
  
  # i <- 20
  
  YEAR <- data.list$YEAR[i]
  cat(paste0('Year: ', YEAR, '\n'))
  
  if (YEAR %in% 2002:2011){
    
    male <-
      get(data.list[i, 'MALE']) %>%
      data.table::as.data.table() %>%
      .[, .SD, .SDcols = c('LSOA11CD', paste0('m', 0:89), 'm90plus')] %>%
      data.table::melt(id.vars = 'LSOA11CD', 
                       variable.name = 'AGE', 
                       value.name = 'POPULATION') %>%
      .[, `:=`(
        AGE = gsub('^m', '', AGE), 
        SEX = 'MALES',
        YEAR = YEAR
      )]
    
    female <-
      get(data.list[i, 'FEMALE']) %>%
      data.table::as.data.table() %>%
      .[, .SD, .SDcols = c('LSOA11CD', paste0('f', 0:89), 'f90plus')] %>%
      data.table::melt(id.vars = 'LSOA11CD', 
                       variable.name = 'AGE', 
                       value.name = 'POPULATION') %>%
      .[, `:=`(
        AGE = gsub('^f', '', AGE), 
        SEX = 'FEMALES',
        YEAR = YEAR
      )]
    
  } else if (YEAR %in% 2012:2018) {
    
    male <-
      get(data.list[i, 'MALE']) %>%
      data.table::as.data.table() %>%
      .[, LSOA11CD := `Area Codes`] %>%
      .[, .SD, .SDcols = c('LSOA11CD', as.character(0:89), '90+')] %>%
      data.table::melt(id.vars = 'LSOA11CD', 
                       variable.name = 'AGE', 
                       value.name = 'POPULATION') %>%
      .[, `:=`(
        AGE = data.table::fifelse(AGE == '90+', '90plus', as.character(AGE)),
        SEX = 'MALES',
        YEAR = YEAR
      )]
    
    female <- 
      get(data.list[i, 'FEMALE']) %>%
      data.table::as.data.table() %>%
      .[, LSOA11CD := `Area Codes`] %>%
      .[, .SD, .SDcols = c('LSOA11CD', as.character(0:89), '90+')] %>%
      data.table::melt(id.vars = 'LSOA11CD', 
                       variable.name = 'AGE', 
                       value.name = 'POPULATION') %>%
      .[, `:=`(
        AGE = data.table::fifelse(AGE == '90+', '90plus', as.character(AGE)),
        SEX = 'FEMALES',
        YEAR = YEAR
      )]
    
  } else if (YEAR %in% 2019:2020){
    
    male <-
      get(data.list[i, 'MALE']) %>%
      data.table::as.data.table() %>%
      .[, LSOA11CD := `LSOA Code`] %>%
      .[, .SD, .SDcols = c('LSOA11CD', as.character(0:89), '90+')] %>%
      data.table::melt(id.vars = 'LSOA11CD', 
                       variable.name = 'AGE', 
                       value.name = 'POPULATION') %>%
      .[, `:=`(
        AGE = data.table::fifelse(AGE == '90+', '90plus', as.character(AGE)),
        SEX = 'MALES',
        YEAR = YEAR
      )]
    
    female <-
      get(data.list[i, 'FEMALE']) %>%
      data.table::as.data.table() %>%
      .[, LSOA11CD := `LSOA Code`] %>%
      .[, .SD, .SDcols = c('LSOA11CD', as.character(0:89), '90+')] %>%
      data.table::melt(id.vars = 'LSOA11CD', 
                       variable.name = 'AGE', 
                       value.name = 'POPULATION') %>%
      .[, `:=`(
        AGE = data.table::fifelse(AGE == '90+', '90plus', as.character(AGE)),
        SEX = 'FEMALES',
        YEAR = YEAR
      )]
    
    
  } else if (YEAR %in% 2021:2022){
    
    lookup.11.21.dt <- 
      lookup.11.21 %>% 
      data.table::as.data.table() %>% 
      unique(by = c('LSOA21CD', 'LSOA11CD')) %>% 
      .[, .(LSOA21CD, LSOA11CD)]
    
    male <- 
      get(data.list[i, 'MALE']) %>%
      data.table::as.data.table() %>%
      .[, LSOA21CD := `LSOA 2021 Code`] %>%
      .[, .SD, .SDcols = c('LSOA21CD', paste0('M', 0:90))] %>%
      data.table::melt(id.vars = 'LSOA21CD', 
                       variable.name = 'AGE', 
                       value.name = 'POPULATION') %>%
      .[, AGE := data.table::fifelse(AGE == 'M90', '90plus', gsub('^M', '', AGE))] %>%
      merge(x = .,
            y = lookup.11.21.dt, 
            by = 'LSOA21CD',
            all.x = TRUE) %>%
      .[, .(POPULATION = sum(POPULATION)), by = .(LSOA11CD, AGE)] %>%
      .[, `:=`(SEX = 'MALES', 
               YEAR = YEAR)]
    
    female <-
      get(data.list[i, 'FEMALE']) %>%
      data.table::as.data.table() %>%
      .[, LSOA21CD := `LSOA 2021 Code`] %>%
      .[, .SD, .SDcols = c('LSOA21CD', paste0('F', 0:90))] %>%
      data.table::melt(id.vars = 'LSOA21CD', 
                       variable.name = 'AGE', 
                       value.name = 'POPULATION') %>%
      .[, AGE := data.table::fifelse(AGE == 'F90', '90plus', gsub('^F', '', AGE))] %>%
      merge(x = .,
            y = lookup.11.21.dt, 
            by = 'LSOA21CD',
            all.x = TRUE) %>%
      .[, .(POPULATION = sum(POPULATION)), by = .(LSOA11CD, AGE)] %>%
      .[, `:=`(SEX = 'FEMALES', 
               YEAR = YEAR)]
    
    
  } else {
    
    stop('Warning: Year not in the correct range')
    
  }
  
  # join male and female
  both <-
    list(male, female) %>% 
    data.table::rbindlist()
  
  num.missing <- both %>% is.na() %>% sum()
  cat(paste0(' Number of misisng values: ', num.missing, '\n'))
  
  population.list[[i]] <- both
  
}

population.temp <- 
  # generic data frame
  data.table::CJ(LSOA11CD = poly.lsoa.england$LSOA11CD,
                 YEAR = label.year,
                 SEX = label.sex,
                 AGE = label.age,
                 unique = TRUE) %>%
  # join population totals
  merge(x = .,
        y = population.list %>% data.table::rbindlist(),
        by = c('LSOA11CD', 'YEAR', 'SEX', 'AGE'),
        all.x = TRUE) %>% 
  # join other geography codes
  merge(x = .,
        y = 
          lookup.11 %>% 
          data.table::as.data.table() %>% 
          unique(by = c('LSOA11CD', 'MSOA11CD', 'LAD11CD')) %>% 
          .[, .(LSOA11CD, MSOA11CD, LAD11CD)],
        by = 'LSOA11CD',
        all.x = TRUE) %>% 
  # join age class 
  merge(x = .,
        y = 
          template.age %>% 
          data.table::as.data.table(),
        by = 'AGE',
        all.x = TRUE) %>% 
  # order columns
  .[, .(LSOA11CD, MSOA11CD, LAD11CD, YEAR, SEX, AGE, AGE_CLASS, POPULATION)]

nrow(poly.lsoa.england)*
  length(label.year)*
  length(label.sex)*
  length(label.age) == 
  nrow(population.temp)

missing.population <-
  population.temp %>% 
  as.data.frame() %>% 
  dplyr::filter(rowSums(is.na(.)) > 0)

# 2. sort by spatial unit ----

## 2.1 lsoa ----

population.lsoa <-
  population.temp %>% 
  # select column 
  .[, .(LSOA11CD, YEAR, SEX, AGE_CLASS, POPULATION)] %>% 
  # summarise 
  .[, .(POPULATION = sum(POPULATION)), by = .(LSOA11CD, YEAR, SEX, AGE_CLASS)] %>% 
  as.data.frame()

missing.lsoa <-
  population.lsoa %>% 
  dplyr::filter(rowSums(is.na(.)) > 0)

setwd(dir.data.organised)
save(population.lsoa, file = 'lsoa/rda/LSOA11_POPULATION_TOTAL.rda')
write.csv(population.lsoa, file = 'lsoa/csv/LSOA11_POPULATION_TOTAL.csv', row.names = FALSE)

## 2.2. msoa ----

population.msoa <-
  population.temp %>% 
  # select column 
  .[, .(MSOA11CD, YEAR, SEX, AGE_CLASS, POPULATION)] %>% 
  # summarise 
  .[, .(POPULATION = sum(POPULATION)), by = .(MSOA11CD, YEAR, SEX, AGE_CLASS)] %>% 
  as.data.frame()

missing.msoa <-
  population.msoa %>% 
  dplyr::filter(rowSums(is.na(.)) > 0)

setwd(dir.data.organised)
save(population.msoa, file = 'msoa/rda/MSOA11_POPULATION_TOTAL.rda')
write.csv(population.msoa, file = 'msoa/csv/MSOA11_POPULATION_TOTAL.csv', row.names = FALSE)

## 2.3. lad ----

population.lad <-
  population.temp %>% 
  # select column 
  .[, .(LAD11CD, YEAR, SEX, AGE_CLASS, POPULATION)] %>% 
  # summarise 
  .[, .(POPULATION = sum(POPULATION)), by = .(LAD11CD, YEAR, SEX, AGE_CLASS)] %>% 
  as.data.frame()

missing.lad <-
  population.lad %>% 
  dplyr::filter(rowSums(is.na(.)) > 0)

setwd(dir.data.organised)
save(population.lad, file = 'lad/rda/LAD11_POPULATION_TOTAL.rda')
write.csv(population.lad, file = 'lad/csv/LAD11_POPULATION_TOTAL.csv', row.names = FALSE)

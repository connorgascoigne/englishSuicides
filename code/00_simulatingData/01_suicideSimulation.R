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
dir.data.suicide <- paste0(dir.data, '/suicide')
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

# 1. simulate suicide data ----

## 1.0. function ----

# random draws from hurdle-poisson distribution
rpois.hp <- function(n, lambda, prob){
  # zero truncated poisson samples
  zero.dens.dpois <- stats::dpois(x = 0, lambda = lambda)
  U.runif <- stats::runif(n = n, min = zero.dens.dpois, max = 1)
  samples.zt.rpois <- stats::qpois(p = U.runif, lambda = lambda)
  # binomial samples
  samples.rbinom <- stats::rbinom(n = n, size = 1, prob = prob)
  # hurdle poisson samples
  dplyr::if_else(samples.rbinom == 1, 0, samples.zt.rpois)
}

## 1.1. data labels ----

year.min <- 2002
year.max <- 2022

label.lsoa <- poly.lsoa.england$LSOA11CD
label.year <- year.min:year.max
label.month <- 1:12
label.sex <- 1:2
label.age <- c('0_4', '5_14', '15_24', '25_34', '35_44', '45_54', '55_64', '65_74', '75_84', '85')

## 1.2. complete data ----

data.temp <- 
  tidyr::expand_grid(LSOA11CD = label.lsoa,
                     YEAR = label.year,
                     MONTH = label.month,
                     SEX = label.sex,
                     AGE_CLASS = label.age)

## 1.3. linear predictor ----

hp.lambda <- 1 # when not zero, average ~1 suicide
hp.prob <- 1 - (112726/165533760) # percent of combinations are 0 (number non-zero in data 112726 --> 112726/165533760)
hp.predicted <- rpois.hp(n = nrow(data.temp), lambda = hp.lambda, prob = hp.prob)

## 1.4. finalise data ----

data.final <-
  data.temp %>% 
  dplyr::mutate(TOTAL = hp.predicted) %>% 
  dplyr::filter(TOTAL > 0)

nrow(data.final)

## 1.5. save ----

setwd(dir.data.suicide)
write.csv(x = data.final, file = 'SUICIDE_DEATHS_SIMULATED.csv', row.names = FALSE)






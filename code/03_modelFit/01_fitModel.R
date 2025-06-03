# 0 set up ----

## 0.1. packages ----

if(!require('tidyverse')) {
  install.packages('tidyverse', dependencies = TRUE)
}
if(!require('INLA')) {
  install.packages('INLA', dependencies = TRUE)
}
if(!require('sn')) {
  install.packages('sn', dependencies = TRUE)
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
dir.res.model.fit <- paste0(dir.res, '/03_modelFit')
## data
dir.masterData <- paste0(sub('(OneDrive - Imperial College London).*', '\\1', dir.path), '/00_masterData')
dir.masterData.spatial <- paste0(dir.masterData, '/onsSpatial')

## 0.3. imports ----

### 0.3.1. functions ----

setwd(dir.code)
source('00_functions.R')

### 0.3.2. organised data ----

setwd(dir.data.organised)
load('ENGLAND_SPATIAL_INFORMATION_LSOA_MSOA_LAD.rda')
load('ENGLAND_AMAT_LSOA_MSOA_LAD.rda')

load('MSOA11_SUICIDES_FINAL.rda')
rm(info.lsoa, mat.lsoa, info.lad, mat.lad) # remove unneeded variables (space)

# 1. data ----

## 1.1. preparation ----

data.model <-
  data.final %>%
  # create id columns for inla 
  dplyr::mutate(deprivation_id = imdScore %>% scale() %>% as.numeric(),
                diversity_id = nonWhite %>% scale() %>% as.numeric(),
                populationDensity_id = populationDensity %>% scale() %>% as.numeric(),
                nighttimeLight_id = nighttimeLight %>% scale() %>% as.numeric(),
                totalRail_id = totalRail %>% scale() %>% as.numeric(),
                totalRoad_id = totalRoad %>% scale() %>% as.numeric(),
                ndvi_id = ndvi %>% scale() %>% as.numeric()) %>% 
  # all _id columns at the end
  dplyr::relocate(., c('space_id', 'year_id', 'spaceYear_id'), .after = tidyr::last_col())

setwd(dir.res.model.fit)
save(data.model, file = 'MODEL_FIT_DATA.rda')

## 1.2. reduce (if testing) ----

REDUCED.DATA <- FALSE

if(REDUCED.DATA) {
  data.model <-
    data.model %>% 
    dplyr::filter(YEAR %in% 2002:2004)
  n.sims <- 10
} else {
  n.sims <- 1000
}

# 2. fit ----

## 2.0. function arguments ----

exposures.all <- c('deprivation', 'diversity', 'populationDensity', 'nighttimeLight', 'totalRail', 'totalRoad', 'ndvi')

## 2.1. model fit ----

fit <- 
  model.fit(data = data.model,
            exposure.names = exposures.all,
            hurdle.model = TRUE,
            family.zero = 'binomial',
            family.count = 'nzpoisson',
            control.compute = list(config = TRUE, dic = TRUE, waic = TRUE, cpo = TRUE),
            aMat = mat.msoa,
            shared.exposures = FALSE)

## 2.2. sampling ----

samples <- INLA::inla.posterior.sample(n = n.sims, result = fit$fit)

## 2.3 extract results ----

# extract posterior sample
temp.results <- 
  INLA::inla.posterior.sample.eval(fun = extract.observations.hp, 
                                   newdata = data.model, 
                                   fit = fit,
                                   # inla.posterior.sample arguments
                                   samples = samples, 
                                   return.matrix = FALSE)

# extract all model parameters
temp.parameters <- 
  INLA::inla.posterior.sample.eval(fun = extract.parameters, 
                                   newdata = data.model, 
                                   fit = fit,
                                   # inla.posterior.sample arguments
                                   samples = samples, 
                                   return.matrix = FALSE)

# name columns
names(temp.results) <-   
  names(temp.parameters) <- 
  paste0('theta:', 1:n.sims)

## 2.4. save ----

# save
setwd(dir.res.model.fit)
save(fit, file = 'MODEL_FIT_MODEL_FIT.rda')
save(samples, file = 'MODEL_FIT_POSTERIOR_SAMPLES.rda')
save(temp.results, temp.parameters, file = 'MODEL_FIT_EXTRACTED_RESULTS.rda')

## 2.5. clear ----

rm(fit, 
   samples,
   temp.results,
   temp.parameters)
gc()


# 0 set up ----

## 0.1. packages ----

if(!require('tidyverse')) {
  install.packages('tidyverse', dependencies = TRUE)
}
if(!require('xtable')) {
  install.packages('xtable', dependencies = TRUE)
}
if(!require('INLA')) {
  install.packages('INLA', dependencies = TRUE)
}
if(!require('reshape2')) {
  install.packages('reshape2', dep = TRUE)
}
if(!require('animation')) {
  install.packages('animation', dep = TRUE)
}

## 0.2. directories ----

# retrieve directories
code.path <- rstudioapi::getActiveDocumentContext()$path
dir.home <- sub('(Policy_Evaluation/01b_updatedAnalysis).*', '\\1', code.path)
dir.code <- paste0(dir.home, '/code')
dir.data <- paste0(dir.home, '/data')
dir.res <- paste0(dir.home, '/results')
dir.data.organised <- paste0(dir.data, '/organised')
dir.data.spatial <- paste0(dir.data, '/spatial')
dir.res.covariate <- paste0(dir.res, '/01b_covariateSpecification')

## 0.3. imports ----

### 0.3.1. functions ----

setwd(dir.code)
source('00_functions.R')

### 0.3.2. organised data ----

setwd(dir.data.organised)
load('ENGLAND_SPATIAL_INFORMATION_LSOA_MSOA_LAD.rda')
# load('ENGLAND_AMAT_LSOA_MSOA_LAD.rda'); rm(lsoa.info, lsoa.mat, lad.info, lad.mat); gc() # remove unneeded variables (space)
load('MSOA11_SUICIDES_FINAL.rda')

## 0.4. plot arguments ----

# plot arguments
text.size <- 20
width <- height <- 10

# colour schemes
colours.diverging <- c('#007D5E', '#E9E0C8', '#C13E2E')
colours.exceedance <- c('#A0D6D0', '#009B92', '#00768D')
colours.sequential <- c('#E9E0C8', '#A175A7')

## 0.4. formatting arguments ----

### 0.4.1. plot/table ----

# saving
height <- width <- 10
text.size <- 20

# colour schemes
# colours.diverging <- c('#007D5E', '#E9E0C8', '#C13E2E')
colours.diverging <- c('#008B00', '#E9E0C8', '#CD0000')
colours.diverging.2 <- c('#0000CD', '#E9E0C8', '#CD0000')
colours.exceedance <- c('#A0D6D0', '#009B92', '#00768D')
colours.sequential <- c('#E9E0C8', '#7D26CD')

# number of digits for table
digits.table <- 4

### 0.4.2. exposure ----

exposure.data.frame <-
  data.frame(name.code  = c('imdScore', 'nonWhite', 'populationDensity', 'nighttimeLight', 'totalRail', 'totalRoad', 'ndvi'),
             name.inla = c('deprivation', 'diversity','populationDensity', 'nighttimeLight', 'totalRail', 'totalRoad', 'ndvi'),
             name.inla.id  = paste0(c('deprivation', 'diversity','populationDensity', 'nighttimeLight', 'totalRail', 'totalRoad', 'ndvi'), '_id'),
             name.label = c('Deprivation', 'Ethnic Density', 'Population Density', 'Light Pollution', 'Railway Network Density', 'Road Network Density', 'Greenspace'),
             name.save = c('DEPRIVATION', 'DIVERSITY', 'POPULATION_DENSITY', 'NIGHTTIME_LIGHT', 'RAILWAY_NETWORK_DENSITY', 'ROAD_NETWORK_DENSITY', 'NDVI'))

## 0.5. final data organisation ----

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

# 1. histogram ----

## 1.0. function ----

make.plot.covariateHistogram <- function(covariate, 
                                         data){
  
  # 0. function arguments ----
  
  # covariate <- 'Deprivation'
  # data <- univariate.histogram.data
  
  # 1. data filter ----
  
  plot.data <- 
    data %>% 
    dplyr::filter(parameter == covariate)
  
  # 2. plot ----
  
  plot <- 
    ggplot2::ggplot(data = plot.data, 
                    aes(x = value, group = parameter)) +
    ggplot2::geom_histogram(bins = 250, colour = 'black', fill = 'white') +
    ggplot2::labs(x = '', y = '') +
    my.theme(text = ggplot2::element_text(size = text.size))
  
  # 3. return ----
  
  return(plot)
  
  
}

## 1.1. data sort ---- 

univariate.histogram.data <-
  data.model %>% 
  dplyr::select(exposure.data.frame$name.inla.id) %>% 
  tidyr::pivot_longer(., 
                      cols = dplyr::everything(),
                      values_to = 'value',
                      names_to = 'parameter') %>% 
  dplyr::mutate(parameter = parameter %>% factor(., levels = exposure.data.frame$name.inla.id, labels = exposure.data.frame$name.label))

## 1.2. plots ----

histogram.all <-
  lapply(X = exposure.data.frame$name.label,
         FUN = function(x){
           make.plot.covariateHistogram(covariate = x,
                                        data = univariate.histogram.data)
         })

## 1.3. save ----

setwd(dir.res.covariate)
# png
for(i in 1:length(histogram.all)){
  
  ggplot2::ggsave(plot = 
                    if(exposure.data.frame$name.save[i] == 'NIGHTTIME_LIGHT'){
                      histogram.all[[i]] + ggplot2::labs(x = '', y = 'Count')
                    } else {
                      histogram.all[[i]]
                    },
                  filename = paste0('png/HISTOGRAM_',
                                    exposure.data.frame$name.save[i],
                                    '.png'),
                  width = width, 
                  height = height)
  
}
# eps
for(i in 1:length(histogram.all)){
  
  ggplot2::ggsave(plot = 
                    if(exposure.data.frame$name.save[i] == 'NIGHTTIME_LIGHT'){
                      histogram.all[[i]] + ggplot2::labs(x = '', y = 'Count')
                    } else {
                      histogram.all[[i]]
                    },
                  filename = paste0('eps/HISTOGRAM_',
                                    exposure.data.frame$name.save[i],
                                    '.eps'),
                  device = 'eps',
                  dpi = 1200,
                  width = width, 
                  height = height)
  
}

# 2. covariate-to-covariate correlation ----

## 2.0. function ----

make.plot.correlation <- function(year = NULL,
                                  data,
                                  covariates.id = paste0(c('deprivation', 'diversity', 'populationDensity', 'nighttimeLight', 'totalRail', 'totalRoad', 'ndvi'), '_id'),
                                  covariates.label = c('Deprivation', 'Ethnic Density', 'Population Density', 'Light Pollution', 'Railway Network Density', 'Road Network Density', 'Greenspace'),
                                  title.legend = 'Correlation'){
  
  # 0. function arguments ----
  
  # year <- 2002
  # data <- data.model
  # covariates.id <- paste0(c('deprivation', 'diversity', 'populationDensity', 'nighttimeLight', 'totalRail', 'totalRoad', 'ndvi'), '_id')
  # covariates.label <- c('Deprivation', 'Ethnic Density', 'Population Density', 'Light Pollution', 'Railway Network Density', 'Road Network Density', 'Greenspace')
  # title.legend <- 'Pearsons Correlation'
  
  # 1. limits ----
  
  limits.corr <- c(-1, 1)
  breaks.corr <- c(-1, 0, 1)
  
  # 2. data sort ----
  
  ## 2.1. filter and select covariates ----
  
  correlation.matrix.data.temp <-
    data %>% 
    # # filter by year IF needed
    {
      if(!is.null(year)) {
        dplyr::filter(., YEAR == year)
      } else {
        .
      }
    } %>% 
    dplyr::select(covariates.id) %>%
    cor(., method = 'pearson')
  
  
  ## 2.2. make a lower triangle ----
  
  get.lower.triangle <- function(matrix){
    matrix[lower.tri(matrix)] <- NA
    return(matrix)
  }
  
  correlation.matrix.data <-
    correlation.matrix.data.temp %>% 
    get.lower.triangle()
  
  ## 2.3. format for plot ----
  
  plot.data <-
    correlation.matrix.data %>% 
    reshape2::melt(., varnames = c('row', 'column'), value.name = 'correlation', na.rm = TRUE) %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate(row = row %>% factor(., levels = covariates.id, labels = covariates.label),
                  column = column %>% factor(., levels = covariates.id, labels = covariates.label))
  
  # 3. plot ----
  
  plot <-
    ggplot2::ggplot(data = plot.data, aes(x = row, y = column, fill = correlation, label = sprintf(paste0('%.', 2, 'f'), round(correlation, digits = 2)) )) +
    ggplot2::geom_tile(colour = 'white') +
    ggplot2::scale_fill_gradient2(name = title.legend,
                                  guide = ggplot2::guide_colorbar(title.pisition = 'top'),
                                  midpoint = 0,
                                  low = colours.diverging[3], 
                                  mid = colours.diverging[2],
                                  high = colours.diverging[1],
                                  limits = limits.corr,
                                  breaks = breaks.corr) +
    ggplot2::geom_text(colour = 'black', size = 5) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(limits = plot.data$row %>% levels() %>% rev(),
                              labels = plot.data$row %>% levels() %>% rev()) +
    ggplot2::scale_y_discrete(labels = plot.data$row %>% levels()) +
    ggplot2::labs(y = '', x = '') +
    ggplot2::annotate('text', x = -Inf, y = -Inf, label = year, vjust = -1, hjust = -1, colour = 'grey', size = 7.5) +
    ggplot2::theme(text = ggplot2::element_text(size = 20),
                   legend.position = 'bottom',
                   legend.key.width = unit(2, 'cm'),
                   legend.title = ggplot2::element_text(hjust = 0.5),
                   axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1),
                   axis.ticks = element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(hjust = 0),
                   legend.key = ggplot2::element_rect(fill = NA, colour = NA))
  
  # 4. return ----
  
  return(plot)
  
}

## 2.1. gif ----

corr.plots <- 
  lapply(X = 2002:2022,
         make.plot.correlation,
         data.model)

setwd(paste0(dir.res.covariate, '/gif'))
animation::saveGIF(
  expr = {for(i in 1:length(corr.plots)) {print(corr.plots[[i]])}},
  move.name = 'CORRELATION_MATRIX.gif',
  interval = 0.5,
  ani.width = width * 100,
  ani.height = height * 100)

## 2.2 overall ----

corr.plot <-
  make.plot.correlation(data = data.model,
                        title.legend = '')

setwd(dir.res.covariate)
ggplot2::ggsave(plot = corr.plot,
                filename = 'png/CORRELATION_MATRIX.png',
                width = width, 
                height = height)
ggplot2::ggsave(plot = corr.plot,
                filename = 'eps/CORRELATION_MATRIX.eps',
                device = 'eps',
                dpi = 1200,
                width = width, 
                height = height)

## 2.3. imd-to-subdomain correlation ----

### 2.3.1. data sort ----

data.model.imd <-
  data.final %>%
  # create id columns for inla 
  dplyr::mutate(imd_id = imdScore %>% scale() %>% as.numeric(),
                income_id = incomeScore %>% scale() %>% as.numeric(),
                employment_id = employmentScore %>% scale() %>% as.numeric(),
                health_id = healthScore %>% scale() %>% as.numeric(),
                education_id = educationScore %>% scale() %>% as.numeric(),
                housing_id = housingScore %>% scale() %>% as.numeric(),
                crime_id = crimeScore %>% scale() %>% as.numeric(),
                environment_id = environmentScore %>% scale() %>% as.numeric())

### 3.2.2. gif ----

corr.plots.imd <- 
  lapply(X = 2002:2022,
         make.plot.correlation,
         covariates.id = c('Y', paste0(c('imd', 'income', 'employment', 'health', 'education', 'housing', 'crime', 'environment'), '_id')),
         covariates.label = c('Suicide Counts',
                              'Index of Multiple Deprivation', 
                              'Income', 
                              'Employment', 
                              'Health & Disability',
                              'Education, Skills & Training', 
                              'Barriers to Housing & Services', 
                              'Crime',
                              'Living Environment'),
         data.model.imd)

setwd(paste0(dir.res.covariate, '/gif'))
animation::saveGIF(
  expr = {for(i in 1:length(corr.plots.imd)) {print(corr.plots.imd[[i]])}},
  move.name = 'CORRELATION_MATRIX_IMD_DOMAINS.gif',
  interval = 0.5,
  ani.width = width * 100,
  ani.height = height * 100)

### 3.2.3. overall ----

corr.plot.imd <-
  make.plot.correlation(data = data.model.imd,
                        covariates.id = c('Y', paste0(c('imd', 'income', 'employment', 'health', 'education', 'housing', 'crime', 'environment'), '_id')),
                        covariates.label = c('Suicide Counts',
                                             'Index of Multiple Deprivation', 
                                             'Income', 
                                             'Employment', 
                                             'Health & Disability',
                                             'Education, Skills & Training', 
                                             'Barriers to Housing & Services', 
                                             'Crime',
                                             'Living Environment'),
                        title.legend = ''); corr.plot.imd

setwd(dir.res.covariate)
ggplot2::ggsave(plot = corr.plot.imd,
                filename = 'png/CORRELATION_MATRIX_IMD_DOMAINS.png',
                width = width, 
                height = height)
ggplot2::ggsave(plot = corr.plot.imd,
                filename = 'eps/CORRELATION_MATRIX_IMD_DOMAINS.eps',
                device = 'eps',
                dpi = 1200,
                width = width, 
                height = height)

## 2.4. time varying correlations ----

### 2.4.0. function ----

make.plot.correlationTime <- function(covarite = NULL, 
                                      data,
                                      title.legend = '') {

  # 0. function arguments ----
  
  # covarite <- 'deprivation_id'
  # data <- data.model
  # title.legend <- ''
  
  # 1. limits ----
  
  limits.corr <- c(0.9, 1)
  breaks.corr <- c(0.9, 0.95, 1)
  
  # 2. data sort ----
  
  ## 2.1. select covariates and make wider ----
  
  correlation.matrix.data.temp <-
    data %>% 
    # # filter by year IF needed
    {
      if(!is.null(covarite)) {
        dplyr::rename(., selected.covariate = {{covarite}})
      } else {
        stop('Warning: covarite needs to be non-null')
      }
    } %>% 
    dplyr::select(YEAR, selected.covariate) %>% 
    dplyr::mutate(id = dplyr::row_number(),
                  .by = c('YEAR')) %>% 
    tidyr::pivot_wider(names_from = YEAR,
                       values_from = 'selected.covariate') %>%
    dplyr::select(-id) %>% 
    cor(., method = 'pearson')
  
  
  ## 2.2. make a lower triangle ----
  
  get.lower.triangle <- function(matrix){
    matrix[lower.tri(matrix)] <- NA
    return(matrix)
  }
  
  correlation.matrix.data <-
    correlation.matrix.data.temp %>% 
    get.lower.triangle()
  
  ## 2.3. format for plot ----
  
  plot.data <-
    correlation.matrix.data %>% 
    reshape2::melt(., varnames = c('row', 'column'), value.name = 'correlation', na.rm = TRUE) %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate(row = row %>% factor(., levels = 2002:2022, labels = 2002:2022),
                  column = column %>% factor(., levels = 2002:2022, labels = 2002:2022))
  
  # 3. plot ----
  
  plot <-
    ggplot2::ggplot(data = plot.data, aes(x = row, y = column, fill = correlation, label = sprintf(paste0('%.', 2, 'f'), round(correlation, digits = 2)) )) +
    ggplot2::geom_tile(colour = 'white') +
    ggplot2::scale_fill_gradient2(name = title.legend,
                                  guide = ggplot2::guide_colorbar(title.pisition = 'top'),
                                  midpoint = breaks.corr[2],
                                  # low = colours.diverging[3], 
                                  # mid = colours.diverging[2],
                                  # high = colours.diverging[1],
                                  low = '#CCEBC5', 
                                  mid = '#66C27C',
                                  high = '#008B00',
                                  limits = limits.corr,
                                  breaks = breaks.corr) +
    ggplot2::geom_text(colour = 'black', size = 5) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(limits = plot.data$row %>% levels() %>% rev(),
                              labels = plot.data$row %>% levels() %>% rev()) +
    ggplot2::scale_y_discrete(labels = plot.data$row %>% levels()) +
    ggplot2::labs(y = '', x = '') +
    ggplot2::theme(text = ggplot2::element_text(size = 20),
                   legend.position = 'bottom',
                   legend.key.width = unit(2, 'cm'),
                   legend.title = ggplot2::element_text(hjust = 0.5),
                   axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1),
                   axis.ticks = element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(hjust = 0),
                   legend.key = ggplot2::element_rect(fill = NA, colour = NA))
  
  # 4. return ----
  
  return(list(plot = plot,
              corr.data = correlation.matrix.data.temp,
              covariate = covarite))
  
    
}

### 2.4.1. data sort ----

correlation.time.all <- 
  lapply(X = paste0(c('deprivation', 'diversity', 'populationDensity', 'nighttimeLight', 'totalRail', 'totalRoad', 'ndvi'), '_id'),
         FUN = function(x) {
           make.plot.correlationTime(covarite = x, data = data.model)
         })

### 2.4.2. save ----

setwd(dir.res.covariate)
# png
for(i in 1:length(correlation.time.all)){
  
  ggplot2::ggsave(plot = correlation.time.all[[i]]$plot,
                  filename = paste0('png/CORRELATION_MATRIX_',
                                    exposure.data.frame %>% dplyr::filter(name.inla.id == correlation.time.all[[i]]$covariate) %>% dplyr::pull(name.save),
                                    '_YEAR.png'),
                  width = width, 
                  height = height)
  
}
# eps
for(i in 1:length(correlation.time.all)){
  
  ggplot2::ggsave(plot = correlation.time.all[[i]]$plot,
                  filename = paste0('eps/CORRELATION_MATRIX_',
                                    exposure.data.frame$name.save[i],
                                    '_YEAR.eps'),
                  device = 'eps',
                  dpi = 1200,
                  width = width, 
                  height = height)
  
}

# 3. suicide-to-covariate correlation ----

## 3.0. function ----

make.correlation.suicideCovariate <- function(filter = NULL,
                                              data){
  
  # 0. function arguments ----
  
  # filter <- covariate.year[1,]
  # data <- data.model
  
  # 1. data sort ----
  
  ## 1.1. filter and select covariates ----
  
  year <- filter$YEAR
  covariates.id <- filter$covariates.id
  
  corr.temp <-
    data %>% 
    # # filter by year IF needed
    {
      if(!is.null(filter)) {
        dplyr::filter(., YEAR == year)
      } else {
        stop('Warning: filter needs to be non-null')
      }
    } %>% 
    dplyr::select(Y, covariates.id) %>%
    cor(., method = 'pearson')
  
  
  ## 1.2. select correlation ----
  
  corr <- corr.temp['Y', covariates.id]
  
  # 2. new dataframe ----
  
  res <- 
    filter %>% 
    dplyr::mutate(correlation = corr)
  
  
  
  # 4. return ----
  
  return(res)
  
}

## 3.1. plots ----

filter.covariate.year <-
  tidyr::expand_grid(YEAR = 2002:2022,
                     covariates.id = paste0(c('deprivation', 'diversity', 'populationDensity', 'nighttimeLight', 'totalRail', 'totalRoad', 'ndvi'), '_id')) %>% 
  dplyr::left_join(.,
                   exposure.data.frame %>% 
                     dplyr::mutate(covariates.id = name.inla.id,
                                   covariates.label = name.label) %>% 
                     dplyr::select(covariates.id, covariates.label),
                   by = 'covariates.id') %>% 
  split(., seq(nrow(.)))

all.suicide.cov.corr <- 
  lapply(X = filter.covariate.year,
         FUN = function(x){
           make.correlation.suicideCovariate(filter = x,
                                             data = data.model)
         }) %>% 
  dplyr::bind_rows()

colours.covariates <- 
  all.suicide.cov.corr$covariates.label %>% 
  unique() %>% 
  length() %>% 
  # RColorBrewer::brewer.pal(n = ., 'Dark2')
  generate.colour.values() %>%
  colorspace::darken(., amount  = 0.25)

plot.suicide.cov.corr <- 
  ggplot2::ggplot(data = all.suicide.cov.corr, 
                  aes(x = YEAR, y = correlation, 
                      group = covariates.label, colour = covariates.label, 
                      linetype = covariates.label)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::scale_color_manual(values = colours.covariates) +
  ggplot2::scale_x_continuous(name = '', breaks = seq(from = 2002, to = 2022, by = 2), limits = c(2002, 2023)) +
  ggplot2::scale_y_continuous(name = 'Correlation with Suicide Counts', limits = c(-1, 1), breaks = c(-1, 0, 1)) +
  ggplot2::guides(colour = ggplot2::guide_legend(ncol = 3)) +
  my.theme(text = ggplot2::element_text(size = text.size),
           legend.title = ggplot2::element_blank(),
           legend.position = c(0.5, 0.10)); plot.suicide.cov.corr

## 3.2. save ----

setwd(dir.res.covariate)
ggplot2::ggsave(plot = plot.suicide.cov.corr,
                filename = 'png/SUICIDE_COVARAIATE_YEARLY_CORRELATION.png',
                width = width, 
                height = height)
ggplot2::ggsave(plot = plot.suicide.cov.corr,
                filename = 'eps/SUICIDE_COVARAIATE_YEARLY_CORRELATION.eps',
                device = 'eps',
                dpi = 1200,
                width = width, 
                height = height)

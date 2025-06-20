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
if(!require('sf')) {
  install.packages('sf', dependencies = TRUE)
}
if(!require('viridis')) {
  install.packages('viridis', dependencies = TRUE)
}
if(!require('patchwork')) {
  install.packages('patchwork', dependencies = TRUE)
}
if(!require('colorspace')) {
  install.packages('colorspace', dependencies = TRUE)
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
dir.res.model.fit <- paste0(dir.res, '/02_modelFit')
dir.res.main.analysis <- paste0(dir.res, '/03_mainAnalysis')

## 0.3. imports ----

### 0.3.1. functions ----

setwd(dir.code)
source('00_functions.R')

### 0.3.2. organised data ----

setwd(dir.data.organised)
load(file = 'ENGLAND_SPATIAL_INFORMATION_LSOA_MSOA_LAD.rda')
load(file = 'ENGLAND_AMAT_LSOA_MSOA_LAD.rda')
rm(info.lsoa, info.lad, mat.msoa, mat.lsoa, mat.lad); gc() # remove unneeded variables (space)

### 0.3.3. spatial ----

setwd(dir.data.spatial)
# raw
poly.msoa <- sf::st_read(dsn = dir.data.spatial, layer = 'ONS11_MSOA')
poly.rgn <- sf::st_read(dsn = dir.data.spatial, layer = 'ONS21_RGN')

# england only
poly.msoa.england <-
  poly.msoa %>%
  dplyr::filter(stringr::str_detect(MSOA11CD, '^E'))
poly.rgn.england <-
  poly.rgn %>%
  dplyr::filter(stringr::str_detect(RGN21CD, '^E'))

# simple polygon
simple.spatial.plot <- FALSE
if(simple.spatial.plot){
  # less detailed but quicker to plot
  poly.msoa.england <-
    poly.msoa.england %>% 
    sf::st_simplify(., preserveTopology = TRUE, dTolerance = 1000)
  poly.rgn.england <-
    poly.rgn.england %>% 
    sf::st_simplify(., preserveTopology = TRUE, dTolerance = 1000)
}

lookup.msoa.rgn <- 
  read.csv('MSOA11_LAD11_RGN11_lookUp.csv') %>% 
  # RGN21NM - for plotting as cannot find RGN11 poly and nothing has changed
  dplyr::rename(RGN21NM = 'RGN11NM')

### 0.3.4. model results ----

setwd(dir.res.model.fit)
load(file = 'MODEL_FIT_DATA.rda')
load(file = 'MODEL_FIT_EXTRACTED_RESULTS.rda')

theta.predicted <-
  temp.results %>% 
  # extract predicted observations
  purrr::map(.x = ., 
             .f = function(x) {x$prediction$predicted}) %>% 
  dplyr::bind_cols() %>% 
  # join to data
  dplyr::bind_cols(.,
                   data.model) %>% 
  # send theta to back 
  dplyr::relocate(dplyr::starts_with('theta:'), .after = dplyr::last_col())

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

# number of groups for socio-environmental variables 
n.groups <- 5

### 0.4.2. distribution ----

distribution.name <- c('Zero Distribution', 'Count Distribution')

### 0.4.3. exposure ----

exposure.data.frame <-
  data.frame(name.inla = c('deprivation', 'diversity', 'populationDensity', 'nighttimeLight', 'totalRail', 'totalRoad', 'ndvi'),
             name.label = c('Deprivation', 'Ethnic Density', 'Population Density', 'Light Pollution', 'Railway Network Density', 'Road Network Density', 'Greenspace'))

# 1. spatio-temporal results ----

## 1.2. national-year ----

### 1.2.1. data organise ----

temporal.data <-
  theta.predicted %>% 
  dplyr::summarise(Y = sum(Y), 
                   E = sum(E),
                   N = sum(N),
                   dplyr::across(dplyr::starts_with('theta:'), ~ .x %>% sum()),
                   .by = c('YEAR')) %>%  
  # SMR = .x/E
  dplyr::mutate(dplyr::across(dplyr::starts_with('theta:'), ~ .x /E)) %>% 
  # calculate posterior summaries
  dplyr::mutate(dplyr::select(., dplyr::starts_with('theta:')) %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>% 
                  do.call(rbind, .)) %>% 
  # drop full samples
  dplyr::select(-dplyr::starts_with('theta:'))

### 1.2.2. plot ----

temporal.plot <-
  ggplot2::ggplot(data = temporal.data, aes(x = YEAR)) +
  ggplot2::geom_hline(aes(yintercept = 1), colour = 'red3', linetype = 'dashed') +
  ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, colour = NA) +
  # ggplot2::geom_point(aes(y = Y/E), colour = 'blue3', shape = 4) +
  ggplot2::geom_line(aes(y = median)) +
  ggplot2::scale_x_continuous(name = '', breaks = seq(from = 2002, to = 2022, by = 2), limits = c(2002, 2022)) +
  ggplot2::scale_y_continuous(name = 'Relative Risk') +
  my.theme(text = ggplot2::element_text(size = text.size)); temporal.plot

### 1.2.3. table ----

temporal.table <- 
  temporal.data %>% 
  # drop full samples
  dplyr::mutate(estimate = paste0(median %>% round(., digits = digits.table), ' (', lower %>% round(., digits = digits.table), ', ', upper %>% round(., digits = digits.table), ')')) %>% 
  dplyr::arrange(YEAR) %>% 
  dplyr::select(YEAR, estimate)

### 1.2.4. save ----

setwd(dir.res.main.analysis)
# png
ggplot2::ggsave(plot = temporal.plot,
                filename = 'png/MODEL_RESULT_TEMPORAL_PLOT.png',
                width = width,
                height = height)
# eps
ggplot2::ggsave(plot = temporal.plot,
                filename = 'eps/MODEL_RESULT_TEMPORAL_PLOT.eps',
                device = 'eps',
                dpi = 1200,
                width = width,
                height = height)
# txt
print(xtable::xtable(temporal.table),
      include.rownames = FALSE,
      file = 'txt/MODEL_RESULTS_SMR_TEMPORAL.txt')

### 1.2.5. clear ----

rm(temporal.data,
   temporal.plot,
   temporal.table)
gc()

## 1.3. region-year ----

### 1.3.1. data orgainse ----

temporal.regional.data <-
  theta.predicted %>% 
  dplyr::summarise(Y = sum(Y), 
                   E = sum(E),
                   N = sum(N),
                   dplyr::across(dplyr::starts_with('theta:'), ~ .x %>% sum()),
                   .by = c('YEAR', 'RGN21NM')) %>%  
  # SMR = .x/E
  dplyr::mutate(dplyr::across(dplyr::starts_with('theta:'), ~ .x /E)) %>% 
  # calculate posterior summaries
  dplyr::mutate(dplyr::select(., dplyr::starts_with('theta:')) %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>% 
                  do.call(rbind, .)) %>% 
  # drop full samples
  dplyr::select(-dplyr::starts_with('theta:'))

colours.region.temp <- 
  temporal.regional.data$RGN21NM %>% 
  unique() %>% 
  length() %>% 
  # RColorBrewer::brewer.pal(n = ., 'Dark2')
  generate.colour.values() %>%
  colorspace::darken(., amount  = 0.5)

ordered.names.region.all <-
  temporal.regional.data %>% 
  dplyr::select(RGN21NM) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(RGN21NM) %>% 
  dplyr::pull(RGN21NM)

ordered.names.region.selected <-
  temporal.regional.data %>% 
  dplyr::select(RGN21NM) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(RGN21NM) %>% 
  dplyr::mutate(id = 1:n()) %>% 
  dplyr::filter(RGN21NM %in% c('London', 'North East', 'North West', 'South West')) %>% 
  dplyr::pull(id)
colours.region <- colours.region.temp
colours.region[ordered.names.region.selected] <- c('blue3', 'red3', 'green4', 'pink2') # highlighting specific
# colours.region[ordered.names.region.selected] <- c(colours.diverging[1], colours.diverging[3]) # not highlight specific

### 1.3.2. plot ----

temporal.regional.plot <-
  ggplot2::ggplot(data = 
                    temporal.regional.data %>% 
                    dplyr::filter(!(RGN21NM %in% c('London', 'North East', 'North West', 'South West'))), 
                  aes(group = RGN21NM, colour = RGN21NM)) +
  ggplot2::geom_hline(aes(yintercept = 1), colour = 'black', linetype = 'dashed') +
  # ggplot2::geom_point(position = ggplot2::position_dodge(0.5)) +
  ggplot2::geom_line(aes(x = YEAR, y = median)) +
  ggplot2::geom_line(data = 
                       temporal.regional.data %>% 
                       dplyr::filter((RGN21NM %in% c('London', 'North East', 'North West', 'South West'))),
                     linewidth = 1.5,
                     aes(x = YEAR, y = median)) +
  ggplot2::scale_color_manual(values = colours.region) +
  ggplot2::scale_x_continuous(name = '', breaks = seq(from = 2002, to = 2022, by = 2), limits = c(2002, 2023)) +
  ggplot2::scale_y_continuous(name = 'Relative Risk') +
  ggplot2::guides(colour = ggplot2::guide_legend(ncol = 3)) +
  my.theme(text = ggplot2::element_text(size = text.size),
           legend.title = ggplot2::element_blank(),
           legend.position = c(0.375, 0.925)); temporal.regional.plot

### 1.3.3. table ----

national.data <-
  theta.predicted %>% 
  dplyr::mutate(RGN21NM = 'England') %>% 
  dplyr::summarise(Y = sum(Y), 
                   E = sum(E),
                   N = sum(N),
                   dplyr::across(dplyr::starts_with('theta:'), ~ .x %>% sum()),
                   .by = c('RGN21NM')) %>%  
  # SMR = .x/E
  dplyr::mutate(dplyr::across(dplyr::starts_with('theta:'), ~ .x /E)) %>% 
  # calculate posterior summaries
  dplyr::mutate(dplyr::select(., dplyr::starts_with('theta:')) %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>% 
                  do.call(rbind, .)) %>% 
  # drop full samples
  dplyr::mutate(estimate = paste0(median %>% round(., digits = digits.table), ' (', lower %>% round(., digits = digits.table), ', ', upper %>% round(., digits = digits.table), ')')) %>% 
  dplyr::arrange(dplyr::desc(median)) %>% 
  dplyr::select(RGN21NM, estimate)

regional.data <-
  theta.predicted %>% 
  dplyr::summarise(Y = sum(Y), 
                   E = sum(E),
                   N = sum(N),
                   dplyr::across(dplyr::starts_with('theta:'), ~ .x %>% sum()),
                   .by = c('RGN21NM')) %>%  
  # SMR = .x/E
  dplyr::mutate(dplyr::across(dplyr::starts_with('theta:'), ~ .x /E)) %>% 
  # calculate posterior summaries
  dplyr::mutate(dplyr::select(., dplyr::starts_with('theta:')) %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>% 
                  do.call(rbind, .)) %>% 
  # drop full samples
  dplyr::mutate(estimate = paste0(median %>% round(., digits = digits.table), ' (', lower %>% round(., digits = digits.table), ', ', upper %>% round(., digits = digits.table), ')')) %>% 
  dplyr::arrange(dplyr::desc(median)) %>% 
  dplyr::select(RGN21NM, estimate)

regional.national.table <- 
  dplyr::bind_rows(regional.data,
                   national.data)

### 1.3.4. save ----

setwd(dir.res.main.analysis)
# png
ggplot2::ggsave(plot = temporal.regional.plot,
                filename = 'png/MODEL_RESULT_TEMPORAL_REGIONAL_PLOT.png',
                width = width,
                height = height)
# eps
ggplot2::ggsave(plot = temporal.regional.plot,
                filename = 'eps/MODEL_RESULT_TEMPORAL_REGIONAL_PLOT.eps',
                device = 'eps',
                dpi = 1200,
                width = width,
                height = height)
# txt
print(xtable::xtable(regional.national.table),
      include.rownames = FALSE,
      file = 'txt/MODEL_RESULTS_SMR_NATIONAL_REGION.txt')

### 1.3.5. clear ----

rm(temporal.regional.data,
   colours.region.temp,
   ordered.names.region.all,
   ordered.names.region.selected,
   colours.region,
   temporal.regional.plot,
   national.data,
   regional.data,
   regional.national.table)
gc()

## 1.4. msoa-year ----

### 1.4.1. data organise ----

my.spatio.temporal.smr <- function(posterior, spatial.level){
  
  # posterior <- theta.predicted
  # spatial.level <- c('CTY21NM', 'RGN21NM', 'MSOA11CD')[2]
  
  ### 1. aggregated to required level ----
  
  if(spatial.level == 'CTY21NM'){
    posterior.temp <- 
      posterior %>% 
      dplyr::mutate(spatial.level = 'England')
  } else {
    posterior.temp <- 
      posterior %>% 
      dplyr::rename(spatial.level = {{spatial.level}})
  }
  
  spatial.level.order <- 
    posterior.temp %>% 
    # summarise by spatial.level
    dplyr::summarise(Y = sum(Y),
                     E = sum(E),
                     N = sum(N),
                     dplyr::across(dplyr::starts_with('theta:'), ~ .x %>% sum()),
                     .by = c('spatial.level')) %>% 
    # turn samples into rate
    dplyr::mutate(dplyr::across(dplyr::starts_with('theta:'), ~ .x / E)) %>% 
    dplyr::mutate(
      # calculate posterior summarise
      dplyr::select(., dplyr::starts_with('theta:')) %>% 
        apply(., 1, my.summary) %>% 
        lapply(., data.frame) %>% 
        do.call(rbind, .)) %>% 
    dplyr::select(spatial.level, mean, median) %>% 
    dplyr::mutate(id.spatial.order = mean %>% as.factor() %>% as.numeric(),
                  spatial.level = spatial.level %>% forcats::fct_reorder(., id.spatial.order)) %>% 
    dplyr::arrange() %>% 
    dplyr::pull(spatial.level) %>% 
    levels()
  
  posterior.aggregated <-
    posterior.temp %>% 
    # summarise by spatial.level
    dplyr::summarise(Y = sum(Y),
                     E = sum(E),
                     N = sum(N),
                     dplyr::across(dplyr::starts_with('theta:'), ~ .x %>% sum()),
                     .by = c('spatial.level', 'YEAR')) %>% 
    # SMR = .x/E
    dplyr::mutate(dplyr::across(dplyr::starts_with('theta:'), ~ .x / E)) %>% 
    dplyr::mutate(
      # calculate posterior summarise
      dplyr::select(., dplyr::starts_with('theta:')) %>% 
        apply(., 1, my.summary) %>% 
        lapply(., data.frame) %>% 
        do.call(rbind, .),
      # calculate exceedance of 1
      dplyr::select(., dplyr::starts_with('theta:')) %>% 
        apply(., 1, function(x) { (x > 1) %>% mean() %>% data.frame(exceedance = .) }) %>% 
        lapply(., data.frame) %>% 
        do.call(rbind, .)) %>% 
    # discrete versions and order
    dplyr::mutate(spatial.level = spatial.level %>% factor(., levels = spatial.level.order),
                  exceedance.discrete = exceedance %>% cut(x = ., 
                                                           breaks = c(-10^(-100), 0.2, 0.8, 1), 
                                                           label = c('[0%, 20%]', '(20%, 80%]', '(80%, 100%]'),
                                                           right = TRUE, 
                                                           include.lowest = TRUE)) %>%
    dplyr::select(-dplyr::starts_with('theta:'))
  
  return(posterior.aggregated)
  
  
}

smr.spatio.temporal.rgn <- my.spatio.temporal.smr(posterior = theta.predicted, spatial.level = 'RGN21NM')
smr.spatio.temporal.msoa <- my.spatio.temporal.smr(posterior = theta.predicted, spatial.level = 'MSOA11CD')

### 1.4.2. plot ----

smr.spatio.temporal.msoa.rgn <- 
  smr.spatio.temporal.msoa %>% 
  dplyr::mutate(MSOA11CD = spatial.level %>% as.character()) %>% 
  dplyr::left_join(., 
                   lookup.msoa.rgn %>% dplyr::select(MSOA11CD, MSOA11NM, RGN21NM),
                   by = 'MSOA11CD') %>% 
  dplyr::mutate(RGN21NM = RGN21NM %>% factor(., levels = levels(smr.spatio.temporal.rgn$spatial.level))) %>%  
  dplyr::mutate(order.rgn = RGN21NM %>% as.numeric(),
                order.msoa = spatial.level %>% as.numeric(),
                order = 
                  paste0(order.rgn, '.', sprintf(fmt = '%04d', order.msoa)) %>% as.numeric() %>% 
                  as.factor() %>% as.numeric())

smr.spatio.temporal.msoa.rgn.rank <-
  theta.predicted %>% 
  # will be bring rgn21nm with levels in
  dplyr::select(-RGN21NM) %>% 
  dplyr::left_join(.,
                   smr.spatio.temporal.msoa.rgn %>% 
                     dplyr::select(MSOA11CD, RGN21NM, order) %>% 
                     dplyr::distinct() %>% 
                     dplyr::group_by(RGN21NM) %>% 
                     dplyr::mutate(rank = order %>% dplyr::ntile(x = ., n = 10)) %>% 
                     dplyr::ungroup(),
                   by = 'MSOA11CD') %>% 
  # summarise by spatial.level
  dplyr::summarise(Y = sum(Y),
                   E = sum(E),
                   N = sum(N),
                   dplyr::across(dplyr::starts_with('theta:'), ~ .x %>% sum()),
                   .by = c('RGN21NM', 'rank', 'YEAR')) %>% 
  # SMR = .x/E
  dplyr::mutate(dplyr::across(dplyr::starts_with('theta:'), ~ .x / E)) %>% 
  dplyr::mutate(
    # calculate posterior summarise
    dplyr::select(., dplyr::starts_with('theta:')) %>% 
      apply(., 1, my.summary) %>% 
      lapply(., data.frame) %>% 
      do.call(rbind, .),
    # calculate exceedance of 1
    dplyr::select(., dplyr::starts_with('theta:')) %>% 
      apply(., 1, function(x) { (x > 1) %>% mean() %>% data.frame(exceedance = .) }) %>% 
      lapply(., data.frame) %>% 
      do.call(rbind, .)) %>% 
  # discrete versions and order
  dplyr::mutate(exceedance.discrete = exceedance %>% cut(x = ., 
                                                         breaks = c(-10^(-100), 0.2, 0.8, 1), 
                                                         label = c('[0%, 20%]', '(20%, 80%]', '(80%, 100%]'),
                                                         right = TRUE, 
                                                         include.lowest = TRUE)) %>%
  dplyr::select(-dplyr::starts_with('theta:')) %>%  
  dplyr::mutate(order.rgn = RGN21NM %>% as.numeric(),
                order = 
                  paste0(order.rgn, '.', sprintf(fmt = '%04d', rank)) %>% as.numeric() %>% 
                  as.factor() %>% as.numeric())

breaks.rgn.rank <- 
  smr.spatio.temporal.msoa.rgn.rank %>% 
  dplyr::summarise(min.order = min(order),
                   max.order = max(order),
                   line.position = max(order) + 0.5,
                   label.position = median(min(order):max(order)),
                   .by = 'RGN21NM') %>% 
  dplyr::arrange(RGN21NM)

labels.rank <-
  smr.spatio.temporal.msoa.rgn.rank %>% 
  dplyr::select(order, rank) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(order)

smr.spatio.temporal.msoa.rgn.rank.plot.1 <- 
  ggplot2::ggplot(data = smr.spatio.temporal.msoa.rgn.rank) +
  ggplot2::geom_tile(aes(x = YEAR, y = order, fill = median)) +
  ggplot2::scale_fill_gradient2(name = '',
                                midpoint = 1,
                                low = colours.diverging[1], 
                                mid = colours.diverging[2],
                                high = colours.diverging[3]) +
  ggplot2::scale_x_continuous(name = '', breaks = seq(from = 2002, to = 2022, by = 2)) +
  ggplot2::scale_y_continuous(name = 'MSOA (Decile)', breaks = labels.rank$order, labels = labels.rank$rank) +
  ## splitting by region
  ### lines
  ggplot2::geom_segment(data = data.frame(x = rep(-Inf, times = nrow(breaks.rgn.rank)), xend = rep(Inf, times = nrow(breaks.rgn.rank)),
                                          y = breaks.rgn.rank$line.position, yend = breaks.rgn.rank$line.position),
                        aes(x = x, xend = xend, y = y, yend = yend),
                        colour = 'black') +
  ### labels
  ggplot2::annotate('text',
                    x = rep(2002, times = nrow(breaks.rgn.rank)),
                    y = breaks.rgn.rank$label.position,
                    label = breaks.rgn.rank$RGN21NM,
                    fontface = 'bold',
                    colour = 'black',
                    size = 4,
                    hjust = 0) +
  my.theme(text = ggplot2::element_text(size = text.size),
           legend.position = 'bottom',
           legend.key.width = unit(2, 'cm'),
           legend.title = ggplot2::element_text(hjust = 0.5),
           axis.text.y = ggplot2::element_blank(),
           axis.ticks.y = ggplot2::element_blank());

smr.spatio.temporal.msoa.rgn.rank.plot.2 <- 
  ggplot2::ggplot(data = smr.spatio.temporal.msoa.rgn.rank) +
  ggplot2::geom_tile(aes(x = YEAR, y = order, fill = exceedance.discrete)) +
  ggplot2::scale_fill_manual(name = '', 
                             values = colours.exceedance) +
  ggplot2::scale_x_continuous(name = '', breaks = seq(from = 2002, to = 2022, by = 2)) +
  ggplot2::scale_y_continuous(name = '', breaks = labels.rank$order, labels = labels.rank$rank) +
  ## splitting by region
  ### lines
  ggplot2::geom_segment(data = data.frame(x = rep(-Inf, times = nrow(breaks.rgn.rank)), xend = rep(Inf, times = nrow(breaks.rgn.rank)),
                                          y = breaks.rgn.rank$line.position, yend = breaks.rgn.rank$line.position),
                        aes(x = x, xend = xend, y = y, yend = yend),
                        colour = 'black') +
  my.theme(text = ggplot2::element_text(size = text.size),
           legend.position = 'bottom',
           legend.key.width = unit(2, 'cm'),
           legend.title = ggplot2::element_text(hjust = 0.5),
           axis.text.y = ggplot2::element_blank(),
           axis.ticks.y = ggplot2::element_blank())

### 1.4.3. save ----

setwd(dir.res.main.analysis)
# png
ggplot2::ggsave(plot = smr.spatio.temporal.msoa.rgn.rank.plot.1,
                filename = 'png/MODEL_RESULTS_SMR_SPATIO_TEMPORAL_MSOA_REGION_MEDIAN_DECILE.png',
                height = height,
                width = width)
ggplot2::ggsave(plot = smr.spatio.temporal.msoa.rgn.rank.plot.2,
                filename = 'png/MODEL_RESULTS_SMR_SPATIO_TEMPORAL_MSOA_REGION_EXCEEDANCE_DECILE.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = smr.spatio.temporal.msoa.rgn.rank.plot.1,
                filename = 'eps/MODEL_RESULTS_SMR_SPATIO_TEMPORAL_MSOA_REGION_MEDIAN_DECILE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)
ggplot2::ggsave(plot = smr.spatio.temporal.msoa.rgn.rank.plot.2,
                filename = 'eps/MODEL_RESULTS_SMR_SPATIO_TEMPORAL_MSOA_REGION_EXCEEDANCE_DECILE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)

### 1.4.4. clear ----

rm(smr.spatio.temporal.rgn,
   smr.spatio.temporal.msoa,
   smr.spatio.temporal.msoa.rgn,
   smr.spatio.temporal.msoa.rgn.rank,
   breaks.rgn.rank,
   labels.rank,
   smr.spatio.temporal.msoa.rgn.rank.plot.1,
   smr.spatio.temporal.msoa.rgn.rank.plot.2)
gc()

# 2. variance proportion ----

## 2.1. data organise ----

variance.proportion <-
  temp.results %>% 
  # extract linear predictor and calc. variance
  purrr::map(.x = .,
             .f = function(x){ 
               x$linear.predictor %>% 
                 dplyr::summarise(dplyr::across(dplyr::everything(), var)) %>% 
                 tidyr::pivot_longer(cols = dplyr::everything(),
                                     names_to = 'model.component',
                                     values_to = 'variance.parition')
             }) %>% 
  dplyr::bind_rows(.id = 'theta') %>%
  # pivot to have long for theta:1-100
  tidyr::pivot_wider(names_from = theta,
                     names_prefix = 'theta:',
                     values_from = 'variance.parition') %>% 
  # formatting
  dplyr::mutate(model = dplyr::if_else(endsWith(x = model.component, suffix = '.z'), distribution.name[1], distribution.name[2]) %>% factor(., levels = distribution.name),
                model.component = model.component %>% gsub(pattern = '\\.[^.]*$', replacement = '', x = .)) %>% 
  # define ratios
  dplyr::mutate(dplyr::across(dplyr::starts_with('theta'), ~ (.x / .x[model.component == 'full']) * 100),
                .by = 'model') %>% 
  # summarise posterior
  dplyr::mutate(
    # calculate posterior summarise
    dplyr::select(., dplyr::starts_with('theta:')) %>% 
      apply(., 1, my.summary) %>% 
      lapply(., data.frame) %>% 
      do.call(rbind, .)) %>%
  dplyr::mutate(estimate = paste0(median %>% round(., digits = digits.table), ' (', lower %>% round(., digits = digits.table), ', ', upper %>% round(., digits = digits.table), ')')) %>% 
  dplyr::select(-dplyr::starts_with('theta:')) %>% 
  dplyr::arrange(model)

variance.proportion.zero.data <-
  variance.proportion %>% 
  dplyr::filter(model == distribution.name[1],
                !(model.component %in% c('full', 'random'))) %>% 
  # get donut upper and lower
  dplyr::mutate(fraction = median/sum(median),
                ymax = cumsum(fraction),
                ymin = lag(ymax, n =1)) %>% 
  dplyr::mutate(dplyr::across(dplyr::everything(), ~ replace_na(., replace = 0))) %>% 
  # label
  dplyr::mutate(estimate.2 = paste0(median %>% round(., digits = 2), '%\n(', lower %>% round(., digits = 2), '%, ', upper %>% round(., digits = 2), '%)'),,
                label.position = (ymax + ymin)/2,
                label.1 = 
                  model.component %>% 
                  factor(., 
                         levels = c('fixed', 'random.spatial', 'random.temporal', 'random.spatiotemporal'),
                         labels = c('Socio-environmental', 'Spatial', 'Temporal', 'Spatio-temporal')) %>% 
                  as.character(),
                label.2 = paste0(label.1, '\n', estimate.2))

variance.proportion.count.data <-
  variance.proportion %>% 
  dplyr::filter(model == distribution.name[2],
                !(model.component %in% c('full', 'random'))) %>% 
  # get donut upper and lower
  dplyr::mutate(fraction = median/sum(median),
                ymax = cumsum(fraction),
                ymin = lag(ymax, n =1)) %>% 
  dplyr::mutate(dplyr::across(dplyr::everything(), ~ replace_na(., replace = 0))) %>% 
  # label
  dplyr::mutate(estimate.2 = paste0(median %>% round(., digits = 2), '%\n(', lower %>% round(., digits = 2), '%, ', upper %>% round(., digits = 2), '%)'),,
                label.position = (ymax + ymin)/2,
                label.1 = 
                  model.component %>% 
                  factor(., 
                         levels = c('fixed', 'random.spatial', 'random.temporal', 'random.spatiotemporal'),
                         labels = c('Socio-environmental', 'Spatial', 'Temporal', 'Spatio-temporal')) %>% 
                  as.character(),
                label.2 = paste0(label.1, '\n', estimate.2))

variance.proportion.both <-
  temp.results %>% 
  # extract linear predictor and calc. variance
  purrr::map(.x = .,
             .f = function(x){ 
               x$linear.predictor %>% 
                 dplyr::summarise(dplyr::across(dplyr::everything(), var)) %>% 
                 tidyr::pivot_longer(cols = dplyr::everything(),
                                     names_to = 'model.component',
                                     values_to = 'variance.parition')
             }) %>% 
  dplyr::bind_rows(.id = 'theta') %>%
  # pivot to have long for theta:1-100
  tidyr::pivot_wider(names_from = theta,
                     names_prefix = 'theta:',
                     values_from = 'variance.parition') %>% 
  # formatting
  dplyr::mutate(model = 'Both',
                model.component = model.component %>% gsub(pattern = '\\.[^.]*$', replacement = '', x = .)) %>% 
  dplyr::summarise(dplyr::across(dplyr::starts_with('theta:'), ~ mean(.x)),
                   .by = c('model.component', 'model')) %>% 
  # define ratios
  dplyr::mutate(dplyr::across(dplyr::starts_with('theta'), ~ (.x / .x[model.component == 'full']) * 100),
                .by = 'model') %>% 
  # summarise posterior
  dplyr::mutate(
    # calculate posterior summarise
    dplyr::select(., dplyr::starts_with('theta:')) %>% 
      apply(., 1, my.summary) %>% 
      lapply(., data.frame) %>% 
      do.call(rbind, .)) %>%
  dplyr::mutate(estimate = paste0(median %>% round(., digits = digits.table), ' (', lower %>% round(., digits = digits.table), ', ', upper %>% round(., digits = digits.table), ')')) %>% 
  dplyr::select(-dplyr::starts_with('theta:')) 

## 2.2. table ----

variance.table <-
  dplyr::bind_rows(variance.proportion %>% 
                     dplyr::select(model.component, model, estimate) ,
                   variance.proportion.both %>% 
                     dplyr::select(model.component, model, estimate))

## 2.3. save ----

setwd(dir.res.main.analysis)
# txt
print(xtable::xtable(variance.table),
      include.rownames = FALSE,
      file = 'txt/MODEL_RESULTS_VARIANCE_PROPORTION.txt')

## 2.4. clear ----

rm(variance.proportion,
   variance.proportion.zero.data,
   variance.proportion.count.data,
   variance.proportion.both,
   variance.table)
gc()

# 3. socio-environmental level change change ----

## 3.1. data organise ----

my.exposure.smr.change <- function(data, fit, samples, exposure){
  
  # 0. function arguments ----
  
  # data <- data.model
  # fit <- fit
  # samples <- samples
  # exposure <- exposure.data.frame$name.inla[1]
  
  # 1. new data ----
  
  time.start <- Sys.time()
  
  cat(paste0(stringr::str_to_title(exposure), ':\n'))
  cat(' defining new data...\n')
  
  ## 1.1. exposure ----
  
  # all exposures
  exps.0 <-  
    fit$fit$summary.fixed %>% 
    rownames() %>% 
    gsub(pattern = '^z\\.|^y\\.', replacement = '', x = .) %>% 
    unique() %>% 
    grep(pattern = 'intercept', x = ., value = TRUE, invert = TRUE)
  
  # exposures to change
  ## selected
  exps.1a <-
    exps.0 %>% 
    grep(pattern = paste0(exposure, '_id'), x = ., value = TRUE, invert = FALSE)
  ## others
  exps.1b <-
    exps.0 %>% 
    grep(pattern = paste0(exposure, '_id'), x = ., value = TRUE, invert = TRUE)
  
  ## 1.2. new data ----
  
  newdata.a <-
    data %>% 
    dplyr::mutate(
      # fix selected exposure to -0.5
      dplyr::across(dplyr::all_of(exps.1a), ~ -0.5),
      # average over all other exposures
      dplyr::across(dplyr::all_of(exps.1b), mean))
  
  newdata.b <-
    data %>% 
    dplyr::mutate(
      # fix selected exposure to -0.5
      dplyr::across(dplyr::all_of(exps.1a), ~ 0.5),
      # average over all other exposures
      dplyr::across(dplyr::all_of(exps.1b), mean))
  
  newdata <-
    dplyr::bind_rows(newdata.a,
                     newdata.b)
  
  # 2. linear predictor samples ----
  
  cat(' sampling posterior...\n')
  
  temp.results <-
    INLA::inla.posterior.sample.eval(fun = extract.observations.hp,
                                     newdata = newdata, 
                                     fit = fit,
                                     # inla.posterior.sample arguments
                                     samples = samples, 
                                     return.matrix = FALSE) %>% 
    purrr::set_names(x = ., nm = paste0('theta:', 1:length(samples)))
  
  # 3. linear predictor matrix ----
  
  cat(' calculating change...\n')
  
  theta.predicted <-
    temp.results %>% 
    # extract predicted observations
    purrr::map(.x = ., 
               .f = function(x) {x$prediction$predicted}) %>% 
    dplyr::bind_cols() %>% 
    # join ot data
    dplyr::bind_cols(.,
                     newdata)
  
  # 4. change in smr ----
  
  summary.smr.change <-
    theta.predicted %>%
    dplyr::mutate(exposure.value = .[[exps.1a]]) %>% 
    dplyr::summarise(Y = sum(Y), 
                     E = sum(E),
                     dplyr::across(dplyr::starts_with('theta:'), ~ sum(.x)),
                     .by = exposure.value) %>% 
    # SMR = .x/E
    dplyr::mutate(dplyr::across(dplyr::starts_with('theta:'), ~ .x /E)) %>% 
    # calute SMR change for 1sd
    dplyr::mutate(dplyr::across(dplyr::starts_with('theta'), ~ {
      ref.value <- .x[exposure.value == -0.5]
      ratio <- (.x - ref.value) / ref.value * 100 })) %>% 
    # summarise posterior
    dplyr::mutate(
      # calculate posterior summarise
      dplyr::select(., dplyr::starts_with('theta:')) %>% 
        apply(., 1, my.summary) %>% 
        lapply(., data.frame) %>% 
        do.call(rbind, .)) %>%
    dplyr::select(-dplyr::starts_with('theta:')) %>% 
    dplyr::mutate(exposure = exps.1a)
  
  # 5. timing ----
  
  time.end <- Sys.time()
  time.taken <- time.end - time.start
  time.taken.seconds <- time.taken %>% as.numeric(., units = 'secs')
  
  if(time.taken.seconds < 60){
    cat(paste0(' time taken is ', round(time.taken.seconds, 2), ' seconds\n'))
  } else if (time.taken.seconds < 3600){
    time.taken.minutes <-  time.taken.seconds / 60
    cat(paste0(' time taken is ', round(time.taken.minutes, 2), ' minutes\n'))
  } else {
    time.taken.hours <-  time.taken.seconds / 3600
    cat(paste0(' time taken is ', round(time.taken.hours, 2), ' hours\n'))
  }
  
  # 6. return ----
  
  summary.smr.change
  
}

setwd(dir.res.main.analysis)
if(file.exists('MODEL_FIT_SMR_CHANGE_EXPOSURE_LIST.rda')){
  
  load(file = 'MODEL_FIT_SMR_CHANGE_EXPOSURE_LIST.rda')
  
} else {
  
  setwd(dir.res.model.fit)
  load(file = 'MODEL_FIT_MODEL_FIT.rda')
  load(file = 'MODEL_FIT_POSTERIOR_SAMPLES.rda')
  
  smr.change.exposure.list <-
    lapply(X = exposure.data.frame$name.inla, 
           FUN = my.exposure.smr.change,
           data = data.model,
           fit = fit, 
           samples = samples)
  
  setwd(dir.res.main.analysis)
  save(smr.change.exposure.list, file = 'MODEL_FIT_SMR_CHANGE_EXPOSURE_LIST.rda')
  
  rm(fit,
     samples)
  gc()
  
}

smr.change.exposure.data <-
  smr.change.exposure.list %>% 
  dplyr::bind_rows() %>% 
  dplyr::mutate(exposure = exposure %>% factor(., levels = paste0(exposure.data.frame$name.inla, '_id'), labels = exposure.data.frame$name.label),
                index = exposure %>% as.numeric()) %>% 
  dplyr::filter(exposure.value == 0.5)

## 3.2. plot ----

smr.change.exposure.plot <-
  ggplot2::ggplot(data = smr.change.exposure.data, aes(x = index, y = median)) +
  ggplot2::geom_hline(yintercept = 0, colour = 'red3', linetype = 'dashed') +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, position = ggplot2::position_dodge(0.5)) +
  ggplot2::labs(x = '', y = 'Change in Risk for One Standard Deviation Increment (%)') +
  # plot annotation
  ## axis labels
  ggplot2::scale_x_continuous(trans = 'reverse') +
  ## splitting by exposure
  ### lines
  ggplot2::geom_segment(data = data.frame(y = rep(-Inf, times = 6), yend = rep(Inf, times = 6),
                                          x = seq(from = 1.5, to = 6.5, by = 1),
                                          xend = seq(from = 1.5, to = 6.5, by = 1)),
                        aes(x = x, xend = xend, y = y, yend = yend),
                        colour = 'grey') +
  ### labels
  ggplot2::annotate('text',
                    y = rep(-13, times = 7),
                    x = 1:7,
                    label = stringr::str_wrap(levels(smr.change.exposure.data$exposure), width = 12.5),
                    fontface = 'bold',
                    colour = 'black',
                    size = 4,
                    hjust = 0,
                    vjust = 0.5) +
  # make sure coor_flip at the end
  ggplot2::coord_flip() +
  my.theme(text = ggplot2::element_text(size = text.size),
           axis.text.y = ggplot2::element_blank(),
           axis.ticks.y = ggplot2::element_blank()); smr.change.exposure.plot

## 3.3. table ----

digits.table.smr.change <- 2
smr.change.exposure.table <-
  smr.change.exposure.data %>% 
  dplyr::mutate(estimate = paste0(median %>% round(., digits = digits.table.smr.change), '% (', lower %>% round(., digits = digits.table.smr.change), '%, ', upper %>% round(., digits = digits.table.smr.change), '%)')) %>% 
  dplyr::arrange(index) %>% 
  dplyr::select(exposure, estimate)

## 3.4 save ----

setwd(dir.res.main.analysis)
# png
ggplot2::ggsave(plot = smr.change.exposure.plot,
                filename = 'png/MODEL_RESULTS_SMR_CHANGE_EXPOSURE.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = smr.change.exposure.plot,
                filename = 'eps/MODEL_RESULTS_SMR_CHANGE_EXPOSURE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)
# txt
print(xtable::xtable(smr.change.exposure.table),
      include.rownames = FALSE,
      file = 'txt/MODEL_RESULTS_SMR_CHANGE_EXPOSURE.txt')

## 3.5. clear ----

rm(smr.change.exposure.list,
   smr.change.exposure.data,
   smr.change.exposure.plot,
   digits.table.smr.change,
   smr.change.exposure.table)
gc()

# 4. socio-environmental profile heatmap ----

## 4.1. data organise ----

n.tiles <- 100

profile.data <- 
  # to generate all mean
  theta.predicted %>% 
  # SMR = .x/E
  dplyr::mutate(dplyr::across(dplyr::starts_with('theta:'), ~ .x /E)) %>% 
  # calculate posterior summaries
  dplyr::mutate(dplyr::select(., dplyr::starts_with('theta:')) %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>% 
                  do.call(rbind, .)) %>% 
  # rank
  dplyr::mutate(index.tile = 
                  mean %>% 
                  cut(x = ., breaks = stats::quantile(x = ., probs = 0:n.tiles/n.tiles), include.lowest = TRUE) %>% 
                  as.factor() %>% 
                  as.numeric()) %>% 
  # average over rank
  dplyr::summarise(Y = mean(Y), 
                   E = mean(E),
                   N = mean(N),
                   dplyr::across(paste0(exposure.data.frame$name.inla, '_id'), ~ .x %>% mean()),
                   dplyr::across(dplyr::starts_with('theta:'), ~ .x %>% mean()),
                   .by = 'index.tile') %>% 
  # calculate posterior summaries
  dplyr::mutate(dplyr::select(., dplyr::starts_with('theta:')) %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>% 
                  do.call(rbind, .)) %>% 
  # remove theta
  dplyr::select(-dplyr::starts_with('theta:')) %>% 
  # make longers as we want to have expoure on x-aaxis
  tidyr::pivot_longer(cols = paste0(exposure.data.frame$name.inla, '_id'), 
                      names_to = 'exposure',
                      values_to = 'exposure.average') %>% 
  # better labels
  dplyr::mutate(exposure = exposure %>% factor(., level = paste0(exposure.data.frame$name.inla, '_id'), label = exposure.data.frame$name.label),
                index.exposure = exposure %>% as.numeric())

## 4.2. plot ----

profile.plot <- 
  ggplot2::ggplot(data = profile.data, aes(y = index.exposure, x = index.tile)) +
  ggplot2::geom_tile(aes(fill = exposure.average)) +
  ggplot2::scale_fill_gradient2(name = '',
                                midpoint = 0,
                                low = colours.diverging.2[1],
                                mid = colours.diverging.2[2],
                                high = colours.diverging.2[3],
                                limits = c(-1, 1),
                                breaks = c(-1, 0, 1),
                                labels = c('Low\nscore', '', 'High\nscore'),
                                oob = scales::squish) +
  ggplot2::scale_x_continuous(name = '',
                              breaks = c(1, 100),
                              labels = c('Lowest\nRR', 'Highest\nRR')) +
  ggplot2::scale_y_continuous(name = '',
                              trans = 'reverse') +
  ## splitting by exposure
  ### lines
  ggplot2::geom_segment(data = data.frame(x = rep(-Inf, times = 6), xend = rep(Inf, times = 6),
                                          y = seq(from = 1.5, to = 6.5, by = 1),
                                          yend = seq(from = 1.5, to = 6.5, by = 1)),
                        aes(x = x, xend = xend, y = y, yend = yend),
                        colour = 'grey') +
  ### labels
  ggplot2::annotate('text',
                    x = rep(1, times = 7),
                    y = 1:7,
                    label = stringr::str_wrap(levels(profile.data$exposure), width = 12.5),
                    fontface = 'bold',
                    colour = 'black',
                    size = 4,
                    hjust = 0,
                    vjust = 0.5) +
  my.theme(text = ggplot2::element_text(size = text.size),
           legend.key.width = unit(2, 'cm'),
           legend.position = 'bottom',
           legend.title = ggplot2::element_text(hjust = 0.5),
           axis.text.y = ggplot2::element_blank(),
           axis.ticks.y = ggplot2::element_blank()); profile.plot

## 4.3. save ----

setwd(dir.res.main.analysis)
# png
ggplot2::ggsave(plot = profile.plot,
                filename = 'png/MODEL_RESULTS_SMR_SPATIAL_PROFILE_PLOT.png',
                height = height,
                width = width)
# txt
ggplot2::ggsave(plot = profile.plot,
                filename = 'eps/MODEL_RESULTS_SMR_SPATIAL_PROFILE_PLOT.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)

## 4.4. lowest RR area map ----

profile.data.lowestRR <- 
  # to generate all mean
  theta.predicted %>% 
  # SMR = .x/E
  dplyr::mutate(dplyr::across(dplyr::starts_with('theta:'), ~ .x /E)) %>% 
  # calculate posterior summaries
  dplyr::mutate(dplyr::select(., dplyr::starts_with('theta:')) %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>% 
                  do.call(rbind, .)) %>% 
  # rank
  dplyr::mutate(index.tile = 
                  mean %>% 
                  cut(x = ., breaks = stats::quantile(x = ., probs = 0:n.tiles/n.tiles), include.lowest = TRUE) %>% 
                  as.factor() %>% 
                  as.numeric())  %>% 
  # aggergate over rank
  dplyr::summarise(index.tile.1 = any(index.tile == 1) %>% as.integer() %>% factor(., levels = 0:1, labels = c('Never', 'At least once')),
                   .by = 'MSOA11CD') %>% 
  dplyr::left_join(.,
                   poly.msoa.england,
                   by = 'MSOA11CD') %>% 
  ## make a spatial object
  sf::st_as_sf()

profile.plot.lowestRR <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = profile.data.lowestRR, aes(fill = index.tile.1), colour = NA) +
  ggplot2::geom_sf(data = poly.rgn.england, fill = NA, colour = 'black', size = 0.5) +
  ggplot2::scale_fill_manual(name = '',
                             values = c('Never' = 'white',
                                        'At least once' = 'black')) +
  my.map.theme(text = ggplot2::element_text(size = text.size),
               legend.position = 'none',
               legend.key.width = unit(2, 'cm'))

setwd(dir.res.main.analysis)
# png
ggplot2::ggsave(plot = profile.plot.lowestRR,
                filename = 'png/MODEL_RESULTS_SMR_SPATIAL_PROFILE_PLOT_LOWEST_RR.png',
                height = height,
                width = width)
# txt
ggplot2::ggsave(plot = profile.plot.lowestRR,
                filename = 'eps/MODEL_RESULTS_SMR_SPATIAL_PROFILE_PLOT_LOWEST_RR.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)

## 4.5. clear ----

rm(n.tiles,
   profile.data,
   profile.plot,
   profile.data.lowestRR,
   profile.plot.lowestRR)
gc()


# 5. parameters ----

## 5.1. socio-environmental ----

theta.exposure.parameters <-
  temp.parameters %>% 
  purrr::map(.x = .,
             .f = function(x){ x$fixed.effects }) %>% 
  purrr::map2(.x = .,
              .y = names(.),
              .f = { ~ .x %>% dplyr::rename(theta = value) %>% dplyr::mutate(source = .y)}) %>% 
  dplyr::bind_rows() %>%
  tidyr::pivot_wider(names_from = source, values_from = theta) %>% 
  dplyr::filter(!(parameter %>% stringr::str_ends('intercept'))) %>% 
  dplyr::mutate(model = dplyr::if_else(startsWith(x = parameter, prefix = 'z.'), distribution.name[1], distribution.name[2]) %>% factor(., levels = distribution.name),
                parameter = gsub('.*\\.', '', parameter))

exposure.parameters.template <- 
  expand.grid(model = distribution.name,
              exposure = exposure.data.frame$name.inla) %>% 
  dplyr::mutate(parameter = paste0(exposure, '_id'))

exposure.parameter.data <-
  # bind model parameters together
  theta.exposure.parameters %>% 
  # join the labeling template and organise
  dplyr::right_join(., exposure.parameters.template, by = c('model', 'parameter')) %>% 
  dplyr::relocate(model, exposure, .before = parameter) %>% 
  # reference levels: turn NAs to 0
  # factors for ordering
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0)),
                exposure = exposure %>% factor(., levels = exposure.data.frame$name.inla)) %>%
  # order
  dplyr::arrange(model, exposure) %>% 
  # summarise posterior
  dplyr::mutate(dplyr::select(., dplyr::starts_with('theta:')) %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>% 
                  do.call(rbind, .),
                exposure = exposure %>% factor(., levels = exposure.data.frame$name.inla, labels = exposure.data.frame$name.label)) %>% 
  # remove thetas for parsimony
  dplyr::select(-dplyr::starts_with('theta:')) %>%
  dplyr::mutate(index.exposure = exposure %>% as.numeric())

exposure.parameter.plot <-
  ggplot2::ggplot(data = exposure.parameter.data, aes(x = index.exposure, y = median)) +
  ggplot2::geom_hline(yintercept = 0, colour = 'black', linetype = 'dashed') +
  ggplot2::geom_point(position = ggplot2::position_dodge(0.5), aes(group = model, colour = model)) +
  ggplot2::geom_errorbar(aes(ymin = lower, ymax = upper, group = model, colour = model), width = 0, position = ggplot2::position_dodge(0.5)) +
  ggplot2::scale_colour_manual(values = c('red3', 'blue3')) +
  ggplot2::scale_x_continuous(name = '',
                              trans = 'reverse') +
  ggplot2::scale_y_continuous(name = '') +
  ## splitting by exposure
  ### lines
  ggplot2::geom_segment(data = data.frame(y = rep(-Inf, times = 6), yend = rep(Inf, times = 6),
                                          x = seq(from = 1.5, to = 6.5, by = 1),
                                          xend = seq(from = 1.5, to = 6.5, by = 1)),
                        aes(x = x, xend = xend, y = y, yend = yend),
                        colour = 'grey') +
  ### labels
  ggplot2::annotate('text',
                    y = rep(-0.2, times = 7),
                    x = 1:7,
                    label = stringr::str_wrap(levels(exposure.parameter.data$exposure), width = 12.5),
                    fontface = 'bold',
                    colour = 'black',
                    size = 4,
                    hjust = 0,
                    vjust = 0.5) +
  # make sure coord_flip at the end
  ggplot2::coord_flip() +
  my.theme(text = ggplot2::element_text(size = text.size),
           legend.title = ggplot2::element_blank(),
           legend.position = 'bottom',
           axis.text.y = ggplot2::element_blank(),
           axis.ticks.y = ggplot2::element_blank()); exposure.parameter.plot

setwd(dir.res.main.analysis)
ggplot2::ggsave(plot = exposure.parameter.plot,
                filename = 'png/MODEL_RESULTS_PARAMETERS_EXPOSURE.png',
                width = width, 
                height = height)
ggplot2::ggsave(plot = exposure.parameter.plot,
                filename = 'eps/MODEL_RESULTS_PARAMETERS_EXPOSURE.eps',
                device = 'eps',
                dpi = 1200,
                width = width, 
                height = height)

rm(theta.exposure.parameters,
   exposure.parameters.template,
   exposure.parameter.plot)
gc()

## 5.2. full parameter tables ----

### 5.2.1. data organise ----

setwd(dir.res.model.fit)
load(file = 'MODEL_FIT_MODEL_FIT.rda')

parameters.fixed.template <-
  exposure.data.frame %>% 
  dplyr::select(name.inla, name.label) %>% 
  # one set for zero and count
  dplyr::cross_join(.,
                    data.frame(model = distribution.name)) %>% 
  # make sure inla.name is correct with prefix and suffix
  # ad factor for ordering
  dplyr::mutate(name.inla = dplyr::if_else(model == distribution.name[1], paste0('z.', name.inla, '_id'), paste0('y.', name.inla, '_id')),
                name.label = name.label %>% factor(., levels = exposure.data.frame$name.label),
                model = model %>% factor(., levels = distribution.name))

digits.table.parameter <- 5

parameters.fixed <- 
  # join to template
  parameters.fixed.template %>% 
  dplyr::left_join(.,
                   fit$fit$summary.fixed %>% 
                     tibble::rownames_to_column(., var = 'name.inla'),
                   by = 'name.inla') %>% 
  dplyr::rename(lower = `0.025quant`,
                median = `0.5quant`,
                upper = `0.975quant`) %>% 
  dplyr::select(-mean, -sd, -mode, -kld) %>% 
  dplyr::mutate(estimate = paste0(median %>% round(., digits = digits.table.parameter), ' (', lower %>% round(., digits = digits.table.parameter), ', ', upper %>% round(., digits = digits.table.parameter), ')')) %>% 
  dplyr::select(name.label, model, estimate) %>% 
  # dplyr::mutate(model = dplyr::if_else(startsWith(x = parameter, prefix = 'z.'), distribution.name[1], distribution.name[2]) %>% factor(., levels = distribution.name),
  #               parameter = gsub('.*\\.', '', parameter)) %>% 
  tidyr::pivot_wider(names_from = model, values_from = estimate)

parameters.random <-
  fit$fit$summary.hyperpar %>% 
  tibble::rownames_to_column(., var = 'parameter') %>%
  dplyr::rename(lower = `0.025quant`,
                median = `0.5quant`,
                upper = `0.975quant`) %>% 
  dplyr::mutate(estimate = paste0(median %>% round(., digits = digits.table.parameter), ' (', lower %>% round(., digits = digits.table.parameter), ', ', upper %>% round(., digits = digits.table.parameter), ')')) %>% 
  dplyr::select(parameter, estimate) %>% 
  dplyr::mutate(model = dplyr::if_else(grepl(pattern = 'z\\.', x = parameter), distribution.name[1], distribution.name[2]) %>% factor(., levels = distribution.name),
                parameter = gsub('[a-zA-Z]\\.', '', parameter))

### 5.2.2. save ----

setwd(dir.res.main.analysis)
# txt
print(xtable::xtable(parameters.fixed),
      include.rownames = FALSE,
      file = 'txt/MODEL_RESULTS_PARAMETERS_FIXED.txt')
print(xtable::xtable(parameters.random),
      include.rownames = FALSE,
      file = 'txt/MODEL_RESULTS_PARAMETERS_RANDOM.txt')

### 5.2.3. clear ----

rm(fit,
   digits.table.parameter,
   parameters.fixed,
   parameters.random)

# 6. Additional values ----

## 6.1. temporal smrs ----

### 6.1.1. national (check) ----

# national SMR
theta.predicted %>% 
  dplyr::summarise(Y = sum(Y), 
                   E = sum(E),
                   N = sum(N),
                   dplyr::across(dplyr::starts_with('theta:'), ~ .x %>% sum())) %>%  
  # SMR = .x/E
  dplyr::mutate(dplyr::across(dplyr::starts_with('theta:'), ~ .x /E)) %>% 
  # calculate posterior summaries
  dplyr::mutate(dplyr::select(., dplyr::starts_with('theta:')) %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>% 
                  do.call(rbind, .)) %>% 
  # drop full samples
  dplyr::select(median, lower, upper) %>% 
  print(., digits = 3)

### 6.1.2. all years ----

theta.predicted %>% 
  dplyr::summarise(Y = sum(Y), 
                   E = sum(E),
                   N = sum(N),
                   dplyr::across(dplyr::starts_with('theta:'), ~ .x %>% sum()),
                   .by = c('YEAR')) %>%  
  # SMR = .x/E
  dplyr::mutate(dplyr::across(dplyr::starts_with('theta:'), ~ .x /E)) %>% 
  # calculate posterior summaries
  dplyr::mutate(dplyr::select(., dplyr::starts_with('theta:')) %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>% 
                  do.call(rbind, .)) %>% 
  # drop full samples
  dplyr::select(YEAR, median, lower, upper) %>%
  print(., digits = 2)

### 6.1.3. start and end ----

theta.predicted %>% 
  dplyr::summarise(Y = sum(Y), 
                   E = sum(E),
                   N = sum(N),
                   dplyr::across(dplyr::starts_with('theta:'), ~ .x %>% sum()),
                   .by = c('YEAR')) %>%  
  # SMR = .x/E
  dplyr::mutate(dplyr::across(dplyr::starts_with('theta:'), ~ .x /E)) %>% 
  # calculate posterior summaries
  dplyr::mutate(dplyr::select(., dplyr::starts_with('theta:')) %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>% 
                  do.call(rbind, .)) %>% 
  # drop full samples
  dplyr::select(YEAR, median, lower, upper) %>%
  print(., digits = 2) %>% 
  dplyr::filter(YEAR %in% c(2002, 2022))

### 6.1.4. min/max year ----

theta.predicted %>% 
  dplyr::summarise(Y = sum(Y), 
                   E = sum(E),
                   N = sum(N),
                   dplyr::across(dplyr::starts_with('theta:'), ~ .x %>% sum()),
                   .by = c('YEAR')) %>%  
  # SMR = .x/E
  dplyr::mutate(dplyr::across(dplyr::starts_with('theta:'), ~ .x /E)) %>% 
  # calculate posterior summaries
  dplyr::mutate(dplyr::select(., dplyr::starts_with('theta:')) %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>% 
                  do.call(rbind, .)) %>% 
  # drop full samples
  dplyr::select(YEAR, median, lower, upper) %>%
  print(., digits = 2) %>% 
  dplyr::filter(median == min(median) | median == max(median))

### 6.1.5. end/start ----

theta.predicted %>% 
  dplyr::mutate(START_END = dplyr::case_when(YEAR == 2002 ~ 0,
                                             YEAR == 2022 ~ 1,
                                             .default = 2)) %>%
  dplyr::filter(START_END != 2) %>% 
  dplyr::summarise(Y = sum(Y), 
                   E = sum(E),
                   N = sum(N),
                   dplyr::across(dplyr::starts_with('theta:'), ~ .x %>% sum()),
                   .by = c('START_END')) %>%  
  # SMR = .x/E
  dplyr::mutate(dplyr::across(dplyr::starts_with('theta:'), ~ .x /E)) %>%  
  # change
  ## make long
  tidyr::pivot_longer(cols = dplyr::starts_with('theta:'), names_to = 'theta', names_prefix = 'theta:', values_to = 'value') %>% 
  ## find avergae difference
  dplyr::summarise(rho = (value[START_END == 1] - value[START_END == 0])/value[START_END == 0] * 100,
                   .by = 'theta') %>% 
  ## retrieve to wider format
  tidyr::pivot_wider(names_from = 'theta', values_from = 'rho', names_prefix = 'theta:', names_sep = '') %>% 
  # calculate posterior summaries
  dplyr::mutate(dplyr::select(., dplyr::starts_with('theta:')) %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>% 
                  do.call(rbind, .)) %>% 
  # drop full samples
  dplyr::select(median, lower, upper) %>%
  print(., digits = 4)

## 6.2. spatio-temporal ----

### 6.2.1. region-year over/under one ----

theta.predicted %>% 
  dplyr::summarise(Y = sum(Y), 
                   E = sum(E),
                   N = sum(N),
                   dplyr::across(dplyr::starts_with('theta:'), ~ .x %>% sum()),
                   .by = c('RGN21NM', 'YEAR')) %>%  
  # SMR = .x/E
  dplyr::mutate(dplyr::across(dplyr::starts_with('theta:'), ~ .x /E)) %>% 
  # calculate posterior summaries
  dplyr::mutate(dplyr::select(., dplyr::starts_with('theta:')) %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>% 
                  do.call(rbind, .)) %>% 
  # drop full samples
  dplyr::mutate(more.than.one = dplyr::if_else(median > 1, 1, 0),
                less.than.one = dplyr::if_else(median < 1, 1, 0)) %>% 
  dplyr::summarise(more.than.one = more.than.one %>% sum(),
                   less.than.one = less.than.one %>% sum(),
                   .by = 'RGN21NM') %>%
  print(., digits = 2)

### 6.2.2. region-year over/under one: south west ----

## North and South West
theta.predicted %>% 
  dplyr::summarise(Y = sum(Y), 
                   E = sum(E),
                   N = sum(N),
                   dplyr::across(dplyr::starts_with('theta:'), ~ .x %>% sum()),
                   .by = c('RGN21NM', 'YEAR')) %>%  
  # SMR = .x/E
  dplyr::mutate(dplyr::across(dplyr::starts_with('theta:'), ~ .x /E)) %>% 
  # calculate posterior summaries
  dplyr::mutate(dplyr::select(., dplyr::starts_with('theta:')) %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>% 
                  do.call(rbind, .)) %>% 
  # drop full samples
  dplyr::mutate(more.than.one = dplyr::if_else(median > 1, 1, 0),
                less.than.one = dplyr::if_else(median < 1, 1, 0)) %>% 
  dplyr::filter(RGN21NM %in% c('South West'),
                less.than.one == 1) %>%
  dplyr::select(RGN21NM, YEAR, median, lower, upper, less.than.one) %>% 
  print(., digits = 2)


### 6.2.3. largest region / smallest region ----

theta.predicted %>% 
  dplyr::mutate(HIGH_LOW = dplyr::case_when(RGN21NM == 'North East' ~ 1,
                                            RGN21NM == 'London' ~ 0,
                                            .default = 2)) %>% 
  dplyr::filter(HIGH_LOW != 2) %>% 
  dplyr::summarise(Y = sum(Y), 
                   E = sum(E),
                   N = sum(N),
                   dplyr::across(dplyr::starts_with('theta:'), ~ .x %>% sum()),
                   .by = c('HIGH_LOW')) %>%  
  # SMR = .x/E
  dplyr::mutate(dplyr::across(dplyr::starts_with('theta:'), ~ .x /E)) %>%  
  # change
  ## make long
  tidyr::pivot_longer(cols = dplyr::starts_with('theta:'), names_to = 'theta', names_prefix = 'theta:', values_to = 'value') %>% 
  ## find avergae difference
  dplyr::summarise(rho = (value[HIGH_LOW == 1] - value[HIGH_LOW == 0])/value[HIGH_LOW == 0] * 100,
                   .by = 'theta') %>% 
  ## retrieve to wider format
  tidyr::pivot_wider(names_from = 'theta', values_from = 'rho', names_prefix = 'theta:', names_sep = '') %>% 
  # calculate posterior summaries
  dplyr::mutate(dplyr::select(., dplyr::starts_with('theta:')) %>% 
                  apply(., 1, my.summary) %>% 
                  lapply(., data.frame) %>% 
                  do.call(rbind, .)) %>% 
  # drop full samples
  dplyr::select(median, lower, upper) %>%
  print(., digits = 4)

# 0 set up ----

## 0.1. packages ----

if(!require('tidyverse')) {
  install.packages('tidyverse', dep = TRUE)
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
dir.data.suicide <- paste0(dir.data, '/suicide')
dir.res.data.explore <- paste0(dir.res, '/01a_dataExploration')

## 0.3. import ----

### 0.3.1. functions ----

setwd(dir.code)
source('00_functions.R')

### 0.3.2. organised data ----

setwd(dir.data.organised)
# both month and year
load('ENGLAND_SPATIAL_INFORMATION_LSOA_MSOA_LAD.rda')
load('MSOA11_POPULATION_TOTAL.rda')
load('MSOA11_COVARIATES.rda')

# suicide data
setwd(dir.data.suicide)
suicide.data.extract <- read.csv('E20241210_DEATHS_CONNOR.csv')

### 0.3.3. look up ----

setwd(dir.data.spatial)
lookup.lsoa.msoa <- read.csv('OA11_LSOA11_MSOA11_LAD11_lookUp.csv')
lookup.msoa.rgn <-  
  read.csv('MSOA11_LAD11_RGN11_lookUp.csv') %>% 
  # renaming
  ## RGN21NM - for plotting as cannot find RGN11 poly and nothing has changed
  dplyr::rename(RGN21NM = 'RGN11NM')

## 0.4. formatting arguments ----

### 0.4.1. labelling data frames ----

year.min <- 2002
year.max <- 2022

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

age.label <- c('[0, 5)', '[5, 15)', '[15, 25)', '[25, 35)', '[35, 45)', '[45, 55)', '[55, 65)', '[65, 75)', '[75, 85)', '85+')
sex.label <- c('MALES', 'FEMALES')

### 0.4.2. plot ----

# saving
height <- width <- 10
text.size <- 20

# colour schemes
colours.diverging <- c('#007D5E', '#E9E0C8', '#C13E2E')
colours.exceedance <- c('#A0D6D0', '#009B92', '#00768D')
colours.sequential <- c('#E9E0C8', '#A175A7')

# 1. build data ----

## 1.1. suicides by msoa ----

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


## 1.2. balanced data ----

# all combinations of MSOA-YEAR-MONTH-AGE-SEX
data.balanced <- 
  tidyr::expand_grid(MSOA11CD = info.msoa$MSOA11CD,
                     YEAR = year.min:year.max,
                     AGE_CLASS = age.label[-c(1,2)],
                     SEX = sex.label)

## 1.3. reference data ----

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

## 1.4. final data ----

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
  # join additional covariates
  ## region 
  dplyr::left_join(., lookup.msoa.rgn %>% dplyr::select(MSOA11CD, RGN21NM),
                   by = c('MSOA11CD')) %>%
  ## socio-environmental
  dplyr::left_join(., covariates.msoa, 
                   by = c('MSOA11CD', 'YEAR')) %>%   
  ## correct space, time, space-time labels
  dplyr::left_join(., space.year.label, by = c('MSOA11CD', 'YEAR'))

# 2. data exploration ----

## 2.1. counts ----

### 2.1.1. box plots ----

boxplot.data <-
  data.final %>% 
  dplyr::rename(N = POPULATION)

boxplot.population <-
  ggplot2::ggplot(data = boxplot.data, aes(x = YEAR, y = N, group = YEAR)) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(x = '', y = 'Population') +
  ggplot2::scale_x_continuous(breaks = seq(from = 2002, to = 2022, by = 2), limits = c(2001, 2023)) +
  my.theme(text = ggplot2::element_text(size = text.size))

boxplot.observations <-
  ggplot2::ggplot(data = boxplot.data, aes(x = YEAR, y = Y, group = YEAR)) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(x = '', y = 'Suicides') +
  ggplot2::scale_x_continuous(breaks = seq(from = 2002, to = 2022, by = 2), limits = c(2001, 2023)) +
  my.theme(text = ggplot2::element_text(size = text.size))

setwd(dir.res.data.explore)
# png
ggplot2::ggsave(plot = boxplot.population,
                filename = 'png/DATA_EXPLORATION_BOXPLOT_POPULATION.png',
                width = width, 
                height = height)
ggplot2::ggsave(plot = boxplot.observations,
                filename = 'png/DATA_EXPLORATION_BOXPLOT_OBSERVATION.png',
                width = width, 
                height = height)
# eps
ggplot2::ggsave(plot = boxplot.population,
                filename = 'eps/DATA_EXPLORATION_BOXPLOT_POPULATION.eps',
                device = 'eps',
                dpi = 1200,
                width = width, 
                height = height)
ggplot2::ggsave(plot = boxplot.observations,
                filename = 'eps/DATA_EXPLORATION_BOXPLOT_OBSERVATION.eps',
                device = 'eps',
                dpi = 1200,
                width = width, 
                height = height)

### 2.1.2. line plot ----

lineplot.data <-
  data.final %>% 
  dplyr::rename(N = POPULATION) %>% 
  dplyr::filter(YEAR %in% year.min:year.max) %>% 
  dplyr::summarise(Y = sum(Y),
                   N = sum(N),
                   E = sum(E),
                   .by = 'YEAR')

lineplot.population <-
  ggplot2::ggplot(data = lineplot.data, aes(x = YEAR, y = N)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(x = '', y = 'Population') +
  ggplot2::scale_x_continuous(breaks = seq(from = 2002, to = 2022, by = 2), limits = c(2002, 2022)) +
  my.theme(text = ggplot2::element_text(size = text.size))

lineplot.observations <-
  ggplot2::ggplot(data = lineplot.data, aes(x = YEAR, y = Y)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(x = '', y = 'Suicides') +
  ggplot2::scale_x_continuous(breaks = seq(from = 2002, to = 2022, by = 2), limits = c(2002, 2022)) +
  my.theme(text = ggplot2::element_text(size = text.size))

setwd(dir.res.data.explore)# png
ggplot2::ggsave(plot = lineplot.population,
                filename = 'png/DATA_EXPLORATION_LINEPLOT_POPULATION.png',
                width = width, 
                height = height)
ggplot2::ggsave(plot = lineplot.observations,
                filename = 'png/DATA_EXPLORATION_LINEPLOT_OBSERVATION.png',
                width = width, 
                height = height)
# eps
ggplot2::ggsave(plot = lineplot.population,
                filename = 'eps/DATA_EXPLORATION_LINEPLOT_POPULATION.eps',
                device = 'eps',
                dpi = 1200,
                width = width, 
                height = height)
ggplot2::ggsave(plot = lineplot.observations,
                filename = 'eps/DATA_EXPLORATION_LINEPLOT_OBSERVATION.eps',
                device = 'eps',
                dpi = 1200,
                width = width, 
                height = height)

## 2.2. rates ----

### 2.2.1. temporal x region ----

rate.temporal.region.data <-
  data.final %>% 
  dplyr::rename(N = POPULATION) %>% 
  dplyr::filter(YEAR %in% year.min:year.max) %>% 
  dplyr::summarise(Y = sum(Y),
                   N = sum(N),
                   E = sum(E),
                   .by = c('YEAR', 'RGN21NM'))

rate.temporal.region.plot <- 
  ggplot2::ggplot(data = rate.temporal.region.data, aes(x = YEAR, y = 100000*Y/N, group = RGN21NM, colour = RGN21NM)) +
  # ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::scale_x_continuous(name = '', breaks = seq(from = year.min, to = 2022, by = 2), limits = c(year.min, 2022)) +
  ggplot2::scale_y_continuous(name = 'Suicide Rate (per 100,000)') +
  my.theme(text = ggplot2::element_text(size = text.size),
           legend.title = ggplot2::element_blank(),
           legend.position = c(0.2, 0.80)); rate.temporal.region.plot

setwd(dir.res.data.explore)
# png
ggplot2::ggsave(plot = rate.temporal.region.plot,
                filename = 'png/DATA_EXPLORATION_RATE_TEMPORAL_REGION.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = rate.temporal.region.plot,
                filename = 'eps/DATA_EXPLORATION_RATE_TEMPORAL_REGION.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)

### 2.2.3. temporal x age ----

rate.temporal.age.data <-
  data.final %>% 
  dplyr::rename(N = POPULATION) %>% 
  dplyr::mutate(AGE_CLASS = AGE_CLASS %>% factor(., levels = age.label[-c(1,2)])) %>% 
  dplyr::filter(YEAR %in% year.min:year.max) %>% 
  dplyr::summarise(Y = sum(Y),
                   N = sum(N),
                   E = sum(E),
                   .by = c('YEAR', 'AGE_CLASS')) %>% 
  dplyr::mutate(R = 100000*Y/N)

limits.rate <- c(5, 16)
breaks.rate <- seq(from = limits.rate[1], to = limits.rate[2], by = limits.rate[2]/4)

rate.temporal.age.plot <- 
  ggplot2::ggplot(data = rate.temporal.age.data, aes(x = YEAR, y = AGE_CLASS, fill = R)) +
  ggplot2::geom_tile() +
  ggplot2::scale_fill_gradient(name = 'Suicide Rate (per 100,000)',
                               guide = ggplot2::guide_colorbar(title.position = 'top'),
                               low = colours.sequential[1],
                               high = colours.sequential[2],
                               limits = limits.rate,
                               breaks = breaks.rate) +
  ggplot2::scale_x_continuous(name = '', breaks = seq(from = year.min, to = 2022, by = 2), limits = c(year.min, 2022)) +
  ggplot2::scale_y_discrete(name = '', limits = rev) +
  my.theme(text = ggplot2::element_text(size = text.size),
           legend.key.width = unit(2, 'cm'),
           legend.title = ggplot2::element_text(hjust = 0.5),
           legend.position = 'bottom'); rate.temporal.age.plot

setwd(dir.res.data.explore)
# png
ggplot2::ggsave(plot = rate.temporal.age.plot,
                filename = 'png/DATA_EXPLORATION_RATE_TEMPORAL_AGE.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = rate.temporal.age.plot,
                filename = 'eps/DATA_EXPLORATION_RATE_TEMPORAL_AGE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)

## temporal x sex ----

rate.temporal.sex.data <-
  data.final %>% 
  dplyr::rename(N = POPULATION) %>% 
  dplyr::mutate(SEX = SEX %>% factor(., levels = sex.label)) %>% 
  dplyr::filter(YEAR %in% year.min:year.max) %>% 
  dplyr::summarise(Y = sum(Y),
                   N = sum(N),
                   E = sum(E),
                   .by = c('YEAR', 'SEX'))

rate.temporal.sex.plot <- 
  ggplot2::ggplot(data = rate.temporal.sex.data, aes(x = YEAR, y = 100000*Y/N, group = SEX, colour = SEX)) +
  ggplot2::geom_line() +
  ggplot2::scale_color_manual(values = c('red3', 'blue3')) +
  ggplot2::scale_x_continuous(name = '', breaks = seq(from = year.min, to = 2022, by = 2), limits = c(year.min, 2022)) +
  ggplot2::scale_y_continuous(name = 'Suicide Rate (per 100,000)') +
  my.theme(text = ggplot2::element_text(size = text.size),
           legend.title = ggplot2::element_blank(),
           legend.position = c(0.2, 0.90)); rate.temporal.sex.plot

setwd(dir.res.data.explore)
# png
ggplot2::ggsave(plot = rate.temporal.sex.plot,
                filename = 'png/DATA_EXPLORATION_RATE_TEMPORAL_SEX.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = rate.temporal.sex.plot,
                filename = 'eps/DATA_EXPLORATION_RATE_TEMPORAL_SEX.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)

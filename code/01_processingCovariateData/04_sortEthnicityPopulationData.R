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
dir.masterData.ethnic <- paste0(dir.masterData, '/ethnicDiversity')
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
colours.sequential <- c('#E9E0C8', '#7D26CD')

## 0.6. import ----

### 0.6.1. ethnicity data ----

setwd(dir.masterData.ethnic)
ed.01.import <- 
  read.csv('2001_lsoa_ethnicDiversity.csv', skip = 7) %>% 
  # remove last three rows as they are empty
  dplyr::filter(row_number() <= n() - 3) 
ed.11.import <- read.csv('2011_lsoa_ethnicDiversity.csv')
ed.21.import <- read.csv('2021_lsoa_ethnicDiversity.csv')

# 1. sort ethncity data ----

## 1.1. 2001 census ----

ed.01.temp <-
  # all the LSOA11CD
  data.frame(LSOA11CD = poly.lsoa.england$LSOA11CD) %>% 
  # join MSOA11 and LAD11 codes
  dplyr::left_join(.,
                   lookup.11 %>% dplyr::select(LSOA11CD, MSOA11CD, LAD11CD),
                   by = 'LSOA11CD') %>% 
  dplyr::left_join(.,
                   # sort and join ed 2001
                   ed.01.import %>% 
                     ## make proportions
                     dplyr::mutate(
                       ## make this numeric as it had text in one entry
                       Total = All.categories..Ethnic.group %>% as.numeric(),
                       ## geographical identifiers
                       LSOA01CD = mnemonic,
                       LSOA01NM = X2001.super.output.areas...lower.layer,
                       ## ethnic proportions
                       ### include Chinese in the Asian group
                       Asian = (Asian.Asian.British + Chinese.Other..Chinese),
                       Black = Black.Black.British,
                       Mixed = Mixed,
                       ### only other
                       Other = Chinese.Other..Other,
                       White = White) %>%
                     ## join the LSOA11CD
                     dplyr::left_join(., 
                                      lookup.01.11 %>% 
                                        dplyr::select(LSOA01CD, LSOA11CD),
                                      by = 'LSOA01CD',
                                      relationship = 'many-to-many') %>%
                     ## average over repeated LSOAs because multiple LSOA01CD got to LSOA11CD
                     dplyr::summarise(Asian = Asian %>% sum(),
                                      Black =  Black %>% sum(),
                                      Mixed =  Mixed %>% sum(),
                                      Other =  Other %>% sum(),
                                      White =  White %>% sum(),
                                      Total = Total %>% sum(),
                                      .by = 'LSOA11CD'),
                   # join by the newly created LSOA11CDs 
                   by = 'LSOA11CD') %>% 
  # join spatial polygons to impute missing data
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

ed.01.missing <- 
  ed.01.temp %>% 
  dplyr::filter(rowSums(is.na(.)) > 0) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(LSOA11CD, LSOA11NM) %>% 
  dplyr::mutate(censusYear = 2001)

ed.01.sorted <- 
  ed.01.temp %>% 
  dplyr::mutate(censusYear = 2001)

if(nrow(ed.01.missing) > 0){
  ed.01.sorted$Asian[is.na(ed.01.sorted$Asian)] <-
    sapply(lsoa.nbs[which(is.na(ed.01.sorted$Asian))],
           FUN = function(x) { mean(ed.01.sorted$Asian[x], na.rm = TRUE)})
  ed.01.sorted$Black[is.na(ed.01.sorted$Black)] <-
    sapply(lsoa.nbs[which(is.na(ed.01.sorted$Black))],
           FUN = function(x) { mean(ed.01.sorted$Black[x], na.rm = TRUE)})
  ed.01.sorted$Mixed[is.na(ed.01.sorted$Mixed)] <-
    sapply(lsoa.nbs[which(is.na(ed.01.sorted$Mixed))],
           FUN = function(x) { mean(ed.01.sorted$Mixed[x], na.rm = TRUE)})
  ed.01.sorted$Other[is.na(ed.01.sorted$Other)] <-
    sapply(lsoa.nbs[which(is.na(ed.01.sorted$Other))],
           FUN = function(x) { mean(ed.01.sorted$Other[x], na.rm = TRUE)})
  ed.01.sorted$White[is.na(ed.01.sorted$White)] <-
    sapply(lsoa.nbs[which(is.na(ed.01.sorted$White))],
           FUN = function(x) { mean(ed.01.sorted$White[x], na.rm = TRUE)})
  ed.01.sorted$Total[is.na(ed.01.sorted$Total)] <-
    sapply(lsoa.nbs[which(is.na(ed.01.sorted$Total))],
           FUN = function(x) { mean(ed.01.sorted$Total[x], na.rm = TRUE)})
}

# ed.01.sorted <- 
#   ed.01.temp %>% 
#   dplyr::select(LSOA11CD, MSOA11CD, LAD11CD, Asian, Black, Mixed, Other, White, Total) %>% 
#   sf::st_drop_geometry() %>% 
#   # define rank
#   dplyr::mutate(nonWhite = Asian + Black + Mixed + Other,
#                 censusYear = 2001) 

## 1.2. 2011 census ----

ed.11.temp <-
  # all the LSOA11CD
  data.frame(LSOA11CD = poly.lsoa.england$LSOA11CD) %>%  
  # join MSOA11 and LAD11 codes
  dplyr::left_join(.,
                   lookup.11 %>% dplyr::select(LSOA11CD, MSOA11CD, LAD11CD),
                   by = 'LSOA11CD') %>% 
  dplyr::left_join(.,
                   # sort and join ed 2001
                   ed.11.import %>% 
                     ## make proportions
                     dplyr::mutate(
                       ## geographical identifiers
                       LSOA11CD = geography.code,
                       LSOA11NM = geography,
                       ## Total
                       Total = Ethnic.Group..All.categories..Ethnic.group..measures..Value,
                       ## ethnic proportions
                       Asian = Ethnic.Group..Asian..measures..Value,
                       Black = Ethnic.Group..Black..measures..Value,
                       Mixed = Ethnic.Group..Mixed..measures..Value,
                       Other = Ethnic.Group..Other..measures..Value,
                       White = Ethnic.Group..White..measures..Value) %>%
                     ## average over repeated any potential repeated LSOAs (non for 2011 census)
                     dplyr::summarise(Asian = Asian %>% sum(),
                                      Black =  Black %>% sum(),
                                      Mixed =  Mixed %>% sum(),
                                      Other =  Other %>% sum(),
                                      White =  White %>% sum(),
                                      Total = Total %>% sum(),
                                      .by = 'LSOA11CD'),
                   # join by the newly created LSOA11CDs 
                   by = 'LSOA11CD') %>% 
  # join spatial polygons to impute missing data
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

ed.11.missing <- 
  ed.11.temp %>% 
  dplyr::filter(rowSums(is.na(.)) > 0) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(LSOA11CD, LSOA11NM) %>% 
  dplyr::mutate(censusYear = 2011)

ed.11.sorted <- 
  ed.11.temp %>% 
  dplyr::mutate(censusYear = 2011)

if(nrow(ed.11.missing) > 0){
  ed.11.sorted$Asian[is.na(ed.11.sorted$Asian)] <-
    sapply(lsoa.nbs[which(is.na(ed.11.sorted$Asian))],
           FUN = function(x) { mean(ed.11.sorted$Asian[x], na.rm = TRUE)})
  ed.11.sorted$Black[is.na(ed.11.sorted$Black)] <-
    sapply(lsoa.nbs[which(is.na(ed.11.sorted$Black))],
           FUN = function(x) { mean(ed.11.sorted$Black[x], na.rm = TRUE)})
  ed.11.sorted$Mixed[is.na(ed.11.sorted$Mixed)] <-
    sapply(lsoa.nbs[which(is.na(ed.11.sorted$Mixed))],
           FUN = function(x) { mean(ed.11.sorted$Mixed[x], na.rm = TRUE)})
  ed.11.sorted$Other[is.na(ed.11.sorted$Other)] <-
    sapply(lsoa.nbs[which(is.na(ed.11.sorted$Other))],
           FUN = function(x) { mean(ed.11.sorted$Other[x], na.rm = TRUE)})
  ed.11.sorted$White[is.na(ed.11.sorted$White)] <-
    sapply(lsoa.nbs[which(is.na(ed.11.sorted$White))],
           FUN = function(x) { mean(ed.11.sorted$White[x], na.rm = TRUE)})
  ed.11.sorted$Total[is.na(ed.11.sorted$Total)] <-
    sapply(lsoa.nbs[which(is.na(ed.11.sorted$Total))],
           FUN = function(x) { mean(ed.11.sorted$Total[x], na.rm = TRUE)})
}

# ed.11.sorted <- 
#   ed.11.temp %>% 
#   dplyr::select(LSOA11CD, MSOA11CD, LAD11CD, Asian, Black, Mixed, Other, White, Total) %>% 
#   sf::st_drop_geometry() %>% 
#   # define diversity
#   dplyr::mutate(nonWhite = Asian + Black + Mixed + Other,
#                 censusYear = 2011) 

## 1.3. 2021 census ----

ed.21.temp <-
  # all the LSOA11CD
  data.frame(LSOA11CD = poly.lsoa.england$LSOA11CD) %>%  
  # join MSOA11 and LAD11 codes
  dplyr::left_join(.,
                   lookup.11 %>% dplyr::select(LSOA11CD, MSOA11CD, LAD11CD),
                   by = 'LSOA11CD') %>% 
  dplyr::left_join(.,
                   # sort and join ed 2001
                   ed.21.import %>%
                     ## group by year and make proportions
                     dplyr::mutate(
                       ## geographical identifiers
                       LSOA21CD = Lower.layer.Super.Output.Areas.Code,
                       LSOA21NM = Lower.layer.Super.Output.Areas) %>% 
                     ## remove does not apply code
                     dplyr::filter(Ethnic.group..20.categories..Code > 0) %>% 
                     ## select only relavent columns and expand
                     dplyr::select(LSOA21CD, LSOA21NM, `Ethnic.group..20.categories.`, Observation) %>% 
                     dplyr::mutate(Group =
                                     dplyr::case_when(stringr::str_starts(`Ethnic.group..20.categories.`, pattern = 'Asian') ~ 'Asian',
                                                      stringr::str_starts(`Ethnic.group..20.categories.`, pattern = 'Black') ~ 'Black',
                                                      stringr::str_starts(`Ethnic.group..20.categories.`, pattern = 'Mixed') ~ 'Mixed',
                                                      stringr::str_starts(`Ethnic.group..20.categories.`, pattern = 'Other') ~ 'Other',
                                                      stringr::str_starts(`Ethnic.group..20.categories.`, pattern = 'White') ~ 'White',
                                                      .default = NA)) %>% 
                     dplyr::summarise(Observation = Observation %>% sum(),
                                      .by = c('LSOA21CD', 'LSOA21NM', 'Group')) %>% 
                     tidyr::pivot_wider(., 
                                        names_from = 'Group', 
                                        values_from = 'Observation') %>% 
                     dplyr::mutate(Total = dplyr::select(., -c('LSOA21CD', 'LSOA21NM')) %>% rowSums()) %>% 
                     ## ethnic proportions
                     dplyr::mutate(Asian = Asian,
                                   Black = Black,
                                   Mixed = Mixed,
                                   Other = Other,
                                   White = White) %>% 
                     dplyr::left_join(., 
                                      lookup.11.21 %>% 
                                        dplyr::select(LSOA21CD, LSOA11CD), 
                                      by = 'LSOA21CD',
                                      'many-to-many') %>%
                     ## average over repeated LSOAs because multiple LSOA01CD got to LSOA11CD
                     dplyr::summarise(Asian = Asian %>% sum(),
                                      Black =  Black %>% sum(),
                                      Mixed =  Mixed %>% sum(),
                                      Other =  Other %>% sum(),
                                      White =  White %>% sum(),
                                      Total = Total %>% sum(),
                                      .by = 'LSOA11CD'),
                   # join by the newly created LSOA11CDs 
                   by = 'LSOA11CD') %>% 
  # join spatial polygons to impute missing data
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

ed.21.missing <- 
  ed.21.temp %>% 
  dplyr::filter(rowSums(is.na(.)) > 0) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(LSOA11CD, LSOA11NM) %>% 
  dplyr::mutate(censusYear = 2021)

ed.21.sorted <- 
  ed.21.temp %>% 
  dplyr::mutate(censusYear = 2021)

if(nrow(ed.21.missing) > 0){
  ed.21.sorted$Asian[is.na(ed.21.sorted$Asian)] <-
    sapply(lsoa.nbs[which(is.na(ed.21.sorted$Asian))],
           FUN = function(x) { mean(ed.21.sorted$Asian[x], na.rm = TRUE)})
  ed.21.sorted$Black[is.na(ed.21.sorted$Black)] <-
    sapply(lsoa.nbs[which(is.na(ed.21.sorted$Black))],
           FUN = function(x) { mean(ed.21.sorted$Black[x], na.rm = TRUE)})
  ed.21.sorted$Mixed[is.na(ed.21.sorted$Mixed)] <-
    sapply(lsoa.nbs[which(is.na(ed.21.sorted$Mixed))],
           FUN = function(x) { mean(ed.21.sorted$Mixed[x], na.rm = TRUE)})
  ed.21.sorted$Other[is.na(ed.21.sorted$Other)] <-
    sapply(lsoa.nbs[which(is.na(ed.21.sorted$Other))],
           FUN = function(x) { mean(ed.21.sorted$Other[x], na.rm = TRUE)})
  ed.21.sorted$White[is.na(ed.21.sorted$White)] <-
    sapply(lsoa.nbs[which(is.na(ed.21.sorted$White))],
           FUN = function(x) { mean(ed.21.sorted$White[x], na.rm = TRUE)})
  ed.21.sorted$Total[is.na(ed.21.sorted$Total)] <-
    sapply(lsoa.nbs[which(is.na(ed.21.sorted$Total))],
           FUN = function(x) { mean(ed.21.sorted$Total[x], na.rm = TRUE)})
}

# ed.21.sorted <- 
#   ed.21.temp %>% 
#   dplyr::select(LSOA11CD, MSOA11CD, LAD11CD, Asian, Black, Mixed, Other, White, Total) %>% 
#   sf::st_drop_geometry() %>% 
#   # define diversity
#   dplyr::mutate(nonWhite = Asian + Black + Mixed + Other,
#                 censusYear = 2021)

# 2. combine all ----

## 2.1. missing ----

ed.missing <-
  dplyr::bind_rows(ed.01.missing,
                   ed.11.missing,
                   ed.21.missing)

setwd(dir.res.exploration)
print(x = xtable::xtable(x = ed.missing),
      include.rownames = FALSE,
      file = 'txt/lsoa/LSOA11_ETHNIC_DIVERSITY_MISSING.txt')

## 2.2. ethnicity score ----

ed.00.06.lsoa <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              YEAR = 2000:2006) %>% 
  dplyr::left_join(., ed.01.sorted, by = 'LSOA11CD')

ed.07.16.lsoa <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              YEAR = 2007:2016) %>% 
  dplyr::left_join(., ed.11.sorted, by = 'LSOA11CD')

ed.17.22.lsoa <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              YEAR = 2017:2022) %>% 
  dplyr::left_join(., ed.21.sorted, by = 'LSOA11CD')

ed.lsoa.temp <-
  dplyr::bind_rows(ed.00.06.lsoa,
                   ed.07.16.lsoa,
                   ed.17.22.lsoa) %>% 
  dplyr::select(LSOA11CD, MSOA11CD, LAD11CD, YEAR, censusYear, Asian, Black, Mixed, Other, White, Total) %>%
  sf::st_drop_geometry() %>%
  # define diversity
  dplyr::rowwise() %>% 
  dplyr::mutate(nonWhite = Asian + Black + Mixed + Other) %>% 
  dplyr::ungroup()

nrow(poly.lsoa.england)*length(2000:2022) == nrow(ed.lsoa.temp)
ed.lsoa.temp %>% is.na() %>% sum()

# 3. organise for different level ----

## 2.1. lsoa ----

ed.lsoa <- 
  ed.lsoa.temp %>% 
  # drop the other geographies
  dplyr::select(-MSOA11CD, -LAD11CD) %>% 
  dplyr::arrange(YEAR, LSOA11CD) %>%
  # make sure rowwise
  ## define proportions
  dplyr::rowwise() %>% 
  dplyr::mutate(Asian = Asian / Total,
                Black = Black / Total,
                Mixed = Mixed / Total,
                Other = Other / Total,
                White = White / Total,
                nonWhite = nonWhite / Total) %>% 
  dplyr::ungroup()

setwd(dir.data.organised)
save(ed.lsoa, file = 'lsoa/rda/LSOA11_ETHNIC_DIVERSITY.rda')
write.csv(ed.lsoa, file = 'lsoa/csv/LSOA11_ETHNIC_DIVERSITY.csv', , row.names = FALSE)

## 2.2. msoa ----

ed.msoa <- 
  ed.lsoa.temp %>% 
  # summarise over MSOA
  dplyr::summarise(Asian = Asian %>% sum(),
                   Black = Black %>% sum(),
                   Mixed = Mixed %>% sum(),
                   Other = Other %>% sum(),
                   White = White %>% sum(),
                   nonWhite = nonWhite %>% sum(),
                   Total = Total %>% sum(),
                   .by = c('MSOA11CD', 'YEAR', 'censusYear')) %>% 
  dplyr::arrange(YEAR, MSOA11CD) %>% 
  # make sure rowwise
  ## define proportions
  dplyr::rowwise() %>% 
  dplyr::mutate(Asian = Asian / Total,
                Black = Black / Total,
                Mixed = Mixed / Total,
                Other = Other / Total,
                White = White / Total,
                nonWhite = nonWhite / Total) %>% 
  dplyr::ungroup()

setwd(dir.data.organised)
save(ed.msoa, file = 'msoa/rda/MSOA11_ETHNIC_DIVERSITY.rda')
write.csv(ed.msoa, file = 'msoa/csv/MSOA11_ETHNIC_DIVERSITY.csv', row.names = FALSE)

## 2.3. lad ----

ed.lad <- 
  ed.lsoa.temp %>% 
  # drop the other geographies
  dplyr::select(-LSOA11CD, -MSOA11CD) %>%
  # summarise over LAD
  dplyr::summarise(Asian = Asian %>% sum(),
                   Black = Black %>% sum(),
                   Mixed = Mixed %>% sum(),
                   Other = Other %>% sum(),
                   White = White %>% sum(),
                   nonWhite = nonWhite %>% sum(),
                   Total = Total %>% sum(),
                   .by = c('LAD11CD', 'YEAR', 'censusYear')) %>% 
  dplyr::arrange(YEAR, LAD11CD) %>% 
  # make sure rowwise
  ## define proportions
  dplyr::rowwise() %>% 
  dplyr::mutate(Asian = Asian / Total,
                Black = Black / Total,
                Mixed = Mixed / Total,
                Other = Other / Total,
                White = White / Total,
                nonWhite = nonWhite / Total) %>% 
  dplyr::ungroup()

setwd(dir.data.organised)
save(ed.lad, file = 'lad/rda/LAD11_ETHNIC_DIVERSITY.rda')
write.csv(ed.lad, file = 'lad/csv/LAD11_ETHNIC_DIVERSITY.csv', row.names = FALSE)

# 3. plots ----

## 3.0 function ----

make.plot.ethnicDiversity <- function(data, 
                                      spatial.level = NULL, 
                                      poly, 
                                      lookup = NULL,
                                      title.legend = 'Non-White Population'){
  
  # 0. function arguments ----
  
  # data <- ed.01.sorted.lsoa
  # spatial.level <- c('LSOA11CD', 'MSOA11CD', 'LAD11CD')[1]
  # poly <- poly.lsoa.england
  # lookup <- lookup.11.21.lsoa
  
  # 1. filter and set limits ----
  
  census.year <- data$censusYear %>% unique()
  limits.spatial <- c(0, 1)
  breaks.spatial <- seq(from = limits.spatial[1] , to = limits.spatial[2], by = limits.spatial[2]/4)
  
  # 2. data sort ----
  
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
  
  
  
  # 3. plot ----
  
  plot.london <-
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = plot.data.london, aes(fill = nonWhite), colour = NA) +
    ggplot2::scale_fill_gradient(name = '',
                                 low = colours.sequential[1], high = colours.sequential[2],
                                 limits = limits.spatial, breaks = breaks.spatial, 
                                 labels = scales::label_percent()) +
    my.map.theme(legend.position = 'none')
  
  plot <-
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = plot.data, aes(fill = nonWhite), colour = NA) +
    ggplot2::scale_fill_gradient(name = title.legend,
                                 guide = guide_colourbar(title.position = 'top'),
                                 low = colours.sequential[1], high = colours.sequential[2],
                                 limits = limits.spatial, breaks = breaks.spatial, 
                                 labels = scales::label_percent()) +
    # imd year label label in top right
    ggplot2::annotate('text', x = Inf, y = Inf, label = census.year, 
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

ed.data.list.lsoa <- list(ed.lsoa %>% dplyr::filter(censusYear == 2001),
                          ed.lsoa %>% dplyr::filter(censusYear == 2011),
                          ed.lsoa %>% dplyr::filter(censusYear == 2021))

ed.plots.lsoa <- lapply(X =ed.data.list.lsoa,
                        FUN = make.plot.ethnicDiversity,
                        spatial.level = 'LSOA11CD',
                        poly = poly.lsoa.england,
                        lookup = lookup.11 %>% dplyr::select(LSOA11CD, LAD11CD) %>% unique())

setwd(dir.res.exploration)
animation::saveGIF(
  expr = {for (i in 1:length(ed.plots.lsoa)) { print(ed.plots.lsoa[[i]]) } }, 
  movie.name = paste0(dir.res.exploration, '/gif/lsoa/LSOA11_DIVERSITY.gif'), 
  interval = 0.5,
  ani.width = width * 100,
  ani.height = height * 100)

### 3.1.2. msoa ----

ed.data.list.msoa <- list(ed.msoa %>% dplyr::filter(censusYear == 2001),
                          ed.msoa %>% dplyr::filter(censusYear == 2011),
                          ed.msoa %>% dplyr::filter(censusYear == 2021))

ed.plots.msoa <- lapply(X = ed.data.list.msoa,
                        FUN = make.plot.ethnicDiversity,
                        spatial.level = 'MSOA11CD',
                        poly = poly.msoa.england,
                        lookup = lookup.11 %>% dplyr::select(MSOA11CD, LAD11CD) %>% unique())

setwd(dir.res.exploration)
animation::saveGIF(
  expr = {for (i in 1:length(ed.plots.msoa)) { print(ed.plots.msoa[[i]]) } }, 
  movie.name = paste0(dir.res.exploration, '/gif/msoa/MSOA11_DIVERSITY.gif'), 
  interval = 0.5,
  ani.width = width * 100,
  ani.height = height * 100)

### 3.1.3. lad ----

ed.data.list.lad <- list(ed.lad %>% dplyr::filter(censusYear == 2001),
                         ed.lad %>% dplyr::filter(censusYear == 2011),
                         ed.lad %>% dplyr::filter(censusYear == 2021))

ed.plots.lad <- lapply(X =ed.data.list.lad,
                       FUN = make.plot.ethnicDiversity,
                       spatial.level = 'LAD11CD',
                       poly = poly.lad.england)

setwd(dir.res.exploration)
animation::saveGIF(
  expr = {for (i in 1:length(ed.plots.lad)) { print(ed.plots.lad[[i]]) } }, 
  movie.name = paste0(dir.res.exploration, '/gif/lad/LAD11_DIVERSITY.gif'), 
  interval = 0.5,
  ani.width = width * 100,
  ani.height = height * 100)

## 3.2. plot (average) ----

### 3.2.1. lsoa ----

ed.average.plot.lsoa <- 
  ed.lsoa %>% 
  # average
  dplyr::summarise(nonWhite = nonWhite %>% mean(),
                   .by = 'LSOA11CD') %>% 
  make.plot.ethnicDiversity(data = .,
                            spatial.level = 'LSOA11CD',
                            poly = poly.lsoa.england,
                            lookup = lookup.11 %>% dplyr::select(LSOA11CD, LAD11CD) %>% unique(),
                            title.legend = '')

setwd(dir.res.exploration)
# png
ggplot2::ggsave(plot = ed.average.plot.lsoa,
                filename = 'png/lsoa/LSOA11_DIVERSITY_AVERAGE.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = ed.average.plot.lsoa,
                filename = 'eps/lsoa/LSOA11_DIVERSITY_AVERAGE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)

### 3.2.2. msoa ----

ed.average.plot.msoa <- 
  ed.msoa %>% 
  # average
  dplyr::summarise(nonWhite = nonWhite %>% mean(),
                   .by = 'MSOA11CD') %>% 
  make.plot.ethnicDiversity(data = .,
                            spatial.level = 'MSOA11CD',
                            poly = poly.msoa.england,
                            lookup = lookup.11 %>% dplyr::select(MSOA11CD, LAD11CD) %>% unique(),
                            title.legend = '')

setwd(dir.res.exploration)
# png
ggplot2::ggsave(plot = ed.average.plot.msoa,
                filename = 'png/msoa/MSOA11_DIVERSITY_AVERAGE.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = ed.average.plot.msoa,
                filename = 'eps/msoa/MSOA11_DIVERSITY_AVERAGE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)

### 3.2.2. lad ----

ed.average.plot.lad <- 
  ed.lad %>% 
  # average
  dplyr::summarise(nonWhite = nonWhite %>% mean(),
                   .by = 'LAD11CD') %>% 
  make.plot.ethnicDiversity(data = .,
                            spatial.level = 'LAD11CD',
                            poly = poly.lad.england,
                            title.legend = '')

setwd(dir.res.exploration)
# png
ggplot2::ggsave(plot = ed.average.plot.lad,
                filename = 'png/lad/LAD11_DIVERSITY_AVERAGE.png',
                height = height,
                width = width)
# eps
ggplot2::ggsave(plot = ed.average.plot.lad,
                filename = 'eps/lad/LAD11_DIVERSITY_AVERAGE.eps',
                device = 'eps',
                dpi = 1200,
                height = height,
                width = width)

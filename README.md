# Spatio-temporal trends and socio-environmental determinants of suicides in England from 2002 - 2022: an ecological population-based study

## Overview

### Project description

### 🔧 Running the Code
In general, all code-related files are designed to be as automated as possible. However, a few manual steps are required to get everything working after cloning the repository.

1. 📦 Installing Required Packages
Each `.R` file includes a section titled `## 0.1. packages`. This section will attempt to load or install the necessary `R` packages.

🧠 Note: One package, `R-INLA`, is not on CRAN and requires a custom installation.

Use the following code to install R-INLA:
'''{r}
if (!require('INLA')) {
  install.packages(
    'INLA',
    repos = c(getOption('repos'), INLA = 'https://inla.r-inla-download.org/R/testing'),
    dependencies = TRUE
  )
}
'''
If installation issues occur, please consult the [R-INLA Download & Install guide](https://www.r-inla.org/download-install).

2. 📁 Fixing the Working Directory
Each `.R` script contains a section titled` ## 0.2. directories`. The working directory is determined using:
'''{r}
dir.path <- rstudioapi::getActiveDocumentContext()$path
'''
This works only in RStudio (and only if the file is saved). Then, the root directory is extracted using:
'''{r}
dir.home <- sub('(englishSuicides).*', '\\1', dir.path)
'''
📝 Action Required: If you've renamed the englishSuicides folder, update "englishSuicides" in the line above to match your local folder name.

3. 📂 Providing the Master Dataset
Within the `## 0.2.` directories section in each `.R` file, another directory is referenced to access the required external source data (e.g., mid-year population, IMD, etc.). This folder is not included in the repository.

This is defined as:
'''{r}
dir.masterData <- paste0(sub('(OneDrive - Imperial College London).*', '\\1', dir.path), '/00_masterData')
'''
📥 Action Required: Download the necessary data and store it in the above folder path on your local machine.


### 🗂️ Data Sources

| Data type | Source | Naming Convention | Note |
| -------- | ------- | -------- | ------- |
| Spatial polygons (LSOA11) | [ONS](https://geoportal.statistics.gov.uk/datasets/357ee15b1080431491bf965394090c72_0/explore) | `ONS11_LSOA` | - |
| Spatial polygons (MSOA11) | [ONS](https://geoportal.statistics.gov.uk/datasets/8200e7683bba4de8a1a47e6b1c323099_0/explore) | `ONS11_MSOA` | - |
| Spatial polygons (LAD11) | [ONS](https://geoportal.statistics.gov.uk/datasets/8019e36335064f43ae8f199cb4310fa3_0/explore) | `ONS11_LAD` | - |
| Spatial polygons (NAT21) | [ONS](https://geoportal.statistics.gov.uk/search?q=BDY_CTRY%20DEC_2021&sort=Title%7Ctitle%7Casc) | `ONS21_NAT` | - |
| Look up LSOA01-LSOA11 |[ONS](https://geoportal.statistics.gov.uk/datasets/3dd1bc5dd053426aa84a068c7afbb3b2_0/explore) | `LSOA01_LSOA11_LAD11_lookUp` |- |
| Look up LSOA11-LSOA21 | [ONS](https://geoportal.statistics.gov.uk/datasets/b14d449ba10a48508bd05cd4a9775e2b_0/explore) | `LSOA11_LSOA21_LAD22_lookUp` | - |
| Look up LSOA11-MSOA11-LAD11 | [ONS](https://geoportal.statistics.gov.uk/datasets/d382604321554ed49cc15dbc1edb3de3_0/explore) | `LSOA11_MSOA11_LAD11_lookUp` | - |
| Mid-year population estimates | [ONS](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates) | See 📄 `02_sortPopulationData.R` | Located in `code/01_processingCovariateData/` |
| Index of Multiple Deprivaton | [GOV.UK](https://www.gov.uk/government/collections/english-indices-of-deprivation) | See 📄 `03_sortIMDdata.R` | Located in `code/01_processingCovariateData/` |
| Ethnicity Population Total (2001) | [ONS Nomis](https://www.nomisweb.co.uk/datasets/ks006) | `2001_lsoa_ethnicDiversity` | - |
| Ethnicity Population Total (2011) | [ONS Nomis](https://www.nomisweb.co.uk/census/2011/ks201ew) | `2011_lsoa_ethnicDiversity` | - |
| Ethnicity Population Total (2021) | [ONS Nomis](https://www.nomisweb.co.uk/datasets/c2021ts021) | `2021_lsoa_ethnicDiversity` | - |
| Nighttime Light | [Harmonization of DMSP and VIIRS nighttime light data](https://figshare.com/articles/dataset/Harmonization_of_DMSP_and_VIIRS_nighttime_light_data_from_1992-2018_at_the_global_scale/9828827) | As downloaded | See the [paper](https://www.nature.com/articles/s41597-020-0510-y) |
| Railway network | [OpenStreetMap](https://download.geofabrik.de/) | As downloaded | - |
| Road network | [Ordance Survery (OS)](https://osdatahub.os.uk/downloads/open/OpenRoads) | As downloaded | Requires free OS account |
| NDVI | [NASA Earth Data via MODIStsp](https://docs.ropensci.org/MODIStsp/) | As downloaded | Requires free NASA Eath Data account |


### Repository Structure

englishSuicides/
├── 📁 code/                                 # All coding files
│   ├── 📁 00_simulatingData/
│   │   ├── 📄 01_suicideSimulation.R
│   │   └── 📄 02_covariateSimulation.R
│   ├── 📁 01_processingCovariateData/
│   │   ├── 📄 01_sortSpatialData.R
│   │   ├── 📄 02_sortPopulationData.R
│   │   ├── 📄 03_sortIMDdata.R
│   │   ├── 📄 04_sortEthnicityDensityData.R
│   │   ├── 📄 05_sortPopulationDensityData.R
│   │   ├── 📄 06_sortNighttimeLightData.R
│   │   ├── 📄 07_sortRailwayData.R
│   │   ├── 📄 08_sortRoadData.R
│   │   ├── 📄 09_sortNDVIdata.R
│   │   └── 📄 10_combineAllCovatriates.R
│   ├── 📁 02_processingSuicideData/
│   │   ├── 📄 01_sortSuicideData.R
│   │   └── 📄 02_exploreCovariateData.R
│   ├── 📁 03_modelFit/
│   │   └── 📄 01_fitModel.R
│   ├── 📁 03_mainResults/
│   │   └── 📄 01_generateResults.R
│   └── 📄 00_functions.R
│
├── 📁 results/                              # Generated results
│   ├── 📁 01_covariateData/
│   │   ├── 📁 gif/
│   │   │   └── 📁 msoa/
│   │   │       ├── 🌀 MSOA11_DIVERSITY.gif
│   │   │       ├── 🌀 MSOA11_IMD.gif
│   │   │       ├── 🌀 MSOA11_NDVI.gif
│   │   │       ├── 🌀 MSOA11_NIGHTTIME_LIGHT.gif
│   │   │       ├── 🌀 MSOA11_POPULATION_DENSITY.gif
│   │   │       ├── 🌀 MSOA11_RAIL_DENSITY.gif
│   │   │       └── 🌀 MSOA11_ROAD_DENSITY.gif
│   │   ├── 📁 png/
│   │   │   └── 📁 msoa/
│   │   │       ├── 🖼️ MSOA11_DIVERSITY_AVERAGE.png
│   │   │       ├── 🖼️ MSOA11_IMD_AVERAGE.png
│   │   │       ├── 🖼️ MSOA11_NDVI_AVERAGE.png
│   │   │       ├── 🖼️ MSOA11_NIGHTTIME_LIGHT_AVERAGE.png
│   │   │       ├── 🖼️ MSOA11_POPULATION_DENSITY_AVERAGE.png
│   │   │       ├── 🖼️ MSOA11_RAIL_DENSITY_AVERAGE.png
│   │   │       └── 🖼️ MSOA11_ROAD_DENSITY_AVERAGE.png
│   │   ├── 📁 txt/
│   │   │   ├── 📁 lsoa/
│   │   │   │   ├── 📜 LSOA11_DEPRIVATION_MISSING.txt
│   │   │   │   └── 📜 LSOA11_ETHNIC_DIVERSITY_MISSING.txt
│   │   │   └── 📁 msoa/
│   │   │       ├── 📜 MSOA11_NDVI_MISSING.txt
│   │   │       └── 📜 MSOA11_NIGHTTIME_LIGHTs_MISSING.txt
│
│   ├── 📁 02_suicideData/
│   │   ├── 📁 gif/
│   │   │   └── 🌀 CORRELATION_MATRIX.gif
│   │   └── 📁 png/
│   │       ├── 🖼️ CORRELATION_MATRIX.png
│   │       ├── 🖼️ CORRELATION_MATRIX_IMD_DOMAINS.png
│   │       ├── 🖼️ HISTOGRAM_DEPRIVATION.png
│   │       ├── 🖼️ HISTOGRAM_DIVERSITY.png
│   │       ├── 🖼️ HISTOGRAM_NDVI.png
│   │       ├── 🖼️ HISTOGRAM_NIGHTTIME_LIGHT.png
│   │       ├── 🖼️ HISTOGRAM_POPULATION_DENSITY.png
│   │       ├── 🖼️ HISTOGRAM_RAILWAY_NETWORK_DENSITY.png
│   │       └── 🖼️ HISTOGRAM_ROAD_NETWORK_DENSITY.png
│
│   ├── 📁 03_modelFit/
│   └── 📁 04_mainResults/
│       ├── 📁 png/
│       │   ├── 🖼️ MODEL_RESULT_TEMPORAL_PLOT.png
│       │   ├── 🖼️ MODEL_RESULT_TEMPORAL_REGIONAL_PLOT.png
│       │   ├── 🖼️ MODEL_RESULTS_SMR_CHANGE_EXPOSURE.png
│       │   ├── 🖼️ MODEL_RESULTS_SMR_SPATIAL_PROFILE_PLOT.png
│       │   ├── 🖼️ MODEL_RESULTS_SMR_SPATIO_TEMPORAL_MSOA_REGION_EXCEEDANCE_DECILE.png
│       │   └── 🖼️ MODEL_RESULTS_SMR_SPATIO_TEMPORAL_MSOA_REGION_MEDIAN_DECILE.png
│       └── 📁 txt/
│           ├── 📜 MODEL_RESULTS_PARAMETERS_FIXED.txt
│           ├── 📜 MODEL_RESULTS_PARAMETERS_RANDOM.txt
│           ├── 📜 MODEL_RESULTS_SMR_CHANGE_EXPOSURE.txt
│           ├── 📜 MODEL_RESULTS_SMR_NATIONAL_REGION.txt
│           ├── 📜 MODEL_RESULTS_SMR_TEMPORAL.txt
│           └── 📜 MODEL_RESULTS_VARIANCE_PROPORTION.txt
│
├── 📁 tests/                               # Test files
├── 📄 .gitignore                           # Git ignore rules
└── 📄 README.md                            # Project overview



## Data sources

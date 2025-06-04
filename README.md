# Spatio-temporal trends and socio-environmental determinants of suicides in England from 2002 - 2022: an ecological population-based study

## Overview

### Project description

### Running the code

In general, all code-related files are designed to be as automated as possible. However, a few manual steps are required to get everything working after cloning the repository.

---

#### 1. Installing Required Packages

Each `.R` file includes a section titled `## 0.1. packages`. This section will attempt to load or install the necessary R packages.

**Note:** One package—`R-INLA`—is *not on CRAN* and requires a custom installation.

Use the following code to install `R-INLA`:

```r
if (!require('INLA')) {
  install.packages(
    'INLA',
    repos = c(getOption('repos'), INLA = 'https://inla.r-inla-download.org/R/testing'),
    dependencies = TRUE
  )
}


### Data sources





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

## `📁 code/` 

This folder contains all the code used needed to simulate/clean the data, run the model, and then generate all the results. In general, the folders and files within the folders should be run using 

## `📁 results/ `

## Data sources

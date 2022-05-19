# DRHHCM
Code and data for:

Dodd PJ, Mafirakureva N, Seddon JA, McQuaid CF. The global impact of household contact management for children on multidrug-resistant and rifampicin-resistant tuberculosis cases, deaths, and health-system costs in 2019: a modelling study. The Lancet Global Health. 2022 May 18
https://doi.org/10.1016/S2214-109X(22)00113-9

# Notes on analysis

The analysis takes the form of a probabilistic sensitivity analysis (PSA) and is split into a number of analyses which are designed to be run in a particular order, usually indicated by the two digits at the start of their filename.

## Structure

The directory structure is

```
.
├── data
├── indata
├── output
└── R
    └── utils
```

NB the folder data/ is excluded from the repo - it must be created by hand in the right relative location for scripts to write large temporary data to. All necessary input data to run the analysis is in indata/.


## Dependencies

This analysis was run using R version 4.1.0 (2021-05-18).

The following R packages must be available:

- utility: here, glue, stringr
- data manipulation: data.table, dplyr,
- plotting: ggplot2, ggthemes, scales, ggrepel, ggpubr
- calculations: discly, HEdtree (via devtools::install_github('petedodd/packagename'))
- spatial strucutre (optional, since output also included): tmap, rworldmap, spdep
- additional for cost analysis (optional, since output also included): tidyverse, viridis, readxl, httr


The estimates of household size by index TB age/sex is based on previously published work:

- https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(18)30401-7/fulltext
- https://github.com/petedodd/PINT


## Memory requirements

With a full-size PSA, a reasonable amount of RAM is needed (it was run on a machine with 36Gb; it probably needs at least 10Gb to not cause trouble).

Commenting out the rep limitation in file 04 around line 14 (ie using 100 replicates) should allow use on a machine with (eg) 8Gb RAM.


## Description of analysis scripts


### 00a.FQRinRR.R ###

This file generates the estimates of FQR resistance in RR. It is optional to run this analysis as the output is included in indata/. Within this analysis, there are two parts: part 1 generates the nearest neighbour structure and is also optional (output in indata/); part 2 uses the neighbour structure to generate the FQR estimates. 

### 00b.unit.costs.R ###

This file generates the country-level unit cost estimates. It calls utils/TB_care_costs.R to download some data from the web if not already present, and do some initial processing. Further collated input from indata/other_public_costs.csv is used. The outputs are saved in outdata/country_unit_costs.R. It is optional to run this script as the output data is included in the repo.


### 01.HHcontactNumbers.R ###

This file uses notification data and previous estimates of household contacts to construct numbers at risk as the starting point for the PSA object.

### 02.RRnumCDR.R ###

This file calculatates the RR case detection ratios as RRest.csv for later use, but is otherwise self-contained.

### 03.DResistance.R ###

This file builds the base PSA object for onward analyses.


### 04.calculations.R ###

This file performs the calculations. It contains functions defining the interventions, and calculating the costs from unit costs. After running the two scripts below in utils/, the outcome data for all scenarios are computed and saved. 

It sources:

utils/
- maketree.R (builds decision tree)
- ModelFuns.R (functions for constructing necessary variables)

#### maketree.R ####

This file constructs the decision tree and makes the functions to compute outcomes.

#### ModelFuns.R ####

This contains most of the non-structural modelling logic. It loads the parameter distributions (with test output if uncommented), specifies the dependencies of transition probabilities on attributes, and includes other functions to generate a PSA initial dataset.

### 05.outputs.R ###

This file computes tabular and graphical summary output from the PSA data.

It sources utils/makeLYs.R which will be a bit slow first time around. This script computes mean discounted life-years for each country to associate with deaths.

These LY computations uses the R package https://github.com/petedodd/discly whose underlying data is described in the paper and repo:

- http://dx.doi.org/10.1016/S1473-3099(20)30919-1
- https://github.com/petedodd/post


### sensitivity analysis ###

The sensitivity analysis to apply additional restriction factors for pulmonary TB are run by changing the input indata/pulmonary.sensitivity.analysis.txt to an integer >0 and then running scripts 01 through to 05. The R/utils/uploadresults.R (authors only) will load the tables to a separate part of the document.

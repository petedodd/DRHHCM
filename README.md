# DRHHCM
tbc

# Notes on analysis


## Dependencies

This analysis was run using R version 4.1.0 (2021-05-18).

The following R packages must be available:

- utility: here, glue
- data manipulation: data.table, dplyr,
- plotting: ggplot2, ggthemes, scales, ggrepel, ggpubr
- calculations: discly, HEdtree (via devtools::install_github('petedodd/packagename'))

The estimates of household size by index TB age/sex is based on previously published work:

- https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(18)30401-7/fulltext
- https://github.com/petedodd/PINT


## Memory requirements

With a full-size PSA, a reasonable amount of RAM is needed (it was run on a machine with 36Gb).
Commenting out the rep limitation in file 04 (ie using 100 replicates) should allow use on a machine with 8Gb RAM.

## Structure

Directory structure:

```
.
├── data
├── indata
├── output
└── R
    └── utils
```

NB data is excluded from the repo - it must be created for scripts to write large temporary data to.

## TODO other changes
- need to include the resampling for FQR 
(/home/pjd/Dropbox/Holocron/tmp/hhmdrstaging/dr)
- need to include the costs


### 01.HHcontactNumbers.R ###

TODO
- pulmo restrict?
- update CDR? check handles 0 for ab


### 02.RRnumCDR.R ###

This file calculatates the RR case detection ratios as RRest.csv for later use, but is otherwise self-contained.

### 03.DResistance.R ###

This file builds the base PSA object for onward analyses.


### 04.calculations.R ###

This file performs the calculations. 

TODO
- remove 100 rep limitation (late adjustment, not on laptop)
- add fracSymptomatic & fracAE to parms
- various checks and comments to remove

It loads:

utils/
- maketree.R (builds decision tree)
- ModelFuns.R (functions for constructing necessary variables)


#### ModelFuns.R ####

TODO
- various

### 05.outputs.R ###

Calls:

utils/
- makeLYs.R

which will be a bit slow first time around

TODO
- life-years bug
- see the few flagged checks in script


## Other processing TODOs ##

- pretty googlesheets drawing on the sheets where data is written, for paper inclusion

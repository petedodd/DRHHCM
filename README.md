# DRHHCM
tbc

# Notes on analysis


## Dependencies

TODO

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
- need to include the costs
- probably need to remove the RRcdr file (after changing)?
- add file to write results to googledocs


### 01.HHcontactNumbers.R ###

TODO
- remove some of the commented sections (eg LEA, WBIL)
- pulmo restrict?
- update CDR?
- HIV proportion from est??
- check HIV line 303/366?
- remove HHV stuff at end?

### 02.RRnumCDR.R ###

TODO
- this needs swapping out for the new data

### 03.DResistance.R ###

This file builds the base PSA object for onward analyses.

TODO
- update and rename parameter file

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

TODO
- harmonize new and new2 parms

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

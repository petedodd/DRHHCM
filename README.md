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
(/home/pjd/Dropbox/Holocron/tmp/hhmdrstaging/dr)
- need to include the costs


### 01.HHcontactNumbers.R ###

TODO
- remove some of the commented sections (eg LEA, WBIL)
- pulmo restrict?
- update CDR?
- HIV proportion from est??
- check HIV line 303/366?
- remove HHV stuff at end?

### 02.RRnumCDR.R ###

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

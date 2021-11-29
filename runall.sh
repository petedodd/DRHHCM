#!/bin/bash
# Are you feeling lucky?
# If so, you might be able to run the analaysis in one go by calling this bash script.
# It's probably a better idea to run each script separately by hand in the first instance to check you have adequate hardware, dependencies satisfied, and flags set-up correctly.

# first is commented as optional (output included)
# there are a couple of flags you may wish to change
# R --vanilla --slave < R/00.FQRinRR.R

# the rest of the calculations
R --vanilla --slave < R/01.HHcontactNumbers.R
R --vanilla --slave < R/02.RRnumCDR.R
R --vanilla --slave < R/03.DResistance.R
R --vanilla --slave < R/04.calculations.R
R --vanilla --slave < R/05.outputs.R


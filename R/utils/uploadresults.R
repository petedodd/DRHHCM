## =======================================
## LOADING TO GOOGLE SHEETS (authors only)
## =======================================
rm(list=ls())

library(here)
library(data.table)
library(glue)
library(googlesheets4)


## are we doing a sensitivity analysis including pulmonary fraction
(pulmsa <- (scan(here('indata/pulmonary.sensitivity.analysis.txt'))>1))

## setup - only accessible to those with access to this sheet
yourl <- "https://docs.google.com/spreadsheets/d/1leFkszUMRhR7i2iLgEWbnO1bbHlCq1mE8ikYYMaVG24/edit#gid=0"
shid <- as.character(as_sheets_id(yourl))

## utility function
upload.to.sheets <- function(filename,sheetid,
                             sap #pulmonary sa?
                             ){
  fn <- glue(here('output/{filename}'))
  tmp <- fread(file=fn)
  sht <- gsub("\\.csv","",filename)
  if(sap) sht <- paste0(sht,'_SAP')
  write_sheet(tmp,sheetid,sheet=sht)
}

## read & upload relevant data
upload.to.sheets("table_resources.csv",shid,pulmsa)
upload.to.sheets("table_outcomes.csv",shid,pulmsa)
upload.to.sheets("table_HE.csv",shid,pulmsa)

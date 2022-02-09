## =======================================
## LOADING TO GOOGLE SHEETS (authors only)
## =======================================
rm(list=ls())

library(here)
library(data.table)
library(glue)
library(googlesheets4)


## are we doing a sensitivity analysis including pulmonary fraction
(pulmsa <- (scan(here('indata/pulmonary.sensitivity.analysis.txt'))>0))

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


## --- gather & write discount rate SAs
L <- list()
for(vr in c("","1","5")){
  tmp <- fread(glue(here('output/table_HE{vr}.csv')))
  L[[vr]] <- tmp[nrow(tmp)]
}
dsctsa <- rbindlist(L)
dsctsa[,c('variable','none'):=NULL]
dsctsa[,`discount rate`:=c('3%','1%','5%')]
setcolorder(dsctsa,c('discount rate',names(dsctsa)[1:(ncol(dsctsa)-1)]))

dscr <- melt(dsctsa,id='discount rate')
dscr[,c('target group','TPT regimen'):=tstrsplit(variable,"_")]
dscr <- dscr[,.(`target group`,`TPT regimen`,`discount rate`,ICER=value)]


write_sheet( dsctsa,shid,sheet="dsctSA")
write_sheet(dscr,shid,sheet="dsctSA2")

## quick check of ratios
dscr[,ref:=value[1],by=.(`target group`,`TPT regimen`)]
dscr[,ratio:=as.numeric(value)/as.numeric(ref)]
dscr[,mean(ratio),by=`discount rate`] #0.57 & 1.52


## adding in extras or make base PSA
rm(list=ls())

library(here)
library(data.table)
library(HEdtree)

PZ <- read.csv(here('indata/PTBHHCTnew.csv')) #TODO check most recent
PZ <- parse.parmtable(PZ)               #for concordance

load(here('indata/FRF.Rdata')) #TODO need to include script & move to data/

## data on paed RR CDRs TODO update
RRest <- fread(here('data/RRest.csv'))
RRest[,c('rrcdr.a','rrcdr.b'):=.(cdr014*cdr014ab,(1-cdr014)*cdr014ab)]
RRest[,c('cdr014','cdr014ab'):=NULL]
RRest[,c('rrcdr04.a','rrcdr04.b'):=.(cdr04*cdr04ab,(1-cdr04)*cdr04ab)]
RRest[,c('cdr04','cdr04ab'):=NULL]
RRest[,c('rrcdr514.a','rrcdr514.b'):=.(cdr514*cdr514ab,(1-cdr514)*cdr514ab)]
RRest[,c('cdr514','cdr514ab'):=NULL]


## use regional averages where missing
K <- fread(here('indata/TB_notifications_2020-10-15.csv'))
K <- unique(K[,.(iso3,g_whoregion)])
RRest <- merge(RRest,K)
RRestav <- RRest[rrcdr.a+rrcdr.b>0,
                 .(rrcdr.av=mean(rrcdr.a),rrcdr.bv=mean(rrcdr.b),
                   rrcdr04.av=mean(rrcdr04.a),rrcdr04.bv=mean(rrcdr04.b),
                   rrcdr514.av=mean(rrcdr514.a),rrcdr514.bv=mean(rrcdr514.b)),
                 by=g_whoregion]
RRest <- merge(RRest,RRestav,by='g_whoregion',all.x=TRUE)
RRest[rrcdr.a+rrcdr.b==0,c('rrcdr.a','rrcdr.b'):=.(rrcdr.av,rrcdr.bv)]
RRest[rrcdr04.a+rrcdr04.b==0,c('rrcdr04.a','rrcdr04.b'):=.(rrcdr04.av,rrcdr04.bv)]
RRest[rrcdr514.a+rrcdr514.b==0,c('rrcdr514.a','rrcdr514.b'):=.(rrcdr514.av,rrcdr514.bv)]
RRest[,c('g_whoregion','rrcdr.av','rrcdr.bv'):=NULL]


## load up contact data
nrep <- 1e3                             #number of reps for PSA
## ======= children 0-4
load(here('data/DLC.Rdata'))                  #parent data for children 0-4
PSA <- DLC[rep(1:nrow(DLC),nrep),]      #build PSA data frame
PSA[,repn:=rep(1:nrep,each=nrow(DLC))]
## ======= children 5-14
load(here('data/DLO.Rdata'))                  #parent data for children 5-14
PSO <- DLO[rep(1:nrow(DLO),nrep),]      #build PSA data frame
PSO[,repn:=rep(1:nrep,each=nrow(DLO))]

## variables needed for PSA
PSA[,az:=rep(1,nrow(PSA))]               #dummy ages for functions defined like that
PSA[,cm:=cdr04]
PSA[,cab:=cdr04ab]
PSA[,acat:="[0,5)"]                     #age group
PSO[,az:=rep(10,nrow(PSO))]                #dummy ages for functions defined like that
PSO[,cm:=cdr514]
PSO[,cab:=cdr514ab]
PSO[,acat:="[5,15)"]                     #age group


## join these after harmonizing names
del <- grep('l$',names(PSA),value=TRUE)
PSA[,c(del):=NULL]
del <- grep('l$',names(PSO),value=TRUE)
PSO[,c(del):=NULL]
(nmz <- grep('hhc',names(PSA),value=TRUE))
who <- which(names(PSA)%in%nmz)
nmz <- gsub('u5','',nmz)
names(PSA)[who] <- nmz
(nmz <- grep('hhc',names(PSO),value=TRUE))
who <- which(names(PSO)%in%nmz)
nmz <- gsub('o5','',nmz)
names(PSO)[who] <- nmz
## join
PSA <- rbind(PSA,PSO,fill=TRUE)
rm(PSO)

## NOTE have CDR stuff in twice
PSA[,c('cdr.a','cdr.b'):=.(cm*cab,(1-cm)*cab)]
PSA[,c('cdr04','cdr04ab','cdr514','cdr514ab'):=NULL]
PSA[,c('cm','cab'):=NULL]

## jj
## merge in secondline data NOTE would need altering if nrep!=1e3
PSA <- merge(PSA,FRF[,.(iso3,repn=id,FQR=fqr)],by=c('iso3','repn')) #

## merge in rrcdr
PSA <- merge(PSA,RRest[,.(iso3,rrmdr_15plus_tx,
                          rrcdr04.a,  rrcdr04.b,
                          rrcdr514.a,rrcdr514.b)],
             by='iso3',all.x=TRUE)

names(PSA)
## unify the rr cdr parameters
PSA[acat=='[0,5)',c('rrcdr.a','rrcdr.b'):=.(rrcdr04.a,rrcdr04.b)]
PSA[acat=='[5,15)',c('rrcdr.a','rrcdr.b'):=.(rrcdr514.a,rrcdr514.b)]
PSA[,c('rrcdr04.a','rrcdr04.b','rrcdr514.a','rrcdr514.b'):=NULL]
PSA[cdr.a+cdr.b==0,c('cdr.a','cdr.b'):=.(1e-3,1e-3)] #safety

## checks
PSA[,summary(hhc)]
PSA[repn==1,sum(rrmdr_15plus_tx)/2e5]   #170K

## concordance (assuming all RR index cases):
## NOTE read in PZ for this parameter
## NOTE at level of country x replicate
concord <- as.data.table(expand.grid(iso3=PSA[,unique(iso3)],
                                     repn=1:PSA[,max(repn)]))
concord[,concord:=PZ$concord$r(nrow(concord))]
PSA <- merge(PSA,concord,by=c('iso3','repn'))

## checks
PSA[iso3=='USA']
PSA[iso3=='IND']
PSA[,summary(hhc)]
PSA[repn==1,sum(rrmdr_15plus_tx)/6e5]   #170K

save(PSA,file=here('data/PSA.Rdata'))

load(file=here('data/PSA.Rdata'))

## === HIV splits
H <- fread(here('indata/TB_burden_countries_2020-10-15.csv')) #for HIV in TB
mxyr <- max(H$year)
H <- H[year==mxyr] #most recent
H <- H[,.(iso3,e_tbhiv_prct)]
D <- fread(here('indata/TB_notifications_2020-10-15.csv')) #for ART coverage
D <- D[year==mxyr]
D <- D[,.(iso3,artprop=newrel_art/newrel_hivpos)]
H <- merge(H[,.(iso3,hivprop=e_tbhiv_prct/1e2)],D,by='iso3')
## safety
H[!is.finite(hivprop),artprop:=0]
H[!is.finite(artprop),artprop:=0]
H[!is.finite(hivprop),hivprop:=0]
PSA <- merge(PSA,H,by='iso3')

## scale down using HIV HH concordance for children
PSA[acat=="[0,5)", hivprop:=hivprop*PZ$HHhivprev04$r(nrow(PSA)/2)] #child infection risk
PSA[acat=="[5,15)", hivprop:=hivprop*PZ$HHhivprev514$r(nrow(PSA)/2)]#child infection risk

PSA


save(PSA,file=here('data/PSA.Rdata'))

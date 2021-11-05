##
library(HEdtree)
library(here)

## read in parameters
## anything with 'assum*' in SOURCE column needs work
## also: need to revisit age dependence, potentially HIV
## P <- parse.parmtable(read.csv(here('indata/PTBHHCTnew.csv')))
## str(P)
## NOTE parameters now read in in ModelFuns.R



## extras
## missed, missedi - related to LTFU, CDR resp
## PTcov  - defines intervention

## also need coprev, CFRnotx, progn: take these for now from old functions, using same parms
source(here('R/utils/ModelFuns.R'))
## coprev(age), CFRtxN(age),
## ltbi.prev(age,coprev)
## progprob(a,bcgcov,lat)


## NOTE
## single concordance TODO
## only RR index cases
## (BDQ, DLD), FQ - shared efficacy for susceptible
## Marks review of different regimens as central estimate
## range of efficacies 0.1, 0.5 cost vs efficacy
## no effect for resistance
## 3 target options: 0) a) u5+HIV, b) u5+HIV & LTBI+, c) u15
## 3 x 3 interventions vs nothing
## no dependence on index case DST
## visual around FQR resistance vs y axis efficacy (eg NNT for FQ regimen)
## splitting out RR vs RS ATT courses

## add variables
addVariables <- function(D){
    ## CDR stuff
    D[,CDR:=rbeta(nrow(D),cdr.a,cdr.b)]  #coprev CDR
    D[,CDRi:=rbeta(nrow(D),cdr.a,cdr.b)] #incidence CDR
    ## see Appendix: scaled up for incidence
    D[,CDRi:=pmin((1+runif(nrow(D)))*CDRi,1)] #scaled mean
    D[,rrCDR:=rbeta(nrow(D),rrcdr.a,rrcdr.b)]
    D[,rrCDRi:=rbeta(nrow(D),rrcdr.a,rrcdr.b)] #
    D[rrCDR>CDR,rrCDR:=CDR]; D[rrCDRi>CDRi,rrCDRi:=CDRi];                      #safety
    ## prob. RS ATT if ATT, coprev/omc
    ## (1-p)CDR = RRCDR; p=1-RRCDR/CDR
    D[DST=='RS',pRSATT:=1.0]; D[DST=='RS',pRSATTi:=1.0];          # no FP
    D[DST!='RS',pRSATT:=1-rrCDR/CDR]; D[DST!='RS',pRSATTi:=1-rrCDRi/CDRi];
    print('CDRs added!')
    ## CFR on RS-ATT
    D[,CFRnotx:=CFRtxN(age,hiv,art)]                #CFR not on ATT NOTE HIV
    ## TODO make CFRnotx
    ## D[DST=='RS',CFRstx:=PZ$CFRstx.RS$r(sum(DST=='RS'))] #CFR as before,
    D[DST=='RS',CFRstx:=CFRtxY(age,hiv,art)]
    D[DST!='RS',CFRstx:=CFRnotx] #assume like no tx
    ## CFR on RR-ATT
    D[DST=='RS',CFRrtx:=PZ$CFRrtx.RS$r(sum(DST=='RS'))] #TODO improve nm
    D[DST!='RS',CFRrtx:=PZ$CFRrtx.RR$r(sum(DST!='RS'))] #Harausz
    ## TODO HIV effect?
    print('CFRs added!')
    ## RR of incident TB under PT
    D[,RR0:=IPTrr(sum(nrow(D)))]        #base efficacy of IPT TODO check back HIV dependence
    D[,RR1:=IPTrr(sum(nrow(D)),tst="+ve")]   #base efficacy of PT among TST+ve
    D[,RR:=RR0]                         #default
    ## D[DST=='RS',RR:=IPTrr(sum(DST=='RS'))]
    ## D[DST=='FS',RR:=1.0]
    ## D[DST=='FR',RR:=1.0] #TODO different parm
    ## other variables from prevous work TODO check
    print('PT variables added!')
    D[,coprev:=coprev(age)]                #coprevalent TB
    D[,ltbi.prev:=ltbi.prev(age,coprev)]   #LTBI prevalence
    ## HIV
    print('Prevalences added!')
    D[,pprogn:=progprob(age,hiv,art)] #prgn in LTBI+
    D[,progn:=ltbi.prev * pprogn]           #TB incidence, total
    ##  TODO not needed here?
    D[,progn.LP.PTn:=pprogn*1] #TB incidence in LTBI +ve PT-ve
    D[,progn.LN.PTn:=pprogn*0]     #TB incidence in LTBI -ve PT-ve
    print('Progression added!')
    ## PT coverage
    D[,PTcov:=0]
}


## function to enlarge template PSA to include the DR types using concordance
splitbyDRtypes <- function(D){
    ## split out rows by DST
    value0 <- D$value             #baseline value
    D <- D[rep(1:nrow(D),each=3)]     #triplicate
    D[,DST:=rep(c('RS','FS','FR'),nrow(D)/3)]
    ## hhc = (notes fraction: age, sex) x (HHC | age,sex)
    ## rr-hhc = (RR index cases) x hhc
    D[,value:=rrmdr_15plus_tx * hhc]   #TODO check + uncertainty
    ## D[,c('hhc','hhc.sd'):=NULL]        #drop these now
    ## prob 'concordance' match source; rest RS
    D[DST=='RS',value:=value0 * (1-concord) ]
    D[DST=='FS',value:=value0 * concord *  (1-FQR)]
    D[DST=='FR',value:=value0 * concord * FQR ]
    return(D)
}

## function to enlarge the PSA template to additionally (and independently) include HIV/ART status
splitbyHIV <- function(D){
    ## split out rows by DST
    D <- D[rep(1:nrow(D),each=3)]     #triplicate
    D[,hiv:=rep(c(0,1,1),nrow(D)/3)]
    D[,art:=rep(c(0,0,1),nrow(D)/3)]
    ## prob using props
    valuehn <- D[hiv==0 & art==0,value]
    valuehp <- D[hiv==1 & art==0,value]
    valueha <- D[hiv==1 & art==1,value] #cautiously align baseline values
    D[hiv==0,value:=valuehn * (1-hivprop) ]
    D[hiv==1 & art==0,value:=valuehp * hivprop *  (1-artprop)]
    D[hiv==1 & art==1,value:=valueha * hivprop * artprop ]
    return(D)
}


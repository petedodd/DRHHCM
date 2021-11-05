## This file reads in the parameters for the model, constructs all the
## necessary distributions and defines the functions that are needed in the

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

## read in data & build distributions (second version with automatic test output)
PD <- read.csv(here('indata/PTBHHCTnew2.csv'))
PZ <- parse.parmtable(data = PD) #no test
## PZ <- parse.parmtable(data = PD, #test!
##                       outfile=here('test/zzout.csv'),
##                       testdir = here('test'))
names(PZ)                               #check what we've got


## =========== function definitions ===============
oddit <- function(x) x/(1-x)
ioddit <- function(x) x/(1+x)
logit <- function(x) log(oddit(x))
ilogit <- function(x) ioddit(exp(x))

## ## what function are needed?
## clx <- showParmz(kexp)$calcs
## clx                                     #avoid the 1- or the 1
## clx[!grepl('1',clx)]

## testing

## == co-prevalence (empirical)
coprev <- function(a,hinco=FALSE){
  if(length(a)>1 & length(hinco)==1) hinco <- rep(hinco,length(a))
  tmp <- PZ$coprevDRkids$r(length(a))/1e2 #NOTE swtiched to Shah
  ## tmp <- PZ$coprev04$r(length(a))
  ## tmp[hinco] <- PZ$coprev04hi$r(sum(hinco))
  ## tmp[a>=5] <- PZ$coprev514$r(sum(a>=5))
  ## tmp[a>=5 & hinco] <- PZ$coprev514hi$r(sum(a>=5 & hinco))
  tmp
}
## coprev(1:10,c(rep(FALSE,5),rep(TRUE,5)))

## ## == case detection
## CDR <- function(mn,ab){
##   mn <- mn*(1 + runif(length(mn))) #CDR adjustment 2
##   mn <- pmin(mn,1)
##   a <- mn*ab
##   b <- (1-mn)*ab
##   rbeta(n=length(mn),shape1 = a,shape2 = b)
##   ## 0.4
## }


## == CFR on tx
CFRtxY <- function(a,hiv=0,art=0){#NB optimized for clarity not speed
  if(length(a)>1 & length(hiv)==1) hiv <- rep(hiv,length(a))
  if(length(a)>1 & length(art)==1) art <- rep(art,length(a))
  tmp <- PZ$ontxY$r(length(a))
  tmp[a>=5] <- PZ$ontxO$r(sum(a>=5))  #NB this could be achieved in  the tree model
  ## hivartOR
  Z <- PZ$hivartOR$r(length(a))
  hor <- rep(1,length(a))
  tmp <- logit(tmp)                     #transformt
  tmp[hiv>0] <- tmp[hiv>0]+Z[hiv>0,1]
  tmp[art>0] <- tmp[art>0]+Z[art>0,2]
  tmp <- ilogit(tmp)                    #inverse transform
  tmp
}
CFRtxY(1:10)                            #test
summary(CFRtxY(1:1e3))
summary(CFRtxY(1:1e3,hiv=1))
summary(CFRtxY(1:1e3,hiv=1,art=1))


## == CFR off tx
CFRtxN <- function(a,hiv=0,art=0){
  if(length(a)>1 & length(hiv)==1) hiv <- rep(hiv,length(a))
  if(length(a)>1 & length(art)==1) art <- rep(art,length(a))
  tmp <- PZ$notxY$r(length(a))          #default a<5 and hiv=art=0
  tmp[a<5 & hiv>0 & art==0] <- PZ$notxHY$r(sum(a<5 & hiv>0 & art==0)) #u5,HIV+,ART-
  tmp[a<5 & hiv>0 & art>0] <- PZ$notxHAY$r(sum(a<5 & hiv>0 & art>0)) #u5,HIV+,ART+
  tmp[a>=5] <- PZ$notxO$r(sum(a>=5))    #o5, HIV-ve
  tmp[a>=5 & hiv>0 & art==0] <- PZ$notxHO$r(sum(a>=5 & hiv>0 & art==0)) #o5,HIV+,ART-
  tmp[a>=5 & hiv>0 & art>0] <- PZ$notxHAO$r(sum(a>=5 & hiv>0 & art>0)) #o5,HIV+,ART+
  tmp
}
CFRtxN(1:10)                            #test
summary(CFRtxN(1:1e3))
summary(CFRtxN(1:1e3,hiv=1))
summary(CFRtxN(1:1e3,hiv=1,art=1))

## == LTBI infection probability
#NB this is LTBI given not active: it is taken to be max(0,LTBI-coprev)
ltbi.prev <- function(a,coprev,hinco=FALSE){
  if(length(a)>1 & length(hinco)==1) hinco <- rep(hinco,length(a))
  tmp <- PZ$LTBI04$r(length(a))
  tmp[hinco] <- PZ$LTBI04hi$r(sum(hinco))
  tmp[a>=5] <- PZ$LTBI514$r(sum(a>=5))
  tmp[a>=5 & hinco] <- PZ$LTBI514hi$r(sum(a>=5 & hinco))
  tmp
  ## pmax(0,tmp - coprev) # already taken into account with decision tree
}
ltbi.prev(1:10,0.1,hinco=TRUE)

## progression probability
progprob <- function(a,hiv=0,art=0){
  if(length(a)>1 & length(hiv)==1) hiv <- rep(hiv,length(a))
  if(length(a)>1 & length(art)==1) art <- rep(art,length(a))
  ans <- PZ$prog04$r(length(a))
  ans[a>=5] <- PZ$prog514$r(sum(a>=5))
  if(any(hiv>0)){ #treat as IRR for escape
    hr <- PZ$hivpi$r(sum(hiv>0))
    ans[hiv>0] <- 1-(1-ans[hiv>0])^hr
  }
  if(any(art>0)){ #treat as IRR for escape
    hr <- PZ$artp$r(sum(art>0))
    ans[art>0] <- 1-(1-ans[art>0])^hr
  }
  ans
}
## progprob(c(rep(3,5),rep(10,5)))

## ## === disaggregating by TST status - used for TST-driven interventions
## RRtst <- function(a){
##   PZ$RRtst10$r(length(a))
## }
## ## RRtst(rep(1,10))

## === IPT efficacy
IPTrr <- function(a,hiv=0,
                  tst='none'            #a flag for: given to TST+ or not
                  ){
  if(length(a)>1 & length(hiv)==1) hiv <- rep(hiv,length(a))
  if(tst=='none'){
    ans <- PZ$iptRR$r(length(a))
    ans[hiv>0] <- PZ$iptRRhivpos$r(sum(hiv>0))  #HIV+
  } else {
    ans <- PZ$iptRRtstpos$r(length(a))  #NOTE only applied to HIV-ves
  }
  ans
}
IPTrr(1:10)
summary(IPTrr(runif(1e3),hiv=0)) #0.66
summary(IPTrr(runif(1e3),hiv=1))
summary(IPTrr(runif(1e3),tst='yes')) #0.37

## IPTrrLP <- function(a){
##   PZ$iptRRtstpos$r(length(a))
## }
## summary(IPTrrLP(runif(1e3)))
## make cost data
MakeCostData <- function(csts,          #base data table of cost data
                         nrep
                         ){
  if(nrow(csts[cost.sd>0 & cost.m==0])>0) warning(paste0('Some cost input variables have zero mean & SD>0. These will be treated as fixed variables:\n',paste0(csts[cost.sd>0 & cost.m==0,cost],collapse='\n')))
  csts[cost.m>0,gmsc:=cost.sd^2/cost.m]
  csts[!is.na(gmsc) & gmsc > 0, gmk:=cost.m/gmsc]
  NR <- nrow(csts)
  csts <- csts[rep(1:NR,nrep)]
  csts[,repn:=rep(1:nrep,each=NR)]
  csts[,rnd:=!is.na(gmsc) & !is.na(gmk) & gmk>0 & gmsc > 0]
  csts[rnd==TRUE,value:=rgamma(sum(rnd),shape=gmk,scale = gmsc)] #random sample from gamma distribution
  csts[rnd!=TRUE,value:=cost.m]                                  #fixed values
  csts[,c('gmk','gmsc','cost.m','cost.sd','rnd'):=NULL]
  csts
}


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


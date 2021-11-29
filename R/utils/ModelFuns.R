## This file reads in the parameters for the model, constructs all the
## necessary distributions and defines the functions that are needed in the
library(HEdtree)
library(here)

## PTcov  - defines intervention

## read in data & build distributions (second version with automatic test output)
PD <- read.csv(here('indata/DRHHCMparms.csv'))
PZ <- parse.parmtable(data = PD) #build parameter object no test
## ## uncomment below to generate test plots etc:
## if(!file.exists(here('output/test'))) dir.create(here('output/test'))
## PZ <- parse.parmtable(data = PD, #test!
##                       outfile=here('output/test/zzout.csv'),
##                       testdir = here('output/test'))
## ## montage *.png -mode concatenate -tile 5x6 zout.jpg
names(PZ)                               #check what we've got


## =========== function definitions ===============
oddit <- function(x) x/(1-x)
ioddit <- function(x) x/(1+x)
logit <- function(x) log(oddit(x))
ilogit <- function(x) ioddit(exp(x))


## == co-prevalence (empirical)
coprev <- function(a,hinco=FALSE){
  if(length(a)>1 & length(hinco)==1) hinco <- rep(hinco,length(a))
  tmp <- PZ$coprevDRkids$r(length(a))/1e2 #NOTE swtiched to Shah
  tmp
}
## coprev(1:10,c(rep(FALSE,5),rep(TRUE,5)))


## == CFR on tx
CFRtxY <- function(a,hiv=0,art=0){#NB optimized for clarity not speed
  if(length(a)>1 & length(hiv)==1) hiv <- rep(hiv,length(a))
  if(length(a)>1 & length(art)==1) art <- rep(art,length(a))
  tmp <- PZ$ontxY$r(length(a))
  tmp[a>=5] <- PZ$ontxO$r(sum(a>=5))  #NB this could be achieved in  the tree model
  ## hivartOR
  Z <- PZ$hivartOR$r(length(a))
  hor <- rep(1,length(a))
  tmp <- logit(tmp)                     #transform
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

## add variables
addVariables <- function(D){
  ## CDR stuff
  D[,CDR:=rbeta(nrow(D),cdr.a,cdr.b)]  #coprev CDR
  D[,CDRi:=rbeta(nrow(D),cdr.a,cdr.b)] #incidence CDR
  ## see Appendix: scaled up for incidence
  D[,CDR:=pmin((1+runif(nrow(D)))*CDR,1)] #scaled mean
  D[,CDRi:=pmin((1+runif(nrow(D)))*CDRi,1)] #scaled mean
  D[,rrCDR:=rbeta(nrow(D),rrcdr.a,rrcdr.b)]
  D[,rrCDRi:=rbeta(nrow(D),rrcdr.a,rrcdr.b)] #
  D[rrCDR>CDR,rrCDR:=CDR]; D[rrCDRi>CDRi,rrCDRi:=CDRi]; #safety
  ## prob. RS ATT if ATT, coprev/omc
  ## (1-p)CDR = RRCDR; p=1-RRCDR/CDR
  D[DST=='RS',pRSATT:=1.0]; D[DST=='RS',pRSATTi:=1.0];          # no FP
  D[DST!='RS',pRSATT:=1-rrCDR/CDR]; D[DST!='RS',pRSATTi:=1-rrCDRi/CDRi];
  print('CDRs added!')
  ## CFR on RS-ATT
  D[,CFRnotx:=CFRtxN(age,hiv,art)]   #CFR not on ATT NOTE HIV
  D[DST=='RS',CFRstx:=CFRtxY(age,hiv,art)]
  D[DST!='RS',CFRstx:=CFRnotx] #assume like no tx
  ## CFR on RR-ATT
  D[DST=='RS',CFRrtx:=CFRstx] #same as RS if actually RS
  D[DST!='RS',CFRrtx:=PZ$CFRrtx.RR$r(sum(DST!='RS'))] #Harausz
  print('CFRs added!')
  ## RR of incident TB under PT
  D[,RR0:=IPTrr(sum(nrow(D)),hiv)]  #base efficacy of IPT
  D[,RR1:=IPTrr(sum(nrow(D)),tst="+ve")]   #base efficacy of PT: TST+ve
  D[,RR:=RR0]                         #default
  print('PT variables added!')
  D[,coprev:=coprev(age)]                #coprevalent TB
  D[,ltbi.prev:=ltbi.prev(age,coprev,hinc)]   #LTBI prevalence
  ## HIV
  print('Prevalences added!')
  D[,pprogn:=progprob(age,hiv,art)] #prgn in LTBI+
  D[,progn:=ltbi.prev * pprogn]           #TB incidence, total
  print('Progression added!')
  ## PT coverage
  D[,PTcov:=0]
  ## for costing: AEs & symptoms
  D[,fracSymptomatic:=PZ$fracSymptomatic$r(nrow(D))]
  D[,fracAE:=PZ$fracAE$r(nrow(D))]
}


## function to enlarge template PSA to include the DR types using concordance
splitbyDRtypes <- function(D){
  ## split out rows by DST
  value0 <- D$value             #baseline value
  D <- D[rep(1:nrow(D),each=3)]     #triplicate
  D[,DST:=rep(c('RS','FS','FR'),nrow(D)/3)]
  ## hhc = (notes fraction: age, sex) x (HHC | age,sex)
  ## rr-hhc = (RR index cases) x hhc
  D[,value:=rrmdr_15plus_tx * hhc]
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


## This file prepares the initial data needed in the modelling analysis
## i.e. in order to build the parent data.table for the PSA
## NB the first statement wipes all data so be careful when applied!
rm(list=ls())
library(here)
library(data.table)
library(ggplot2)

## are we doing a sensitivity analysis including pulmonary fraction
(pulmsa <- (scan(here('indata/pulmonary.sensitivity.analysis.txt'))>0))

## ================ initial data pooling ===========

## --- WHO data from
## http://www.who.int/tb/country/data/download/en/
N <- fread(here('indata/TB_notifications_2020-10-15.csv'))

## below introduces an analysis of the proportions pulmonary
## relevant variables
nmz <- grep("^newrel",names(N),value=TRUE)  #keep right patterns
nmz <- nmz[!grepl("unk",nmz)] #drop unknown
nmz <- nmz[!grepl("fu|mu",nmz)]       #more unknowns dropped
nmz <- nmz[!grepl("hiv|art",nmz)]       #HIV stuff dropped
nmz <- nmz[!grepl('plus|514|04|014|59',nmz)] #drop children & not-child catchall
nmz <- c('iso3','year',nmz)
## NOTE need to aggregate 15-19 and 19-24

## reduce to relevant data
NP <- N[year==2019,..nmz]
NP <- melt(NP,id.vars = c('iso3','year'))
NP[,sex:=ifelse(grepl("f",variable),'F','M')]
NP[,age:=gsub("[a-z]|_","",variable)]
NP[,age:=gsub("(\\d{2})(\\d*)","\\1-\\2",age,perl=TRUE)]
NP[,total.notes:=sum(value,na.rm=TRUE),by=iso3]
origiso <- NP[,unique(iso3)]
NP <- NP[total.notes>200]                     #remove small-TB countries
(dropped <- setdiff(origiso,NP[,unique(iso3)]))

cat(dropped,file=here('output/drop_lonote.txt'))
NP[age=='65-',age:='65+']

## NOTE aggregating over new age categories
NP[age=='15-19',age:='15-24']
NP[age=='20-20',age:='15-24'] 
NP[,value:=sum(value,na.rm=TRUE),by=.(iso3,sex,age)]
NP

## estimations
E <- fread(here('indata/TB_burden_countries_2020-10-15.csv'))
E <- E[year==2019]                      #restrict
## merge
D <- merge(N,E,by=c('country','iso2','iso3','iso_numeric','g_whoregion','year'))

## === load WHO age-specific incidence estimates
A <- fread(here('indata/TB_burden_age_sex_2020-10-15.csv'))
## keep only relevant categories
A <- A[risk_factor=='all']
A <- A[year==2019]
A <- A[sex!='a']
A <- A[age_group %in% c('0-4','0-14','5-14')]
A

rmn <- function(x) round(mean(x,na.rm=TRUE))


## --- tidy notifications by age/sex
D[,totnotes:=c_newinc]
D[is.na(totnotes),totnotes:=0]

## males
D[,n_m_0_4:=newrel_m04]
D[,n_m_5_14:=newrel_m514]
D[,n_m_15_24:=newrel_m1524]
D[,n_m_25_34:=newrel_m2534]
D[,n_m_35_44:=newrel_m3544]
D[,n_m_45_54:=newrel_m4554]
D[,n_m_55_64:=newrel_m5564]
D[,n_m_65_Inf:=newrel_m65]

## females
D[,n_f_0_4:=newrel_f04]
D[,n_f_5_14:=newrel_f514]
D[,n_f_15_24:=newrel_f1524]
D[,n_f_25_34:=newrel_f2534]
D[,n_f_35_44:=newrel_f3544]
D[,n_f_45_54:=newrel_f4554]
D[,n_f_55_64:=newrel_f5564]
D[,n_f_65_Inf:=newrel_f65]


names(D)[(ncol(D)-16+1):ncol(D)]

## total from disagg
rnp <- D[,lapply(.SD,rmn),.SDcols=(ncol(D)-16+1):ncol(D),by=g_whoregion] #regional note pattern
setkey(rnp,g_whoregion)
thna <- rowSums(D[,(ncol(D)-16+1):ncol(D),with=FALSE])            #is there an NA?
tfd <- rowSums(D[,(ncol(D)-16+1):ncol(D),with=FALSE],na.rm=TRUE)  #totes w/o NA
tfd <- D$totnotes - tfd
tfd[tfd<0] <- 0

## - add in the remaining notifications following regional pattern
## build key
rtots <- rowSums(rnp[,2:ncol(rnp),with=FALSE]);names(rtots) <- rnp$g_whoregion
rnpat <- list()
for(reg in rnp$g_whoregion) rnpat[[reg]] <- rnp[reg,2:ncol(rnp),with=FALSE]/rtots[reg]

## loop through
for(j in (ncol(D)-16+1):ncol(D))        #set NAs to 0 here
  set(D,which(is.na(D[[j]])),j,0)

for(i in 1:nrow(D)){                    #allocate excess by regional pattern
  if(tfd[i]>0){
    addon <- round(tfd[i]*rnpat[[D[i,as.character(g_whoregion)]]]) #regional patterned addon
    ## print(addon)
    D[i,((ncol(D)-16+1):ncol(D)):=D[i,(ncol(D)-16+1):ncol(D),with=FALSE]+addon]
  }
}

## -- compute CDR for each age group (all TB)
A
AW <- dcast(A,iso3~sex+age_group,value.var = c('best','lo','hi'))
D <- merge(D,AW,by='iso3',all.x=TRUE,all.y=FALSE)

## --- CDR calculations
## estimates
D[,.(iso3,
     n_m_0_4,n_m_5_14,n_f_0_4,n_f_5_14,
     `best_f_0-14`,`best_f_0-4`,`best_f_5-14`,
     `best_m_0-14`,`best_m_0-4`,`best_m_5-14`,
     `lo_f_0-14`,`lo_f_0-4`,`lo_f_5-14`,
     `lo_m_0-14`,`lo_m_0-4`,`lo_m_5-14`,
     `hi_f_0-14`,`hi_f_0-4`,`hi_f_5-14`,
     `hi_m_0-14`,`hi_m_0-4`,`hi_m_5-14`)]

## 04 CDR
D[,cdr04:=(n_m_0_4+n_f_0_4)/(`best_f_0-4` + `best_m_0-4`)]
D[!is.finite(cdr04),.(iso3,n_m_0_4,n_f_0_4)] #check
D[!is.finite(cdr04),cdr04:=0]
D[cdr04>1,cdr04:=1] #next, var=
D[,V:=(cdr04/3.92)^2 * ((`hi_f_0-4`-`lo_f_0-4`)^2+(`hi_m_0-4`-`lo_m_0-4`)^2)/(`best_f_0-4` + `best_m_0-4`)^2]
D[,cdr04ab:=(cdr04*(1-cdr04))/V-1] #a+b
D[!is.finite(cdr04ab) | cdr04ab<0, cdr04ab:=0] #NB CDR sampling needs to handle 0s


## 514 CDR
D[,cdr514:=(n_m_5_14+n_f_5_14)/(`best_f_5-14` + `best_m_5-14`)]
D[!is.finite(cdr514),.(iso3,n_m_5_14,n_f_5_14)] #check
D[!is.finite(cdr514),cdr514:=0]
D[cdr514>1,cdr514:=1] #next, var=
D[,V:=(cdr514/3.92)^2 * ((`hi_f_5-14`-`lo_f_5-14`)^2+(`hi_m_5-14`-`lo_m_5-14`)^2)/
     (`best_f_5-14` + `best_m_5-14`)^2]
D[,cdr514ab:=(cdr514*(1-cdr514))/V-1] #a+b
D[!is.finite(cdr514ab) | cdr514ab<0, cdr514ab:=0] #NB CDR sampling needs to handle 0s

## drop temp variable
D[,V:=NULL]


## ---  working out the proportion pulmonary
## see PINT repo including graphs 05

## relevant variables
nmz <- grep("new_.{2}_.+",names(N),value=TRUE)  #keep right patterns
nmz <- nmz[!grepl("unk",nmz)] #drop unknown
nmz <- nmz[!grepl("fu|mu",nmz)]       #more unknowns dropped
nmz <- nmz[!grepl('plus|514|04|014',nmz)] #drop children & not-child catchall
nmz <- c('iso3','year',nmz)

## reduce to relevant data
NP <- N[,nmz,with=FALSE]
tmp <- NP[,.(total=rowSums(.SD)),.SDcols=3:ncol(NP),by=.(iso3,year)]
NP <- merge(NP,tmp,by=c('iso3','year'))
NP <- NP[!is.na(total)]
NP <- NP[total>200]                     #remove small-TB countries

## reshape
NP[,total:=NULL]
NP <- melt(NP,id.vars = c('iso3','year'))

## re-parse variable as separate characteristics
NP[,tbtype:=ifelse(grepl("ep",variable),'EP','Pulm')]
NP[,sex:=ifelse(grepl("f",variable),'F','M')]
NP[,age:=gsub("[a-z]|_","",variable)]
NP[,age:=gsub("(\\d{2})(\\d*)","\\1-\\2",age,perl=TRUE)]

## aggregate over years
NPR <- NP[,.(value=sum(value)),by=.(iso3,tbtype,sex,age)]
NPR[,total:=sum(value),by=.(iso3,age,sex)]
NPR[,proportion:=value/total,by=.(iso3,tbtype,sex,age)]
NPR[,bad:=any(!is.finite(proportion)),by=iso3]
NPR <- NPR[bad==FALSE]                  #drop those with things amiss

## regional averages
NPR <- merge(NPR,unique(N[,.(iso3,g_whoregion)]),
             by='iso3',all.x=TRUE,all.y=FALSE)
NPRreg <- NPR[,.(proportion=mean(proportion)),
              by=.(g_whoregion,tbtype,sex,age)]

## use these regional averages to fill out to all countries
NPR2 <- merge(unique(N[,.(iso3,g_whoregion)]),
              NPRreg,by='g_whoregion',
              allow.cartesian = TRUE)#reg averges for all iso3
NPR2 <- NPR2[!(iso3 %in% NPR[,unique(iso3)])]  #restrict to those w/o data
NPR <- rbind(NPR[,.(iso3,sex,age,tbtype,proportion)],
             NPR2[,.(iso3,sex,age,tbtype,proportion)]) #add in w/o data iso3s as means
setkey(NPR,iso3)

save(NPR,file=here('data/NPR.Rdata'))
## ---  end of pulmonary analysis

## reshape
NPRW <- dcast(NPR[tbtype=="Pulm"],
             iso3 ~ age + sex, value.var = 'proportion')

## harmonize names
nmzp <- names(NPRW)[-1]
nmzp <- gsub("-","_",nmzp)
nmzp[grepl("M",nmzp)] <- paste0("pn_m_",nmzp[grepl("M",nmzp)])
nmzp[grepl("F",nmzp)] <- paste0("pn_f_",nmzp[grepl("F",nmzp)])
nmzp <- gsub("_M","",nmzp); nmzp <- gsub("_F","",nmzp)
nmzp <- gsub("65_","65_Inf",nmzp)
names(NPRW)[2:ncol(NPRW)] <- nmzp
nmz <- gsub("p","",nmzp)

## merge & multiply
D <- merge(D,NPRW,by='iso3',all.x = TRUE,all.y=FALSE) #join in
D[,c(nmz):=lapply(.SD,as.numeric),.SDcols=nmz] #make numeric from int
## only apply the pulmonary props as a SA
print(sum(D[,..nmz])/1e6)
pfact <- data.table(iso3=D[,iso3],rawn=rowSums(D[,..nmz])) #pulmonary correction factor
if(pulmsa){
  cat("============ RUNNING PULMONARY SENSITIVITY ANALYSIS ===========\n")
  for(nm in nmz) # multiply by corresponding pulmonary factor
    D[,c(nm):=D[,nm,with=FALSE] * D[,paste0('p',nm),with=FALSE]]
}
D[,c(nmzp):=NULL]                       #ditch the pulmonary props
pfact$newn <- rowSums(D[,..nmz])
pfact[,pfac:=newn/rawn]
pfact[!is.finite(pfac),pfac:=1]

save(pfact,file=here('data/pfact.Rdata')) #load parent data - from above

## --- restrict
D <- D[,.(iso3,country,g_whoregion,
          n_m_0_4,n_m_5_14,n_m_15_24,n_m_25_34,n_m_35_44,n_m_45_54,
          n_m_55_64,n_m_65_Inf,
          n_f_0_4,n_f_5_14,n_f_15_24,n_f_25_34,n_f_35_44,n_f_45_54,
          n_f_55_64,n_f_65_Inf,
          cdr04,cdr04ab,cdr514,cdr514ab)]
D

print(sum(D[,..nmz])/1e6)

save(D,file=here('data/D.Rdata'))

## =========== merging and reshaping
## could restart here
rm(list=ls())
load(here('data/D.Rdata'))     #load parent data - from above
load(here('data/pfact.Rdata')) #load parent data - from above
load(here('indata/U5.Rdata'))  #load HH size predictions - prev work
load(here('indata/O5.Rdata'))  #load HH size predictions - prev work

## reshape
DL <- melt(D,id.vars = c("iso3","country","g_whoregion",#
                         "cdr04","cdr04ab",
                         "cdr514","cdr514ab"))

## remap for consistency
U5$acat <- plyr::mapvalues(U5$acat,from=levels(U5$acat),
                           to=c(levels(U5$acat)[1:5],'[65,Inf)'))

O5$acat <- plyr::mapvalues(O5$acat,from=levels(O5$acat),
                           to=c(levels(O5$acat)[1:5],'[65,Inf)'))


## test <- head(DL$variable)               #for testing functions below

## extract sex from notification variables
getsex <- function(x){
  toupper(unlist(lapply(strsplit(as.character(x),"_"),
                        function(x)x[[2]])))
}
## getsex(test)                            #test

## extract age category from notification variables
getacat <- function(x){
  bot <- unlist(lapply(strsplit(as.character(x),"_"),function(x)x[[3]]))
  top <- unlist(lapply(strsplit(as.character(x),"_"),function(x)x[[4]]))
  paste0('[',bot,',',top,')')
}
## getacat(test) #test

## tidy up
DL[,sex:=factor(getsex(variable))]      #add sex category
DL[,acat:=factor(getacat(variable))]    #add age category
DL <- DL[!acat %in%c('[0,4)','[5,14)')] #drop child notifications
DL$acat <- plyr::mapvalues(DL$acat,from=as.character(DL[,unique(acat)]),
                           to=levels(U5$acat))


## --- merge HH predictions
names(U5)[2:5] <- c('sex','acat','HHu5mu','HHu5logsd')
U5[,sex:=factor(c('M','F')[as.numeric(as.character(sex))])]
DL <- merge(DL,U5,by=c('iso3','acat','sex'),all.x=TRUE)

names(O5)[2:5] <- c('sex','acat','HHo5mu','HHo5logsd')
O5[,sex:=factor(c('M','F')[as.numeric(as.character(sex))])]
DL <- merge(DL,O5,by=c('iso3','acat','sex'),all.x=TRUE)

## U5[iso3=='ZWE']
## DL[iso3=='ZWE']

## regional average for NAs
wcols <- c('HHu5mu','HHu5logsd','HHo5mu','HHo5logsd')
DL[,(wcols):=lapply(wcols,function(x){
  x <- get(x)
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}),by=.(g_whoregion,acat,sex)]


## fill in NAs with regional average
wcols <- names(DL)[(ncol(DL)-3):ncol(DL)]
DL[,(wcols):=lapply(wcols,function(x){
  x <- get(x)
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}),by=g_whoregion]

DL[iso3=='ABW']
DL[iso3=='ZWE']

save(DL,file=here('data/DL.Rdata'))


## === making parent data frame for younger children and calculating HH contacts
load(here('data/DL.Rdata'))


## making notifications proportions
DL[,totv:=sum(value),by=iso3]
DL[,value:=value / (totv+1e-7)] #pattern
DL[iso3=='ZAF']
DL[iso3=='ZAF',sum(value)]
DL[,c('variable','totv'):=NULL]

## country level version
DLC <- unique(DL[,.(iso3,g_whoregion,
                    cdr04,cdr04ab)])
DLC

## NB this is value hh stuff ( acat adult, but we can aggregate these)
DLK <- DL[,.(iso3,acat,sex,value,HHu5mu,HHu5logsd)]

## extend for calculating numbers mean numbers of HH contacts
nrep <- 1e3
DLKL <- DLK[rep(1:nrow(DLK),nrep),]
DLKL[,repn:=rep(1:nrep,each=nrow(DLK))]

## u5 contacts found
DLKL[,phh := rlnorm(n = nrow(DLKL), meanlog=HHu5mu, sdlog=HHu5logsd)]
DLKL[,u5hhc := value * phh]             #under 5 HH mean contacts
## DLKL[,summary(phh)]                     #check
## DLKL[,qplot(phh)]           #check

## aggregates
chhc <- DLKL[,.(u5hhc=sum(u5hhc),notes=sum(value),phh=mean(phh)),
             by=.(repn,iso3)] #country aggregates
print(chhc)

## ## checking!
## CHK<- merge(chhc,AM[,.(ny=mean(n04_m),no=mean(n514_m)),by=iso3],all.x=FALSE,all.y=TRUE)
## CHK[,ny:=ny*notes]
## CHK
## CHK[,summary(u5hhc/ny)]
## qplot(data=CHK,y=u5hhc,x=ny) + geom_abline(intercept=0,slope=1,col=2)


ghhc <- DLKL[,.(u5hhc=sum(u5hhc),notes=sum(value),phh=mean(phh)),
             by=.(repn)] #global aggregates


summary(ghhc)
ghhc[,mean(u5hhc)*1e-6]                 #around 3 million u5 contacts
## sanity checks
chhc[iso3=='ZAF',summary(u5hhc)]/DLK[iso3=='ZAF',sum(value)] #check
## chhc[iso3=='ZAF',qplot(u5hhc)]
## chhc[iso3=='AFG',qplot(u5hhc)]

##log-normal approx
chhc <- chhc[,.(u5hhc=mean(phh),u5hhc.sd=sd(phh),
                u5hhc.l=mean(log(phh)),u5hhc.sdl=sd(log(phh))),by=iso3]

## merge to make parent data table for PSA
DLC <- merge(DLC,chhc,by='iso3')
DLC <- merge(DLC,pfact[,.(iso3,pfac)],by='iso3')

save(DLC,file=here('data/DLC.Rdata'))

## ====== same but for 5-15
load(here('data/DL.Rdata'))

## country level version
DLO <- unique(DL[,.(iso3,g_whoregion,
                    cdr514,cdr514ab)])
DLO

## NB this is value hh stuff ( acat adult, but we can aggregate these)
DLK <- DL[,.(iso3,acat,sex,value,HHo5mu,HHo5logsd)]

## extend for calculating numbers mean numbers of HH contacts
nrep <- 1e3
DLKL <- DLK[rep(1:nrow(DLK),nrep),]
DLKL[,repn:=rep(1:nrep,each=nrow(DLK))]

## u5 contacts found
DLKL[,phh := rlnorm(n = nrow(DLKL), meanlog=HHo5mu, sdlog=HHo5logsd)]
DLKL[,o5hhc := value * phh]             #under 5 HH mean contacts
## DLKL[,summary(phh)]                     #check
## DLKL[,qplot(phh)]           #check

## aggregates 
ohhc <- DLKL[,.(o5hhc=sum(o5hhc),notes=sum(value),phh=mean(phh)),
             by=.(repn,iso3)] #country aggregates

ghhc <- DLKL[,.(o5hhc=sum(o5hhc),notes=sum(value),phh=mean(phh)),
             by=.(repn)] #global aggregates

summary(ghhc)
ghhc[,mean(o5hhc)*1e-6]                 #around 5 million u5 contacts
## sanity checks
ohhc[iso3=='ZAF',summary(o5hhc)]/DLK[iso3=='ZAF',sum(value)] #check
## ohhc[iso3=='ZAF',qplot(o5hhc)]
## ohhc[iso3=='AFG',qplot(o5hhc)]


ohhc <- ohhc[,.(o5hhc=mean(phh),o5hhc.sd=sd(phh),
                o5hhc.l=mean(log(phh)),o5hhc.sdl=sd(log(phh))),by=iso3]


DLO <- merge(DLO,ohhc,by='iso3')
DLO <- merge(DLO,pfact[,.(iso3,pfac)],by='iso3')

save(DLO,file=here('data/DLO.Rdata'))

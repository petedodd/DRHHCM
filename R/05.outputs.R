## calculations from outputs 
rm(list=ls())


## =================================
## PACKAGES
## =================================

library(here)
library(data.table)
library(ggplot2)
library(scales)
library(ggthemes)
library(ggpubr)
library(ggrepel)
library(glue)
library(stringr)

## =================================
## UTILITY FUNCTIONS
## =================================


## data utilities
see <- function(x,ns=3)formatC(signif(x,ns),big.mark = ",",format='fg') #for reading big numbers
lo <- function(x) quantile(x,.025)
hi <- function(x) quantile(x,.975)
tv <- function(x) tableHTML::tableHTML(x) #looking at tables
fmean <- function(x) mean(x[is.finite(x)]) #a mean for finite (no NA or Inf)
gh <- function(x) glue(here(x))

seehere <- function(x) paste0(see(mean(x)),
                              " (",see(lo(x))," - ",
                              see(hi(x)),")")


## NOTE costs: LVX < MXF; DLM < BDQ
## function for doing by regimen and intervention
makehilo <- function(X,cls){
  Xm <- X[,lapply(.SD,mean),
          by=.(`PT regimen`,intervention),
          .SDcols=cls]
  Xh <- X[,lapply(.SD,hi),
          by=.(`PT regimen`,intervention),
          .SDcols=cls]
  Xl <- X[,lapply(.SD,lo),
          by=.(`PT regimen`,intervention),
          .SDcols=cls]
  clslo <- paste0(cls,'.lo')
  clshi <- paste0(cls,'.hi')
  names(Xl)[names(Xl) %in% cls] <- clslo
  names(Xh)[names(Xh) %in% cls] <- clshi
  Xlmh <- merge(Xm,Xl,by=c('PT regimen','intervention'))
  Xlmh <- merge(Xlmh,Xh,by=c('PT regimen','intervention'))
  acls <- c(cls,clslo,clshi)
  Xlmh[,(acls):=lapply(.SD,see),.SDcols=acls]
  for(nm in cls){
    nnm <- paste0(nm,'.all')
    Xlmh[,(nm):=paste0(Xlmh[[nm]],
                       " (",
                       Xlmh[[paste0(nm,'.lo')]],
                       " - ",
                       Xlmh[[paste0(nm,'.hi')]],
                       ")")]
  }
  Xlmh
}


## graph utilities
absspace <- function(x,...) {
  format(abs(x), ..., big.mark=" ",scientific = FALSE, trim = TRUE)
}
rot45 <- theme(axis.text.x = element_text(angle = 45, hjust = 1))
pnlth <- theme(
  axis.text.x = element_text(angle=45,hjust=1),
  panel.grid.major = element_line(colour="grey"),
  panel.grid.minor = element_line(colour="grey"),
  strip.background = element_blank(),
  legend.position = 'top',
  panel.border = element_rect(colour = "black",fill=NA)
)


## =================================
## DATA
## =================================

cat("==== DATA preparation =======\n")


## load or create LYs
source(here('R/utils/makeLYs.R'))
## NOTE discount rate sensitivity analses set here!
drv <- 3; drx <- ifelse(drv==1,'1',ifelse(drv==5,'5',''))
LY <- LYD[dr==drv] #choosing discount rate
LY <- melt(LY[,.(iso3,`[0,5)`=u5,`[5,15)`=o5)],id='iso3')
LY <- LY[,.(iso3,acat=variable,lys=value)] #rename


## load all results
load(here('data/IV.Rdata'))
load(here('data/IVb.Rdata')) #version
load(here('data/IVT.Rdata'))
load(here('indata/FRF.Rdata'))
load(here('indata/GDP.Rdata'))
WK <- fread(here('indata/TB_notifications_2020-10-15.csv'))
WK <- unique(WK[,.(iso3,g_whoregion,country)])  #key!
setkey(WK,iso3)
load(here('indata/HBC.Rdata'))          #HBC lists
WB <- fread(here('indata/WBIL.csv'))    #income

## parameters for AE output only
PZ <- HEdtree::parse.parmtable(data = read.csv(here('indata/DRHHCMparms.csv'))) #build parameter object no test


## safety
(bad <- IV[!is.finite(cost),unique(iso3)])
(badb <- IVb[!is.finite(cost),unique(iso3)])
IV <- IV[!iso3 %in% bad]
IVb <- IVb[!iso3 %in% badb]

if(drx==""){
  cat(IV[,length(unique(iso3))],file=here('output/isolistN.txt'))
  cat(IV[,unique(iso3)],file=here('output/isolist.txt'),sep=', ')
}

## =================================
## DATA WORK
## =================================



## === AE output
AE <- IV[`PT regimen`=='FQ',.(iso3,repn,acat,ptc,intervention)]
aex <- data.table(ae=PZ$fracAE$r(max(AE$repn)),sae=PZ$fracSAE$r(max(AE$repn)),repn=1:max(AE$repn))
aex[,.(mean(ae),mean(sae))] #CHECK 8%,1.6%
AE <- merge(AE,aex,by='repn',all.x=TRUE)
AE <- merge(AE,WK[,.(iso3,g_whoregion)],by='iso3')
AE[,ae:=ptc*ae]; AE[,sae:=ptc*sae];
AE[repn==1,.(sum(ae),0.08*sum(ptc),sum(sae),0.016*sum(ptc))] #check
AE <- AE[,.(ae=sum(ae),sae=sum(sae)),by=.(acat,g_whoregion,repn)]
AE <- AE[,.(`Adverse Events`=seehere(ae),`Serious Adverse Events`=seehere(sae)),
         by=.(`WHO region`=g_whoregion,`Age cateogory (years)`=acat)]
if(drx==""){
  fwrite(AE,file=here('output/AEests.csv'))
}


## === cost x country data for appendix
KK1 <- IV[,.(cost=sum(cost),costacf=sum(costacf),
             costtpt=sum(costtpt),costatt=sum(costatt)),
          by=.(repn,iso3,intervention,`PT regimen`)]
KK1 <- KK1[,.(cost=mean(cost),costacf=mean(costacf),
             costtpt=mean(costtpt),costatt=mean(costatt)),
          by=.(iso3,intervention,`PT regimen`)]
KK2 <- IVb[,.(cost=sum(cost),costacf=sum(costacf),
             costtpt=sum(costtpt),costatt=sum(costatt)),
          by=.(repn,iso3,intervention,`PT regimen`)]
KK2 <- KK2[,.(cost=mean(cost),costacf=mean(costacf),
              costtpt=mean(costtpt),costatt=mean(costatt)),
           by=.(iso3,intervention,`PT regimen`)]
KK1[`PT regimen`=='FQ',`PT regimen`:='Lfx']
KK <- rbind(KK1,KK2)
KK[,`PT regimen`:=str_to_title(`PT regimen`)]
KK[`PT regimen`=='None',`PT regimen`:='none']
rm(KK1,KK2)

setnames(KK,old=c('cost','costacf','costtpt','costatt'),
         new=c('total','case-finding','TPT-related','ATT-related'))

KKR <- KK[iso3 %in% HBC[g.hbmdr==1,iso3]]
KKR[,total:=NULL]
KKR <- melt(KKR,id=c('iso3','intervention','PT regimen'))
KKR$intervention <- factor(KKR$intervention,
                           levels=c("HHCM, no PT",
                                    "PT to <5/HIV+",
                                    "PT to <5/HIV+/TST+",
                                    "PT to <15"),
                           ordered = TRUE)


## === rename FQR
DX <- FRF[,.(prop=mean(fqr)),by=iso3]

## === data for NNS plot (with safety for FSM)

IVD <- IV[,.(deaths=sum((deaths-deaths0)),
             incdeaths=sum((incdeaths-incdeaths0)),
             inctb=sum((inctb-inctb0)),
             rsatt=sum((rsatt-rsatt0)),
             rratt=sum((rratt-rratt0)),
             ptc=sum((ptc-ptc0)),hhc=sum(hhc)),
          by = .(repn,intervention,`PT regimen`)]

midi <- IVD[,.(rsatt=mean(rsatt/abs(inctb)),
              rratt=mean(rratt/abs(inctb)),
              ptc=mean(ptc/abs(inctb)),
              hhc=mean(hhc/abs(inctb))),
           by=.(intervention,`PT regimen`)]

midd <- IVD[,.(rsatt=mean(rsatt/abs(deaths)),
              rratt=mean(rratt/abs(deaths)),
              ptc=mean(ptc/abs(deaths)),
              hhc=mean(hhc/abs(deaths))),
           by=.(intervention,`PT regimen`)]

midi[,quantity:='per TB case averted']
midd[,quantity:='per TB death averted']

B <- rbind(midi,midd)
BM <- melt(B,id=c('intervention','PT regimen','quantity'))
## BM
## BM[variable=='ptc' & `PT regimen`=='FQ'] #right pattern


BMR <- BM[variable %in% c('ptc','hhc') &
          quantity %in% c('per TB case averted','per TB death averted')]
BMR[grep('BDQ',`PT regimen`),`PT regimen`:='Bdq/Dlm']
BMR[grep('FQ',`PT regimen`),`PT regimen`:='Fq']
BMR[grepl('death',quantity),quantity:='Death']
BMR[grepl('case',quantity),quantity:='Incident TB']
BMR[grepl('ptc',variable),variable:='Preventive therapy courses']
BMR[grepl('hhc',variable),variable:='Household contacts screened']
BMR$intervention <- factor(BMR$intervention,
                           levels=c("PT to <5/HIV+",
                                    "PT to <5/HIV+/TST+",
                                    "PT to <15"),
                           ordered = TRUE)
BMR <- BMR[!is.na(intervention)]    #NOTE this is to drop the no PT
BMR[,intervention2:=gsub('PT','TPT',intervention)]
BMR[quantity=='Incident TB',quantity:='Incident tuberculosis']
BMR$intervention2 <- factor(BMR$intervention2,
                           levels=c("TPT to <5/HIV+",
                                    "TPT to <5/HIV+/TST+",
                                    "TPT to <15"),
                           ordered = TRUE)
BMR$`PT regimen` <- factor(BMR$`PT regimen`,levels=c('Fq','Bdq/Dlm'),ordered = TRUE)

## === data for FQR figures
## NNT vs prop RR that are FQR (by country)
IVC <- IV[,.(deaths=sum((deaths-deaths0)),inctb=sum((inctb-inctb0)),
             rsatt=sum((rsatt-rsatt0)),rratt=sum((rratt-rratt0)),
             ptc=sum((ptc-ptc0))),
          by = .(repn,iso3,intervention,`PT regimen`)]
## per qty with safety for 0
ICM1 <- IVC[,.(ptc=mean(ptc/(abs(inctb)+1e-10))),
            by=.(iso3,intervention,`PT regimen`)]
ICM2 <- IVC[,.(ptc=mean(ptc/(abs(deaths)+1e-10))),
            by=.(iso3,intervention,`PT regimen`)]

## FQR data
ICM1 <- merge(ICM1,DX,by='iso3',all.x=TRUE)
ICM2 <- merge(ICM2,DX,by='iso3',all.x=TRUE)
## whoregion
ICM1 <- merge(ICM1,WK,by='iso3',all.x=TRUE)
ICM2 <- merge(ICM2,WK,by='iso3',all.x=TRUE)

## === compile CE results (needed for CE plot and tables)
## NOTE check
IV[,table(`PT regimen`,intervention)]

## adding coprevalence deaths
IV[,prevdeaths:=deaths-incdeaths]
IV[,prevdeaths0:=deaths0-incdeaths0]
IVb[,prevdeaths:=deaths-incdeaths]
IVb[,prevdeaths0:=deaths0-incdeaths0]
IVT[,prevdeaths:=deaths-incdeaths]
IVT[,prevdeaths0:=deaths0-incdeaths0]

## add in DALYS
IV <- merge(IV,LY,by=c('iso3','acat'))
IV[,c('lys0','lys1'):=.(lys*deaths0,lys*deaths)]
IVb <- merge(IVb,LY,by=c('iso3','acat'))
IVb[,c('lys0','lys1'):=.(lys*deaths0,lys*deaths)]
IVT <- merge(IVT,LY,by=c('iso3','acat'))
IVT[,c('lys0','lys1'):=.(lys*deaths0,lys*deaths)]

## regional global numbers
IV <- merge(IV,WK,by='iso3',all.x = TRUE)
IVb <- merge(IVb,WK,by='iso3',all.x = TRUE)

## restrict to LVX/DLM for CEACs
IVE <- rbind(IV,IVb)
IVE <- IVE[!`PT regimen` %in% c('MXF','BDQ')]
IVE[`PT regimen`=='FQ',`PT regimen`:='Lfx']
IVE[`PT regimen`=='BDQ',`PT regimen`:='Bdq']

## data for CEACs
EAC <- IVE[iso3 %in% HBC[g.hbmdr==1,iso3]]
EAC <- EAC[,.(dDALYs=sum(lys-lys0),dcost=sum(cost-cost0)),
           by=.(iso3,`PT regimen`,intervention,repn)]
threshold <- seq(from=0,to=5e3,by=100)
CEAC <- list()
for(th in threshold){
  tmp <- EAC[,.(prob=mean(-th*dDALYs-dcost>0)),
             by=.(iso3,`PT regimen`,intervention)]
  tmp[,threshold:=th]
  CEAC[[paste0(th)]] <- tmp
}
CEAC <- rbindlist(CEAC)
CEAC[`PT regimen`=='DLM',`PT regimen`:='Dlm']

## --- data for CE plot

## by country
IVc <- IVE[,.(deaths=sum(deaths),cost=sum(cost),lys=sum(lys1)),
          by = .(repn,iso3,intervention,`PT regimen`)]
IVc <- IVc[,.(deaths=mean(deaths),cost=mean(cost),lys=mean(lys)),
           by = .(iso3,intervention,`PT regimen`)]
IVc0 <- IVE[,.(deaths=sum(deaths0),cost=sum(cost0),lys=sum(lys0)),
           by = .(repn,iso3,intervention,`PT regimen`)]
IVc0 <- IVc0[,.(deaths0=mean(deaths),cost0=mean(cost),lys0=mean(lys)),
             by = .(iso3,intervention,`PT regimen`)]

CEC <- merge(IVc,IVc0)
CEC <- merge(CEC,GDP,by='iso3',all.x=TRUE,all.y = FALSE)
CEC[,cpda:=(cost-cost0)/(lys0-lys+1e-6)]

tmp <- unique(CEC[!is.na(gdp),.(gdp,iso3)])
lvls <- tmp[order(gdp),iso3]
tmp[,c('intervention','PT regimen'):=NA]

CEC$iso3 <- factor(CEC$iso3,levels=lvls,ordered = TRUE)
tmp$iso3 <- factor(tmp$iso3,levels=lvls,ordered = TRUE)


## --- all RS version for comparison
TIV <- IVT[,.(deaths=sum((deaths-deaths0)),
             incdeaths=sum((incdeaths-incdeaths0)),
             inctb=sum((inctb-inctb0)),
             rsatt=sum((rsatt-rsatt0)),
             rratt=sum((rratt-rratt0)),
             ptc=sum((ptc-ptc0)),hhc=sum(hhc)),
          by = .(repn,intervention,`PT regimen`)]

tivi <- TIV[,.(rsatt=mean(rsatt/abs(inctb)),
              rratt=mean(rratt/abs(inctb)),
              ptc=mean(ptc/abs(inctb)),
              hhc=mean(hhc/abs(inctb))),
           by=.(intervention,`PT regimen`)]
tivd <- TIV[,.(rsatt=mean(rsatt/abs(deaths)),
              rratt=mean(rratt/abs(deaths)),
              ptc=mean(ptc/abs(deaths)),
              hhc=mean(hhc/abs(deaths))),
           by=.(intervention,`PT regimen`)]
tivi[,quantity:='per TB case averted']
tivd[,quantity:='per TB death averted']

TB <- rbind(tivi,tivd)
TM <- melt(TB,id=c('intervention','PT regimen','quantity'))
TMR <- TM[variable %in% c('ptc','hhc') &
          quantity %in% c('per TB case averted','per TB death averted')]
TMR[grepl('death',quantity),quantity:='Death']
TMR[grepl('case',quantity),quantity:='Incident TB']
TMR[grepl('ptc',variable),variable:='Preventive therapy courses']
TMR[grepl('hhc',variable),variable:='Household contacts screened']
TMR$intervention <- factor(TMR$intervention,
                           levels=c("PT to <5/HIV+",
                                    "PT to <5/HIV+/TST+",
                                    "PT to <15"),
                           ordered = TRUE)

## benchmarks/checks
IVT[,.(inCFR=1e2*sum(incdeaths)/sum(inctb),
       inCDR=1e2*sum(rsatti)/sum(inctb),
       hhinc=1e2*sum(inctb)/sum(hhc),
       hhdeaths=1e2*sum(deaths)/sum(hhc))]


## =================================
## FIGURES
## =================================
cat("==== doing FIGURES =======\n")

colz2 <- c('Bdq/Dlm'="#E69F00",'Fq'="#000000")

if(drx==""){ #begin SA exclusion

  ## also save data
  fwrite(BMR[,.(intervention,`PT regimen`,quantity,variable,value)],
         file=here('output/etable_NN.csv'))

  ## === figure_NN
  GP <- ggplot(BMR,aes(intervention2,value,fill=`PT regimen`)) +
    geom_bar(stat='identity',position='dodge') +
    facet_grid(variable ~ quantity ,scales='free') +
    scale_fill_manual(values=colz2,name='TPT regimen')+
    xlab('Intervention')+
    ylab('Additional resource (row) per quantity averted (column)') +
    pnlth
  ## GP

  ggsave(GP,file=here('output/figure_NN.eps'),w=6,h=7)
  ggsave(GP,file=here('output/figure_NN.png'),w=6,h=7)


  ## --- all RS version
  ## also save data
  fwrite(TMR,
         file=here('output/etable_RSNN.csv'))

  ## graph
  GP <- ggplot(TMR,aes(intervention,value)) +
    geom_bar(stat='identity',position='dodge') +
    geom_text(aes(label=ceiling(value)),vjust=-.1,col=2)+
    facet_grid(variable ~ quantity ,scales='free') +
    xlab('Intervention')+
    ylab('Additional resource (row) per quantity averted (column)') +
    pnlth
  ## GP

  ggsave(GP,file=here('output/Sfigure_NNRS.png'),w=6,h=7)


  ## === figure_FQR
  ## checking countries etc
  tmp <- ICM1[`PT regimen`!='INH' & intervention=='PT to <15']
  tmp[,lbl:=as.character(iso3)]
  tmp[`PT regimen`!='FQ',lbl:=NA]
  tmp[,region:=g_whoregion]
  tmp <- merge(tmp,WB[,.(iso3,hic=(income=='High income'))],by='iso3')
  tmp <- tmp[hic!=TRUE] #strip out high income countries
  tmp <- tmp[ptc>0]     #the zeros are pacific islands with 0 across the board
  tmp[`PT regimen`!='FQ',`PT regimen`:='Bdq/Dlm']

  ## face validity checks
  GPd <- ggplot(tmp,
                aes(prop,ptc,col=region,shape=`PT regimen`)) +
    geom_point() +
    scale_x_continuous(label=percent) +
    guides(shape='none')+
    scale_color_colorblind(position='top')+ 
    xlab('Proportion of RR/MDR in children that is FQR')+
    ylab('PT courses to prevent a TB case')+
    expand_limits(y=0) +
    geom_text_repel(aes(label=lbl))+
    theme_classic() + ggpubr::grids()+
    theme(legend.position = c(0.2,0.8))+
    guides(colour = guide_legend(nrow = 1))
  ## GPd

  ggsave(GPd,file=here('output/figure.labelled.FQRscatter.pdf'),w=18,h=15)

  ## -- panel a
  ## plot
  GPa <- ggplot(tmp,
                aes(prop,ptc,col=`PT regimen`,shape=intervention)) +
    geom_point() +
    scale_x_continuous(label=percent) +
    guides(shape='none')+
    scale_color_manual(values=colz2,name='TPT regimen')+
    xlab('Proportion of RR/MDR-TB that is fluoroquinolone-resistant')+
    ylab('TPT courses to prevent one occurrence of tuberculosis')+
    geom_smooth(method='lm')+
    expand_limits(y=0) +
    ggpubr::stat_cor(show.legend = FALSE)+
    theme_classic() + ggpubr::grids()
  ## GPa

  ## -- panel b

  ## reshape & aggregate
  tmp2 <- dcast(tmp,iso3+g_whoregion ~ `PT regimen`,value.var = 'ptc')
  tmp2[,df:=FQ-`Bdq/Dlm`]
  tmp2 <- tmp2[,.(df=median(df,na.rm=TRUE)),by=.(g_whoregion)]
  tmp2 <- tmp2[order(as.character(g_whoregion))]
  tmp2[,region:=g_whoregion]
  tmp2$region <- factor(tmp2$region,levels=unique(tmp2$region),
                        ordered=TRUE)

  ## plot
  GPb <- ggplot(tmp2,aes(region,df)) +
    geom_bar(stat='identity') +
    xlab('WHO region') +
    ylab('Median difference in TPT courses per\n occurrence of tuberculosis prevented')+
    theme_classic() + ggpubr::grids()
  ## GPb

  ## combine
  GPB <- ggarrange(GPa,GPb,nrow=1,labels=c('A','B'))

  ggsave(GPB,file=here('output/figure_FQR.eps'),w=12,h=5)
  ggsave(GPB,file=here('output/figure_FQR.png'),w=12,h=5)

} #end SA exclusion

## === figure_CE
## restrict to hbmdr
CECR <- CEC[cpda>0 & iso3 %in% unique(tmp$iso3)]
CECR <- CECR[iso3 %in% HBC[g.hbmdr==1,iso3]]
CECR$iso3 <- factor(CECR$iso3)

tmp <- unique(CEC[!is.na(gdp),.(gdp,iso3)])
tmp <- tmp[iso3%in%HBC[g.hbmdr==1,iso3]]
lvls <- as.character(tmp[order(gdp),iso3])

tmp[,c('intervention','PT regimen'):=NA]
CECR$iso3 <- factor(CECR$iso3,levels=lvls,ordered = TRUE)
tmp$iso3 <- factor(tmp$iso3,levels=lvls,ordered = TRUE)
CECR[`PT regimen`=='DLM',`PT regimen`:='Dlm']
CECR$`PT regimen` <- factor(CECR$`PT regimen`,
                            levels=c('none','Lfx','Dlm'))
CECR[,intervention2:=gsub('PT','TPT',intervention)]
tmp[,intervention2:=gsub('PT','TPT',intervention)]

colz <- c('none'="#56B4E9",'Lfx'="#000000",'Dlm'="#E69F00")
shpz <- c("HHCM, no TPT"=1,"TPT to <5/HIV+"=2,
          "TPT to <5/HIV+/TST+"=3,"TPT to <15"=4)
shps <- c("HHCM, no PT"=1,"PT to <5/HIV+"=2,
          "PT to <5/HIV+/TST+"=3,"PT to <15"=4)

xlb <- 'Incremental cost-effectiveness ratio\n(USD per 3%-discounted DALY averted)'
if(drx!="")
  xlb <- glue('Incremental cost-effectiveness ratio\n(USD per {drx}%-discounted DALY averted)')

## make plot
tp <- 5e3
GP <- ggplot(CECR,
             aes(iso3,cpda,
                 shape=intervention2,
                 group=iso3,
                 col=`PT regimen`))+
  geom_line(data=tmp,aes(x=iso3,y=gdp/1,group=1),col='darkgrey')+
  geom_line(data=tmp,aes(x=iso3,y=gdp/2,group=1),lty=2,col='darkgrey')+
  geom_point(size=3) +
  scale_shape_manual(values=shpz,name='Intervention')+
  scale_color_manual(values=colz,name='TPT regimen')+
  annotate(geom='text',y=tp*0.8,x=19,label='1.0xGDP')+
  annotate(geom='text',y=tp*0.8,x=25,label='0.5xGDP')+
  coord_flip()+
  scale_y_continuous(label=absspace,limits=c(0,tp))+
  xlab('Country ISO3 code')+
  ylab(xlb)+
  theme_classic()+ggpubr::grids()+
  theme(legend.position=c(0.8,0.2))
GP

## NOTE seems to want to be run interactively?
## ggsave(GP,file=gh('output/figure_iCE{drx}.eps'),w=10,h=10)
ggsave(GP,file=gh('output/figure_iCE{drx}.jpg'),w=10,h=10)



## new version under review
inmz <- c("Household contact management, no tuberculosis preventive therapy",
          "Tuberculosis preventive therapy for children with HIV or under 5",
          "Tuberculosis preventive therapy for children with HIV, a positive tuberculin skin test, or under 5",
          "Tuberculosis preventive therapy for all children under 15"
          )
CECR[intervention2=="HHCM, no TPT",intervention3:=inmz[1]]
CECR[intervention2=="TPT to <5/HIV+",intervention3:=inmz[2]]
CECR[intervention2=="TPT to <5/HIV+/TST+",intervention3:=inmz[3]]
CECR[intervention2=="TPT to <15",intervention3:=inmz[4]]
CECR[,ptreg:="None"]
CECR[`PT regimen`=="Lfx",ptreg:="Levofloxacin"]
CECR[`PT regimen`=="Dlm",ptreg:="Delamanid"]
tmp[,c('intervention3','ptreg'):=NA]
colz2 <- c('None'="#56B4E9",'Levofloxacin'="#000000",'Delamanid'="#E69F00")
shpz2 <- shpz
names(shpz2) <- inmz
clbz <- WK[lvls,country] #full country names



## make plot
GP <- ggplot(CECR,
             aes(iso3,cpda,
                 shape=intervention3,
                 group=iso3,
                 col=ptreg))+
  geom_line(data=tmp,aes(x=iso3,y=gdp/1,group=1),col='darkgrey')+
  geom_line(data=tmp,aes(x=iso3,y=gdp/2,group=1),lty=2,col='darkgrey')+
  geom_point(size=3) +
  scale_x_discrete(labels=clbz)+
  scale_shape_manual(values=shpz2,name='Intervention')+
  scale_color_manual(values=colz2,name='Tuberculosis preventive therapy regimen')+
  annotate(geom='text',y=tp*0.8,x=19,label='1.0xGDP')+
  annotate(geom='text',y=tp*0.8,x=25,label='0.5xGDP')+
  coord_flip()+
  scale_y_continuous(label=absspace,limits=c(0,tp))+
  xlab('')+
  ylab(xlb)+
  theme_classic()+ggpubr::grids()+
  theme(legend.position=c(0.65,0.2))
## GP

## NOTE seems to want to be run interactively?
ggsave(GP,file=gh('output/figure_nCE{drx}.pdf'),w=10,h=10)
ggsave(GP,file=gh('output/figure_nCE{drx}.eps'),w=10,h=10)
ggsave(GP,file=gh('output/figure_nCE{drx}.jpg'),w=10,h=10)


## --- supplementary figure CEACs
GP <- ggplot(CEAC,aes(threshold,prob,col=`PT regimen`,lty=intervention))+
  scale_color_manual(values=colz)+
  geom_line() +
  facet_wrap(~iso3,ncol=5)+
  scale_y_continuous(label=scales::percent)+
  scale_x_continuous(label=scales::comma)+
  xlab('Cost-effectiveness threhsold (USD)')+
  ylab('Probability cost-effective')
## GP

ggsave(GP,file=gh('output/Sfigure_CEAC{drx}.png'),w=12,h=12)


## --- supplementary version with all countries
tmp <- unique(CEC[!is.na(gdp),.(gdp,iso3)])
lvls <- tmp[order(gdp),iso3]
tmp[,c('intervention','PT regimen'):=NA]
CEC[`PT regimen`=='DLM',`PT regimen`:='Dlm']
CEC$iso3 <- factor(CEC$iso3,levels=lvls,ordered = TRUE)
tmp$iso3 <- factor(tmp$iso3,levels=lvls,ordered = TRUE)

tp <- 7e3
GP <- ggplot(CEC[cpda>0 & iso3 %in% unique(tmp$iso3)],
       aes(iso3,cpda,
           shape=intervention,
           group=iso3,
           col=`PT regimen`))+
  geom_line(data=tmp,aes(iso3,gdp/1,group=1))+
  geom_line(data=tmp,aes(iso3,gdp/2,group=1),lty=2)+
  geom_line(data=tmp,aes(iso3,gdp/5,group=1),lty=3)+
  geom_point() +
  annotate(geom='text',y=tp*0.8,x=100,label='1.0xGDP')+
  annotate(geom='text',y=tp*0.8,x=120,label='0.5xGDP')+
  annotate(geom='text',y=tp*0.8,x=140,label='0.2xGDP')+
  coord_flip()+
  scale_shape_manual(values=shps,name='Intervention')+
  scale_color_manual(values=colz,name='TPT regimen')+
  scale_y_continuous(label=absspace,limits=c(0,tp))+
  xlab('Country ISO3 code')+
  ylab(xlb)+
  theme_classic()+ggpubr::grids()+
  theme(legend.position=c(0.8,0.2))

ggsave(GP,file=gh('output/Sfigure_CE{drx}.png'),w=20,h=25)


## --- (really table) ---
## CSV output of above data
cecsv <- CEC[cpda>0 & iso3 %in% unique(tmp$iso3)]
cecsv <- dcast(cecsv,iso3 ~ intervention + `PT regimen` ,value.var='cpda')
if(drx=="")
  fwrite(format(cecsv,digits=0,nsmall=0,scientific=FALSE),file=here('output/F3_ICERdata.csv'))


if(drx==""){ #begin SA exclusion
  ## --- supplementary figure on cost breakdown

  GP <- ggplot(KKR[`PT regimen` %in% c('Lfx','none')],
               aes(intervention,value,fill=`variable`))+
    geom_bar(stat='identity')+
    facet_wrap(~iso3,scales='free')+
    scale_y_continuous(label=comma)+
    xlab('Intervention (with Lfx-based TPT)')+ylab('Cost')+
    theme(legend.position = 'top',
          legend.title = element_blank(),
          axis.text.x =element_text(angle = 45, hjust=1)
          )
  ## GP

  ggsave(GP,file=here('output/Sfigure_costs.png'),w=25,h=15)


  ## --- understanding odd cost countries
  oddcs <- c('RUS','KAZ','PER','BLR')
  KK[`PT regimen` %in% 'Lfx' &
     intervention=='PT to <15',
     attfrac:=`ATT-related`/total]
  tmp <- KK[is.finite(attfrac) &
            iso3 %in% HBC[g.hbmdr==1,iso3],
            .(iso3,attfrac)]
  tmp[,odd:=ifelse(iso3 %in% oddcs,TRUE,FALSE)]
  tmp <- merge(tmp,DX,by='iso3',all.x=TRUE,all.y=FALSE)
  tmp[,cl:=ifelse(odd,'red','black')]

  GP <- ggplot(tmp,
               aes(prop,attfrac,
                   col=I(cl),label=iso3))+
    geom_point()+
    scale_y_continuous(label=percent)+
    scale_x_continuous(label=percent)+
    geom_text_repel()+
    xlab('Proportion of MDR/RR-TB that is FQR')+
    ylab('Proportion of costs that are ATT-related')+
    theme_classic() + ggpubr::grids()
  ## GP

  ggsave(GP,file=here('output/Sfigure_cost_explore.png'),w=10,h=10)

  ## look at unit costs
  C0 <- fread(here('output/country_unit_costs.csv'))
  CM <- C0[,.(cost.m=mean(cost.m),cost.me=median(cost.m)),by=unit_cost]
  Codd <- C0[iso3 %in% oddcs,.(iso3,unit_cost,cost.m)]
  Codd <- dcast(Codd,unit_cost ~ iso3,value.var = 'cost.m')
  CM <- merge(CM,Codd,by='unit_cost')
  names(CM)[2:3] <- c('mean','median')
  fwrite(CM,file=here('output/tmp_uc.csv'))

} #end SA exclusion

cat("==== FIGURES done =======\n")

## =================================
## TABLES
## =================================

cat("==== starting TABLES =======\n")

## === outcomes table
IVS <- IV[,.(
  ## absolute
  deaths=sum(deaths),
  lys=sum(lys1),
  incdeaths=sum(incdeaths),
  prevdeaths=sum(prevdeaths),
  inctb=sum(inctb),
  rsatt=sum(rsatt),rratt=sum(rratt),
  ptc=sum(ptc),hhc=sum(hhc),cost=sum(cost)
  ),
  by = .(repn,intervention,`PT regimen`)]

DIVS <- IV[,.(
  ## increments
  Ddeaths=sum(deaths-deaths0),
  Dlys=sum(lys1-lys0),
  Dincdeaths=sum(incdeaths-incdeaths0),
  Dprevdeaths=sum(prevdeaths-prevdeaths0),
  Dinctb=sum(inctb-inctb0),
  Drsatt=sum(rsatt-rsatt0),Drratt=sum(rratt-rratt0),
  Dptc=sum(ptc-ptc0),Dhhc=sum(hhc),Dcost=sum(cost-cost0)
),by = .(repn,intervention,`PT regimen`)]

IVS0 <- IV[`PT regimen`=='FQ', #NOTE otherwise duplicated
           .(deaths=sum(deaths0),
             lys=sum(lys0),
             incdeaths=sum(incdeaths0),
             prevdeaths=sum(prevdeaths0),
             inctb=sum(inctb0),
             rsatt=sum(rsatt0),
             rratt=sum(rratt0),
             ptc=sum(ptc0),
             hhc=0, #hhc not recorded in 04, but =0
             cost=sum(cost0)),
          by = .(repn,intervention,`PT regimen`)]
IVS0 <- IVS0[intervention=='PT to <15']
IVS0[,intervention:='No HHCM']
IVS0[,`PT regimen`:='none']
IVS <- rbind(IVS,IVS0)

## NOTE check
IVS[,table(`PT regimen`,intervention)]

rsIVS <- IV[DST=='RS',.(deaths=sum(deaths),
                        lys=sum(lys1),
                        incdeaths=sum(incdeaths),
                        prevdeaths=sum(prevdeaths),
                        inctb=sum(inctb)),
            by = .(repn,intervention,`PT regimen`)]
rrIVS <- IV[DST!='RS',.(deaths=sum(deaths),
                        lys=sum(lys1),
                        incdeaths=sum(incdeaths),
                        prevdeaths=sum(prevdeaths),
                        inctb=sum(inctb)),
            by = .(repn,intervention,`PT regimen`)]

rsDIVS <- IV[DST=='RS',.(Ddeaths=sum(deaths-deaths0),
                         Dlys=sum(lys1-lys0),
                         Dincdeaths=sum(incdeaths-incdeaths0),
                         Dprevdeaths=sum(prevdeaths-prevdeaths0),
                         Dinctb=sum(inctb-inctb0)),
            by = .(repn,intervention,`PT regimen`)]
rrDIVS <- IV[DST!='RS',.(Ddeaths=sum(deaths-deaths0),
                         Dlys=sum(lys1-lys0),
                         Dincdeaths=sum(incdeaths-incdeaths0),
                         Dprevdeaths=sum(prevdeaths-prevdeaths0),
                         Dinctb=sum(inctb-inctb0)),
            by = .(repn,intervention,`PT regimen`)]

rsIVS0 <- IV[DST=='RS' & `PT regimen`=='none',
             .(deaths=sum(deaths0),
               lys=sum(lys0),
               incdeaths=sum(incdeaths0),
               prevdeaths=sum(prevdeaths0),
               inctb=sum(inctb0)),
            by = .(repn,intervention,`PT regimen`)]
rsIVS0[,table(intervention,`PT regimen`)]
rrIVS0 <- IV[DST!='RS'  & `PT regimen`=='none',
             .(deaths=sum(deaths0),
               lys=sum(lys0),
               incdeaths=sum(incdeaths0),
               prevdeaths=sum(prevdeaths0),
               inctb=sum(inctb0)),
            by = .(repn,intervention,`PT regimen`)]

## rrIVS0 <- rrIVS0[intervention=='PT to <15']
rrIVS0[,intervention:='No HHCM']
rrIVS <- rbind(rrIVS,rrIVS0)
## rsIVS0 <- rsIVS0[intervention=='PT to <15']
rsIVS0[,intervention:='No HHCM']
rsIVS <- rbind(rsIVS,rsIVS0)

## outcomes in total
vrs <- c('incdeaths','prevdeaths','inctb')
out1 <- makehilo(IVS,cls=vrs)
rstmp <- makehilo(rsIVS,cls='inctb')
rrtmp <- makehilo(rrIVS,cls='inctb')
drop <- c(paste(vrs,"lo",sep="."),paste(vrs,"hi",sep="."))
out1[,c(drop):=NULL]
out1[,rtype:="total"]

## incremental outcomes
dvrs <- paste0('D',vrs)
out2 <- makehilo(DIVS,cls=dvrs)
drstmp <- makehilo(rsDIVS,cls='Dinctb')
drrtmp <- makehilo(rrDIVS,cls='Dinctb')
drop <- c(paste(dvrs,"lo",sep="."),paste(dvrs,"hi",sep="."))
out2[,c(drop):=NULL]
out2[,rtype:="incremental"]
setnames(out2,old=dvrs,new=vrs)

## extra data splitting RR/RS inctb
tmp1 <-  merge(rstmp[,.(`PT regimen`,intervention,rsinctb=inctb)],
               rrtmp[,.(`PT regimen`,intervention,rrinctb=inctb)],
               by=c("PT regimen","intervention")) #split in inctb
tmp2 <-  merge(drstmp[,.(`PT regimen`,intervention,rsinctb=Dinctb)],
               drrtmp[,.(`PT regimen`,intervention,rrinctb=Dinctb)],
               by=c("PT regimen","intervention")) #split in inctb (incremental)
tmp1[,rtype:='total']; tmp2[,rtype:='incremental']
tmp <- rbind(tmp1,tmp2)

## combine & relevel
out <- rbind(out1,out2)
out <- merge(out,tmp)

out <- melt(out,id=c("rtype","intervention","PT regimen"))
out$rtype <- factor(out$rtype,levels=c('total','incremental'))
out$variable <- factor(out$variable,
                       levels=c('inctb','rsinctb','rrinctb','incdeaths','prevdeaths'))
out$intervention <- factor(out$intervention,
                           levels=c("No HHCM","HHCM, no PT",
                                    "PT to <5/HIV+/TST+","PT to <5/HIV+",
                                    "PT to <15"))
out$`PT regimen` <- factor(out$`PT regimen`,
                           levels=c("none","FQ","BDQ"))

## reshape
outcomes <- dcast(out,rtype  + variable ~
                        intervention + `PT regimen`)

## reorder cols
resnames <- c("rtype","variable","No HHCM_none","HHCM, no PT_none",
              "PT to <5/HIV+_FQ",
              "PT to <5/HIV+_BDQ",
              "PT to <5/HIV+/TST+_FQ",
              "PT to <5/HIV+/TST+_BDQ",
              "PT to <15_FQ",
              "PT to <15_BDQ")
setcolorder(outcomes,resnames)

if(drx=="") #SA exclusion
  fwrite(outcomes,file=here('output/table_outcomes.csv'))


## === resources table
rzn <- c('hhc','ptc','rsatt','rratt')
rznl <- c('repn','intervention','PT regimen',rzn)

## total
rez1 <- IVS[,..rznl]
rez1 <- makehilo(rez1,cls = rzn)
rez1[,rtype:="total"]

## incremental
setnames(DIVS,old=paste0("D",rzn),new=rzn)
rez2 <- DIVS[,..rznl]
rez2 <- makehilo(rez2,cls = rzn)
rez2[,rtype:="incremental"]


## combine & relevel
keep <- c('rtype',rznl[-1])
rez <- rbind(rez1[,..keep],rez2[,..keep])

rez <- melt(rez,id=c("rtype","intervention","PT regimen"))
rez$rtype <- factor(rez$rtype,levels=c('total','incremental'))
rez$variable <- factor(rez$variable,levels=rzn)
rez$intervention <- factor(rez$intervention,
                           levels=c("No HHCM","HHCM, no PT",
                                    "PT to <5/HIV+/TST+","PT to <5/HIV+",
                                    "PT to <15"))
rez$`PT regimen` <- factor(rez$`PT regimen`,
                           levels=c("none","FQ","BDQ"))

## reshape
resources <- dcast(rez,rtype + variable ~
                        intervention + `PT regimen`)

## reorder cols
setcolorder(resources,resnames)

if(drx=="")
  fwrite(resources,file=here('output/table_resources.csv'))

## === health economics table

WRa <- IV[,.(deaths0=sum(deaths0),cost0=sum(cost0),lys0=sum(lys0)),
         by = .(repn,intervention,`PT regimen`,g_whoregion)]
WRc <- IV[,.(deaths=sum(deaths),cost=sum(cost),lys=sum(lys1)),
         by = .(repn,intervention,`PT regimen`,g_whoregion)]
WRb <- IVb[,.(deaths=sum(deaths),cost=sum(cost),lys=sum(lys1)),
          by = .(repn,intervention,`PT regimen`,g_whoregion)]

WRa[,table(`PT regimen`,intervention)]


WRc[`PT regimen`=="FQ",`PT regimen`:="Lfx"]

WR0 <- unique(WRa[,.(repn,deaths0,cost0,lys0,g_whoregion)])

WR <- rbind(WRc,WRb)
WR <- merge(WR,WR0,by=c('repn','g_whoregion'),all.x=TRUE)
WR[,c('Dcost','Dlys'):=.(cost-cost0,lys0-lys)]
WR[,Ddeaths:=deaths-deaths0]

WR[,table(`PT regimen`,intervention)]

## global numbers
Wa <- IV[,.(deaths0=sum(deaths0),cost0=sum(cost0),lys0=sum(lys0)),
         by = .(repn,intervention,`PT regimen`)]
Wc <- IV[,.(deaths=sum(deaths),cost=sum(cost),lys=sum(lys1)),
         by = .(repn,intervention,`PT regimen`)]
Wb <- IVb[,.(deaths=sum(deaths),cost=sum(cost),lys=sum(lys1)),
          by = .(repn,intervention,`PT regimen`)]

Wc[`PT regimen`=="FQ",`PT regimen`:="Lfx"]

unique(Wa[,.(repn,`PT regimen`,deaths0,cost0,lys0)])
W0 <- unique(Wa[,.(repn,deaths0,cost0,lys0)])

W <- rbind(Wc,Wb)
W <- merge(W,W0,by='repn',all.x=TRUE)
W[,c('Dcost','Dlys'):=.(cost-cost0,lys0-lys)]
W[,Ddeaths:=deaths-deaths0]


WM <- W[,.(Dcost=mean(Dcost),Dlys=mean(Dlys)),
        by=.(`PT regimen`,intervention)]
WM[,ICER:=Dcost/Dlys]

## rescale costs to millions
W[,c('cost','cost0','Dcost'):=.(cost/1e6,cost0/1e6,Dcost/1e6)]
W0[,cost0:=cost0/1e6]


## --- compute table from here:-----
## compute mean/hi/lo
qtys <- c('deaths','cost','lys',
          'Ddeaths','Dcost','Dlys')
Wo <- makehilo(W,qtys)

## add ICERS
Wo <- merge(Wo,
           W[,.(ICER=paste0(round(1e6*mean(Dcost)/mean(Dlys)))), #NOTE including 1 mln due to scaling above
             by=.(`PT regimen`,intervention)],
           by=c('PT regimen','intervention')
           )

## reformat
qtys <- c('PT regimen','intervention','ICER',qtys)
Wo <- melt(Wo[,..qtys],id=c('PT regimen','intervention'))
Wo$intervention <- factor(Wo$intervention,
                          levels=c("HHCM, no PT",
                                   "PT to <5/HIV+",
                                   "PT to <5/HIV+/TST+",
                                   "PT to <15"),
                          ordered = TRUE)
Wo$`PT regimen` <- factor(Wo$`PT regimen`,
                          levels=c("none","Lfx","MXF","DLM","BDQ"),
                          ordered = TRUE)

Wo <- dcast(Wo,variable~intervention+`PT regimen`,value.var = 'value')
setkey(Wo,variable)
Wo <- Wo[c("cost","deaths","lys","Dcost","Ddeaths","Dlys","ICER")]



tmp <- W0[,lapply(.SD,seehere),.SDcols=c('deaths0','cost0','lys0')]
tmp <- unlist(tmp)
tmp <- tmp[c('cost0','deaths0','lys0')]
tmp <- c(unname(tmp),rep(0,3),'-')

Wo[,none:=tmp]
nmz <- names(Wo)
setcolorder(Wo,c('variable','none',nmz[2:14]))

## Wo


fwrite(Wo,file=gh('output/table_HE{drx}.csv'))
## =====================================================

cat("==== TABLES done =======\n")

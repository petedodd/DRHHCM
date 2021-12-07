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

## =================================
## UTILITY FUNCTIONS
## =================================


## data utilities
see <- function(x,ns=3)formatC(signif(x,ns),big.mark = ",",format='fg') #for reading big numbers
lo <- function(x) quantile(x,.025)
hi <- function(x) quantile(x,.975)
tv <- function(x) tableHTML::tableHTML(x) #looking at tables
fmean <- function(x) mean(x[is.finite(x)]) #a mean for finite (no NA or Inf)


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
LY <- LYD[dr==3] #choosing discount rate
LY <- melt(LY[,.(iso3,`[0,5)`=u5,`[5,15)`=o5)],id='iso3')
LY <- LY[,.(iso3,acat=variable,lys=value)] #rename


## load all results
load(here('data/IV.Rdata'))
load(here('data/IVb.Rdata')) #version
load(here('data/IVT.Rdata'))
load(here('indata/FRF.Rdata'))
load(here('indata/GDP.Rdata'))
WK <- fread(here('indata/TB_notifications_2020-10-15.csv'))
WK <- unique(WK[,.(iso3,g_whoregion)])  #key!
load(here('indata/HBC.Rdata'))          #HBC lists
WB <- fread(here('indata/WBIL.csv'))    #income

## safety
(bad <- IV[!is.finite(cost),unique(iso3)])
(badb <- IVb[!is.finite(cost),unique(iso3)])
IV <- IV[!iso3 %in% bad]
IVb <- IVb[!iso3 %in% badb]


## =================================
## DATA WORK
## =================================

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
BMR[grep('BDQ',`PT regimen`),`PT regimen`:='BDQ/DLM']
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


## === data for FQR figures

## NNT vs prop RR that are FQR (by country)
IVC <- IV[,.(deaths=sum((deaths-deaths0)),inctb=sum((inctb-inctb0)),
             rsatt=sum((rsatt-rsatt0)),rratt=sum((rratt-rratt0)),
             ptc=sum((ptc-ptc0))),
          by = .(repn,iso3,intervention,`PT regimen`)]

ICM1 <- IVC[,.(ptc=mean(ptc/abs(inctb))),
            by=.(iso3,intervention,`PT regimen`)]
ICM2 <- IVC[,.(ptc=mean(ptc/abs(deaths))),
            by=.(iso3,intervention,`PT regimen`)]

## FQR data
ICM1 <- merge(ICM1,DX,by='iso3',all.x=TRUE)
ICM2 <- merge(ICM2,DX,by='iso3',all.x=TRUE)
## whoregion
ICM1 <- merge(ICM1,WK,by='iso3',all.x=TRUE)
ICM2 <- merge(ICM2,WK,by='iso3',all.x=TRUE)

## TODO fix
ICM1[!is.finite(ptc)]
ICM2[!is.finite(ptc)]


ICM1 <- ICM1[is.finite(ptc)]
ICM2 <- ICM2[is.finite(ptc)]

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
IVE[`PT regimen`=='FQ',`PT regimen`:='LVX']

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



## =================================
## FIGURES
## =================================
cat("==== doing FIGURES =======\n")

## === figure_NN

## TODO fine tune format

GP <- ggplot(BMR,aes(intervention,value,fill=`PT regimen`)) +
  geom_bar(stat='identity',position='dodge') +
  facet_grid(variable ~ quantity ,scales='free') +
  scale_fill_colorblind()+
  xlab('Intervention')+
  ylab('Additional resource (row) per quantity averted (column)') +
  pnlth

ggsave(GP,file=here('output/figure_NN.eps'),w=6,h=7)
ggsave(GP,file=here('output/figure_NN.png'),w=6,h=7)

## === figure_FQR
## checking countries etc
tmp <- ICM1[`PT regimen`!='INH' & intervention=='PT to <15']
tmp[,lbl:=as.character(iso3)]
tmp[`PT regimen`!='FQ',lbl:=NA]
tmp[,region:=g_whoregion]
tmp <- merge(tmp,WB[,.(iso3,hic=(income=='High income'))],by='iso3')
tmp <- tmp[hic!=TRUE] #strip out high income countries

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
  scale_color_colorblind(position='top')+ #not sure why not working
  xlab('Proportion of RR/MDR in children that is FQR')+
  ylab('PT courses to prevent a TB case')+
  geom_smooth(method='lm')+
  expand_limits(y=0) +
  ggpubr::stat_cor(show.legend = FALSE)+
  theme_classic() + ggpubr::grids()
## GPa

## -- panel b

## reshape & aggregate
tmp <- dcast(tmp,iso3+g_whoregion ~ `PT regimen`,value.var = 'ptc')
tmp[,df:=FQ-BDQ]
tmp <- tmp[,.(df=median(df,na.rm=TRUE)),by=.(g_whoregion)]
tmp <- tmp[order(as.character(g_whoregion))]
tmp[,region:=g_whoregion]
tmp$region <- factor(tmp$region,levels=unique(tmp$region),
                      ordered=TRUE)

## plot
GPb <- ggplot(tmp,aes(region,df)) +
    geom_bar(stat='identity') +
    xlab('Region') +
    ylab('Median difference in PT courses per case prevented')+
    theme_classic() + ggpubr::grids()
## GPb

## combine
GPB <- ggarrange(GPa,GPb,nrow=1,labels=c('A','B'))

ggsave(GPB,file=here('output/figure_FQR.eps'),w=12,h=5)
ggsave(GPB,file=here('output/figure_FQR.png'),w=12,h=5)


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
CECR$`PT regimen` <- factor(CECR$`PT regimen`,
                            levels=rev(c('DLM','LVX','none')))

colz <- c('DLM'="#56B4E9",'LVX'="#E69F00",'none'="#000000")


## make plot
tp <- 5e3
GP <- ggplot(CECR,
             aes(iso3,cpda,
                 shape=intervention,
                 group=iso3,
                 col=`PT regimen`))+
  geom_line(data=tmp,aes(x=iso3,y=gdp/1,group=1),col='darkgrey')+
  geom_line(data=tmp,aes(x=iso3,y=gdp/2,group=1),lty=2,col='darkgrey')+
  geom_point(size=3,lwd=4) +
  scale_shape(solid=FALSE)+
  scale_color_manual(values=colz)+
  annotate(geom='text',y=tp*0.8,x=19,label='1.0xGDP')+
  annotate(geom='text',y=tp*0.8,x=25,label='0.5xGDP')+
  coord_flip()+
  scale_y_continuous(label=absspace,limits=c(0,tp))+
  xlab('Country ISO3 code')+
  ylab('Incremental cost-effectiveness ratio\n(USD per 3%-discounted DALY averted)')+
  theme_classic()+ggpubr::grids()+
  theme(legend.position=c(0.8,0.2))

ggsave(GP,file=here('output/figure_CE.eps'),w=10,h=10)
ggsave(GP,file=here('output/figure_CE.jpg'),w=10,h=10)


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

ggsave(GP,file=here('output/Sfigure_CEAC.png'),w=12,h=12)


## --- supplementary version with all countries
tmp <- unique(CEC[!is.na(gdp),.(gdp,iso3)])
lvls <- tmp[order(gdp),iso3]
tmp[,c('intervention','PT regimen'):=NA]

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
  scale_y_continuous(label=absspace,limits=c(0,tp))+
  xlab('Country ISO3 code')+
  ylab('Cost per 3%-discounted DALY averted (USD)')+
  theme_classic()+ggpubr::grids()+
  theme(legend.position=c(0.8,0.2))

ggsave(GP,file=here('output/Sfigure_CE.png'),w=20,h=25)


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
             hhc=sum(hhc),
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

fwrite(resources,file=here('output/table_resources.csv'))

## === health economics table

WRa <- IV[,.(deaths0=sum(deaths0),cost0=sum(cost0),lys0=sum(lys0)),
         by = .(repn,intervention,`PT regimen`,g_whoregion)]
WRc <- IV[,.(deaths=sum(deaths),cost=sum(cost),lys=sum(lys1)),
         by = .(repn,intervention,`PT regimen`,g_whoregion)]
WRb <- IVb[,.(deaths=sum(deaths),cost=sum(cost),lys=sum(lys1)),
          by = .(repn,intervention,`PT regimen`,g_whoregion)]

WRa[,table(`PT regimen`,intervention)]


WRc[`PT regimen`=="FQ",`PT regimen`:="LVX"]

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

Wc[`PT regimen`=="FQ",`PT regimen`:="LVX"]

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
                          levels=c("none","LVX","MXF","DLM","BDQ"),
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


fwrite(Wo,file=here('output/table_HE.csv'))
## =====================================================

cat("==== TABLES done =======\n")

## =======================================
## LOADING TO GOOGLE SHEETS (authors only)
## =======================================

if(FALSE){
  library(googlesheets4)

  ## setup - only accessible to those with access to this sheet
  yourl <- "https://docs.google.com/spreadsheets/d/1leFkszUMRhR7i2iLgEWbnO1bbHlCq1mE8ikYYMaVG24/edit#gid=0"
  shid <- as.character(as_sheets_id(yourl))

  ## utility function
  upload.to.sheets <- function(filename,sheetid){
    fn <- glue(here('output/{filename}'))
    tmp <- fread(file=fn)
    write_sheet(tmp,sheetid,sheet=gsub("\\.csv","",filename))
  }

  ## read & upload relevant data
  upload.to.sheets("table_resources.csv",shid)
  upload.to.sheets("table_outcomes.csv",shid)
  upload.to.sheets("table_HE.csv",shid)

}


## temporary below here to be reworked into Appendix results:
## RR split to outcomes
## all country plot
## RS analog




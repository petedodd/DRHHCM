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


## =================================
## UTILITY FUNCTIONS
## =================================


## data utilities
see <- function(x,ns=3)formatC(signif(x,ns),big.mark = ",",format='fg') #for reading big numbers
lo <- function(x) quantile(x,.025)
hi <- function(x) quantile(x,.975)
tv <- function(x) tableHTML::tableHTML(x) #looking at tables
fmean <- function(x) mean(x[is.finite(x)]) #a mean for finite (no NA or Inf)

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

## =================================
## DATA WORK
## =================================

## === rename FQR
DX <- FRF[,.(prop=mean(fqr)),by=iso3]

## === data for NNS plot
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
BM

BM[variable=='ptc' & `PT regimen`=='FQ'] #right pattern


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

BMR <- BMR[!is.na(intervention)]        #TODO check


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

ICM1 <- merge(ICM1,DX,by='iso3',all.x=TRUE)
ICM2 <- merge(ICM2,DX,by='iso3',all.x=TRUE)

## fill in NAs by region TODO check
ICM1 <- merge(ICM1,WK,by='iso3',all.x=TRUE)
ICM2 <- merge(ICM2,WK,by='iso3',all.x=TRUE)

ICM1[,c('ptc.av','prop.av'):=.(fmean(ptc),fmean(prop)),by=.(g_whoregion,intervention,`PT regimen`)]
ICM2[,c('ptc.av','prop.av'):=.(fmean(ptc),fmean(prop)),by=.(g_whoregion,intervention,`PT regimen`)]

ICM1[!is.finite(ptc),ptc:=ptc.av]; ICM2[!is.finite(ptc),ptc:=ptc.av]
ICM1[!is.finite(prop),prop:=prop.av]; ICM2[!is.finite(prop),prop:=prop.av];

ICM1[,c('ptc.av','prop.av'):=NULL]
ICM2[,c('ptc.av','prop.av'):=NULL]



## === compile CE results
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

## TODO include no PT strategy
## regional global numbers
IV <- merge(IV,WK,by='iso3',all.x = TRUE)
IVb <- merge(IVb,WK,by='iso3',all.x = TRUE)

## by country
IVc <- IV[,.(deaths=sum(deaths),cost=sum(cost),lys=sum(lys1)),
          by = .(repn,iso3,intervention,`PT regimen`)]
IVc <- IVc[,.(deaths=mean(deaths),cost=mean(cost),lys=mean(lys)),
           by = .(iso3,intervention,`PT regimen`)]
IVc0 <- IV[,.(deaths=sum(deaths0),cost=sum(cost0),lys=sum(lys0)),
           by = .(repn,iso3,intervention,`PT regimen`)]
IVc0 <- IVc0[,.(deaths0=mean(deaths),cost0=mean(cost),lys0=mean(lys)),
             by = .(iso3,intervention,`PT regimen`)]

## data for CE plot
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

## === figure_NN

## TODO check regimen
## TODO fine format

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

## -- panel a

## checking countries etc
tmp <- ICM1[`PT regimen`!='INH' & intervention=='PT to <15']
tmp[,lbl:=as.character(iso3)]
tmp[`PT regimen`!='FQ',lbl:=NA]
tmp[,region:=g_whoregion]

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

## TODO swtich to LVX, DLM?

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
                            levels=rev(c('BDQ','FQ','none')))

colz <- c('BDQ'="#56B4E9",'FQ'="#E69F00",'none'="#000000")
## c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## make plot
tp <- 5e3
GP <- ggplot(CECR,
             aes(iso3,cpda,
                 shape=intervention,
                 group=iso3,
                 col=`PT regimen`))+
  geom_line(data=tmp,aes(x=iso3,y=gdp/1,group=1),col='darkgrey')+
  geom_line(data=tmp,aes(x=iso3,y=gdp/2,group=1),lty=2,col='darkgrey')+
  geom_point(size=3,lwd=3) +
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

## =================================
## TABLES
## =================================


## === table_resources

## === table_outcomes

## === table_HE




## ==================== TO PRUNE: ======================



## main table
## names(IV)[which(names(IV)=='screened')] <- 'value'
## IV[,unique(intervention)]

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
WR[,c('Dcost','Dlys'):=.(cost-cost0,lys-lys0)]
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
W[,c('Dcost','Dlys'):=.(cost-cost0,lys-lys0)]
W[,Ddeaths:=deaths-deaths0]


WM <- W[,.(Dcost=mean(Dcost),Dlys=mean(Dlys)),
        by=.(`PT regimen`,intervention)]
WM[,ICER:=-Dcost/Dlys]


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

## compute mean/hi/lo
qtys <- c('deaths','cost','lys',
          'Ddeaths','Dcost','Dlys')
Wo <- makehilo(W,qtys)

## add ICERS
Wo <- merge(Wo,
           W[,.(ICER=paste0(round(-mean(Dcost)/mean(Dlys)))),
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


seehere <- function(x) paste0(see(mean(x)),
                              " (",see(lo(x))," - ",
                              see(hi(x)),")")

tmp <- W0[,lapply(.SD,seehere),.SDcols=c('deaths0','cost0','lys0')]
tmp <- unlist(tmp)
tmp <- tmp[c('cost0','deaths0','lys0')]
tmp <- c(unname(tmp),rep(0,3),'-')

Wo[,none:=tmp]
nmz <- names(Wo)
setcolorder(Wo,c('variable','none',nmz[2:14]))

Wo
fwrite(Wo,file=here('output/TABLE4.csv'))

## jj

WM2 <- W[,.(Dcost=mean(Dcost),Dlys=mean(Dlys),
            Dcost.lo=lo(Dcost),Dlys.lo=lo(Dlys),
            Dcost.hi=hi(Dcost),Dlys.hi=hi(Dlys)),
        by=.(`PT regimen`,intervention)]


WMR2 <- WR[,.(Dcost=mean(Dcost),Dlys=mean(Dlys),
            Dcost.lo=lo(Dcost),Dlys.lo=lo(Dlys),
            Dcost.hi=hi(Dcost),Dlys.hi=hi(Dlys)),
           by=.(`PT regimen`,intervention,g_whoregion)]




TIVS <- IVT[,.(deaths=sum(deaths),lys=sum(lys1),
               incdeaths=sum(incdeaths),
               prevdeaths=sum(prevdeaths),
               inctb=sum(inctb),
               rsatt=sum(rsatt),rratt=sum(rratt),
               ptc=sum(ptc),hhc=sum(hhc),cost=sum(cost)),
            by = .(repn,intervention,`PT regimen`)]
TIVS0 <- IVT[,.(deaths=sum(deaths0),lys=sum(lys0),
                incdeaths=sum(incdeaths0),
                prevdeaths=sum(prevdeaths0),
                inctb=sum(inctb0),
                rsatt=sum(rsatt0),rratt=sum(rratt0),
                rsatti=sum(rsatti0),rratti=sum(rratti0),
                ptc=sum(ptc0),hhc=sum(hhc),cost=sum(cost0)),
             by = .(repn,intervention,`PT regimen`)]

IVS <- IV[,.(deaths=sum(deaths),
             lys=sum(lys1),
             incdeaths=sum(incdeaths),
             prevdeaths=sum(prevdeaths),
             inctb=sum(inctb),
             rsatt=sum(rsatt),rratt=sum(rratt),
             ptc=sum(ptc),hhc=sum(hhc),cost=sum(cost)),
          by = .(repn,intervention,`PT regimen`)]

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


IVS[,unique(intervention)]
IVS[,table(intervention)]
rrIVS[,table(intervention)] #
rsIVS[,table(intervention)]
IV[,table(DST,intervention)]
IV[,unique(intervention)]

timid <- TIVS[,.(deaths=mean(deaths),
                 lys=mean(lys),
                 incdeaths=mean(incdeaths),
                 prevdeaths=mean(prevdeaths),
                 inctb=mean(inctb),
                 rsatt=mean(rsatt),rratt=mean(rratt),
                 ptc=mean(ptc),hhc=mean(hhc),cost=mean(cost)),
              by=.(intervention,`PT regimen`)]
timid0 <- TIVS0[,.(deaths=mean(deaths),
                   lys=mean(lys),
                   incdeaths=mean(incdeaths),
                   prevdeaths=mean(prevdeaths),
                   inctb=mean(inctb),
                   rsatt=mean(rsatt),rratt=mean(rratt),
                   rsatti=mean(rsatti),rratti=mean(rratti),
                   ptc=mean(ptc),hhc=mean(hhc),cost=mean(cost)),
                by=.(intervention,`PT regimen`)]
mid <- IVS[,.(deaths=mean(deaths),
              lys=mean(lys),
              incdeaths=mean(incdeaths),
              prevdeaths=mean(prevdeaths),
              inctb=mean(inctb),
              rsatt=mean(rsatt),rratt=mean(rratt),
              ptc=mean(ptc),hhc=mean(hhc),cost=mean(cost)),
          by=.(intervention,`PT regimen`)]
L <- IVS[,.(deaths=lo(deaths),
            lys=lo(lys),
            incdeaths=lo(incdeaths),
            prevdeaths=lo(prevdeaths),
            inctb=lo(inctb),
            rsatt=lo(rsatt),rratt=lo(rratt),
            ptc=lo(ptc),hhc=lo(hhc),cost=lo(cost)),
          by=.(intervention,`PT regimen`)]
HI <- IVS[,.(deaths=hi(deaths),
             lys=hi(lys),
             incdeaths=hi(incdeaths),
             prevdeaths=hi(prevdeaths),
             inctb=hi(inctb),
            rsatt=hi(rsatt),rratt=hi(rratt),
            ptc=hi(ptc),hhc=hi(hhc),cost=hi(cost)),
          by=.(intervention,`PT regimen`)]

## RS & RR -- missing no intervention
rrmid <- rrIVS[,.(deaths=mean(deaths),
                  lys=mean(lys),
                  incdeaths=mean(incdeaths),
              prevdeaths=mean(prevdeaths),
              inctb=mean(inctb)),
           by=.(intervention,`PT regimen`)]
rrL <- rrIVS[,.(deaths=lo(deaths),
                lys=lo(lys),
                incdeaths=lo(incdeaths),
            prevdeaths=lo(prevdeaths),
            inctb=lo(inctb)),
         by=.(intervention,`PT regimen`)]
rrHI <- rrIVS[,.(deaths=hi(deaths),
                 lys=hi(lys),
                 incdeaths=hi(incdeaths),
             prevdeaths=hi(prevdeaths),
             inctb=hi(inctb)),
          by=.(intervention,`PT regimen`)]
rsmid <- rsIVS[,.(deaths=mean(deaths),
                  lys=mean(lys),
                  incdeaths=mean(incdeaths),
                  prevdeaths=mean(prevdeaths),
                  inctb=mean(inctb)),
               by=.(intervention,`PT regimen`)]
rsL <- rsIVS[,.(deaths=lo(deaths),
                lys=lo(lys),
                incdeaths=lo(incdeaths),
                prevdeaths=lo(prevdeaths),
                inctb=lo(inctb)),
             by=.(intervention,`PT regimen`)]
rsHI <- rsIVS[,.(deaths=hi(deaths),
                 lys=hi(lys),
                 incdeaths=hi(incdeaths),
                 prevdeaths=hi(prevdeaths),
                 inctb=hi(inctb)),
              by=.(intervention,`PT regimen`)]


names(mid)


## formatting
X <- as.matrix(mid[,lapply(.SD,see),.SDcols=3:ncol(mid)])
Y <- as.matrix(L[,lapply(.SD,see),.SDcols=3:ncol(L)])
Z <- as.matrix(HI[,lapply(.SD,see),.SDcols=3:ncol(HI)])
XYZ <- paste0(X,' (',Y,' - ',Z,')')
XYZ <- matrix(XYZ,ncol=ncol(X),nrow=nrow(X))
colnames(XYZ) <- colnames(X)

A <- cbind(mid[,.(intervention,`PT regimen`)],as.data.table(XYZ))
A
A$intervention <- factor(A$intervention,
                         levels=c('No HHCM',
                                  'HHCM, no PT',
                                  'PT to <5/HIV+',
                                  'PT to <5/HIV+/TST+',
                                  'PT to <15'),
                         ordered=TRUE)
A$`PT regimen` <- factor(A$`PT regimen`,
                         levels=c('none','FQ','BDQ'),
                         ordered=TRUE)

fwrite(A,file=here('output/A.csv'))

## RR & RS
rrX <- as.matrix(rrmid[,lapply(.SD,see),.SDcols=3:ncol(rrmid)])
rrY <- as.matrix(rrL[,lapply(.SD,see),.SDcols=3:ncol(rrL)])
rrZ <- as.matrix(rrHI[,lapply(.SD,see),.SDcols=3:ncol(rrHI)])
rrXYZ <- paste0(rrX,' (',rrY,' - ',rrZ,')')
rrXYZ <- matrix(rrXYZ,ncol=ncol(rrX),nrow=nrow(rrX))
colnames(rrXYZ) <- paste0('rr',colnames(rrX))
rrA <- cbind(rrmid[,.(intervention,`PT regimen`)],
             as.data.table(rrXYZ))
rsX <- as.matrix(rsmid[,lapply(.SD,see),.SDcols=3:ncol(rsmid)])
rsY <- as.matrix(rsL[,lapply(.SD,see),.SDcols=3:ncol(rsL)])
rsZ <- as.matrix(rsHI[,lapply(.SD,see),.SDcols=3:ncol(rsHI)])
rsXYZ <- paste0(rsX,' (',rsY,' - ',rsZ,')')
rsXYZ <- matrix(rsXYZ,ncol=ncol(rsX),nrow=nrow(rsX))
colnames(rsXYZ) <- paste0('rs',colnames(rsX))
rsA <- cbind(rsmid[,.(intervention,`PT regimen`)],
             as.data.table(rsXYZ))

rsAM <- melt(rsA,id=c('intervention','PT regimen'))
rrAM <- melt(rrA,id=c('intervention','PT regimen'))

AM <- melt(A,id=c('intervention','PT regimen'))
AM <- rbindlist(list(AM,rsAM,rrAM))
AM[,unique(variable)]

## TODO need prevalence variables from above
keep <- c(
  'hhc',
  'rsatt',
  'rratt',
  'ptc',
  'rsinctb',
  'rrinctb',
  'inctb',
  'rrprevtb', #TODO
  'rrprevtb', #TODO
  'prevtb',   #TODO
  'incdeaths',
  'prevdeaths',
  'deaths',
  'lys',
  'cost')

AM <- dcast(AM[variable %in% keep],
            variable ~ intervention + `PT regimen`,value='value')
AM['hhc']$`No HHCM_none` <- '0 (0 - 0)'

setkey(AM,variable)
AM <- AM[c('RESOURCE',keep[1:4],
           'OUTCOMES',keep[5:length(keep)])]
## AM[,c('No HHCM_FQ','No HHCM_BDQ'):=NULL]
## names(AM)[grepl('No',names(AM))] <- 'No HHCM'

fwrite(AM,file=here('output/TABLE2.csv'))

## NOTE not including AEs

## --- figure 2c ---
tmp <- copy(mid)
dvd <- c(names(tmp)[3:(ncol(tmp))])
tmp[,c(dvd):=lapply(.SD,function(x)1e2*x/hhc[1]),.SDcols=dvd]
midm <- melt(tmp,id=c('intervention','PT regimen'))

tmp <- midm[variable=='inctb']
tmp[,variable:='prevtb'] #TODO until prev included
midm <- rbind(midm,tmp)

unique(midm$variable)

lls <- c('deaths', 'incdeaths','prevdeaths',
         'prevtb','inctb','rsatt','rratt','ptc','hhc' )
midm$variable <- factor(midm$variable,levels=rev(lls),ordered = TRUE)
intz <- c('No HHCM','HHCM, no PT','PT to <5/HIV+/TST+','PT to <5/HIV+','PT to <15')

midm$intervention <- factor(midm$intervention,levels=(intz),ordered = TRUE)

tosee <- midm[,unique(variable)]
tosee <- tosee[!tosee %in% c('hhc','rsatt','rratt')]

ggplot(midm[variable %in% tosee],
       aes(intervention,value,fill=`PT regimen`))+
    geom_bar(stat='identity',position = 'dodge')+
    facet_wrap(~variable,scales='free')+
    scale_fill_colorblind() +
    theme_classic()+
    ## ## scale_y_sqrt()+
    ggpubr::grids() + rot45

ggsave(file=here('output/FIGURE2x.png'),w=10,h=7)
ggsave(file=here('output/FIGURE2x.pdf'),w=10,h=7)

## RS test
## Figure 1
IVDT <- IVT[,.(deaths=sum((deaths-deaths0)),
             incdeaths=sum((incdeaths-incdeaths0)),
             inctb=sum((inctb-inctb0)),
             rsatt=sum((rsatt-rsatt0)),rratt=sum((rratt-rratt0)),
             ptc=sum((ptc-ptc0)),hhc=sum(hhc)),
          by = .(repn,intervention,`PT regimen`)]
midit <- IVDT[,.(rsatt=mean(rsatt/abs(inctb)),
               rratt=mean(rratt/abs(inctb)),
               ptc=mean(ptc/abs(inctb)),hhc=mean(hhc/abs(inctb))),
            by=.(intervention,`PT regimen`)]
middt <- IVDT[,.(rsatt=mean(rsatt/abs(deaths)),
               rratt=mean(rratt/abs(deaths)),
               ptc=mean(ptc/abs(deaths)),hhc=mean(hhc/abs(deaths))),
            by=.(intervention,`PT regimen`)]
midit[,quantity:='per TB case averted']
middt[,quantity:='per TB death averted']
BT <- rbind(midit,middt)
BMT <- melt(BT,id=c('intervention','PT regimen','quantity'))
BMT[quantity=='per TB case averted']
BMT[quantity=='per TB death averted']

fwrite(BMT,file=here('output/test.csv'))


GP <- ggplot(BMT,aes(intervention,value,fill=`PT regimen`)) +
  geom_bar(stat='identity',position='dodge') +
  facet_grid(quantity ~ variable,scales='free') +
  scale_fill_colorblind()+
  ylab('number') +
  theme_classic() + ggpubr::grids() +
  theme(axis.text.x = element_text(angle=45,hjust=1))
GP

ggsave(GP,file=here('output/test.pdf'),w=8,h=6)


BMT[variable=='ptc'] #paper: ~ 50 ptc/c or d (higher for c)
BMT[variable=='hhc'] #paper: ~ 100 hhc /c or d (higher for c)


## NOTE some differences here
## comparison appendix table
## coprevalence is Shah
## LTBI is Fox as before
## progression Leo now
timid0[,.(fracdinc=1e2*incdeaths/deaths, #36/133 ~ 27%
          inCFR=1e2*incdeaths/inctb, #36/257 ~ 14%
          inCDR=1e2*rsatti/inctb,
          hhinc=1e2*inctb/hhc, #257/8e3 ~ 3%
          hhdeaths=1e2*deaths/hhc)] #133/8e3 ~ 2%



## Figure 1, but regional
regs <- fread(here('indata/TB_burden_countries_2020-10-15.csv'))
regs <- unique(regs[,.(iso3,g_whoregion)])
IVT <- merge(IVT,regs,by='iso3')
IVDTR <- IVT[,.(deaths=sum((deaths-deaths0)),
             incdeaths=sum((incdeaths-incdeaths0)),
             inctb=sum((inctb-inctb0)),
             rsatt=sum((rsatt-rsatt0)),rratt=sum((rratt-rratt0)),
             ptc=sum((ptc-ptc0)),hhc=sum(hhc)),
             by = .(repn,intervention,`PT regimen`,g_whoregion)]
miditr <- IVDTR[,.(rsatt=mean(rsatt/abs(inctb)),
               rratt=mean(rratt/abs(inctb)),
               ptc=mean(ptc/abs(inctb)),hhc=mean(hhc/abs(inctb))),
              by=.(intervention,`PT regimen`,g_whoregion)]
middtr <- IVDTR[,.(rsatt=mean(rsatt/abs(deaths)),
                   rratt=mean(rratt/abs(deaths)),
                   ptc=mean(ptc/abs(deaths)),hhc=mean(hhc/abs(deaths))),
                by=.(intervention,`PT regimen`,g_whoregion)]

miditr[,quantity:='per TB case averted']
middtr[,quantity:='per TB death averted']

BTR <- rbind(miditr,middtr)
BMTR <- melt(BTR,id=c('intervention','PT regimen',
                      'quantity','g_whoregion'))

## Shah: around 8%
## 7.8 (5.6-10.0)
## getLNparms(7.8, ((5.6-10.0)/3.92)^2)
## getAB(7.8/1e2, ((5.6-10.0)/392)^2)
## Fox: nearer 10% weighted towards LIC
## PSA[,mean(coprev)] #4% TODO which assumption? 739/8e3 ~ 10%
## PSA[,mean(ltbi.prev)] #44%


## ============
## ============
## graphs

## ====== new  expts
## Figure 1 new
midi2 <- IVD[,.(ptc.i=mean(ptc/abs(inctb)),
                hhc.i=mean(hhc/abs(inctb))),
            by=.(intervention,`PT regimen`)]
midd2 <- IVD[,.(ptc.td=mean(ptc/abs(deaths)),
                hhc.td=mean(hhc/abs(deaths)),
                ptc.id=mean(ptc/abs(incdeaths)),
                hhc.id=mean(hhc/abs(incdeaths)),
                ptc.pd=mean(ptc/abs(deaths-incdeaths)),
                hhc.pd=mean(hhc/abs(deaths-incdeaths))
                ),
            by=.(intervention,`PT regimen`)]


B1 <- melt(midi2,id=c('intervention','PT regimen'))
B2 <- melt(midd2,id=c('intervention','PT regimen'))
BM <- rbind(B1,B2)
BM[,c('qty','outcome'):=tstrsplit(variable,split="\\.")]


GP <- ggplot(BM,aes(intervention,value,fill=`PT regimen`)) +
    geom_bar(stat='identity',position='dodge') +
    facet_grid(qty ~ outcome ,scales='free') +
    scale_fill_colorblind()+
    scale_y_sqrt(label=comma)+
    ylab('Number extra') +
    theme_classic() + ggpubr::grids() +
    theme(axis.text.x = element_text(angle=45,hjust=1))
GP

## NOTE not sure about pd x ptc
ggsave(GP,file=here('output/Figure1x1.pdf'),w=6,h=12)

midi2 <- IVD[,.(ptc.i=mean(abs(inctb)/ptc),
                hhc.i=mean(abs(inctb)/hhc)),
            by=.(intervention,`PT regimen`)]
midd2 <- IVD[,.(ptc.td=mean(abs(deaths)/ptc),
                hhc.td=mean(abs(deaths)/hhc),
                ptc.id=mean(abs(incdeaths)/ptc),
                hhc.id=mean(abs(incdeaths)/hhc),
                ptc.pd=mean(abs(deaths-incdeaths)/ptc),
                hhc.pd=mean(abs(deaths-incdeaths)/hhc)
                ),
            by=.(intervention,`PT regimen`)]


B1 <- melt(midi2,id=c('intervention','PT regimen'))
B2 <- melt(midd2,id=c('intervention','PT regimen'))
BM <- rbind(B1,B2)
BM[,c('qty','outcome'):=tstrsplit(variable,split="\\.")]


## checking countries etc
tmp <- ICM1[`PT regimen`!='INH' & intervention=='PT to <15']
tmp <- merge(tmp,WK,by='iso3')
tmp[,lbl:=as.character(iso3)]
tmp[`PT regimen`!='FQ',lbl:=NA]


## =========================================
## additional regional incremental results


## make all incremental
IV[,deaths:=(deaths-deaths0)]
IV[,incdeaths:=(incdeaths-incdeaths0)]
IV[,inctb:=(inctb-inctb0)]
IV[,rsatt:=(rsatt-rsatt0)]
IV[,rratt:=(rratt-rratt0)]
IV[,ptc:=(ptc-ptc0)]
IV[,c('deaths0','incdeaths0','inctb0','rsatt0','rratt0','ptc0'):=NULL]


## aggregate
hi <- function(x,qn=.975)quantile(x,prob=qn)
lo <- function(x,qn=.025)quantile(x,prob=qn)
K <- fread(here('indata/TB_burden_countries_2020-10-15.csv'))
K <- unique(K[,.(iso3,g_whoregion)])

IV <- merge(IV,K,by='iso3',all.x=TRUE,all.y=FALSE)

IV[iso3=='ZAF' & repn==1 & acat=='[0,5)' & intervention=='PT to <15']

## Global
Global <- IV[,.(deaths=sum(deaths),incdeaths=sum(incdeaths),inctb=sum(inctb),rsatt=sum(rsatt),
                rratt=sum(rratt),ptc=sum(ptc),hhc=sum(hhc)),by=.(repn,intervention,`PT regimen`)]
Global.m <- Global[,.(deaths=mean(deaths),incdeaths=mean(incdeaths),inctb=mean(inctb),rsatt=mean(rsatt),
                      rratt=mean(rratt),ptc=mean(ptc),hhc=mean(hhc)),by=.(intervention,`PT regimen`)]
Global.h <- Global[,.(deaths=hi(deaths),incdeaths=hi(incdeaths),inctb=hi(inctb),rsatt=hi(rsatt),
                      rratt=hi(rratt),ptc=hi(ptc),hhc=hi(hhc)),by=.(intervention,`PT regimen`)]
Global.l <- Global[,.(deaths=lo(deaths),incdeaths=lo(incdeaths),inctb=lo(inctb),rsatt=lo(rsatt),
                      rratt=lo(rratt),ptc=lo(ptc),hhc=lo(hhc)),by=.(intervention,`PT regimen`)]
Global.m[,type:='mid']; Global.l[,type:='lo']; Global.h[,type:='hi'];
Global <- rbind(Global.m,Global.l,Global.h)
Global[,Region:='Global']

## this file includes the analysis to generate the FQR in RR estimates
library(here)
library(data.table)


## this is in two parts
## part 1 generaters a nearest neighbour structure
## part 2 does the resampling to generate the FQR in RR estimates
## part 1 is included for reproducibility really
## the nearest neighbour structure is included in repo/indata so part 2 can be run without running part 1
runpart1 <- FALSE #run part 1 too?
load(here('indata/isoidx.Rdata')) #country list

if(runpart1){

  ## === part 1: generate spatial structure  ===
  library(tmap)
  library(rworldmap)
  library(spdep)


  ## Checking the countries and constructing adjacencies
  ## Load spatial data and compare with covariates
  data(countriesCoarse)                   #load rworldmap data
  names(countriesCoarse@data)[30] <- 'iso3' #rename


  ## Remove Antarctica and Greenland (ATA, GRL)
  WTB <- countriesCoarse
  WTB <- WTB[which(!(WTB@data$iso3 %in% c('ATA','GRL'))),]
  WTB <- WTB[which(WTB@data$iso3 %in% isoidx),] #drops BES (Bonnaire) and TKL (Tokelau)
  dim(WTB)


  ## Start making adjacencies
  W.nb <- poly2nb(WTB,row.names=as.character(WTB@data$iso3))
  W.list <- nb2listw( W.nb,style='B',zero.policy = TRUE)

  ## look at
  plot(W.nb,coordinates(WTB),add=TRUE,col=2)


  ## Generate naive nearest neighbout maps
  cs_kn5 <- knn2nb(knearneigh(coordinates(WTB),k=5))


  ## look at the $k=5$ version
  plot(WTB,col='gray');text(coordinates(WTB),labels=WTB$iso3);plot(cs_kn5,coordinates(WTB),add=TRUE,col=2);


  ## Modifying the nearest neighbour linkages
  ## 
  ## The naive maps have some features that are undesirable. In particular;
  ## 
  ## * they are not connected, which can hamper mixing
  ## * Russia is not connected to former Soviet republics
  ##
  ## To address these issues, we introduce transatlantic links based on flight paths carrying more than 900K per year. We also join up the South Pacific islands WLF and TON to FJI. Finally, we join RUS to EST, LVA, UKR, GEO and AZE, which makes sense both in geographical and political terms.
  ##
  ## This function is to carry out these changes and to save the data and plots out.

  appendKNN <- function(X,savename){
    ## connect up RUS to FSRs & consider making connected
    toadd <- c('EST','LVA','UKR','GEO','AZE')
    toadd <- which(WTB$iso3 %in% toadd)
    indiso <- which(WTB$iso3=='RUS')
    X[[indiso]] <- c(X[[indiso]],toadd)
    ## transatlantic links see: https://en.wikipedia.org/wiki/Transatlantic_flight#Busiest_transatlantic_routes
    ## those with >900K 
    ## transatlantic links UK, FRA to US 
    toadd <- c('GBR','FRA')
    toadd <- which(WTB$iso3 %in% toadd)
    indiso <- which(WTB$iso3=='USA')
    X[[indiso]] <- c(X[[indiso]],toadd)
    ## UK to CAN
    toadd <- which(WTB$iso3 == 'GBR')
    indiso <- which(WTB$iso3=='CAN')
    X[[indiso]] <- c(X[[indiso]],toadd)
    ## US to ETH
    toadd <- which(WTB$iso3 == 'ETH')
    indiso <- which(WTB$iso3=='USA')
    X[[indiso]] <- c(X[[indiso]],toadd)
    ## BRA to AGO
    toadd <- which(WTB$iso3 == 'BRA')
    indiso <- which(WTB$iso3=='AGO')
    X[[indiso]] <- c(X[[indiso]],toadd)
    ## plot
    pdf(paste0(savename,'.pdf'),w=18,h=12)
    plot(WTB,col='gray')
    plot(X, coordinates(WTB),add=TRUE,col=2)
    text(coordinates(WTB),labels=WTB$iso3)
    text(x=0,y=-60,labels='Not shown: WLF & TON connected to FJI')
    dev.off()
    ## transatlantic links WLF, TON to FJI (don't map)
    toadd <- c('WLF','TON')
    toadd <- which(WTB$iso3 %in% toadd)
    indiso <- which(WTB$iso3=='FJI')
    X[[indiso]] <- c(X[[indiso]],toadd)
    save(X,file = paste0(savename,'.Rdata'))
    X
  }


  ## apply this function:
  acs_kn5 <- appendKNN(cs_kn5,here('data/acs_kn5'))

  ## copying to indata so that the above doesn't need to be run
  file.copy(here('data/acs_kn5.Rdata'),here('indata/acs_kn5.Rdata'))

} #end of part 1

## === part 2: use spatial structure for generating a sample of FQR in RR ===
## load 5 nearest country structure (generated above)
load(here('indata/acs_kn5.Rdata'))

## read in WHO DS data
RS <- fread(here('indata/TB_dr_surveillance_2020-10-15.csv'))


## variables of interest
nmz <- c('iso3','g_whoregion','year',
         'rr_dst_rlt','rr_dr_fq','rr_fqr')


## rr_dr_fq
## rr_dst_rlt_fq
## rr_fqr
##  = 
## Number of new or previously treated bacteriologically confirmed pulmonary TB patients with resistance to rifampicin and resistance to fluoroquinolones
## Number of new or previously treated bacteriologically confirmed pulmonary TB patients with resistance to rifampicin and with test results for any fluoroquinolone
## Number of new or previously treated bacteriologically confirmed pulmonary TB patients with resistance to rifampicin and resistance to fluoroquinolones

## restrict and aggregate
RS <- RS[,..nmz] #restrict
RS <- RS[,.(rr_N=sum(rr_dst_rlt,na.rm=TRUE),
            f_n=sum(rr_dr_fq,na.rm=TRUE)),
         by=.(iso3,g_whoregion)] #aggregate over years
RSR <- RS[,.(rr_N=sum(rr_N),f_n=sum(f_n)),
          by=.(g_whoregion)] #regional aggregate




## make a data structure containing neighbouts
nbs <- list()
for(i in 1:length(X)){
    nbs[[i]] <- list(iso3=isoidx[i],isoidx[X[[i]]])
}
nbs <- rbindlist(nbs)
names(nbs)[2] <- 'nb'
## key
setkey(nbs,iso3)

## check
nbs['GBR']
nbs['USA']


## countries without data
RS[rr_N==0,c('rr_N','f_n'):=NA]

## countries with data
cnwd <- RS[!is.na(rr_N)]

## countries without data
cnwod <- RS[is.na(rr_N)]

## count those with data
cnwod[,nbswd:=0] #neighbors with data
for(i in 1:nrow(cnwod)){
    cnwod[i,nbswd:=sum(nbs[cnwod[i,iso3],nb] %in% cnwd$iso3)]
}

cnwod[,table(nbswd)]

## two or more for resampling
cnwod1 <- cnwod[!nbswd<2] #>= 2 nhs with data
cnwod2 <- cnwod[nbswd<2]  #fewer than 2 nhs with data

## replicates: 3 cases
set.seed(123)
N <- 1e3
A <- cnwd[rep(1:nrow(cnwd),each=N)] #with data
B <- cnwod1[rep(1:nrow(cnwod1),each=N)] #with nhd data
C <- cnwod2[rep(1:nrow(cnwod2),each=N)] #without nhd data

## loop by country to sample a different neighbour each time
for(cn in B[,unique(iso3)]){
    print(cn)
    tmp <- nbs[cn,nb] #nbs
    tmp <- intersect(tmp,cnwd$iso3) #nbs w data
    tmpl <- sample(tmp,N,replace=TRUE)
    B[iso3==cn,nb:=tmpl]
}

B[,c('rr_N','f_n'):=NULL]
B <- merge(B,cnwd[,.(iso3,rr_N,f_n)],
           by.x='nb',by.y = 'iso3',all.x=TRUE)
B[,c('nb','nbswd'):=NULL]


## loop by country to sample a different country in region each time
for(cn in C[,unique(iso3)]){
    print(cn)
    reg <- unique(C[iso3==cn,g_whoregion])
    tmp <- cnwd[g_whoregion==reg,iso3]
    tmpl <- sample(tmp,N,replace=TRUE)
    C[iso3==cn,nb:=tmpl]
}

C[,c('rr_N','f_n'):=NULL]
C <- merge(C,cnwd[,.(iso3,rr_N,f_n)],
           by.x='nb',by.y = 'iso3',all.x=TRUE)
C[,c('nb','nbswd'):=NULL]


## join together and do beta sampling:
FRF <- rbindlist(list(A,B,C))
FRF[,fqr:=rbeta(nrow(FRF),1+f_n,1+rr_N-f_n)] #flat prior (beta/binom)
FRF[,c('rr_N','f_n'):=NULL] #remove unneeded cols
FRF[,id:=rep(1:N,length(unique(iso3)))] #add a rep count


save(FRF,file=here('data/FRF.Rdata'))


## graph of this data
library(ggplot2)

load(file=here('data/FRF.Rdata'))

GP <- ggplot(FRF,aes(iso3,fqr)) +
  stat_summary(geom='pointrange',fun.data = median_hilow)+
  facet_wrap(~g_whoregion,scales = 'free_y')+
  coord_flip()+
  scale_y_continuous(label=scales::percent)+ylab('FQR in RR TB')

ggsave(GP,file=here('data/FRF.png'),h=12,w=15)

FRF[,mean(fqr),by=g_whoregion]

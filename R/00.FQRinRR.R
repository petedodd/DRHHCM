library(here)
library(data.table)

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


## load 5 nearest country structure
load(here('indata/acs_kn5.Rdata'))
load(here('indata/isoidx.Rdata'))


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

## inspect TODO more
FRF[,.(f=mean(fqr)),by=g_whoregion]
sm <- FRF[,.(f=mean(fqr)),by=iso3]
max(sm$f)
min(sm$f)
sm[which.max(f)]
sm[which.min(f)]
sm[iso3=='RUS']
sm[iso3=='UKR']
sm[iso3=='FRA']
sm[iso3=='ZAF']
sm[iso3=='IND']
sm[iso3=='CHN']



# This file compiles a table of RR/MDR input data
# iso3 code, RR burden, beta distribution for RR CDR and proportion of RR/MDR that is FQR

rm(list=ls())

## ================ data pooling ===========
## --- WHO data from
## http://www.who.int/tb/country/data/download/en/
## <2020-02-25 Tue>
library(data.table)
library(ggplot2)
library(dplyr)
library(here)

## - notifications (start)
N <- fread(here('indata/TB_notifications_2020-10-15.csv'))
N <- N[year==2019]                      #restrict to most recent for onward use
## - notifications (end)

## estimations
E <- fread(here('indata/TB_burden_countries_2020-10-15.csv'))
E <- E[year==2019]                      #restrict
D <- merge(N,E,by=c('country','iso2','iso3','iso_numeric','g_whoregion','year'))

## estimates by age
Eage<-fread(here('indata/TB_burden_age_sex_2020-10-15.csv'))
Eage<-Eage[risk_factor=="all"]
Eage014<-Eage[age_group=="0-14"]
Eage014<-Eage014[sex=="a"]
Eage04<-Eage[age_group=="0-4"]
Eagef04<-Eage04[sex=="f"]
Eagem04<-Eage04[sex=="m"]
Eage514<-Eage[age_group=="5-14"]
Eagef514<-Eage514[sex=="f"]
Eagem514<-Eage514[sex=="m"]
#Renaming incidence estimates for all TB by age
names(Eage014)[names(Eage014) == 'best'] <- 'e_inc_num_014'
names(Eage014)[names(Eage014) == 'hi'] <- 'e_inc_num_014_hi'
names(Eage014)[names(Eage014) == 'lo'] <- 'e_inc_num_014_lo'
names(Eagef04)[names(Eagef04) == 'best'] <- 'e_inc_num_f04'
names(Eagef04)[names(Eagef04) == 'hi'] <- 'e_inc_num_f04_hi'
names(Eagef04)[names(Eagef04) == 'lo'] <- 'e_inc_num_f04_lo'
names(Eagem04)[names(Eagem04) == 'best'] <- 'e_inc_num_m04'
names(Eagem04)[names(Eagem04) == 'hi'] <- 'e_inc_num_m04_hi'
names(Eagem04)[names(Eagem04) == 'lo'] <- 'e_inc_num_m04_lo'
Eage04<-merge(Eagef04,Eagem04,by=c('country','iso2','iso3','iso_numeric','year','measure','unit','risk_factor'))
names(Eagef514)[names(Eagef514) == 'best'] <- 'e_inc_num_f514'
names(Eagef514)[names(Eagef514) == 'hi'] <- 'e_inc_num_f514_hi'
names(Eagef514)[names(Eagef514) == 'lo'] <- 'e_inc_num_f514_lo'
names(Eagem514)[names(Eagem514) == 'best'] <- 'e_inc_num_m514'
names(Eagem514)[names(Eagem514) == 'hi'] <- 'e_inc_num_m514_hi'
names(Eagem514)[names(Eagem514) == 'lo'] <- 'e_inc_num_m514_lo'
Eage514<-merge(Eagef514,Eagem514,by=c('country','iso2','iso3','iso_numeric','year','measure','unit','risk_factor'))
Eagebreakdown<-merge(Eage04,Eage514,by=c('country','iso2','iso3','iso_numeric','year','measure','unit','risk_factor'))
Eage<-merge(Eage014,Eagebreakdown,by=c('country','iso2','iso3','iso_numeric','year','measure','unit','risk_factor'))


## rr/mdr surveillance & estimates
H <- fread(here('indata/TB_dr_surveillance_2020-10-15.csv'))
H <- H[year==2019]
G <- fread(here('indata/MDR_RR_TB_burden_estimates_2020-10-15.csv'))
G <- G[year==2019]
## merge
DR <- merge(G,H,by=c('country','iso3','iso_numeric','g_whoregion','year'))
DR <- merge(DR,Eage,by=c('country','iso3','iso_numeric','year'))
RRest <- merge(DR,D,by=c('country','iso2','iso3','iso_numeric','g_whoregion','year'))

## for map
RRmap <- RRest[,.(RRu5=(e_rr_pct_new/100)*(e_inc_num_f04+e_inc_num_m04),
                  RRo5=(e_rr_pct_new/100)*(e_inc_num_f514+e_inc_num_m514),
                  RR15=(e_rr_pct_new/100)*(e_inc_num_f04+e_inc_num_m04+e_inc_num_f514+e_inc_num_m514),
                  iso3)]
RRmap[,sum(RR15,na.rm=TRUE)]
save(RRmap,file=here('data/RRmap.Rdata'))


## calculate the CDR/treatment coverage
# Use the age-disaggregated treatment initiation where possible
Step1<-RRest[!is.na(RRest$rrmdr_014_tx),]
# Where possible calculate for 0-4 and 5-14 years
Step1a<-Step1[!is.na(Step1$newrel_f04),]
Step1a[,cdr04:=(((newrel_f04+newrel_m04)/(newrel_f04+newrel_m04+newrel_f514+newrel_m514))*rrmdr_014_tx/(e_rr_pct_new/100*(e_inc_num_f04+e_inc_num_m04)))]
Step1a[!is.finite(cdr04),cdr04:=0]
Step1a[cdr04>1,cdr04:=1]
Step1a[,cdr514:=(((newrel_f514+newrel_m514)/(newrel_f04+newrel_m04+newrel_f514+newrel_m514))*rrmdr_014_tx/(e_rr_pct_new/100*(e_inc_num_f514+e_inc_num_m514)))]
Step1a[!is.finite(cdr514),cdr514:=0]
Step1a[cdr514>1,cdr514:=1]
# Where not possible to do 0-4 vs 5-14, presume they are the same
Step1b<-Step1[is.na(Step1$newrel_f04),]
Step1b[,cdr04:=(rrmdr_014_tx/(e_rr_pct_new/100*(e_inc_num_014)))]
Step1b[!is.finite(cdr04),cdr04:=0]
Step1b[cdr04>1,cdr04:=1]
Step1b[,cdr514:=cdr04]
Step1<-rbind(Step1a,Step1b)
# Also calculating the CDR014
Step1[,cdr014:=(rrmdr_014_tx/(e_rr_pct_new/100*(e_inc_num_014)))]
Step1[!is.finite(cdr014),cdr014:=0]
Step1[cdr014>1,cdr014:=1]
# number of adult cases treated
Step1[is.na(unconf_rrmdr_tx),unconf_rrmdr_tx:=0]
Step1[,rrmdr_15plus_tx:=conf_rrmdr_tx+unconf_rrmdr_tx-rrmdr_014_tx]

# Where not possible, assume all diagnosed children with RR started on treatment
Step2<-RRest[is.na(RRest$rrmdr_014_tx), ]
Step2<-Step2[!is.na(Step2$rr_014),]
# Where possible calculate for 0-4 and 5-14 years
Step2a<-Step2[!is.na(Step2$newrel_f04),]
Step2a[,cdr04:=(((newrel_f04+newrel_m04)/(newrel_f04+newrel_m04+newrel_f514+newrel_m514))*rr_014/(e_rr_pct_new/100*(e_inc_num_f04+e_inc_num_m04)))]
Step2a[!is.finite(cdr04),cdr04:=0]
Step2a[cdr04>1,cdr04:=1]
Step2a[,cdr514:=(((newrel_f514+newrel_m514)/(newrel_f04+newrel_m04+newrel_f514+newrel_m514))*rr_014/(e_rr_pct_new/100*(e_inc_num_f514+e_inc_num_m514)))]
Step2a[!is.finite(cdr514),cdr514:=0]
Step2a[cdr514>1,cdr514:=1]
# Where not possible to do 0-4 vs 5-14, presume they are the same
Step2b<-Step2[is.na(Step2$newrel_f04),]
Step2b[,cdr04:=(rr_014/(e_rr_pct_new/100*(e_inc_num_014)))]
Step2b[!is.finite(cdr04),cdr04:=0]
Step2b[cdr04>1,cdr04:=1]
Step2b[,cdr514:=cdr04]
Step2<-rbind(Step2a,Step2b)
# Also calculating the CDR014
Step2[,cdr014:=(rr_014/(e_rr_pct_new/100*(e_inc_num_014)))]
Step2[!is.finite(cdr014),cdr014:=0]
Step2[cdr014>1,cdr014:=1]
# number of adult cases treated
Step2[,rrmdr_15plus_tx:=rr_15plus]

# Where not possible, for this year of data that implies confirmed & unconfirmed cases diagnosed or started on treatment are also not available, so assume cdr=0
Step3<-RRest[is.na(RRest$rrmdr_014_tx), ]
Step3<-Step3[is.na(Step3$rr_014), ]
Step3[,cdr04:=0]
Step3[,cdr514:=0]
# Also calculating the CDR014
Step3[,cdr014:=0]
# number of adult cases treated
Step3[,rrmdr_15plus_tx:=0]
Step3[iso3=="BIH",rrmdr_15plus_tx:=conf_rrmdr]

RRest<-rbind(Step1,Step2,Step3)

# distribution for CDRs
RRest[,cdr04ab:=((1-cdr04)/cdr04)/((e_inc_num_m04_hi-e_inc_num_m04_lo)/(3.92*e_inc_num_m04))^2-1]
RRest[,cdr514ab:=((1-cdr514)/cdr514)/((e_inc_num_m514_hi-e_inc_num_m514_lo)/(3.92*e_inc_num_m514))^2-1]
RRest[,cdr014ab:=((1-cdr014)/cdr014)/((e_inc_num_014_hi-e_inc_num_014_lo)/(3.92*e_inc_num_014))^2-1]
RRest[!is.finite(cdr04ab) | cdr04ab<0, cdr04ab:=0] #NB CDR sampling needs to handle 0s
RRest[!is.finite(cdr514ab) | cdr514ab<0, cdr514ab:=0] #NB CDR sampling needs to handle 0s
RRest[!is.finite(cdr014ab) | cdr014ab<0, cdr014ab:=0] #NB CDR sampling needs to handle 0s

RRestselect<-select(RRest,iso3,cdr014,cdr014ab,cdr04,cdr04ab,cdr514,cdr514ab,rrmdr_15plus_tx)
write.csv(RRestselect,here("data/RRest.csv"), row.names = FALSE)

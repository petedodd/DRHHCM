## do the calculations
rm(list = ls())

## libraries etc
library(here)
source(here('R/utils/maketree.R'))      #tree structure & functions
source(here('R/utils/addparams.R'))     #gets parametrization and tree labeller



## data from previous analyses
load(here('data/PSA.Rdata'))
PSA[,value:=rrmdr_15plus_tx] #index cases

print(object.size(PSA),units='auto')
PSA <- PSA[repn<=100] #NOTE make smaller for laptop testing

## read in cost data
C0 <- fread(here('indata/Country_unit_costs2.csv'))
C2 <- MakeCostData(C0[,.(iso3,unit_cost,cost.m,cost.sd)],max(PSA$repn))
C2 <- dcast(C2,iso3+repn~unit_cost,value.var = 'value')
(miss <- setdiff(PSA$iso3,C2$iso3)) #missing iso3 in cost data
cat(miss,file=here('output/costs_missing_iso3.txt'))


PSA <- merge(PSA,C2,by=c('iso3','repn')) #merge in cost data
PSA[iso3=='ZAF' & repn==1 & acat=='[0,5)'] #HIV//ART absent
PSA[repn==1,sum(value)/1e5]   #check
PSA[,summary(hhc)]

## add DR
PSA <- splitbyDRtypes(PSA) #split by DR
PSA[iso3=='ZAF' & repn==1 & acat=='[0,5)']
PSA[iso3=='ZAF' & repn==1 & acat=='[0,5)',sum(value)]
PSA[repn==1,sum(value)/1e5]   #check

## add HIV
PSA <- splitbyHIV(PSA)   #add HIV
PSA[iso3=='ZAF' & repn==1 & acat=='[0,5)']
PSA[iso3=='ZAF' & repn==1 & acat=='[0,5)',sum(value)]
PSA[repn==1,sum(value)/1e5]   #check

## test
## check
test <- PSA[,.(value=sum(value)),by=.(repn,DST)]
test[,prop:=value/sum(value),by=repn]
test[,mean(prop),by=DST]                #close on 50:50 split for non-FS

## label tree
## PSA[,c('hiv','art'):=0]
PSA[acat=='[0,5)',age:=1.0]; PSA[acat=='[5,15)',age:=10.0] #for funs

## add variables to tree
addVariables(PSA)                #NOTE return by side-effect

## function for setting PT efficacies wrt intervention
## also includes settings costs, which depend on:
## strategy x regimen
setPTeff <- function(D, #data
                     regimen=c('INH','FQ','BDQ'), #representative regimen for PT eff
                     tst="all"                   #TST strategy
                     ){
    ## --- efficacies
    ## RR1 is the sampled efficacy in TST+, RR0 in all TST
    D[,RR:=1.0]                               #null effect
    D[DST=='RS',RR:=RR0]  #base IPT efficacy, no efficacy for in RR NOTE lumping HR and in here
    if(regimen != 'INH') D[DST=='FS',RR:=RR0] #efficacy
    if(regimen=='BDQ') D[DST=='FR',RR:=RR0]   #efficacy
    if(tst!='all'){    #switching to efficacy in TST+ve
        ## coverage to equal LTBI prevalence TODO not here? check HIV
        D[DST=='RS' & hiv==0 & acat=='[5,15)',RR:=RR1]
        if(regimen != 'INH') D[DST=='FS' & hiv==0 & acat=='[5,15)',RR:=RR1]
        if(regimen=='BDQ') D[DST=='FR' & hiv==0 & acat=='[5,15)',RR:=RR1]
    }

}

## cost things -
PSA[,fracSymptomatic:=0.3] #TODO build into parms
PSA[,fracAE:=0.0] #TODO AE for PT build into parms


## set costs (by side-effect)
## NOTE some costs don't depend on args, so inefficient. But no harm done
setCosts <- function(D, #data
                     intervention=c('No HHCM','HHCM, no PT','PT to <5/HIV+/TST+','PT to <5/HIV+','PT to <15'),
                     regimen=c('INH','FQ','BDQ'), #efficacy only
                     rgc="BDQ"){   #if BDQ eff, is it BDQ or DLM costs? if FQ is it MXF or LVX?

    ## cost of TB screen - per child HH contact (both CXR + GXP)
    D[,c_tbscreen := c_hh_visit +
                        fracSymptomatic * (c_cxr_exam +
                                           c_opd_visit)+
                        fracSymptomatic *
                        ifelse(acat=='[0,5)',
                               c_xpert_test.04,
                               c_xpert_test.514)]

    ## costs of treatment of prevalent TB: note OPD visit not included
    D[,c_rsatt:= c_hiv_test + c_dstb_tx] #HIV test + ATT, not age dept
    D[,c_rratt:= c_hiv_test +
           ifelse(acat=='[0,5)',c_mdrtb_tx.04,c_mdrtb_tx.514)] #HIV test + MDR ATT

    ## costs of treatment for incident TB (+/- PT): treatment, but also dx
    ## assumes future presumptive TB proportional to how much TB there is
    ## NOTE: incremental vs total costs & careful reporting
    D[,c_dxrsatt:=c_rsatt +
           (c_cxr_exam+c_opd_visit) + ifelse(acat=='[0,5)',
                                             c_xpert_test.04,
                                             c_xpert_test.514)]
    D[,c_dxrratt:=c_rratt +
           (c_cxr_exam+c_opd_visit) + ifelse(acat=='[0,5)',
                                             c_xpert_test.04,
                                             c_xpert_test.514)]


    ## PT screen costs (depends strategy)
    D[,c_ptscreen:=0] #applies to 2 No PT options & 'PT to <15' where no screening needed
    if(intervention=='PT to <5/HIV+/TST+'){
        D[acat=='[5,15)',c_ptscreen:=(c_hiv_test+c_tst_test)] #NOTE assumes all get both, could change
    }
    if(intervention=='PT to <5/HIV+'){
        D[acat=='[5,15)',c_ptscreen:=c_hiv_test] #
    }

    ## PT costs: NB PTcov is applied by onward computation
    D[,c_pt:=0.0] #for safety & no PT
    if(regimen=='INH'){D[,c_pt:=c_tpt_fu + c_aes_INH * fracAE + c_monit_INH + c_tpt_INH]}
    if(regimen=='FQ' & rgc=='MXF'){
        D[,c_pt:=c_tpt_fu + c_aes_FQ * fracAE + c_monit_FQ + c_tpt_MXF]
    }
    if(regimen=='FQ' & rgc=='LVX'){
        D[,c_pt:=c_tpt_fu + c_aes_FQ * fracAE + c_monit_FQ + c_tpt_LVX]
    }
    if(regimen=='BDQ' & rgc=='BDQ'){
        D[,c_pt:=c_tpt_fu + c_aes_BDQ * fracAE + c_monit_BDQ + c_tpt_BDQ]
    }
    if(regimen=='BDQ' & rgc=='DLM'){
        D[,c_pt:=c_tpt_fu + c_aes_BDQ * fracAE + c_monit_BDQ + c_tpt_DLM]
    }

}

## ====== BASECASE
tosave <-  c('iso3','repn','acat','DST',
             'deaths','incdeaths','inctb','rsatt','rratt',
             'ptc','rsatti','rratti',
             'cost',
             'value')
tosavev <- tosave[-c(1:4)]
tosavev2 <- c(tosavev,'hhc')
## tosavev <- tosavev[tosavev!='value']
tosaveh <- c(tosave,c('hiv','art'))
sumxy <- function(x,y) sum(x*y)

## function to get results & sum over HIV strata
resultnoHIV <- function(D){
    IT <- copy(D[,..tosaveh]) #save answer
    IT[,hhc:=1.0]             #gets multiplied by value=hhc
    IT <- IT[,lapply(.SD,function(x)sumxy(x,value)),
             by=.(iso3,repn,acat,DST),
             .SDcols=tosavev2] #collapse over HIV
    IT[,value:=NULL]
    return(IT)
}

## === No HHCM
## apply functions to tree (from maketree.R)
PSA[,PTcov:=0.0]
setCosts(PSA,intervention='No HHCM',regimen="None")
PSA <- runallfuns(PSA)
I0 <- resultnoHIV(PSA)
## PT coverage & regimen
I0[,intervention:='No HHCM']; I0[,`PT regimen`:='none']
## checks
PSA[,summary(progn)]
PSA[,summary(coprev)]

## ====== INTERVENTIONS
## all intervention scenarios include finding all coprev & appropriate ATT:
PSA[,CDR:=1.0]                  #all coprev detected
PSA[,rrCDR:=1.0]                #all coprev detected TODO check! (not all bac+)
PSA[DST!='RS',pRSATT:=0.0]      #no RR coprev treated as RS, ie rrCDR=1 TODO check

## === PT to <5/HIV
PSA[acat=='[0,5)' | hiv==1, PTcov:=1] #set coverages

## ## --- (INH)
## setPTeff(PSA,regimen='INH') # set PT efficacy
## PSA <- runallfuns(PSA)      #calculate
## I1H <- resultnoHIV(PSA)
## I1H[,intervention:='PT to <5/HIV+']; I1H[,`PT regimen`:='INH'];

## --- (no PT)
PSA[,RR:=1.0] # set PT efficacy
setCosts(PSA,intervention='No HHCM',regimen="None")
PSA <- runallfuns(PSA)      #calculate
I1H <- resultnoHIV(PSA)
I1H[,intervention:='HHCM, no PT']; I1H[,`PT regimen`:='none'];

## --- (FQ)
setPTeff(PSA,regimen='FQ')
setCosts(PSA,intervention='PT to <5/HIV+',regimen="FQ",rgc="LVX")
PSA <- runallfuns(PSA)
I1F <- resultnoHIV(PSA)
I1F[,intervention:='PT to <5/HIV+']; I1F[,`PT regimen`:='FQ'];
## same but MXF
setCosts(PSA,intervention='PT to <5/HIV+',regimen="FQ",rgc="MXF")
PSA <- runallfuns(PSA)
I1Fb <- resultnoHIV(PSA)
I1Fb[,intervention:='PT to <5/HIV+']; I1Fb[,`PT regimen`:='MXF'];

## --- (BDQ)
setPTeff(PSA,regimen='BDQ')
setCosts(PSA,intervention='PT to <5/HIV+',regimen="BDQ",rgc="BDQ")
PSA <- runallfuns(PSA)
I1B <- resultnoHIV(PSA)
I1B[,intervention:='PT to <5/HIV+']; I1B[,`PT regimen`:='BDQ'];
## same for DLM
setCosts(PSA,intervention='PT to <5/HIV+',regimen="BDQ",rgc="DLM")
PSA <- runallfuns(PSA)
I1Bb <- resultnoHIV(PSA)
I1Bb[,intervention:='PT to <5/HIV+']; I1Bb[,`PT regimen`:='DLM'];

## === PT to <15
PSA[,PTcov:=1]                         #set coverages

## ## --- (INH)
## setPTeff(PSA,regimen='INH') # set PT efficacy
## PSA <- runallfuns(PSA)
## I2H <- resultnoHIV(PSA)
## I2H[,intervention:='PT to <15'];I2H[,`PT regimen`:='INH'];

## ## --- (no PT)
## PSA[,RR:=1.0] # set PT efficacy
## PSA <- runallfuns(PSA)      #calculate
## I2H <- resultnoHIV(PSA)
## I2H[,intervention:='HHCM, no PT']; I2H[,`PT regimen`:='none'];

## --- (FQ)
setPTeff(PSA,regimen='FQ') # set PT efficacy
setCosts(PSA,intervention='PT to <15',regimen="FQ",rgc="LVX")
PSA <- runallfuns(PSA)
I2F <- resultnoHIV(PSA)
I2F[,intervention:='PT to <15'];I2F[,`PT regimen`:='FQ'];
## also MXF
setCosts(PSA,intervention='PT to <15',regimen="FQ",rgc="MXF")
PSA <- runallfuns(PSA)
I2Fb <- resultnoHIV(PSA)
I2Fb[,intervention:='PT to <15'];I2Fb[,`PT regimen`:='MXF'];

## --- (BDQ)
setPTeff(PSA,regimen='BDQ') # set PT efficacy
setCosts(PSA,intervention='PT to <15',regimen="BDQ",rgc="BDQ")
PSA <- runallfuns(PSA)
I2B <- resultnoHIV(PSA)
I2B[,intervention:='PT to <15'];I2B[,`PT regimen`:='BDQ']
## also DLM
setCosts(PSA,intervention='PT to <15',regimen="BDQ",rgc="DLM")
PSA <- runallfuns(PSA)
I2Bb <- resultnoHIV(PSA)
I2Bb[,intervention:='PT to <15'];I2Bb[,`PT regimen`:='DLM']

## === PT to <5/HIV/TST
PSA[,PTcov:=0.0]
PSA[acat=='[0,5)' | hiv==1, PTcov:=1] #set coverages
PSA[acat=='[5,15)' & hiv==0, PTcov:=ltbi.prev] # all & only those with LTBI among >5/H+

## ## --- (INH)
## setPTeff(PSA,regimen='INH',tst='+ve') # set PT efficacy
## PSA <- runallfuns(PSA)
## I3H <- resultnoHIV(PSA)
## I3H[,intervention:='PT to <5/HIV+/TST+'];I3H[,`PT regimen`:='INH'];

## ## --- (no PT)
## PSA[,RR:=1.0] # set PT efficacy
## PSA <- runallfuns(PSA)      #calculate
## I3H <- resultnoHIV(PSA)
## I3H[,intervention:='HHCM, no PT']; I3H[,`PT regimen`:='none'];

## --- (FQ)
setPTeff(PSA,regimen='FQ',tst='+ve') # set PT efficacy
setCosts(PSA,intervention='PT to <5/HIV+/TST+',regimen="FQ",rgc="LVX")
PSA <- runallfuns(PSA)
I3F <- resultnoHIV(PSA)
I3F[,intervention:='PT to <5/HIV+/TST+'];I3F[,`PT regimen`:='FQ'];
## also MXF
setCosts(PSA,intervention='PT to <5/HIV+/TST+',regimen="FQ",rgc="MXF")
PSA <- runallfuns(PSA)
I3Fb <- resultnoHIV(PSA)
I3Fb[,intervention:='PT to <5/HIV+/TST+'];I3Fb[,`PT regimen`:='MXF'];

## --- (BDQ)
setPTeff(PSA,regimen='BDQ',tst='+ve') # set PT efficacy
setCosts(PSA,intervention='PT to <5/HIV+/TST+',regimen="BDQ",rgc="BDQ")
PSA <- runallfuns(PSA)
I3B <- resultnoHIV(PSA)
I3B[,intervention:='PT to <5/HIV+/TST+'];I3B[,`PT regimen`:='BDQ']
## also DLM
setCosts(PSA,intervention='PT to <5/HIV+/TST+',regimen="BDQ",rgc="DLM")
PSA <- runallfuns(PSA)
I3Bb <- resultnoHIV(PSA)
I3Bb[,intervention:='PT to <5/HIV+/TST+'];I3Bb[,`PT regimen`:='DLM']

## === test with all RS
## NOTE for safety, may be as well to reload PSA & addvariables
## in case there are any side effects left over from above interventions

## data from previous analyses
load(here('data/PSA.Rdata')); cat('reloading PSA for safety!\n')
PSA[,value:=rrmdr_15plus_tx]
PSA <- PSA[repn<=100] #NOTE make smaller for laptop testing
PSA <- splitbyDRtypes(PSA) #split by DR
PSA <- splitbyHIV(PSA)   #add HIV

PSA[acat=='[0,5)',age:=1.0]; PSA[acat=='[5,15)',age:=10.0] #TODO remove
PSA[,DST:='RS']   #overwrites and makes everything RS NOTE needed before add variables 
addVariables(PSA)                #NOTE return by side-effect
PSA <- merge(PSA,C2,by=c('iso3','repn')) #merge in cost data
PSA[,fracSymptomatic:=0.3] #TODO build into parms
PSA[,fracAE:=0.0] #TODO AE for PT build into parms
PSA[,summary(CDR)]
PSA[,mean(CDR),by=acat]
PSA[,summary(CDRi)]
PSA[,summary(rrCDRi)]
PSA[,summary(rrCDR)]
## ====================
PSA[,DST:='RS']   #overwrites and makes everything RS
PSA[,pRSATT:=1.0]                       #given detected, prob RS ATT
PSA[,pRSATTi:=1.0]
## BASELINE
PSA[,PTcov:=0.0]
setCosts(PSA,intervention='None',regimen="INH")
PSA <- runallfuns(PSA)      #calculate
## checks
PSA[,summary(progn)]
PSA[,summary(coprev)]
TI0H <- resultnoHIV(PSA)
TI0H[,intervention:='none']; TI0H[,`PT regimen`:='none'];
## INTERVENTIONS: DS TEST
## all intervention scenarios include finding all coprev & appropriate ATT:
PSA[,CDR:=1.0]                  #all coprev detected
PSA[,summary(CDRi)]
## PSA[,CDRi:=1.0]                  #all incidence detected TODO NOTE
PSA[,rrCDR:=1.0]                #all coprev detected TODO check! (not all bac+)
PSA[DST!='RS',pRSATT:=0.0]      #no RR coprev treated as RS, ie rrCDR=1 TODO check
PSA[,PTcov:=0.0]

## === PT to <5/HIV
PSA[acat=='[0,5)' | hiv==1, PTcov:=1] #set coverages
## --- (INH)
setPTeff(PSA,regimen='INH') # set PT efficacy
setCosts(PSA,intervention='PT to <5/HIV+',regimen="INH")
PSA <- runallfuns(PSA)      #calculate
TI1H <- resultnoHIV(PSA)
TI1H[,intervention:='PT to <5/HIV+']; TI1H[,`PT regimen`:='INH'];
## === PT to <15
PSA[,PTcov:=1]                         #set coverages
## --- (INH)
setCosts(PSA,intervention='PT to <15',regimen="INH")
setPTeff(PSA,regimen='INH') # set PT efficacy
PSA <- runallfuns(PSA)
TI2H <- resultnoHIV(PSA)
TI2H[,intervention:='PT to <15'];TI2H[,`PT regimen`:='INH'];
## === PT to <5/HIV/TST
PSA[,PTcov:=0.0]
PSA[acat=='[0,5)' | hiv==1, PTcov:=1] #set coverages
PSA[acat=='[5,15)' & hiv==0, PTcov:=ltbi.prev] # all & only those with LTBI among >5/H+
## --- (INH)
setPTeff(PSA,regimen='INH',tst='+ve') # set PT efficacy
setCosts(PSA,intervention='PT to <5/HIV+/TST+',regimen="INH")
PSA <- runallfuns(PSA)
TI3H <- resultnoHIV(PSA)
TI3H[,intervention:='PT to <5/HIV+/TST+'];TI3H[,`PT regimen`:='INH'];
cat('Data made!\n')


## ## TIDY AND SAVE
## rm(PSA)

## join
IV <- rbindlist(list(
  I1H,I1F,I1B,
  ## I2H,
  I2F,I2B,
  ## I3H,
  I3F,I3B))
I0[,`PT regimen`:=NULL]
I0[,intervention:=NULL]

tojoin <- paste0(tosave[5:13],0) #distinguish baseline names
names(I0)[5:13] <- tojoin
tojoin <- names(I0)[1:13]

IV <- merge(IV,I0[,..tojoin],
            by=c('iso3','repn','acat','DST'),
            all.x=TRUE,allow.cartesian = TRUE)

IV                                      #memory hungry
nrow(IV)/1e6
names(IV)


IV[iso3=='ZAF' & repn==1 & acat=='[0,5)' & intervention=='PT to <15']

save(IV,file=here('data/IV.Rdata')) # NOTE this is biggish

## version with Moxy & DLM
IVb <- rbindlist(list(
  I1Fb,I1Bb,
  I2Fb,I2Bb,
  I3Fb,I3Bb))

IVb <- merge(IVb,I0[,..tojoin],
            by=c('iso3','repn','acat','DST'),
            all.x=TRUE,allow.cartesian = TRUE)

save(IVb,file=here('data/IVb.Rdata')) # NOTE this is biggish



IVT <- rbindlist(list(TI1H,TI2H,TI3H))  #RS test
## NOTE including incident treatments here for checks
tojoin <- paste0(tosave[5:13],0) #distinguish baseline names
names(TI0H)[5:13] <- tojoin
tojoin <- names(TI0H)[1:13]

IVT <- merge(IVT,TI0H[,..tojoin],
            by=c('iso3','repn','acat','DST'),
            all.x=TRUE,allow.cartesian = TRUE)



## TODO consider some aggregations before saving
save(IVT,file=here('data/IVT.Rdata')) # NOTE this is biggish


rm(I0,
   I1H,I1F,I1B,
   ## I2H,
   I2F,I2B)

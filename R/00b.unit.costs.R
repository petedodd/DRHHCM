## this file creates the countr-level unit cost data used in calculations
rm(list = ls())
library(here)
library(ggthemes)

## source other file:  NTP, inpatient, outpatient costs
source(here('R/utils/TB_care_costs.R'))

## Other unit costs from relevant public sources:
unit_costs <- fread(here('indata',
                         'other_public_unit_costs.csv'))

# costs <- c('c.contact.tracing', 'c.tst', 'c.qft.plus', 'c.ess.collection', 'c.iss.collection', 'c.sample.transport.shared','c.sample.transport.exclusive', 'c.xpert','c.xray', 'c.lft', 'c.cbc', 'c.dstb_tx', 'c.mdrtb_tx1','c.mdrtb_tx2','c.tpt.inh','c.tpt.lvx','c.tpt.mxf','c.tpt.dlm','c.tpt.bdq')

# model cost parameter names
HHMDR_costnames <- c('c_hh_visit', 'c_hiv_test', 'c_tst_test', 'c_opd_visit', 'c_cxr_exam', 'c_xpert_test.04','c_xpert_test.514','c_tpt_INH.04','c_tpt_INH.514',  'c_tpt_LVX.04','c_tpt_LVX.514','c_tpt_MXF.04','c_tpt_MXF.514','c_tpt_BDQ.04', 'c_tpt_BDQ.514','c_tpt_DLM.04', 'c_tpt_DLM.514', 'c_tpt_fu', 'c_monit_INH', 'c_monit_FQ', 'c_monit_BDQ','c_aes_INH','c_aes_FQ','c_aes_BDQ','c_saes_INH','c_saes_FQ','c_saes_BDQ','c_dstb_tx','c_mdrtb_tx1.04','c_mdrtb_tx2.04','c_mdrtb_tx1.514','c_mdrtb_tx2.514')

# unit cost parameter names
costs <- c('c_hh_visit', 'c_hiv_test', 'c_tst_test', 'c_opd_visit', 'c_cxr_exam', 'c.qft.plus', 'c.ess.collection', 'c.iss.collection', 'c.sample.transport.shared','c.sample.transport.exclusive', 'c.xpert', 'c.lft', 'c.cbc','c_xpert_test.04','c_xpert_test.514','c_tpt_INH.04','c_tpt_INH.514', 'c_tpt_LVX.04', 'c_tpt_LVX.514','c_tpt_MXF.04','c_tpt_MXF.514','c_tpt_BDQ.04', 'c_tpt_BDQ.514','c_tpt_DLM.04', 'c_tpt_DLM.514','c_tpt_fu', 'c_monit_INH', 'c_monit_FQ', 'c_monit_BDQ','c_aes_INH','c_aes_FQ','c_aes_BDQ','c_saes_INH','c_saes_FQ','c_saes_BDQ','c_dstb_tx','c_mdrtb_tx1.04','c_mdrtb_tx2.04','c_mdrtb_tx1.514','c_mdrtb_tx2.514')

# model cost parameter definitions
description <- c('Household visit', 'HIV test', 'Tuberculin skin test', 'Outpatient visit', 'Chest X-ray', 'QuantiFERON-TB Gold test',
                 'Expectoration sputum collection', 'Induced sputum collection', 'Shared sample transportion','Exclusive sample transportion',
                 'Xpert MTB/RIF test', 'Liver function tests', 'Complete blood count','Xpert MTB/RIF test (0-4 years)',
                 'Xpert MTB/RIF test (5-14 years)','Isoniazid-based TPT (0-4 years)','Isoniazid-based TPT (5-14 years)',
                 'Levefloxacin-based TPT (0-4 years)', 'Levefloxacin-based TPT (5-14 years)','Moxifloxacin-based TPT (0-4 years)',
                 'Moxifloxacin-based TPT (5-14 years)','Bedaquiline-based TPT (0-4 years)', 'Bedaquiline-based TPT (5-14 years)',
                 'Delamanid-based TPT (0-4 years)', 'Delamanid-based TPT (5-14 years)','TPT follow-up', 'Isoniazid-based TPT monitoring',
                 'Fluoroquinolone-based TPT monitoring', 'Bedaquiline-based TPT monitoring','Isoniazid-based TPT adverse events treatment',
                 'Fluoroquinolone-based TPT adverse events treatment','Bedaquiline-based TPT adverse events treatment',
                 'Isoniazid-based TPT serious adverse events treatment','Fluoroquinolone-based TPT serious adverse events treatment',
                 'Bedaquiline-based TPT serious adverse events treatment','Treatment for drug sensitive TB',
                 'Treatment for fluoroquinolone resistant multi-drug resistant TB (0-4 years)',
                 'Treatment for fluoroquinolone susceptible multi-drug resistant TB (0-4 years)',
                 'Treatment for fluoroquinolone resistant multi-drug resistant TB (5-15 years)',
                 'Treatment for fluoroquinolone susceptible multi-drug resistant TB (5-15 years)')

country <- unique(tb_exp$country[!is.na(tb_exp$country)])

cost.m <- rep(NA, length(costs))
cost.sd <- rep(NA, length(costs))
hhcm_costs<-expand.grid(country=country,unit_cost=costs)
hhcm_costs <- hhcm_costs %>% group_by(country) %>% mutate(resource=description)
hhcm_costs$iso3 <- tb_exp$iso3[match(hhcm_costs$country, tb_exp$country)]
hhcm_costs$who.region <- tb_exp$g_whoregion[match(hhcm_costs$country, tb_exp$country)]
hhcm_costs$dstb.inc <- tb_exp$e_inc_ds_num[match(hhcm_costs$country, tb_exp$country)]
hhcm_costs$mdrtb.inc <- tb_exp$e_inc_rr_num[match(hhcm_costs$country, tb_exp$country)]

# TODO: Remove some of these parameters not used in these calculations
hhcm_costs$exp_dstb <- prog_costs$exp_dstb[match(hhcm_costs$country, prog_costs$country)]
hhcm_costs$exp_mdrtb <- prog_costs$exp_mdrtb[match(hhcm_costs$country, prog_costs$country)]

hhcm_costs$c.opd_dstb.m <- resources_complete$c.opd_dstb.m[match(hhcm_costs$country, resources_complete$country)]
hhcm_costs$c.opd_dstb.lo <- resources_complete$c.opd_dstb.lo[match(hhcm_costs$country, resources_complete$country)]
hhcm_costs$c.opd_dstb.hi <- resources_complete$c.opd_dstb.hi[match(hhcm_costs$country, resources_complete$country)]

hhcm_costs$c.opd_mdrtb.m <- resources_complete$c.opd_mdrtb.m[match(hhcm_costs$country, resources_complete$country)]
hhcm_costs$c.opd_mdrtb.lo <- resources_complete$c.opd_mdrtb.lo[match(hhcm_costs$country, resources_complete$country)]
hhcm_costs$c.opd_mdrtb.hi <- resources_complete$c.opd_mdrtb.hi[match(hhcm_costs$country, resources_complete$country)]

hhcm_costs$c.ipd_dstb.m <- resources_complete$c.ipd_dstb.m[match(hhcm_costs$country, resources_complete$country)]
hhcm_costs$c.ipd_dstb.lo <- resources_complete$c.ipd_dstb.lo[match(hhcm_costs$country, resources_complete$country)]
hhcm_costs$c.ipd_dstb.hi <- resources_complete$c.ipd_dstb.hi[match(hhcm_costs$country, resources_complete$country)]

hhcm_costs$c.ipd_mdrtb.m <- resources_complete$c.ipd_mdrtb.m[match(hhcm_costs$country, resources_complete$country)]
hhcm_costs$c.ipd_mdrtb.lo <- resources_complete$c.ipd_mdrtb.lo[match(hhcm_costs$country, resources_complete$country)]
hhcm_costs$c.ipd_mdrtb.hi <- resources_complete$c.ipd_mdrtb.hi[match(hhcm_costs$country, resources_complete$country)]

hhcm_costs$gdp <- tb_exp$r.gdp[match(hhcm_costs$country, tb_exp$country)]
hhcm_costs$gdp.pp <- tb_exp$gdp[match(hhcm_costs$country, tb_exp$country)]
hhcm_costs$income <- gdp$IncomeGroup[match(hhcm_costs$iso3, gdp$iso3)]
hhcm_costs$income <- ifelse(is.na(hhcm_costs$income) & hhcm_costs$gdp.pp>12696, 'High income', 'Lower middle income')
hhcm_costs$ppp <- tb_exp$ppp[match(hhcm_costs$country, tb_exp$country)]
hhcm_costs$exch <- tb_exp$exch_ppp[match(hhcm_costs$country, tb_exp$country)]
hhcm_costs$cost.sd <- hhcm_costs$cost.m <- rep(NA,nrow(hhcm_costs))

summary(hhcm_costs %>% select(exp_dstb,exp_mdrtb, c.opd_dstb.m, c.opd_mdrtb.m, c.ipd_dstb.m, c.ipd_mdrtb.m))

# TODO: These are already in TB unit costs_09122021.R - reconcile and simplify code
# WHO-CHOICE country-specific costs for inpatient and outpatient health service delivery
# https://www.who.int/choice/cost-effectiveness/inputs/health_service/en/
# Values of variables used for prediction of the unit cost
# Ownership is set to public provider; location is set to urban location for outpatient care

# Outpatient care
opd.vsts.l1 <- 67656
opd.vsts.l2 <- 46434
opd.vsts.l3 <- 93739
opd.vsts.l4.5 <- 281156
vsts.per.provider.fl1 <- 8.96
vsts.per.provider.fl2 <- 9.52
vsts.per.provider.fl3 <- 3.22
vsts.per.provider.fl4.5 <- 2.36

# Regression coefficients and 95% confidence interval: natural log of cost per outpatient visit expressed in 2007
gdppc.opd <- 0.865; gdppc.opd.lo <- 0.826; gdppc.opd.hi <- 0.905
visits <- -0.0142; visits.lo <- -0.0272; visits.hi <-  -0.00119
vsts.per.provider <- -0.0412; vsts.per.provider.lo <- -0.0578; vsts.per.provider.hi <- -0.0246
urban.opd <- 0.352; urban.opd.lo <- 0.268; urban.opd.hi <- 0.435
public.opd <- -0.290; public.opd.lo <- -0.330; public.opd.hi <- -0.249
private.opd <- 0.0532; private.opd.lo <- 0.00479; private.opd.hi <- 0.102
level.2 <- 0.208; level.2.lo <- 0.144; level.2.hi <- 0.271
level.3 <- 0.304; level.3.lo <- 0.213; level.3.hi <- 0.395
level.4 <- 0.348; level.4.lo <- 0.279; level.4.hi <- 0.417
Colombia.opd <- 0.628; Colombia.opd.lo <- 0.542; Colombia.opd.hi <- 0.713
Brazil.opd <- -1.563; Brazil.opd.lo <- -1.656; Brazil.opd.hi <- -1.470
Brazil.level.3 <- -0.245; Brazil.level.3.lo <- -0.337; Brazil.level.3.hi <- -0.153
constant.opd <- -4.534; constant.opd.lo <- -4.797; constant.opd.hi <- -4.271

opd.mean <- exp(constant.opd + gdppc.opd*log(hhcm_costs$gdp.pp[hhcm_costs$unit_cost=='c_hh_visit']) + visits*log(opd.vsts.l1) + vsts.per.provider*log(vsts.per.provider.fl1) + public.opd)
#resources_complete$c_opd.m
opd.hi <- exp(constant.opd.hi+ gdppc.opd.hi*log(hhcm_costs$gdp.pp[hhcm_costs$unit_cost=='c_hh_visit']) + visits.hi*log(opd.vsts.l1) + vsts.per.provider.hi*log(vsts.per.provider.fl1) + public.opd.hi)
opd.lo <- exp(constant.opd.lo + gdppc.opd.lo*log(hhcm_costs$gdp.pp[hhcm_costs$unit_cost=='c_hh_visit']) + visits.lo*log(opd.vsts.l1) + vsts.per.provider.lo*log(vsts.per.provider.fl1) + public.opd.lo)
opd.sd <- (opd.hi-opd.lo)/4

# Inpatient care

# Total inpatient admissions per facility per year (p80)
admission.l3 <- 4971
admission.l4.5 <- 14028

# Bed occupancy rate (p80)
occupancy.l3 <- 0.756
occupancy.l4.5 <- 0.810

# Average length of stay, inpatient (p80)
a.los.l3 <- 7.14
a.los.l4.5 <- 9.75

# Regression coefficients and 95% confidence interval: natural log of cost per inpatient bed day expressed in 2007
gdppc.ipd <- 1.192; gdppc.ipd.lo <- 1.111; gdppc.ipd.hi <- 1.272;
occupancy <- -0.0201; occupancy.lo <- -0.0340; occupancy.hi <- -0.00623;
a.los <- -0.600; a.los.lo <- -0.649; a.los.hi <- -0.550;
admissions <- 0.0252; admissions.lo <- 0.00471; admissions.hi <- 0.0457;
district <- -0.204;  district.lo <- -0.275; district.hi <- -0.132;
teaching <- 0.257; teaching.lo <- 0.163; teaching.hi <- 0.351;
public.ipd <- -0.144; public.ipd.lo <- -0.182; public.ipd.hi <- -0.107;
private.ipd <- 0.110; private.ipd.lo <- 0.0710; private.ipd.hi <- 0.148;
Brazil.ipd <-  -1.638; Brazil.ipd.lo <- -1.694; Brazil.ipd.hi <- -1.583;
constant.ipd <- -4.277; constant.ipd.lo <- -5.035; constant.ipd.hi <- -3.519;

table(tb_exp$g_whoregion,tb_exp$hosp_type_mdr)

(ipd.mean <- exp(constant.ipd + gdppc.ipd*log(hhcm_costs$gdp.pp[hhcm_costs$unit_cost=='c_hh_visit']) + occupancy*log(occupancy.l3) + a.los*log(a.los.l3) + admissions*log(admission.l3) + district + public.ipd + Brazil.ipd))
(ipd.lo <- exp(constant.ipd.lo + gdppc.ipd.lo*log(hhcm_costs$gdp.pp[hhcm_costs$unit_cost=='c_hh_visit']) + occupancy.lo*log(occupancy.l3) + a.los.lo*log(a.los.l3) + admissions.lo*log(admission.l3) + district.lo + public.ipd.lo + Brazil.ipd.lo))
(ipd.hi <- exp(constant.ipd.hi + gdppc.ipd.hi*log(hhcm_costs$gdp.pp[hhcm_costs$unit_cost=='c_hh_visit']) + occupancy.hi*log(occupancy.l3) + a.los.hi*log(a.los.l3) + admissions.hi*log(admission.l3) + district.hi + public.ipd.hi + Brazil.ipd.lo))
(ipd.sd <- (ipd.hi-ipd.lo)/4)

# household contact tracing
year_of_cost <- unit_costs$Year[unit_costs$`cost parameter`=='c.contact.tracing' & unit_costs$setting=='Peru']

hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_hh_visit'] <- 
    (unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.contact.tracing' & unit_costs$setting=='Peru'] *  # unit cost 
    exch$exch[exch$year==year_of_cost & exch$`Country Name`=='Peru'] *                                       # convert back to the local currency  
    defl$defl[defl$year=='2020' & defl$`Country Name`=='Peru']/ defl$defl[defl$year==year_of_cost & defl$`Country Name`=='Peru']) /  # inflate from 2016 to 2020 using local GDP price deflators
     exch$exch[exch$`Country Name`=='Peru' & exch$year==max(exch$year)] *                                                         # convert to I$
  # hhcm_costs$exch[hhcm_costs$country=='Peru'][1] *  
    (hhcm_costs$ppp[hhcm_costs$unit_cost=='c_hh_visit']/hhcm_costs$ppp[hhcm_costs$country=='Peru'][1])           # Transfer to other countries
  

hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_hh_visit'] <- 
    (unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.contact.tracing' & unit_costs$setting=='Peru']*150/100-unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.contact.tracing' & unit_costs$setting=='Peru']*50/100)/4 *
    exch$exch[exch$year==year_of_cost & exch$`Country Name`=='Peru'] *
    defl$defl[defl$year=='2020' & defl$`Country Name`=='Peru']/ defl$defl[defl$year==year_of_cost & defl$`Country Name`=='Peru'] /
  exch$exch[exch$`Country Name`=='Peru' & exch$year==max(exch$year)] *
  hhcm_costs$ppp[hhcm_costs$unit_cost=='c_hh_visit']/hhcm_costs$ppp[hhcm_costs$country=='Peru'][1]

# hiv test
year_of_cost <- unit_costs$Year[unit_costs$`cost parameter`=='c.hiv' & unit_costs$setting=='hic']

hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_hiv_test'] <- 
    ifelse(hhcm_costs$income=='High income',
           (unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.hiv' & unit_costs$setting=='hic'] * 
              defl$defl[defl$year=='2020' & defl$`Country Code`=='USA']/ defl$defl[defl$year==year_of_cost & defl$`Country Code`=='USA']) *
             hhcm_costs$ppp[hhcm_costs$unit_cost=='c_hiv_test']/hhcm_costs$ppp[hhcm_costs$iso3=='USA'][1],
           (unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.hiv' & unit_costs$setting=='lmic'][1] * 
              defl$defl[defl$year=='2020' & defl$`Country Code`=='USA']/ defl$defl[defl$year==year_of_cost & defl$`Country Code`=='USA']) *
             hhcm_costs$ppp[hhcm_costs$unit_cost=='c_hiv_test']/hhcm_costs$ppp[hhcm_costs$iso3=='USA'][1])

hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_hiv_test'] <- 
    ifelse(hhcm_costs$income=='High income',
    (unit_costs$parm2[unit_costs$`cost parameter`=='c.hiv' & unit_costs$setting=='hic']-unit_costs$parm1[unit_costs$`cost parameter`=='c.hiv' & unit_costs$setting=='hic'])/4 * 
      defl$defl[defl$year=='2020' & defl$`Country Code`=='USA']/ defl$defl[defl$year==year_of_cost & defl$`Country Code`=='USA'] *
    hhcm_costs$ppp[hhcm_costs$unit_cost=='c_hiv_test']/hhcm_costs$ppp[hhcm_costs$iso3=='USA'][1],
    (unit_costs$parm2[unit_costs$`cost parameter`=='c.hiv' & unit_costs$setting=='lmic'][1]-unit_costs$parm1[unit_costs$`cost parameter`=='c.hiv' & unit_costs$setting=='lmic'][1])/4 * 
      defl$defl[defl$year=='2020' & defl$`Country Code`=='USA']/ defl$defl[defl$year==year_of_cost & defl$`Country Code`=='USA'] *
    hhcm_costs$ppp[hhcm_costs$unit_cost=='c_hiv_test']/hhcm_costs$ppp[hhcm_costs$iso3=='USA'][1])

# no_income <- (unique(hhcm_costs$country[is.na(hhcm_costs$income)]))

# tst
year_of_cost <- unit_costs$Year[unit_costs$`cost parameter`=='c.tst' & unit_costs$setting=='Brazil']
hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_tst_test'] <-
    (unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.tst' & unit_costs$setting=='Brazil']) * 
    exch$exch[exch$year==year_of_cost & exch$`Country Name`=='Brazil'] *
    defl$defl[defl$year=='2020' & defl$`Country Name`=='Brazil']/ defl$defl[defl$year==year_of_cost & defl$`Country Name`=='Peru'] /
   exch$exch[exch$`Country Name`=='Brazil' & exch$year==max(exch$year)] *
  (hhcm_costs$ppp[hhcm_costs$unit_cost=='c_tst_test']/hhcm_costs$ppp[hhcm_costs$country=='Brazil'][1])

hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_tst_test'] <-
    (unit_costs$parm2[unit_costs$`cost parameter`=='c.tst' & unit_costs$setting=='Brazil']-unit_costs$parm1[unit_costs$`cost parameter`=='c.tst' & unit_costs$setting=='Brazil'])/4 *
    exch$exch[exch$year==year_of_cost & exch$`Country Name`=='Brazil'] *
    defl$defl[defl$year=='2020' & defl$`Country Name`=='Brazil']/ defl$defl[defl$year==year_of_cost & defl$`Country Name`=='Brazil'] /
   exch$exch[exch$`Country Name`=='Brazil' & exch$year==max(exch$year)] *
  (hhcm_costs$ppp[hhcm_costs$unit_cost=='c_tst_test']/hhcm_costs$ppp[hhcm_costs$country=='Brazil'][1])


# qft-plus
year_of_cost <- unit_costs$Year[unit_costs$`cost parameter`=='c.qft.plus' & unit_costs$setting=='Brazil']
hhcm_costs$cost.m[hhcm_costs$unit_cost=='c.qft.plus'] <-
    (unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.qft.plus' & unit_costs$setting=='Brazil']) *
    exch$exch[exch$year==year_of_cost & exch$`Country Name`=='Brazil'] *
    defl$defl[defl$year=='2020' & defl$`Country Name`=='Brazil']/ defl$defl[defl$year==year_of_cost & defl$`Country Name`=='Brazil'] /
   exch$exch[exch$`Country Name`=='Brazil' & exch$year==max(exch$year)] *
  (hhcm_costs$ppp[hhcm_costs$unit_cost=='c.qft.plus']/hhcm_costs$ppp[hhcm_costs$country=='Brazil'][1])

hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c.qft.plus'] <-
    (unit_costs$parm2[unit_costs$`cost parameter`=='c.qft.plus' & unit_costs$setting=='Brazil']-unit_costs$parm1[unit_costs$`cost parameter`=='c.qft.plus' & unit_costs$setting=='Brazil'])/4 * 
    exch$exch[exch$year==year_of_cost & exch$`Country Name`=='Brazil'] *
    defl$defl[defl$year=='2020' & defl$`Country Name`=='Brazil']/ defl$defl[defl$year==year_of_cost & defl$`Country Name`=='Brazil'] /
   exch$exch[exch$`Country Name`=='Brazil' & exch$year==max(exch$year)] *
  (hhcm_costs$ppp[hhcm_costs$unit_cost=='c.qft.plus']/hhcm_costs$ppp[hhcm_costs$country=='Brazil'][1])

# opd visit cost
hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_opd_visit'] <- opd.mean
hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_opd_visit'] <- opd.sd

# chest x-ray
year_of_cost <- unit_costs$Year[unit_costs$`cost parameter`=='c.xray' & unit_costs$setting=='Brazil']
hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_cxr_exam'] <-
    (unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.xray' & unit_costs$setting=='Brazil']) *
    exch$exch[exch$year==year_of_cost & exch$`Country Name`=='Brazil'] *
    defl$defl[defl$year=='2020' & defl$`Country Name`=='Brazil']/ defl$defl[defl$year==year_of_cost & defl$`Country Name`=='Brazil'] /
   exch$exch[exch$`Country Name`=='Brazil' & exch$year==max(exch$year)] *
  (hhcm_costs$ppp[hhcm_costs$unit_cost=='c.qft.plus']/hhcm_costs$ppp[hhcm_costs$country=='Brazil'][1])

hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_cxr_exam'] <-
    (unit_costs$parm2[unit_costs$`cost parameter`=='c.xray' & unit_costs$setting=='Brazil']-unit_costs$parm1[unit_costs$`cost parameter`=='c.xray' & unit_costs$setting=='Brazil'])/4 * 
    exch$exch[exch$year==year_of_cost & exch$`Country Name`=='Brazil'] *
    defl$defl[defl$year=='2020' & defl$`Country Name`=='Brazil']/ defl$defl[defl$year==year_of_cost & defl$`Country Name`=='Brazil'] /
   exch$exch[exch$`Country Name`=='Brazil' & exch$year==max(exch$year)] *
  (hhcm_costs$ppp[hhcm_costs$unit_cost=='c.qft.plus']/hhcm_costs$ppp[hhcm_costs$country=='Brazil'][1])

# c.ess.collection
year_of_cost <- unit_costs$Year[unit_costs$`cost parameter`=='c.ess.collection' & unit_costs$setting=='Uganda']
hhcm_costs$cost.m[hhcm_costs$unit_cost=='c.ess.collection'] <- 
    (unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.ess.collection' & unit_costs$setting=='Uganda']) *
    exch$exch[exch$year==year_of_cost & exch$`Country Name`=='Uganda'] *
    defl$defl[defl$year=='2020' & defl$`Country Name`=='Uganda']/ defl$defl[defl$year==year_of_cost & defl$`Country Name`=='Uganda'] /
   exch$exch[exch$`Country Name`=='Uganda' & exch$year==max(exch$year)] *
  (hhcm_costs$ppp[hhcm_costs$unit_cost=='c.ess.collection']/hhcm_costs$ppp[hhcm_costs$country=='Uganda'][1])

hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c.ess.collection'] <-
    (unit_costs$parm2[unit_costs$`cost parameter`=='c.ess.collection' & unit_costs$setting=='Uganda']-unit_costs$parm1[unit_costs$`cost parameter`=='c.ess.collection' & unit_costs$setting=='Uganda'])/4 * 
    exch$exch[exch$year==year_of_cost & exch$`Country Name`=='Uganda'] *
    defl$defl[defl$year=='2020' & defl$`Country Name`=='Uganda']/ defl$defl[defl$year==year_of_cost & defl$`Country Name`=='Uganda'] /
   exch$exch[exch$`Country Name`=='Uganda' & exch$year==max(exch$year)] *
  (hhcm_costs$ppp[hhcm_costs$unit_cost=='c.ess.collection']/hhcm_costs$ppp[hhcm_costs$country=='Uganda'][1])

# c.iss.collection
year_of_cost <- unit_costs$Year[unit_costs$`cost parameter`=='c.iss.collection' & unit_costs$setting=='World']
hhcm_costs$cost.m[hhcm_costs$unit_cost=='c.iss.collection'] <- 
    unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.iss.collection' & unit_costs$setting=='World'] *
    exch$exch[exch$year==year_of_cost & exch$`Country Code`=='USA'] *
    defl$defl[defl$year=='2020' & defl$`Country Code`=='USA']/ defl$defl[defl$year==year_of_cost & defl$`Country Code`=='USA'] /
   exch$exch[exch$`Country Name`=='United States' & exch$year==max(exch$year)] *
  (hhcm_costs$ppp[hhcm_costs$unit_cost=='c.iss.collection']/hhcm_costs$ppp[hhcm_costs$iso3=='USA'][1])

hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c.iss.collection'] <- 
    (unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.iss.collection' & unit_costs$setting=='World']*150/100-unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.iss.collection' & unit_costs$setting=='World']*50/100)/4 *
    exch$exch[exch$year==year_of_cost & exch$`Country Code`=='USA'] *
    defl$defl[defl$year=='2020' & defl$`Country Code`=='USA']/ defl$defl[defl$year==year_of_cost & defl$`Country Code`=='USA'] /
   exch$exch[exch$`Country Name`=='United States' & exch$year==max(exch$year)] *
  (hhcm_costs$ppp[hhcm_costs$unit_cost=='c.iss.collection']/hhcm_costs$ppp[hhcm_costs$iso3=='USA'][1])

# Sample transportation shared with other programs
year_of_cost <- unit_costs$Year[unit_costs$`cost parameter`=='c.sample.transport.shared' & unit_costs$setting=='Uganda']
hhcm_costs$cost.m[hhcm_costs$unit_cost=='c.sample.transport.shared'] <-
    (unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.sample.transport.shared' & unit_costs$setting=='Uganda']) * 
    exch$exch[exch$year==year_of_cost & exch$`Country Name`=='Uganda'] *
    defl$defl[defl$year=='2020' & defl$`Country Name`=='Uganda']/ defl$defl[defl$year==year_of_cost & defl$`Country Name`=='Uganda'] /
   exch$exch[exch$`Country Name`=='Uganda' & exch$year==max(exch$year)] *
  (hhcm_costs$ppp[hhcm_costs$unit_cost=='c.sample.transport.shared']/hhcm_costs$ppp[hhcm_costs$country=='Uganda'][1])

hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c.sample.transport.shared'] <-
    unit_costs$parm1[unit_costs$`cost parameter`=='c.sample.transport.shared' & unit_costs$setting=='Uganda'] *
    exch$exch[exch$year==year_of_cost & exch$`Country Name`=='Uganda'] *
    defl$defl[defl$year=='2020' & defl$`Country Name`=='Uganda']/ defl$defl[defl$year==year_of_cost & defl$`Country Name`=='Uganda'] /
   exch$exch[exch$`Country Name`=='Uganda' & exch$year==max(exch$year)] *
  (hhcm_costs$ppp[hhcm_costs$unit_cost=='c.sample.transport.shared']/hhcm_costs$ppp[hhcm_costs$country=='Uganda'][1])

# Sample transportation exclusive to the TB program
year_of_cost <- unit_costs$Year[unit_costs$`cost parameter`=='c.sample.transport.exclusive' & unit_costs$setting=='Uganda']
hhcm_costs$cost.m[hhcm_costs$unit_cost=='c.sample.transport.exclusive'] <-
    (unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.sample.transport.exclusive' & unit_costs$setting=='Uganda']) *
    exch$exch[exch$year==year_of_cost & exch$`Country Name`=='Uganda'] *
    defl$defl[defl$year=='2020' & defl$`Country Name`=='Uganda']/ defl$defl[defl$year==year_of_cost & defl$`Country Name`=='Uganda'] /
   exch$exch[exch$`Country Name`=='Uganda' & exch$year==max(exch$year)] *
  (hhcm_costs$ppp[hhcm_costs$unit_cost=='c.sample.transport.exclusive']/hhcm_costs$ppp[hhcm_costs$country=='Uganda'][1])

hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c.sample.transport.exclusive'] <- 
    unit_costs$parm1[unit_costs$`cost parameter`=='c.sample.transport.exclusive' & unit_costs$setting=='Uganda'] *
    exch$exch[exch$year==year_of_cost & exch$`Country Name`=='Uganda'] *
    defl$defl[defl$year=='2020' & defl$`Country Name`=='Uganda']/ defl$defl[defl$year==year_of_cost & defl$`Country Name`=='Uganda'] /
   exch$exch[exch$`Country Name`=='Uganda' & exch$year==max(exch$year)] *
  (hhcm_costs$ppp[hhcm_costs$unit_cost=='c.sample.transport.exclusive']/hhcm_costs$ppp[hhcm_costs$country=='Uganda'][1])

# Xpert test
year_of_cost <- unit_costs$Year[unit_costs$`cost parameter`=='c.xpert' & unit_costs$setting=='India']
hhcm_costs$cost.m[hhcm_costs$unit_cost=='c.xpert'] <-
    (unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.xpert' & unit_costs$setting=='India'])[1] *
    exch$exch[exch$year==year_of_cost & exch$`Country Name`=='India'] *
    defl$defl[defl$year=='2020' & defl$`Country Name`=='India']/ defl$defl[defl$year==year_of_cost & defl$`Country Name`=='India'] /
   exch$exch[exch$`Country Name`=='India' & exch$year==max(exch$year)] *
  (hhcm_costs$ppp[hhcm_costs$unit_cost=='c.xpert']/hhcm_costs$ppp[hhcm_costs$country=='India'][1])

hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c.xpert'] <-
    (unit_costs$parm2[unit_costs$`cost parameter`=='c.xpert' & unit_costs$setting=='India'][1]-unit_costs$parm1[unit_costs$`cost parameter`=='c.xpert' & unit_costs$setting=='India'][1])/4 *
    exch$exch[exch$year==year_of_cost & exch$`Country Name`=='India'] *
    defl$defl[defl$year=='2020' & defl$`Country Name`=='India']/ defl$defl[defl$year==year_of_cost & defl$`Country Name`=='India'] /
   exch$exch[exch$`Country Name`=='India' & exch$year==max(exch$year)] *
  (hhcm_costs$ppp[hhcm_costs$unit_cost=='c.xpert']/hhcm_costs$ppp[hhcm_costs$country=='India'][1])

# iss + xpert
hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_xpert_test.04'] <- 
    hhcm_costs$cost.m[hhcm_costs$unit_cost=='c.xpert'] + hhcm_costs$cost.m[hhcm_costs$unit_cost=='c.iss.collection']

hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_xpert_test.04'] <-
    sqrt(hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c.xpert']^2 + hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c.iss.collection']^2)

# sess + xpert
hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_xpert_test.514'] <-
    hhcm_costs$cost.m[hhcm_costs$unit_cost=='c.xpert'] + hhcm_costs$cost.m[hhcm_costs$unit_cost=='c.ess.collection']

hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_xpert_test.514'] <-
    sqrt(hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c.xpert']^2 + hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c.ess.collection']^2)

# lfts
year_of_cost <- unit_costs$Year[unit_costs$`cost parameter`=='c.lft' & unit_costs$setting=='Brazil']
hhcm_costs$cost.m[hhcm_costs$unit_cost=='c.lft'] <-
    (unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.lft' & unit_costs$setting=='Brazil']) *
    exch$exch[exch$year==year_of_cost & exch$`Country Name`=='Brazil'] *
    defl$defl[defl$year=='2020' & defl$`Country Name`=='Brazil']/ defl$defl[defl$year==year_of_cost & defl$`Country Name`=='Brazil'] /
   exch$exch[exch$`Country Name`=='Brazil' & exch$year==max(exch$year)] *
  (hhcm_costs$ppp[hhcm_costs$unit_cost=='c.lft']/hhcm_costs$ppp[hhcm_costs$country=='Brazil'][1])

hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c.lft'] <-
    (unit_costs$parm2[unit_costs$`cost parameter`=='c.lft' & unit_costs$setting=='Brazil']-unit_costs$parm1[unit_costs$`cost parameter`=='c.lft' & unit_costs$setting=='Brazil'])/4 *
    exch$exch[exch$year==year_of_cost & exch$`Country Name`=='Brazil'] *
    defl$defl[defl$year=='2020' & defl$`Country Name`=='Brazil']/ defl$defl[defl$year==year_of_cost & defl$`Country Name`=='Brazil'] /
   exch$exch[exch$`Country Name`=='Brazil' & exch$year==max(exch$year)] *
  (hhcm_costs$ppp[hhcm_costs$unit_cost=='c.lft']/hhcm_costs$ppp[hhcm_costs$country=='Brazil'][1])

# cbc
year_of_cost <- unit_costs$Year[unit_costs$`cost parameter`=='c.cbc' & unit_costs$setting=='Brazil']
hhcm_costs$cost.m[hhcm_costs$unit_cost=='c.cbc'] <-
    (unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.cbc' & unit_costs$setting=='Brazil']) *
    exch$exch[exch$year==year_of_cost & exch$`Country Name`=='Brazil'] *
    defl$defl[defl$year=='2020' & defl$`Country Name`=='Brazil']/ defl$defl[defl$year==year_of_cost & defl$`Country Name`=='Brazil'] /
   exch$exch[exch$`Country Name`=='Brazil' & exch$year==max(exch$year)] *
  (hhcm_costs$ppp[hhcm_costs$unit_cost=='c.cbc']/hhcm_costs$ppp[hhcm_costs$country=='Brazil'][1])

hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c.cbc'] <-
    (unit_costs$parm2[unit_costs$`cost parameter`=='c.cbc' & unit_costs$setting=='Brazil']-unit_costs$parm1[unit_costs$`cost parameter`=='c.cbc' & unit_costs$setting=='Brazil'])/4 *
    exch$exch[exch$year==year_of_cost & exch$`Country Name`=='Brazil'] *
    defl$defl[defl$year=='2020' & defl$`Country Name`=='Brazil']/ defl$defl[defl$year==year_of_cost & defl$`Country Name`=='Brazil'] /
   exch$exch[exch$`Country Name`=='Brazil' & exch$year==max(exch$year)] *
  (hhcm_costs$ppp[hhcm_costs$unit_cost=='c.cbc']/hhcm_costs$ppp[hhcm_costs$country=='Brazil'][1])

# dstb treatment
# NTP programme expenditures - estimated based on WHO data. Imputed using regression for countries with no estimates
# visits & hospitalizations -  - estimated based on WHO resource utilization data + regression.
# drug costs - GDG unit costs

# NTP programme expenditures per person treated for TB
ntp.m <- ifelse(is.na(hhcm_costs$exp_dstb[unique(hhcm_costs$country)]),
                exp(predict(llr.ntpcst, data.frame(log.gdp=log(hhcm_costs$gdp.pp[hhcm_costs$unit_cost=='c_dstb_tx'])))),
                hhcm_costs$exp_dstb[unique(hhcm_costs$country)])
ntp.lo <- ifelse(is.na(hhcm_costs$exp_dstb[unique(hhcm_costs$country)]),
                 exp(predict(llr.ntpcst, data.frame(log.gdp=log(hhcm_costs$gdp.pp[hhcm_costs$unit_cost=='c_dstb_tx'])), interval = 'confidence'))[,2],
                 hhcm_costs$exp_dstb[unique(hhcm_costs$country)]*0.5)
ntp.hi <- ifelse(is.na(hhcm_costs$exp_dstb[unique(hhcm_costs$country)]),
                 exp(predict(llr.ntpcst, data.frame(log.gdp=log(hhcm_costs$gdp.pp[hhcm_costs$unit_cost=='c_dstb_tx'])), interval = 'confidence'))[,3],
                 hhcm_costs$exp_dstb[unique(hhcm_costs$country)]*1.5)
ntp.sd <- (ntp.hi-ntp.lo)/4

# visits
c.opd_dstb.m <- ifelse(is.na(hhcm_costs$c.opd_dstb.m[unique(hhcm_costs$country)]) | hhcm_costs$c.opd_dstb.m[unique(hhcm_costs$country)]==0,
                       exp(predict(llr.carecst.dstb.opd, data.frame(gdp_log=log(hhcm_costs$gdp.pp[hhcm_costs$unit_cost=='c_dstb_tx'])))),
                       hhcm_costs$c.opd_dstb.m[unique(hhcm_costs$country)])
c.opd_dstb.lo <- ifelse(is.na(hhcm_costs$c.opd_dstb.hi[unique(hhcm_costs$country)]) | hhcm_costs$c.opd_dstb.lo[unique(hhcm_costs$country)]==0,
                        exp(predict(llr.carecst.dstb.opd, data.frame(gdp_log=log(hhcm_costs$gdp.pp[hhcm_costs$unit_cost=='c_dstb_tx'])), interval = 'confidence'))[,2],
                        hhcm_costs$c.opd_dstb.lo)
c.opd_dstb.hi <- ifelse(is.na(hhcm_costs$c.opd_dstb.hi[unique(hhcm_costs$country)]) | hhcm_costs$c.opd_dstb.hi[unique(hhcm_costs$country)]==0,
                        exp(predict(llr.carecst.dstb.opd, data.frame(gdp_log=log(hhcm_costs$gdp.pp[hhcm_costs$unit_cost=='c_dstb_tx'])), interval = 'confidence'))[,3],
                        hhcm_costs$c.opd_dstb.hi)
c.opd_dstb.sd <- (c.opd_dstb.hi-c.opd_dstb.lo)/4

# hospitalizations
options(scipen=999)

c.ipd_dstb.m <- ifelse(is.na(hhcm_costs$c.ipd_dstb.m[unique(hhcm_costs$country)]) | hhcm_costs$c.ipd_dstb.m[unique(hhcm_costs$country)]==0,
                       exp(predict(llr.carecst.dstb.ipd, data.frame(gdp_log=log(hhcm_costs$gdp.pp[hhcm_costs$unit_cost=='c_dstb_tx'])))),
                       hhcm_costs$c.ipd_dstb.m)
c.ipd_dstb.lo <- ifelse(is.na(hhcm_costs$c.ipd_dstb.lo[unique(hhcm_costs$country)]) | hhcm_costs$c.ipd_dstb.lo[unique(hhcm_costs$country)]==0,
                        exp(predict(llr.carecst.dstb.ipd, data.frame(gdp_log=log(hhcm_costs$gdp.pp[hhcm_costs$unit_cost=='c_dstb_tx'])), interval = 'confidence'))[,2],
                        hhcm_costs$c.ipd_dstb.lo[unique(hhcm_costs$country)])
c.ipd_dstb.hi <- ifelse(is.na(hhcm_costs$c.ipd_dstb.hi[unique(hhcm_costs$country)]) | hhcm_costs$c.ipd_dstb.hi[unique(hhcm_costs$country)]==0,
                        exp(predict(llr.carecst.dstb.ipd, data.frame(gdp_log=log(hhcm_costs$gdp.pp[hhcm_costs$unit_cost=='c_dstb_tx'])), interval = 'confidence'))[,3],
                        hhcm_costs$c.ipd_dstb.hi[unique(hhcm_costs$country)])
c.ipd_dstb.sd <- (c.ipd_dstb.hi-c.ipd_dstb.lo)/4

# Add visits & hospitalizations
vh_dstb <- c.opd_dstb.m + c.ipd_dstb.m
vh_dstb.sd <- sqrt(c.opd_dstb.sd^2 + c.ipd_dstb.sd^2)

# add NTP programme costs
prog_vh_dstb <-(ntp.m + vh_dstb)
prog_vh_dstb.sd <- sqrt(ntp.sd^2+vh_dstb.sd^2)

# Finally adding drug costs
c.dstb.drugs.sd <- (unit_costs$parm2[unit_costs$`cost parameter`=='c.dstb.drugs' & unit_costs$setting=='World']-unit_costs$parm1[unit_costs$`cost parameter`=='c.dstb.drugs' & unit_costs$setting=='World'])/4

hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_dstb_tx'] <-(prog_vh_dstb + unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.dstb.drugs' & unit_costs$setting=='World'])
hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_dstb_tx'] <- sqrt(prog_vh_dstb.sd^2+c.dstb.drugs.sd^2)

# MDR TB treatment

# NTP programme expenditures - estimated based on WHO data. Imputed using regression for countries with no estimates
# visits & hospitalizations -  - estimated based on WHO resource utilization data + regression.
# drug costs - GDG unit costs

# NTP programme expenditures per person treated for MDRTB
ntp_mdrtb.m <- ifelse(is.na(hhcm_costs$exp_mdrtb[unique(hhcm_costs$country)]),
                      exp(predict(llr.ntpcst.mdrtb, data.frame(log.gdp=log(hhcm_costs$gdp.pp[hhcm_costs$unit_cost=='c_dstb_tx'])))),
                      (hhcm_costs$exp_mdrtb[unique(hhcm_costs$country)]))
ntp_mdrtb.lo <- ifelse(is.na(hhcm_costs$exp_mdrtb[unique(hhcm_costs$country)]),
                       exp(predict(llr.ntpcst.mdrtb, data.frame(log.gdp=log(hhcm_costs$gdp.pp[hhcm_costs$unit_cost=='c_dstb_tx'])), interval = 'confidence'))[,2],
                       (hhcm_costs$exp_mdrtb[unique(hhcm_costs$country)])*0.5)
ntp_mdrtb.hi <- ifelse(is.na((hhcm_costs$exp_mdrtb[unique(hhcm_costs$country)])),
                       exp(predict(llr.ntpcst.mdrtb, data.frame(log.gdp=log(hhcm_costs$gdp.pp[hhcm_costs$unit_cost=='c_dstb_tx'])), interval = 'confidence'))[,3],
                       (hhcm_costs$exp_mdrtb[unique(hhcm_costs$country)])*1.5)
ntp_mdrtb.sd <- (ntp_mdrtb.hi-ntp_mdrtb.lo)/4

# visits
c.opd_mdrtb.m <- ifelse(is.na(hhcm_costs$c.opd_mdrtb.m[unique(hhcm_costs$country)]) | hhcm_costs$c.opd_mdrtb.m[unique(hhcm_costs$country)]==0,
                        exp(predict(llr.carecst.mdrtb.opd, data.frame(gdp_log=log(hhcm_costs$gdp.pp[hhcm_costs$unit_cost=='c_dstb_tx'])))),
                        hhcm_costs$c.opd_mdrtb.m[unique(hhcm_costs$country)])
c.opd_mdrtb.lo <- ifelse(is.na(hhcm_costs$c.opd_mdrtb.lo[unique(hhcm_costs$country)]) | hhcm_costs$c.opd_mdrtb.lo[unique(hhcm_costs$country)]==0,
                         exp(predict(llr.carecst.mdrtb.opd, data.frame(gdp_log=log(hhcm_costs$gdp.pp[hhcm_costs$unit_cost=='c_dstb_tx'])), interval = 'confidence'))[,2],
                         hhcm_costs$c.opd_mdrtb.lo[unique(hhcm_costs$country)])
c.opd_mdrtb.hi <- ifelse(is.na(hhcm_costs$c.opd_mdrtb.hi[unique(hhcm_costs$country)]) | hhcm_costs$c.opd_mdrtb.hi[unique(hhcm_costs$country)]==0,
                         exp(predict(llr.carecst.mdrtb.opd, data.frame(gdp_log=log(hhcm_costs$gdp.pp[hhcm_costs$unit_cost=='c_dstb_tx'])), interval = 'confidence'))[,3],
                         hhcm_costs$c.opd_mdrtb.hi[unique(hhcm_costs$country)])
c.opd_mdrtb.sd <- (c.opd_mdrtb.hi-c.opd_mdrtb.lo)/4

# hospitalizations
options(scipen=999)

c.ipd_mdrtb.m <- ifelse(is.na(hhcm_costs$c.ipd_mdrtb.m[unique(hhcm_costs$country)]) | hhcm_costs$c.ipd_mdrtb.m[unique(hhcm_costs$country)]==0,
                        exp(predict(llr.carecst.dstb.ipd, data.frame(gdp_log=log(hhcm_costs$gdp.pp[hhcm_costs$unit_cost=='c_dstb_tx'])))),
                        hhcm_costs$c.ipd_mdrtb.m[unique(hhcm_costs$country)])
c.ipd_mdrtb.lo <- ifelse(is.na(hhcm_costs$c.ipd_mdrtb.lo[unique(hhcm_costs$country)]) | hhcm_costs$c.ipd_mdrtb.lo[unique(hhcm_costs$country)]==0,
                         exp(predict(llr.carecst.dstb.ipd, data.frame(gdp_log=log(hhcm_costs$gdp.pp[hhcm_costs$unit_cost=='c_dstb_tx'])), interval = 'confidence'))[,2],
                         hhcm_costs$c.ipd_mdrtb.lo[unique(hhcm_costs$country)])
c.ipd_mdrtb.hi <- ifelse(is.na(hhcm_costs$c.ipd_mdrtb.hi[unique(hhcm_costs$country)]) | hhcm_costs$c.ipd_mdrtb.hi[unique(hhcm_costs$country)]==0,
                         exp(predict(llr.carecst.dstb.ipd, data.frame(gdp_log=log(hhcm_costs$gdp.pp[hhcm_costs$unit_cost=='c_dstb_tx'])), interval = 'confidence'))[,3],
                         hhcm_costs$c.ipd_mdrtb.hi[unique(hhcm_costs$country)])
c.ipd_mdrtb.sd <- (c.ipd_mdrtb.hi-c.ipd_mdrtb.lo)/4

# Add visits & hospitalizations
vh_mdrtb <- c.opd_mdrtb.m + c.ipd_mdrtb.m
vh_mdrtb.sd <- sqrt(c.opd_mdrtb.sd^2 + c.ipd_mdrtb.sd^2)

# add NTP programme costs
prog_vh_mdrtb <-(ntp_mdrtb.m + vh_mdrtb)
prog_vh_mdrtb.sd <- sqrt(ntp_mdrtb.sd^2+vh_mdrtb.sd^2)

# Finally adding drug costs

# Drug costs - based on GDG unit costs

tx.dur1 <- 12*30 # 12 months, 30 days per month
tx.dur2 <- 15*30 # 12 months, 30 days per month

# Regimen 1 for Children <5 years with fluoroquinolone resistant MDR/RR-TB: 12 months Bdq-Lzd-Cfz-Cs;
c.bdq1 <- 14/tx.dur1*unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.bdq1.2wks'] + (tx.dur1-14)/tx.dur1*unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.bdq1.2wks+']
c.bdq1.sd <- (14/tx.dur1*unit_costs$parm2[unit_costs$`cost parameter`=='c.bdq1.2wks'] + (tx.dur1-14)/tx.dur1*unit_costs$parm2[unit_costs$`cost parameter`=='c.bdq1.2wks+']-14/tx.dur1*unit_costs$parm1[unit_costs$`cost parameter`=='c.bdq1.2wks'] + (tx.dur1-14)/tx.dur1*unit_costs$parm1[unit_costs$`cost parameter`=='c.bdq1.2wks+'])/4

c.mdrtb.drugs1.04 <- tx.dur1*(colSums(unit_costs %>% filter(unit_costs$`cost parameter` %in% c('c.cs1', 'c.cfz1','c.lin1')) %>% select(`unit cost`))+c.bdq1)
c.mdrtb.drugs1.04.sd <- sqrt(c.bdq1.sd^2 + (tx.dur1*(colSums(unit_costs %>% filter(unit_costs$`cost parameter` %in% c('c.cs1', 'c.cfz1','c.lin1')) %>% select(parm2))-colSums(unit_costs %>% filter(unit_costs$`cost parameter` %in% c('c.cs1', 'c.cfz1','c.lin1')) %>% select(parm1)))/4)^2)

# Regimen 2 for Children <5 years with fluoroquinolone susceptible MDR/RR-TB: 12 months Bdq-Lfx-Lzd-Cfz
c.bdq2 <- 14/tx.dur1*unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.bdq2.2wks'] + (tx.dur1-14)/tx.dur1*unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.bdq2.2wks+']
c.bdq2.sd <- (14/tx.dur1*unit_costs$parm2[unit_costs$`cost parameter`=='c.bdq2.2wks'] + (tx.dur1-14)/tx.dur1*unit_costs$parm2[unit_costs$`cost parameter`=='c.bdq2.2wks+']-14/tx.dur1*unit_costs$parm1[unit_costs$`cost parameter`=='c.bdq2.2wks'] + (tx.dur1-14)/tx.dur1*unit_costs$parm1[unit_costs$`cost parameter`=='c.bdq2.2wks+'])/4

c.mdrtb.drugs2.04 <- tx.dur1*(colSums(unit_costs %>% filter(unit_costs$`cost parameter` %in% c('c.lvx1', 'c.cfz1','c.lin1')) %>% select(`unit cost`))+c.bdq1)
c.mdrtb.drugs2.04.sd <- sqrt(c.bdq1.sd^2 + (tx.dur1*(colSums(unit_costs %>% filter(unit_costs$`cost parameter` %in% c('c.lvx1', 'c.cfz1','c.lin1')) %>% select(parm2))-colSums(unit_costs %>% filter(unit_costs$`cost parameter` %in% c('c.lvx1', 'c.cfz1','c.lin1')) %>% select(parm1)))/4)^2)

# Regimen 3 for Children 5-15 years with fluoroquinolone resistant MDR/RR-TB: 15 months Bdq-Lzd-Cs-Cfz
c.mdrtb.drugs1.514 <- tx.dur2*(colSums(unit_costs %>% filter(unit_costs$`cost parameter` %in% c('c.cs2', 'c.cfz2','c.lin2')) %>% select(`unit cost`))+c.bdq2)
c.mdrtb.drugs1.514.sd <- sqrt(c.bdq2.sd^2 + (tx.dur2*(colSums(unit_costs %>% filter(unit_costs$`cost parameter` %in% c('c.cs2', 'c.cfz2','c.lin2')) %>% select(parm2))-colSums(unit_costs %>% filter(unit_costs$`cost parameter` %in% c('c.cs2', 'c.cfz2','c.lin2')) %>% select(parm1)))/4)^2)

# Regimen 4 for Children 5-15 years with fluoroquinolone susceptible MDR/RR-TB: 15 months Bdq-Lzd-Lfx-Cfz
c.mdrtb.drugs2.514 <- tx.dur2*(colSums(unit_costs %>% filter(unit_costs$`cost parameter` %in% c('c.lvx2', 'c.cfz2','c.lin2')) %>% select(`unit cost`))+c.bdq2)
c.mdrtb.drugs2.514.sd <- sqrt(c.bdq2.sd^2 + (tx.dur2*(colSums(unit_costs %>% filter(unit_costs$`cost parameter` %in% c('c.lvx2', 'c.cfz2','c.lin2')) %>% select(parm2))-colSums(unit_costs %>% filter(unit_costs$`cost parameter` %in% c('c.lvx2', 'c.cfz2','c.lin2')) %>% select(parm1)))/4)^2)

# Finally adding drug costs
hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_mdrtb_tx1.04'] <- (prog_vh_mdrtb + c.mdrtb.drugs1.04)
hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_mdrtb_tx1.04'] <- sqrt(prog_vh_mdrtb.sd^2+c.mdrtb.drugs1.04.sd^2)

hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_mdrtb_tx2.04'] <- (prog_vh_mdrtb + c.mdrtb.drugs2.04)
hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_mdrtb_tx2.04'] <- sqrt(prog_vh_mdrtb.sd^2+c.mdrtb.drugs2.04.sd^2)

hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_mdrtb_tx1.514'] <- (prog_vh_mdrtb + c.mdrtb.drugs1.514)
hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_mdrtb_tx1.514'] <- sqrt(prog_vh_mdrtb.sd^2+c.mdrtb.drugs1.514.sd^2)

hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_mdrtb_tx2.514'] <- (prog_vh_mdrtb + c.mdrtb.drugs2.514)
hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_mdrtb_tx2.514'] <- sqrt(prog_vh_mdrtb.sd^2+c.mdrtb.drugs2.514.sd^2)

# TPT 
# Drugs, monthly visits, monthly lfts, adverse events
# TODO: Add adverse events +monitoring 
tpt_dur <- 6    # 6 months
tpt_days <- tpt_dur*30 # 6 months of 30 days each

# TPT INH 
hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_tpt_INH.04'] <- (tpt_days*unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.inh1'])
hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_tpt_INH.04'] <- (tpt_days*(unit_costs$parm2[unit_costs$`cost parameter`=='c.inh1']-unit_costs$parm1[unit_costs$`cost parameter`=='c.inh1'])/4)

hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_tpt_INH.514'] <- (tpt_days*unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.inh2'])
hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_tpt_INH.514'] <- (tpt_days*(unit_costs$parm2[unit_costs$`cost parameter`=='c.inh2']-unit_costs$parm1[unit_costs$`cost parameter`=='c.inh2'])/4)

# TPT follow-up
# TODO: Check how to propagate uncertainty
hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_tpt_fu'] <- (opd.mean*tpt_dur)
hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_tpt_fu'] <- opd.sd*tpt_dur

# TPT Bedaquiline 
hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_tpt_BDQ.04'] <- tpt_days*c.bdq1
hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_tpt_BDQ.04'] <- c.bdq1.sd*tpt_days

hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_tpt_BDQ.514'] <- tpt_days*c.bdq2
hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_tpt_BDQ.514'] <- c.bdq2.sd*tpt_days

# TPT Delamanid 
hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_tpt_DLM.04'] <- tpt_days*(unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.dlm1'])
hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_tpt_DLM.04'] <- (tpt_days*(unit_costs$parm2[unit_costs$`cost parameter`=='c.dlm1']-unit_costs$parm1[unit_costs$`cost parameter`=='c.dlm1'])/4)

hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_tpt_DLM.514'] <- tpt_days*(unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.dlm2'])
hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_tpt_DLM.514'] <- (tpt_days*(unit_costs$parm2[unit_costs$`cost parameter`=='c.dlm2']-unit_costs$parm1[unit_costs$`cost parameter`=='c.dlm2'])/4)

# TPT Levofloxacin 
hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_tpt_LVX.04'] <- tpt_days*(unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.lvx1']) 
hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_tpt_LVX.04'] <- (tpt_days*(unit_costs$parm2[unit_costs$`cost parameter`=='c.lvx1']-unit_costs$parm1[unit_costs$`cost parameter`=='c.lvx1'])/4)

hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_tpt_LVX.514'] <- tpt_days*(unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.lvx2'])
hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_tpt_LVX.514'] <- (tpt_days*(unit_costs$parm2[unit_costs$`cost parameter`=='c.lvx2']-unit_costs$parm1[unit_costs$`cost parameter`=='c.lvx2'])/4)

# TPT Moxifloxacin 
hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_tpt_MXF.04'] <- tpt_days*(unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.mxf1'])
hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_tpt_MXF.04'] <- (tpt_days*(unit_costs$parm2[unit_costs$`cost parameter`=='c.mxf1']-unit_costs$parm1[unit_costs$`cost parameter`=='c.mxf1'])/4)

hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_tpt_MXF.514'] <- tpt_days*(unit_costs$`unit cost`[unit_costs$`cost parameter`=='c.mxf2'])
hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_tpt_MXF.514'] <- (tpt_days*(unit_costs$parm2[unit_costs$`cost parameter`=='c.mxf2']-unit_costs$parm1[unit_costs$`cost parameter`=='c.mxf2'])/4)

# TPT monitoring 
# TODO: add ecg cost
hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_monit_INH'] <- hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_monit_FQ'] <- hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_monit_BDQ'] <- tpt_dur*hhcm_costs$cost.m[hhcm_costs$unit_cost=='c.lft']*hhcm_costs$ppp[hhcm_costs$unit_cost=='c.lft']/hhcm_costs$ppp[hhcm_costs$country=='Brazil'][1]

hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_monit_INH'] <- hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_monit_FQ'] <- hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_monit_BDQ'] <- tpt_dur*hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c.lft']*hhcm_costs$ppp[hhcm_costs$unit_cost=='c.lft']/hhcm_costs$ppp[hhcm_costs$country=='Brazil'][1]

# TPT adverse effects

# mild - one‐time outpatient visit and laboratory testing (complete blood count, electrolyte panel, urinalysis and liver function tests)
hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_aes_INH'] <- hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_aes_FQ'] <- hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_aes_BDQ'] <- (opd.mean + hhcm_costs$cost.m[hhcm_costs$unit_cost=='c.lft'] + hhcm_costs$cost.m[hhcm_costs$unit_cost=='c.cbc'])

hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_aes_INH'] <- hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_aes_FQ'] <- hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_aes_BDQ'] <- sqrt(opd.sd^2 + hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c.lft']^2 + hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c.cbc']^2)

# serious - seven days of hospitalization +  and laboratory testing (complete blood count, electrolyte panel, urinalysis and liver function tests)
hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_saes_INH'] <- hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_saes_FQ'] <- hhcm_costs$cost.m[hhcm_costs$unit_cost=='c_saes_BDQ'] <- (7*ipd.mean + hhcm_costs$cost.m[hhcm_costs$unit_cost=='c.lft'] + hhcm_costs$cost.m[hhcm_costs$unit_cost=='c.cbc'])

hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_saes_INH'] <- hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_saes_FQ'] <- hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c_saes_BDQ'] <- sqrt(ipd.sd^2 + hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c.lft']^2 + hhcm_costs$cost.sd[hhcm_costs$unit_cost=='c.cbc']^2)

# unit cost summaries
country_costs <- hhcm_costs %>% 
  filter(unit_cost %in% HHMDR_costnames) %>% 
  select(country, iso3, who.region, resource, unit_cost, cost.m, cost.sd) %>%
  mutate(cost.m=round(as.numeric(cost.m),2), cost.sd=round(as.numeric(cost.sd),2))

## NOTE uncomment below to save data
## fwrite(country_costs,file=here('output/country_unit_costs.csv')) 


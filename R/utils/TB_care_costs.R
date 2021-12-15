## rm(list = ls())
library(here)
library(tidyverse)
library(data.table)
library(scales)
library(viridis)
library(readxl)
library(httr)
# set_here('~/Dropbox/MDR HHCM')

# Download relevant data from https://www.who.int/teams/global-tuberculosis-programme/data

# Expenditure and utilization of health services for TB since fiscal year 2017 
if(!file.exists(here::here('indata/df_exp.Rdata'))){
  df_exp<- fread("https://extranet.who.int/tme/generateCSV.asp?ds=expenditure_utilisation", header = T)
  save(df_exp,file=here::here('indata/df_exp.Rdata'))
} else {
  load(here::here('indata/df_exp.Rdata'))
}

# # Budgets for TB since fiscal year 2018 - currently not being used much 
if(!file.exists(here::here('indata/df_bdg.Rdata'))){
  df_bdg<- fread("https://extranet.who.int/tme/generateCSV.asp?ds=budget", header = T)
  save(df_bdg,file=here::here('indata/df_bdg'))
} else {
  load(here::here('indata/df_bdg.Rdata'))
}

# Estimated number of incident cases (all forms) 
if(!file.exists(here::here('indata/df.Rdata'))){
  df<- fread("https://extranet.who.int/tme/generateCSV.asp?ds=estimates", header = T)
  save(df,file=here::here('indata/df.Rdata'))
} else {
  load(here::here('indata/df.Rdata'))
}

# Estimated number of incident cases (MDR TB) 
if(!file.exists(here::here('indata/df_mdr.Rdata'))){
  df_mdr<- fread("https://extranet.who.int/tme/generateCSV.asp?ds=mdr_rr_estimates", header = T)
  save(df_mdr,file=here::here('indata/df_mdr.Rdata'))
} else {
  load(here::here('indata/df_mdr.Rdata'))
}

# TB Data dictionary
if(!file.exists(here::here('indata/df_dic.Rdata'))){
  df_dic<-fread("https://extranet.who.int/tme/generateCSV.asp?ds=dictionary")  
  save(df_dic,file=here::here('indata/df_dic.Rdata'))
} else {
  load(here::here('indata/df_dic.Rdata'))
}


# load ISO country codes - for cleaning up 
load(here::here('indata/isodict.Rdata'))
code <- read_csv(here::here("indata", "all.csv")) # has more details on codes
code <- code %>% dplyr::rename(country=name, iso3=`alpha-3`) %>% select(country, `country-code`, iso3, region, `sub-region`)

# country names differ from those in WHO data
ISO$country <-as.character(ISO$country)
ISO$country[ISO$country=="Czech Republic"] <- "Czechia"
ISO$country[ISO$country=="Serbia & Montenegro"] <- "Serbia"
ISO$country[ISO$country=="Swaziland"] <- "Eswatini"
ISO$country[ISO$country=="The Former Yugoslav Republic of Macedonia"] <- "North Macedonia" 

SM <- code %>% filter(country %in% c("Serbia", "Montenegro")) %>% select(country, iso3, `sub-region`) 
names(SM)[3]<-"g_whoregion"
SM$g_whoregion[SM$g_whoregion=="Southern Europe"] <- "EUR"

df_ISO <- ISO %>% select(country, iso3, g_whoregion) 
df_ISO <- rbind(df_ISO,SM)
df_exp <- df_ISO %>% left_join(df_exp, by=c("country", "iso3", "g_whoregion"))
df_bdg <- df_ISO %>% left_join(df_bdg, by=c("country", "iso3", "g_whoregion"))

# Other relevant data - some are used later in the MDRHHCM unit costs code

# World Bank GDP data & Country and Lending Groups
indicators <- c('Country Name', 'Country Code', '2016', '2017', '2018', '2019', '2020')
url <- 'https://api.worldbank.org/v2/en/indicator/NY.GDP.MKTP.CD?downloadformat=excel'
GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
r.gdp <- setDT(readxl::read_excel(tf, sheet = 'Data', skip=3)) # GDP (current US$)
r.gdp <- r.gdp %>% select(indicators)

# World Bank GDP per capita data & Country and Lending Groups
url <- 'https://api.worldbank.org/v2/en/indicator/NY.GDP.PCAP.CD?downloadformat=excel'
GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
gdp <- setDT(readxl::read_excel(tf, sheet = 'Data', skip=3))
gdp <- gdp %>% select(indicators)
income_group <- setDT(readxl::read_excel(tf, sheet = 'Metadata - Countries', skip=0)) 
# income_group <- fread(here('indata', 'income.csv'), blank.lines.skip=TRUE) 
# world_bank <- data.table(read_csv(here('indata', 'gdppc_current.csv')))

# World Bank Exchange rates 
url <- 'https://api.worldbank.org/v2/en/indicator/PA.NUS.FCRF?downloadformat=excel'
GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
exch <- setDT(readxl::read_excel(tf, sheet = 'Data', skip=3)) # Official exchange rate (LCU per US$, period average) 
exch <- exch %>% select(indicators)

# World Bank Price level ratio of PPP conversion factor (GDP) to market exchange rate
url <- 'https://api.worldbank.org/v2/en/indicator/PA.NUS.PPP?downloadformat=excel'
GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
exch_ppp <- setDT(readxl::read_excel(tf, sheet = 'Data', skip=3)) # PPP conversion factor, GDP (LCU per international $)
exch_ppp <- exch_ppp %>% select(indicators)
# ppp <- setDT(read_csv(here('indata','ppp.csv')))

# World Bank Purchasing power parity conversion factors 
url <- 'https://api.worldbank.org/v2/en/indicator/PA.NUS.PPPC.RF?downloadformat=excel'
GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
ppp <- setDT(readxl::read_excel(tf, sheet = 'Data', skip=3)) # Price level ratio of PPP conversion factor (GDP) to market exchange rate
ppp <- ppp %>% select(indicators)
# ppp <- setDT(read_csv(here('indata','ppp.csv')))

# World Bank GDP deflators
url <- 'https://api.worldbank.org/v2/en/indicator/NY.GDP.DEFL.ZS?downloadformat=excel'
GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
defl <- setDT(readxl::read_excel(tf, sheet = 'Data', skip=3)) # GDP deflator (base year varies by country)
defl <- defl %>% select(`Country Name`, `Country Code`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`)
# ppp <- setDT(read_csv(here('indata','ppp.csv')))

un_gdp <- setDT(read_csv(here('indata', 'ungdppc.csv')))
# un_gdppp <- setDT(read_csv(here('indata', 'ungdppcpp.csv')))
tb_budget <- setDT(df_bdg)
df_exp <- setDT(df_exp)

#  Replace Missing Values with previous year data
# setnames(income_group, 'Economy', 'Country Name'); setnames(income_group, 'Code', 'Country Code'); 
r.gdp <- r.gdp %>% gather(year, r.gdp, 3:7) %>% arrange(`Country Name`, `Country Code`) %>% fill(r.gdp)
gdp <- gdp %>% gather(year, gdp, 3:7) %>% arrange(`Country Name`, `Country Code`) %>% fill(gdp)
exch <- exch %>% gather(year, exch, 3:7) %>% arrange(`Country Name`, `Country Code`) %>% fill(exch)
ppp <- ppp %>% gather(year, ppp, 3:7) %>% arrange(`Country Name`, `Country Code`) %>% fill(ppp) 
exch_ppp <- exch_ppp %>% gather(year, exch_ppp, 3:7) %>% arrange(`Country Name`, `Country Code`) %>% fill(exch_ppp) 
defl <- defl %>% gather(year, defl, 3:13) %>% arrange(`Country Name`, `Country Code`) %>% fill(defl)

gdp <- r.gdp %>% select(c('Country Name', 'Country Code',year, r.gdp)) %>% left_join(gdp, by=c('Country Name', 'Country Code', 'year'))
gdp <- exch %>% select(c('Country Name', 'Country Code',year, exch)) %>% left_join(gdp, by=c('Country Name', 'Country Code', 'year'))
gdp <- ppp %>% select(c('Country Name', 'Country Code',year, ppp)) %>% left_join(gdp, by=c('Country Name', 'Country Code', 'year'))
gdp <- exch_ppp %>% select(c('Country Name', 'Country Code',year, exch_ppp)) %>% left_join(gdp, by=c('Country Name', 'Country Code', 'year'))
# gdp <- gdp %>% select(`Country Name`, `Country Code`, year, r.gdp, gdp, exch, ppp)

# TODO: Use UN GDP per capita - looks more complete
# gdp <- un_gdp %>% mutate('Country Name'='Country or Area', year=Year, 'Indicator Name'=Item, gdp=)

# More cleaning up 

# gdp$`Country Name`[!gdp$`Country Code` %in% df_exp$iso3]

gdp <- gdp %>% filter(!`Country Name` %in%
                        c("Arab World","Caribbean small states","Central Europe and the Baltics","Channel Islands","Early-demographic dividend","East Asia & Pacific","East Asia & Pacific (excluding high income)","East Asia & Pacific (IDA & IBRD countries)","Euro area","Europe & Central Asia","Europe & Central Asia (excluding high income)","Europe & Central Asia (IDA & IBRD countries)","European Union","Faroe Islands","Fragile and conflict affected situations","Gibraltar","Heavily indebted poor countries (HIPC)","High income","IBRD only","IDA & IBRD total","IDA blend","IDA only","IDA total","Isle of Man","Kosovo","Late-demographic dividend","Latin America & Caribbean","Latin America & Caribbean (excluding high income)","Latin America & the Caribbean (IDA & IBRD countries)", "Least developed countries: UN classification","Liechtenstein","Low & middle income","Low income","Lower middle income","Middle East & North Africa","Middle East & North Africa (excluding high income)","Middle East & North Africa (IDA & IBRD countries)",    "Middle income","North America","Not classified" ,"OECD members","Other small states","Pacific island small states","Post-demographic dividend","Pre-demographic dividend","Small states","South Asia","South Asia (IDA & IBRD)","St. Martin (French part)","Sub-Saharan Africa","Sub-Saharan Africa (excluding high income)","Sub-Saharan Africa (IDA & IBRD countries)" ,"Upper middle income","Virgin Islands (U.S.)","World"))

# name_mismatch <- gdp$`Country Name`[!gdp$`Country Name` %in% df_exp$country]
# name_mismatch_codes <- gdp$`Country Code`[gdp$`Country Name` %in% name_mismatch]
# unique(df_exp$country[df_exp$iso3 %in% name_mismatch_codes])

# b<- df_ISO %>% filter(!country %in% births_2$country)
# births_1$country[grep("Czechia", births_1$country) ]
# df_ISO$country[grep("Greenland", df_ISO$country) ]
# Changing some country names to match the WHO TB data and ISO codes #

gdp$`Country Name` <- (plyr::mapvalues((gdp$`Country Name`), from = c("Bahamas, The","Bolivia","Congo, Dem. Rep.", "Congo, Rep.","Cote d'Ivoire","Curacao","Czech Republic","Egypt, Arab Rep.","Gambia, The","Hong Kong SAR, China","Iran, Islamic Rep.","Korea, Dem. People’s Rep.","Korea, Rep.","Kyrgyz Republic","Lao PDR","Macao SAR, China","Micronesia, Fed. Sts.","Moldova","Slovak Republic","St. Kitts and Nevis","St. Lucia","St. Vincent and the Grenadines", "Tanzania","United Kingdom","United States","Venezuela, RB","Vietnam","West Bank and Gaza","Yemen, Rep.")
  , to = c("Bahamas","Bolivia (Plurinational State of)","China, Hong Kong SAR","China, Macao SAR","Congo","CÃ´te d'Ivoire","CuraÃ§ao", "Czechia","Democratic People's Republic of Korea","Democratic Republic of the Congo" ,"Egypt","Gambia","Iran (Islamic Republic of)","Kyrgyzstan","Lao People's Democratic Republic" ,"Micronesia (Federated States of)","occupied Palestinian territory, including east Jerusalem", "Republic of Korea","Republic of Moldova","Saint Kitts and Nevis","Saint Lucia","Saint Vincent and the Grenadines","Slovakia","United Kingdom of Great Britain and Northern Ireland","United Republic of Tanzania","United States of America","Venezuela (Bolivarian Republic of)","Viet Nam","Yemen")
))

names <- unique(df_exp$country[!df_exp$iso3 %in% gdp$`Country Code`])
codes <- unique(df_exp$iso3[!df_exp$iso3 %in% gdp$`Country Code`])
years <- 2020
# r.gdp <- un_gdp$Value[un_gdp$`Country or Area` %in% names]

gdps <- un_gdp$Value[un_gdp$`Country or Area` %in% names]
# df_exp$country[(is.na(gdp$gdp))] <- names
# df_exp$country[is.na(df_exp$gdp)] <- c(25529, 21603, 13487, 5800, 6275, 12640) # value for Anguilla  for now
# unique(df_exp$country[!df_exp$country %in% gdp$`Country Name`])
# codes <- unique(df_exp$iso3[!df_exp$iso3 %in% gdp$`Country Code`])
# unique(world_bank$`Country Name`[world_bank$`Country Code` %in% codes])
# unique(world_bank$`Country Name`[world_bank$`Country Name` %in% names])

missing_gdp <- data.table("Country Name"=names, "Country Code"=codes, "year"=years, "ppp"=NA, "exch"=NA, "exch_ppp"=NA,"r.gdp"=NA,   "gdp"=gdps)
gdp <- rbind(gdp,missing_gdp)

gdp <- gdp %>% mutate(ppp=ifelse(is.na(ppp),1,ppp))

# a <- !(df_exp$country %in% gdp$`Country Name`)
# df_2 <- df_exp %>% dplyr::filter(!country %in% gdp$`Country Name`)
# df_3 <- df_exp %>% dplyr::filter(!iso3 %in% gdp$`Country Code`)

# budget data currently not being used in this analysis
tb_budget <- tb_budget[,c("country","iso2","iso3","iso_numeric","g_whoregion","year", "budget_cpp_dstb","budget_cpp_mdr","budget_cpp_tpt","budget_cpp_xdr",
                          "budget_fld","budget_lab","budget_mdrmgt","budget_orsrvy","budget_oth","budget_patsup","tx_dstb",	"tx_mdr",	"tx_tpt",	"tx_xdr")]

tb_budget[,bdstb:=rowSums(.SD), .SDcols= c("budget_fld","budget_lab","budget_mdrmgt","budget_orsrvy","budget_oth","budget_patsup")]

budget <- c('budget_cpp_dstb', 'budget_cpp_mdr', 'budget_cpp_tpt', 'budget_cpp_xdr', 'tx_dstb', 'tx_mdr', 'tx_tpt', 'tx_xdr')

# selecting the relevant expenditure categories
expend <- c('hcfvisit_dstb','hcfvisit_mdr', 'hosp_type_mdr','hospd_dstb_dur','hospd_dstb_prct','hospd_mdr_dur','hospd_mdr_prct',
            'exp_fld', 'exp_lab', 'exp_mdrmgt', 'exp_prog', 'exp_sld', 'exp_staff', 'exp_orsrvy', 'exp_oth')

# take a look at variable definitions using the package getTBinR which can be installed using devtools::install_github("seabbs/getTBinR")
# getTBinR::search_data_dict(var = expend)

tb_exp <- df_exp[,.(country, iso2, iso3, iso_numeric, g_whoregion, year, hcfvisit_dstb, hcfvisit_mdr, hosp_type_mdr, hospd_dstb_dur, hospd_dstb_prct, hospd_mdr_dur, hospd_mdr_prct, exp_fld, exp_lab, exp_mdrmgt, exp_prog,	exp_sld, exp_staff, exp_orsrvy, exp_patsup)]

tb_budget <- tb_budget[year==2019]
tb_exp <- tb_exp[year==2019]
gdp <- data.table(gdp)
gdp <- gdp[year==2020,]
tb_exp <- tb_budget %>% select(iso3,tx_dstb,tx_mdr,	tx_xdr) %>% left_join(tb_exp, by = "iso3")
gdp$iso3 <- gdp$`Country Code`

income_group$`Country Name`[income_group$`Country Name`=='Venezuela, RB'] <- 'Venezuela (Bolivarian Republic of)'
gdp <- income_group %>% 
  select(`Country Name`, `Country Code`, IncomeGroup) %>% 
  filter(`Country Code` %in% gdp$`Country Code`) %>% 
  right_join(gdp,  by=c('Country Name', 'Country Code'))

# gdp$gdp <- gdp$'2019'
tb_exp <- gdp %>% select(iso3, r.gdp, gdp,ppp, exch_ppp) %>% left_join(tb_exp, by = "iso3")
tb_exp <- df %>% filter(year==2019) %>% select(iso3, e_inc_num) %>% left_join(tb_exp, by = "iso3")
tb_exp <- df_mdr %>% select(iso3, e_inc_rr_num) %>% left_join(tb_exp, by = "iso3")

# search_data_dict(var = c("e_inc_num", "e_inc_rr_num"))
tb_exp[, e_inc_ds_num := ifelse(is.na(e_inc_num) & is.na(e_inc_rr_num), NA_real_, rowSums(.SD, na.rm = T)), .SDcols = c("e_inc_num", "e_inc_rr_num")]

expenditures <- c('exp_lab', 'exp_staff', 'exp_prog', 'exp_orsrvy', 'exp_patsup', 'exp_mdrmgt')
resource.use <- c('hcfvisit_dstb', 'hospd_dstb_prct', 'hospd_dstb_dur', 'hcfvisit_mdr', 'hospd_mdr_prct', 'hospd_mdr_dur', 'hosp_type_mdr')
patients <- c('tx_dstb', 'tx_mdr', 'tx_xdr', 'e_inc_ds_num', 'e_inc_rr_num')

# explore data
# missing.exp <- tb_exp %>% select(exp_lab,exp_staff, exp_prog, exp_orsrvy, exp_patsup, exp_mdrmgt)  %>%
#   gather(key = "key", value = "val") %>%
#   mutate(isna = is.na(val)) %>%
#   group_by(key) %>%
#   mutate(total = n()) %>%
#   group_by(key, total, isna) %>%
#   summarise(num.isna = n()) %>%
#   mutate(pct = num.isna / total * 100)
# 
# levels <-
#   (missing.exp  %>% filter(isna == T) %>% arrange(desc(pct)))$key
# 
# percentage.plot <- missing.exp %>%
#   ggplot() +
#   geom_bar(aes(x = reorder(key, desc(pct)), 
#                y = pct, fill=isna), 
#            stat = 'identity', alpha=0.8) +
#   scale_x_discrete(limits = levels) +
#   scale_fill_manual(name = "", 
#                     values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
#   coord_flip() +
#   labs(title = "Percentage of missing values", x =
#          'Variable', y = "% of missing values")
# 
# percentage.plot
# ggsave(plot=percentage.plot, filename = here('output','Missing NTP expenditures.png'), w=10,h=5, dpi = 600)
# 
# row.plot <- tb_exp %>% select(expenditures) %>%
#   mutate(id = row_number()) %>%
#   gather(-id, key = "key", value = "val") %>%
#   mutate(isna = is.na(val)) %>%
#   ggplot(aes(key, id, fill = isna)) +
#   geom_raster(alpha=0.8) +
#   scale_fill_manual(name = "",
#                     values = c('steelblue', 'tomato3'),
#                     labels = c("Present", "Missing")) +
#   scale_x_discrete(limits = levels) +
#   labs(x = "Variable",
#        y = "Row Number", title = "Missing values in rows") +
#   coord_flip()
# 
# row.plot
# ggsave(plot=row.plot, filename = here('output','Missing NTP expenditures1.png'), w=10,h=5, dpi = 600)
# 
# missing.res.use <- tb_exp %>% select(resource.use) %>%
#   gather(key = "key", value = "val") %>%
#   mutate(isna = is.na(val)) %>%
#   group_by(key) %>%
#   mutate(total = n()) %>%
#   group_by(key, total, isna) %>%
#   summarise(num.isna = n()) %>%
#   mutate(pct = num.isna / total * 100)
# 
# levels <-
#   (missing.res.use  %>% filter(isna == T) %>% arrange(desc(pct)))$key
# 
# percentage.plot <- missing.res.use %>%
#   ggplot() +
#   geom_bar(aes(x = reorder(key, desc(pct)), 
#                y = pct, fill=isna), 
#            stat = 'identity', alpha=0.8) +
#   scale_x_discrete(limits = levels) +
#   scale_fill_manual(name = "", 
#                     values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
#   coord_flip() +
#   labs(title = "Percentage of missing values", x =
#          'Variable', y = "% of missing values")
# 
# percentage.plot
# ggsave(plot=percentage.plot, filename = here('output','Missing resource use.png'), w=10,h=5, dpi = 600)
# 
# row.plot <- tb_exp %>% select(resource.use) %>%
#   mutate(id = row_number()) %>%
#   gather(-id, key = "key", value = "val") %>%
#   mutate(isna = is.na(val)) %>%
#   ggplot(aes(key, id, fill = isna)) +
#   geom_raster(alpha=0.8) +
#   scale_fill_manual(name = "",
#                     values = c('steelblue', 'tomato3'),
#                     labels = c("Present", "Missing")) +
#   scale_x_discrete(limits = levels) +
#   labs(x = "Variable",
#        y = "Row Number", title = "Missing values in rows") +
#   coord_flip()
# 
# row.plot
# ggsave(plot=row.plot, filename = here('output','Missing resource use1.png'), w=10,h=5, dpi = 600)

# summary(tb_exp %>% select(expenditures))
# sum(complete.cases(tb_exp %>% select(expenditures)))
exp_complete <- tb_exp %>% drop_na(exp_lab,exp_staff, exp_prog, exp_orsrvy, exp_patsup, exp_mdrmgt)
exp_complete <- setDT(exp_complete)
# 
# summary(tb_exp %>% select(resource.use))
# sum(complete.cases(tb_exp %>% select(resource.use)))

# impute resource use - impute missing data with mean by WHO Region
resources_complete <- tb_exp
resources_complete <- resources_complete %>% group_by(g_whoregion) %>%
  mutate(across(resource.use, ~ifelse(is.na(.x), mean(.x,na.rm=TRUE),.x))) %>%
  ungroup()

# summary(tb_exp %>% select(resource.use))
# summary(tb_exp %>% drop_na(hcfvisit_dstb, hospd_dstb_prct, hospd_dstb_dur, hcfvisit_mdr, hospd_mdr_prct, hospd_mdr_dur, hosp_type_mdr) %>% select(resource.use))
# summary(resources_complete %>% select(resource.use))

# (tb_exp %>% filter(across(resource.use, ~is.na(.x))) %>% filter(is.na(g_whoregion)) %>% select(iso3, iso2, country, g_whoregion)) 

resources_complete <- resources_complete %>% drop_na(hcfvisit_dstb, hospd_dstb_prct, hospd_dstb_dur, hcfvisit_mdr, hospd_mdr_prct, hospd_mdr_dur, hosp_type_mdr)
# sum(complete.cases(resources_complete %>% select(resource.use)))
resources_complete <- setDT(resources_complete)

# WHO-CHOICE country-specific costs for inpatient and outpatient health service delivery
# Based on https://resource-allocation.biomedcentral.com/articles/10.1186/s12962-018-0095-x
# Values of variables used for prediction of the unit cost
# Ownership is set to public provider; location is set to urban location for outpatient care

# Inpatient care
admission.l3 <- 4971
admission.l4.5 <- 14028
occupancy.l3 <- 0.756
occupancy.l4.5 <- 0.810
a.los.l3 <- 7.14
a.los.l4.5 <- 9.75

# Total outpatient visits per facility per year (p80)
opd.vsts.l1 <- 67656
opd.vsts.l2 <- 46434
opd.vsts.l3 <- 93739
opd.vsts.l4.5 <- 281156

# Visits per provider per day (p80)
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

# OPD visit unit costs
resources_complete[,c_opd.m:=exp(constant.opd + gdppc.opd*log(gdp) + visits*log(opd.vsts.l1) + vsts.per.provider*log(vsts.per.provider.fl1) + public.opd)]
resources_complete[,c_opd.lo:=exp(constant.opd.lo + gdppc.opd.lo*log(gdp) + visits.lo*log(opd.vsts.l1) + vsts.per.provider.lo*log(vsts.per.provider.fl1) + public.opd.lo)]
resources_complete[,c_opd.hi:=exp(constant.opd.hi + gdppc.opd.hi*log(gdp) + visits.hi*log(opd.vsts.l1) + vsts.per.provider.hi*log(vsts.per.provider.fl1) + public.opd.hi)]
resources_complete[,c_opd.sd:=(c_opd.hi-c_opd.lo)/4]

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

# TODO: To consider using data on hospital type used for MRD-TB hospitalization
# getTBinR::search_data_dict('hosp_type_mdr')
# table(tb_exp$g_whoregion,tb_exp$hosp_type_mdr)

# IPD stay unit costs
resources_complete[,c_ipd.m:=exp(constant.ipd + gdppc.ipd*log(gdp) + occupancy*log(occupancy.l3) + a.los*log(a.los.l3) + admissions*log(admission.l3) + district + public.ipd + Brazil.ipd)]
resources_complete[,c_ipd.lo:=exp(constant.ipd.lo + gdppc.ipd.lo*log(gdp) + occupancy.lo*log(occupancy.l3) + a.los.lo*log(a.los.l3) + admissions.lo*log(admission.l3) + district.lo + public.ipd.lo + Brazil.ipd.lo)]
resources_complete[,c_ipd.hi:=exp(constant.ipd.hi + gdppc.ipd.hi*log(gdp) + occupancy.hi*log(occupancy.l3) + a.los.hi*log(a.los.l3) + admissions.hi*log(admission.l3) + district.hi + public.ipd.hi + Brazil.ipd.hi)]
resources_complete[,c_ipd.sd:=(c_ipd.hi-c_ipd.lo)/4]

# OPD visit costs for DSTB
resources_complete[,c.opd_dstb.m:=hcfvisit_dstb * c_opd.m]
resources_complete[,c.opd_dstb.lo:=hcfvisit_dstb * c_opd.lo]
resources_complete[,c.opd_dstb.hi:=hcfvisit_dstb * c_opd.hi]
resources_complete[,c.opd_dstb.sd:=(c.opd_dstb.hi-c.opd_dstb.lo)/4]

# OPD visit costs for MDRTB
resources_complete[,c.opd_mdrtb.m:=hcfvisit_mdr * c_opd.m]
resources_complete[,c.opd_mdrtb.lo:=hcfvisit_mdr * c_opd.lo]
resources_complete[,c.opd_mdrtb.hi:=hcfvisit_mdr * c_opd.hi]
resources_complete[,c.opd_mdrtb.sd:=(c.opd_mdrtb.hi-c.opd_mdrtb.lo)/4]

# IPD stay costs for DSTB
resources_complete[,c.ipd_dstb.m:=hospd_dstb_prct/100*hospd_dstb_dur* c_ipd.m]
resources_complete[,c.ipd_dstb.lo:=hospd_dstb_prct/100*hospd_dstb_dur* c_ipd.lo]
resources_complete[,c.ipd_dstb.hi:=hospd_dstb_prct/100*hospd_dstb_dur* c_ipd.hi]
resources_complete[,c.ipd_dstb.sd:=(c.ipd_dstb.hi-c.ipd_dstb.lo)/4]

# IPD stay costs for MDRTB
resources_complete[,c.ipd_mdrtb.m:=hospd_mdr_prct/100*hospd_mdr_dur*c_ipd.m]
resources_complete[,c.ipd_mdrtb.lo:=hospd_mdr_prct/100*hospd_mdr_dur*c_ipd.lo]
resources_complete[,c.ipd_mdrtb.hi:=hospd_mdr_prct/100*hospd_mdr_dur*c_ipd.hi]
resources_complete[,c.ipd_mdrtb.sd:=(c.ipd_mdrtb.hi-c.ipd_mdrtb.lo)/4]

# NTP programme costs

# Correction for Russia as described in the Global TB report 2019
exp_complete[,exp_lab:=ifelse(country=='Russian Federation',exp_lab * 29/100, exp_lab)]
exp_complete[,exp_staff:=ifelse(country=='Russian Federation',exp_staff * 29/100, exp_staff)]
exp_complete[,exp_mdrmgt:=ifelse(country=='Russian Federation', (exp_mdrmgt + exp_lab * 71/100 + exp_staff * 71/100), exp_mdrmgt)]

# DSTB & MDRTB programme costs
exp_complete[,exp_dstb:=rowSums(.SD), .SDcol=c('exp_lab', 'exp_prog', 'exp_staff', 'exp_orsrvy', 'exp_patsup')]
exp_complete[,exp_mdrtb:=rowSums(.SD), .SDcol=c('exp_mdrmgt')]

# DSTB & MDRTB programme unit costs (dividing total costs by number of patients treated)
# Currently using Number of patients expected to start drug-susceptible TB treatment
exp_complete[,exp_dstb:=exp_dstb/e_inc_ds_num]
exp_complete[,exp_mdrtb:=exp_mdrtb/e_inc_rr_num]

# Drugs are excluded
# exp_complete[,dstb_drugs:=exp_fld/(e_inc_ds_num)]
# exp_complete[,mdrtb_drugs:=exp_sld/(e_inc_rr_num)]

prog_costs<- exp_complete[,.(country, iso3, g_whoregion, gdp, tx_dstb,tx_mdr, tx_xdr, exp_dstb, exp_mdrtb, e_inc_ds_num, e_inc_rr_num)]
prog_costs <- prog_costs %>% mutate_all(function(x) ifelse(is.infinite(x), 0, x))
prog_costs <- prog_costs %>% mutate_all(function(x) ifelse(is.nan(x), 0, x))
summary(prog_costs %>% select(exp_dstb, exp_mdrtb))

resources_complete <- data.table(resources_complete)
care_costs<- resources_complete[,.(country, iso3, g_whoregion, gdp, tx_dstb,tx_mdr, tx_xdr,c.opd_dstb.m, c.ipd_dstb.m, c.opd_mdrtb.m, c.ipd_mdrtb.m, e_inc_ds_num, e_inc_rr_num)]

summary(care_costs %>% select(c.opd_dstb.m, c.ipd_dstb.m, c.opd_mdrtb.m, c.ipd_mdrtb.m))

# NTP program cost regression fitting - to be used for countries with missing expenditure data
ntpcst <- prog_costs %>% select(country, g_whoregion, e_inc_ds_num, e_inc_rr_num, gdp, exp_dstb, exp_mdrtb) 
summary(ntpcst %>% select(exp_dstb, exp_mdrtb))

exp <- c('exp_dstb', 'exp_mdrtb')
# (ntpcst %>% filter(across(exp, ~is.na(.x))))

ntpcst <- ntpcst[!(exp_dstb==0)]; ntpcst <- ntpcst[!(exp_mdrtb==0)]
ntpcst <- ntpcst %>% gather(tb, ntpexp, exp_dstb, exp_mdrtb)
ntpcst<-setDT(ntpcst)
ntpcst[,log.gdp:=log(gdp)]; ntpcst[,log.ntpcst:=log(ntpexp)];
ntpcst[,tb:=ifelse(tb=='exp_dstb', 'dstb', 'mdrtb')];
tb <- c('dstb', 'mdrtb')
plot.labels <- c(dstb='Drug-susceptible TB (DS TB)', mdrtb='Multidrug-resistant TB (MDR TB)')


ntpcst <- ntpcst[!is.na(log.ntpcst)]
ntpcst <- ntpcst[!(log.ntpcst=='Inf')]

# Log-Log linear regression
# Relationship between gdp and costs “look more normal” after transformation.
# The relationship between the log transformed variables is more linear

# log(cost) = β0 + β1. log(gdp)
llr.ntpcst <- lm(log.ntpcst~log.gdp, ntpcst)
llr.ntpcst.dstb <- lm(log.ntpcst~log.gdp, subset(ntpcst, tb=='dstb'))
llr.ntpcst.mdrtb <- lm(log.ntpcst~log.gdp, subset(ntpcst, tb=='mdrtb'))
# summary(llr.ntpcst.dstb)
# summary(llr.ntpcst.mdrtb)

# Ploting the log-log regression model results in the original scales
ntp.data = data.table(x = exp(llr.ntpcst$model$log.gdp),
                  y = exp(predict(llr.ntpcst)), 
                  lwr=exp(predict(llr.ntpcst, interval = "confidence")[,2]), 
                  upr = exp(predict(llr.ntpcst, interval = "confidence")[,3]))

names(ntp.data) <- c("gdp1","cstntp",'lwr', 'upr')
ntp.data <- cbind(ntpcst, ntp.data)

plot <- ggplot(ntp.data, aes(x = gdp, y = ntpexp))  +
  geom_jitter() +
  # geom_point(aes(size=tx_mdr+tx_xdr, fill=g_whoregion), alpha = 0.75, shape = 21) +
  # geom_smooth(span   = 1,    color = viridis(1, begin = 0.6), se = FALSE, linetype =
  #               "dashed")  +
  geom_line(data = ntp.data, aes(x = gdp, y = cstntp),
            color = viridis::viridis(1, end = 0), size = 0.7) +
  geom_ribbon(data = ntp.data, aes(ymin = lwr, ymax = upr), alpha = .15)+
  facet_wrap(~tb, scales = "free", labeller = labeller(tb=plot.labels)) +
  scale_x_continuous(trans = 'log2',labels = comma, breaks = c(100,500,1000,5000,10000,20000,50000,150000)) +
  scale_y_continuous(trans = 'log2',labels = comma, breaks = c(100,500,1000,5000,10000,20000)) +
  ggthemes::theme_base() +
  # ggthemes::theme_economist_white() +
  labs(x="GDP per capita (2020 US$, log scale)", y="Cost per patient treated (2020 US$, log scale)") 

# plot
# ggsave(plot=plot, filename = here('output','NTP prog costs.png'), w=10,h=5, dpi = 600)

# Health care cost regression fitting - to be used for countries with missing opd/ipd data
tbcst.care <- care_costs %>% select(country, g_whoregion, e_inc_ds_num, e_inc_rr_num, gdp, c.opd_dstb.m, c.opd_mdrtb.m, c.ipd_dstb.m, c.ipd_mdrtb.m) 
summary(tbcst.care %>% select(c.opd_dstb.m, c.opd_mdrtb.m, c.ipd_dstb.m, c.ipd_mdrtb.m))

care <- c('gdp','c.opd_dstb.m', 'c.opd_mdrtb.m', 'c.ipd_dstb.m', 'c.ipd_mdrtb.m')
# (tbcst.care %>% filter(across(care, ~is.na(.x))))

tbcst.care <- tbcst.care %>% filter_at(vars(care), all_vars((.) != 0)) 
tbcst.care <- tbcst.care %>%
  mutate_at(care, list(log = ~ log(.)))

tb <- c('dstb.opd', 'mdrtb.opd','dstb.ipd', 'mdrtb.ipd')
plot.labels <- c(dstb.opd='Drug-susceptible TB (DS TB), OPD', mdrtb.opd='Multidrug-resistant TB (MDR TB), OPD', dstb.ipd='Drug-susceptible TB (DS TB), IPD', mdrtb.ipd='Multidrug-resistant TB (MDR TB), IPD')
tbcst.care <- tbcst.care %>% gather(tb, log.cost, c.opd_dstb.m_log, c.opd_mdrtb.m_log, c.ipd_dstb.m_log, c.ipd_mdrtb.m_log)
tbcst.care<-setDT(tbcst.care)
tbcst.care <- tbcst.care[!is.na(log.cost)]
tbcst.care <- tbcst.care[!(log.cost=='Inf')]

# Log-Log linear regression
# Relationship between gdp and costs “look more normal” after transformation.
# The relationship between the log transformed variables is more linear

# log(cost) = β0 + β1. log(gdp)
llr.carecst<- lm(log.cost~gdp_log, tbcst.care)
llr.carecst.dstb.opd <- lm(log.cost~gdp_log, subset(tbcst.care, tb=='c.opd_dstb.m_log'))
llr.carecst.mdrtb.opd <- lm(log.cost~gdp_log, subset(tbcst.care, tb=='c.opd_mdrtb.m_log'))
llr.carecst.dstb.ipd <- lm(log.cost~gdp_log, subset(tbcst.care, tb=='c.ipd_dstb.m_log'))
llr.carecst.mdrtb.ipd <- lm(log.cost~gdp_log, subset(tbcst.care, tb=='c.ipd_mdrtb.m_log'))
# summary(llr.carecst.dstb.opd)
# summary(llr.carecst.mdrtb.opd)
# summary(llr.carecst.dstb.ipd)
# summary(llr.carecst.mdrtb.ipd)

# Ploting the log-log regression model results in the original scales
care.data = data.table(x = exp(llr.carecst$model$gdp_log),
                  y = exp(predict(llr.carecst)), 
                  lwr=exp(predict(llr.carecst, interval = "confidence")[,2]), 
                  upr = exp(predict(llr.carecst, interval = "confidence")[,3]))

names(care.data) <- c("gdp1","care.cost",'lwr', 'upr')
care.data <- cbind(tbcst.care, care.data)

care.data <- care.data %>% mutate(tb=factor(tb))
care.data <- care.data %>% mutate(tb=recode_factor(tb,
                                         'c.opd_dstb.m_log'='dstb.opd',
                                         'c.opd_mdrtb.m_log'='mdrtb.opd',
                                         'c.ipd_dstb.m_log'='dstb.ipd',
                                         'c.ipd_mdrtb.m_log'='mdrtb.ipd'))
plot <- ggplot(care.data, aes(x = gdp, y = exp(log.cost)))  +
  geom_jitter() +
  # geom_point(aes(size=tx_mdr+tx_xdr, fill=g_whoregion), alpha = 0.75, shape = 21) +
  # geom_smooth(span   = 1,    color = viridis(1, begin = 0.6), se = FALSE, linetype =
  #               "dashed")  +
  geom_line(data = care.data, aes(x = gdp, y = care.cost),
            color = viridis::viridis(1, end = 0), size = 0.7) +
  geom_ribbon(data = care.data, aes(ymin = lwr, ymax = upr), alpha = .15)+
  scale_x_continuous(trans = 'log2',labels = comma, breaks = c(100,500,1000,5000,10000,20000,50000,150000)) +
  scale_y_continuous(trans = 'log2',labels = comma, breaks = c(100,1000,5000,20000)) +
  facet_wrap(~tb, scales = "free_y", labeller = labeller(tb=plot.labels)) +
  ggthemes::theme_base() +
  # ggthemes::theme_economist_white() +
  labs(x="GDP per capita (2020 US$, log scale)", y="Cost per patient treated (2020 US$, log scale)") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.4),
        text = element_text(size=14))
# plot
# ggsave(plot=plot, filename = here('output','TB care costs1.png'), w=10,h=5, dpi = 600)

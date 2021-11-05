library(here)
library(HEdtree)
library(data.table)

## === mode structure
## --- structure
H <- MSorg2tree(here('indata/mdrhhcm.txt'))     #read in tree structure
print(H)                                #check OK
H <- top(H)                             #drop root

## --- quantity namings
## check var on leaves
H$Set(check=1)
H$Set(check=0,filterFun=function(x)length(x$children)>0)
## deaths
H$Set(deaths=0)
H$Set(deaths=1,filterFun=function(x)x$name=='dies')
## deaths for incident TB
H$Set(incdeaths=0)
H$`no active TB`$`PT`$`incident TB`$Set(incdeaths=1,filterFun=function(x)x$name=='dies')
H$`no active TB`$`no PT`$`incident TB`$Set(incdeaths=1,filterFun=function(x)x$name=='dies')
## print(H,'incdeaths')
## inctb
H$Set(inctb=0)
H$Set(inctb=1,filterFun=function(x)x$name=='incident TB')
## RS-ATT
H$Set(rsatt=0)
H$Set(rsatt=1,filterFun=function(x)x$name=='RS-ATT')
## RR-ATT
H$Set(rratt=0)
H$Set(rratt=1,filterFun=function(x)x$name=='RR-ATT')
## PT counter
H$Set(ptc=0)
H$Set(ptc=1,filterFun=function(x)x$name=='PT')
## RS-ATT in incidence
H$Set(rsatti=0)
H$`no active TB`$Set(rsatti=1,filterFun=function(x)x$name=='RS-ATT')
## RR-ATT in incidence
H$Set(rratti=0)
H$`no active TB`$Set(rratti=1,filterFun=function(x)x$name=='RR-ATT')

## --- probability namings
H$Set(p=1)
## coprev
H$children$`active TB`$p <- 'coprev'
H$children$`active TB`$`no ATT`$p <- '(1-CDR)'
H$children$`active TB`$`RS-ATT`$p <- 'CDR*pRSATT'
H$children$`active TB`$`RR-ATT`$p <- 'CDR*(1-pRSATT)'
## ## coprev outcomes
H$children$`active TB`$`no ATT`$dies$p <- 'CFRnotx'
H$children$`active TB`$`no ATT`$survives$p <- '1-CFRnotx'
H$children$`active TB`$`RS-ATT`$dies$p <- 'CFRstx'
H$children$`active TB`$`RS-ATT`$survives$p <- '1-CFRstx'
H$children$`active TB`$`RR-ATT`$dies$p <- 'CFRrtx'
H$children$`active TB`$`RR-ATT`$survives$p <- '1-CFRrtx'

## not coprev
H$children$`no active TB`$p <- '1-coprev'
H$children$`no active TB`$`no PT`$p <- '1-PTcov'
## no PT
H$children$`no active TB`$`no PT`$`incident TB`$p <- 'progn'
H$children$`no active TB`$`no PT`$`no incident TB`$p <- '1-progn'
## tx type
H$children$`no active TB`$`no PT`$`incident TB`$`no ATT`$p <- '(1-CDRi)'
H$children$`no active TB`$`no PT`$`incident TB`$`RS-ATT`$p <- 'CDRi*pRSATTi'
H$children$`no active TB`$`no PT`$`incident TB`$`RR-ATT`$p <- 'CDRi*(1-pRSATTi)'
## ## PT inc outcomes
H$children$`no active TB`$`no PT`$`incident TB`$`no ATT`$dies$p <- 'CFRnotx'
H$children$`no active TB`$`no PT`$`incident TB`$`no ATT`$survives$p <- '1-CFRnotx'
H$children$`no active TB`$`no PT`$`incident TB`$`RS-ATT`$dies$p <- 'CFRstx'
H$children$`no active TB`$`no PT`$`incident TB`$`RS-ATT`$survives$p <- '1-CFRstx'
H$children$`no active TB`$`no PT`$`incident TB`$`RR-ATT`$dies$p <- 'CFRrtx'
H$children$`no active TB`$`no PT`$`incident TB`$`RR-ATT`$survives$p <- '1-CFRrtx'

## PT yes
H$children$`no active TB`$`PT`$p <- 'PTcov'
H$children$`no active TB`$`PT`$`incident TB`$p <- 'progn*RR'
H$children$`no active TB`$`PT`$`no incident TB`$p <- '1-progn*RR'
## tx type
H$children$`no active TB`$`PT`$`incident TB`$`no ATT`$p <- '(1-CDRi)'
H$children$`no active TB`$`PT`$`incident TB`$`RS-ATT`$p <- 'CDRi*pRSATTi'
H$children$`no active TB`$`PT`$`incident TB`$`RR-ATT`$p <- 'CDRi*(1-pRSATTi)'
## ## PT inc outcomes
H$children$`no active TB`$`PT`$`incident TB`$`no ATT`$dies$p <- 'CFRnotx'
H$children$`no active TB`$`PT`$`incident TB`$`no ATT`$survives$p <- '1-CFRnotx'
H$children$`no active TB`$`PT`$`incident TB`$`RS-ATT`$dies$p <- 'CFRstx'
H$children$`no active TB`$`PT`$`incident TB`$`RS-ATT`$survives$p <- '1-CFRstx'
H$children$`no active TB`$`PT`$`incident TB`$`RR-ATT`$dies$p <- 'CFRrtx'
H$children$`no active TB`$`PT`$`incident TB`$`RR-ATT`$survives$p <- '1-CFRrtx'


## TODO add other quantities
## check
print(H,'check','p','deaths')
print(H,'inctb','incdeaths')
print(H,'rsatt','rratt','ptc') #


## getAQ(H,'inctb')
## output a file to inspect how the tree is structured/label
tree2file(H,
          here('output/treecheck.csv'),
          'p','check','deaths','incdeaths','inctb',
          'rsatt','rsatti','rratt','rratti','ptc')

xtra <- fread(here('indata/costlabels.csv'))

H$Set(cost=xtra$cost)
H$Set(prevtb=xtra$prevtb)

## ## parameters
## showParmz(H)

## functions
F <- makeTfuns(H,qnt=c('check','deaths','incdeaths','inctb',
                       'rsatt','rratt','rsatti','rratti','ptc',
                       'cost','prevtb'))
str(F)

## running all function
runallfuns <- function(D){
  D$check <- F$checkfun(D); print('check run!')
  D$deaths <- F$deathsfun(D); print('deaths run!')
  D$incdeaths <- F$incdeathsfun(D); print('incdeaths run!')
  D$inctb <- F$inctbfun(D); print('inctb run!')
  D$rsatt <- F$rsattfun(D); print('rsatt run!')
  D$rratt <- F$rrattfun(D); print('rratt run!')
  D$rsatti <- F$rsattifun(D); print('rsatti run!')
  D$rratti <- F$rrattifun(D); print('rratti run!')
  D$ptc <- F$ptcfun(D); print('ptc run!')
  D$cost <- F$costfun(D); print('cost run!')
  D$prevtb <- F$prevtbfun(D); print('prevtb run!')
  return(D)
}

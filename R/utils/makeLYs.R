library(here)
library(data.table)


fn <- here('data/LYD_2020.Rdata')
if(!file.exists(fn)){
cat("Life-years not yet made. Be patient - doing this on first run!\n")
  library(discly)
  CC <- unique(TT[,iso3]) #loaded with discly
  ## loop through and generate flat means
  LYD <- list()
  agz <- tmp <- 0:14
  dsr <- c(1,3,5) #discount rates
  m <- 0
  for(i in 1:length(CC)){
    cat(i,"\n")
    for(j in 1:3){
      for(k in 1:15){
        m <- m+1
        tmp[k] <- discly(CC[i],agz[k],2020,dr=dsr[j]/1e2)
      }
      LYD[[m]] <- data.table(iso3=CC[i],dr=dsr[j],
                             u5=mean(tmp[1:5]),
                             o5=mean(tmp[6:15]))
    }
  }
  LYD <- rbindlist(LYD)
  save(LYD,file=fn)
} else {
  load(file=fn)
}

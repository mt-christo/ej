library(xts)
library(foreach)
library(data.table)
#library(RCurl)
#library(lubridate)

#setwd('/home/anton/git/ej/ContractsCSV')
setwd('/home/aslepnev/git/ej/ContractsCSV')
comms = fread('../Commodities.csv')
mon = data.table('num'=1:12, 'month'=c('F','G','H','J','K','M','N','Q','U','V','X','Z'))
fsizes = foreach(f = list.files('.'), .combine=c)%do%{ file.info(f)$size }

data = rbindlist(foreach(f = list.files('.')[fsizes > 14])%do%{
    print(f)
    d = fread(f)
    ftoks = strsplit(f,'_')[[1]]
    d$commodity = ftoks[1]
    d$month = ftoks[2]
    d$year = as.numeric(ftoks[3])
    d
})

contracts = mon[unique(data[, .(commodity, month, year)]), on='month'][order(year, num), ][, id:=1:.N][, idloc:=1:nrow(.SD), by='commodity']
data = contracts[, .(commodity, month, year, id, idloc)][data, on=.(commodity, month, year)]














mp1 = mon[unique(data[, .(commodity, month, year)]), on='month'][order(year, num), ][, N:=nrow(.SD), by=.(commodity, year)]
mp1 = mp1[N>1, ':='(idx=1:nrow(.SD), idx2=c(2:nrow(.SD),1), year2=year, year1=year, month2=month), by=.(commodity, year)][, ':='(num=NULL, N=NULL)]
mp1[idx2 < idx, year2:=year+1]; mp2 = mp1[, .(commodity, idx, month2, year)]; mp1 = mp1[, .(commodity, month, idx2, year, year1, year2)]
setkey(mp1, commodity, year, idx2); setkey(mp2, commodity, year, idx)
monpairs = mp2[mp1, ][, month1:=month][, .(commodity, year1, month1, year2, month2)] 

setkey(data, commodity, year, month); setkey(monpairs, commodity, year1, month1); s = monpairs[data, ][!is.na(month2), ][, ':='(Close1=Close, OI1=OI, Close=NULL, OI=NULL)]
setkey(s, Date, commodity, year2, month2); setkey(data, Date, commodity, year, month); s = data[s, ][!is.na(Close1) & !is.na(Close), ]

s = s[, ':='(spread=Close-Close1, close1=Close1, close2=Close, oi2=OI, oi1=OI1, date=Date, months=paste(month1, month, sep='-'))][, .(date, year, commodity, months, close1, oi1, close2, oi2, spread)]
s$date = as.Date(s$date)
s$zerodate = s$date
year(s$zerodate) = 1990

#save(s, file='/home/aslepnev/git/ej/cmedata1.RData')
#s <<- get(load('/home/aslepnev/git/ej/cmedata1.RData'))

# dt_start = Sys.Date(); dt_end = dt_start+15




s2 = spread_ival(Sys.Date(), Sys.Date()+15)
s3 = s2[, .(tp_long=as.numeric(quantile(tp, 0.5)), dd_long=as.numeric(quantile(dd, 0.1)),tp_short=as.numeric(quantile(dd, 0.5)), dd_short=as.numeric(quantile(tp, 0.9))), by=.(commodity, months)]
s3 = s3[, .(commodity, months, ratiol=-round(tp_long/dd_long, 3), tp_long=round(tp_long, 3), dd_long=round(dd_long, 3), 
            ratios=-round(tp_short/dd_short, 3), tp_short=round(tp_short, 3), dd_short=round(dd_short, 3))]
s3[order(ratiol),]
s3[order(ratios),]





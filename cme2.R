library(xts)
library(foreach)
library(data.table)
#library(RCurl)
library(lubridate)

setwd('/home/anton/git/ej/ContractsCSV')
#setwd('/home/aslepnev/git/ej/ContractsCSV')
comms = fread('../Commodities.csv')
mon = data.table('num'=1:12, 'month'=c('F','G','H','J','K','M','N','Q','U','V','X','Z'))
fsizes = foreach(f = list.files('.'), .combine=c)%do%{ file.info(f)$size }

data0 = rbindlist(foreach(f = list.files('.')[fsizes > 14])%do%{
    print(f)
    d = fread(f)
    ftoks = strsplit(f,'_')[[1]]
    d$commodity = ftoks[1]
    d$month = ftoks[2]
    d$year = as.numeric(ftoks[3])
    d
})
data0$Date=as.Date(data0$Date)
# save(data0, file='cme_contracts_data.RData')
# data0=get(load('cme_contracts_data.RData'))

contracts = mon[unique(data0[, .(commodity, month, year)]), on='month'][order(year, num), ][, id:=1:.N][, idloc:=1:nrow(.SD), by='commodity'][, num:=NULL]
data = contracts[, .(commodity, month, year, id, idloc)][data0, on=.(commodity, month, year)]

# comdtys=c('Feeder Cattle', 'Lean Hog'); lags=1:2; min_year=1990
build_nplets = function(comdtys, data0, lags, min_year){
    data = data0[commodity%in%comdtys & year>=min_year, ]
    contracts = mon[unique(data[, .(commodity, month, year)]), on='month']
    contracts = contracts[order(year, num), ][, id:=1:.N][, idloc:=1:nrow(.SD), by='commodity'][, expiry:=as.Date(ISOdate(year,num,15))][, num:=NULL]
    data = contracts[, .(commodity, month, year, expiry, id, idloc)][data, on=.(commodity, month, year)]

    icols = c('month', 'expiry', 'id', 'idloc', 'Close', 'OI')
    data1 = data[, .SD, .SDcols=c(icols, 'commodity', 'Date')]
    colnames(data1) = c(paste0(icols,0), 'commodity', 'date')
    for(i in lags){
        idloci = paste0('idloc', i)
        data1[, eval(idloci):=idloc0+i]
        datai = data[, .SD, .SDcols=c(icols, 'commodity', 'Date')]
        colnames(datai) = c(paste0(icols,i), 'commodity', 'date')
        data1 = datai[data1, on=c('commodity', 'date', idloci)][!is.na(get(paste0('Close', i))), ]
    }
    return(data1)
}

# nplets = data1; weights=c(1,-2,1)
calc_spreads = function(nplets, weights){
    res = nplets
    res$spread = foreach(i=1:length(weights),.combine='+')%do%{ weights[i]*res[, get(paste0('Close', i-1))] }
    res$expiry = eval(parse(text=paste0('pmin(', paste(paste0('res$expiry', 1:length(weights)-1), collapse=','),')')))
    res$months = eval(parse(text=paste0('paste(', paste(paste0('res$month', 1:length(weights)-1), collapse=','),',sep="-")')))
    res$year = year(res$expiry)  # this is a hack - we simply assign "year" to mean "year of expiration"
    res = res[, .(commodity, date, months, year, expiry, spread)]
    
}

# comms
# comdtys=c('Feeder Cattle', 'Lean Hog'); lags=1:2; min_year=1990
# comdtys=c('Corn', 'Coffee', 'Chicago SRW Wheat', 'Soybean', 'Soybean Meal'); lags=1:2; min_year=1985
# weights=c(1,-2,1)
# curr_date=Sys.Date(); wnd=15; conf_level=0.05
seasonal_metrics = function(comdtys, data0, lags, min_year, weights, curr_date, wnd, conf_level, rank){
    zcd = curr_date
    year(zcd) = 1900
    d = calc_spreads(build_nplets(comdtys, data0, lags, min_year), weights)
    d = d[expiry-date < 360, ]  # only browse the histories within year of expiration
    year(d$date) = 1900  # this is nasty: over-NewYear periods get torn; but we still measure what happens till the end of the 1900
    d = d[!is.na(date), ]  # 29 Feb :)
    ds1 = d[date>=zcd & date<=zcd+wnd,][, .(commodity, months, year, init_spread=head(spread, 1), max_spread=max(spread), min_spread=min(spread)), by=c('commodity', 'months', 'year')]
    ds2 = ds1[, .(.N, commodity, months,
                  max_95=quantile(max_spread-init_spread, 1-conf_level),
                  max_50=quantile(max_spread-init_spread, 0.5),
                  max_05=quantile(max_spread-init_spread, conf_level),
                  min_95=quantile(min_spread-init_spread, 1-conf_level),
                  min_50=quantile(min_spread-init_spread, 0.5),
                  min_05=quantile(min_spread-init_spread, conf_level)), by=c('commodity', 'months')]

    s = ds2[N>10, .(commodity, months, up=-max_50/min_05, down=-min_50/max_95)][order(if(rank < 0) down else up, decreasing=TRUE),]
#    s[1:4,]
    i = abs(rank)
    dx = d[date>=zcd & date<=zcd+wnd & commodity==s[i, commodity] & months==s[i,months], ]
    h = foreach(y=unique(dx$year), .combine=cbind)%do%{ x=xts(dx[year==y, spread], order.by=dx[year==y, date]); x-as.numeric(x[1]) }
    plot(na.locf(na.locf(h, na.rm=FALSE), fromLast=TRUE))
}

seasonal_metrics(c('Corn', 'Coffee', 'Chicago SRW Wheat', 'Soybean', 'Soybean Meal'), data0, 1:2, 1985, c(1,-2,1), Sys.Date(), 15, 0.05, 2)






s





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





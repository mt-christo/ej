library(xts)
library(foreach)
library(data.table)
#library(RCurl)
library(lubridate)
homedir = 'aslepnev' #; homedir = 'anton'

setwd(paste0('/home/', homedir, '/git/ej/ContractsCSV'))
#setwd('/home/aslepnev/git/ej/ContractsCSV')
comms = fread('../Commodities.csv')
mon = data.table('num'=1:12, 'month'=c('F','G','H','J','K','M','N','Q','U','V','X','Z'))

if(FALSE){
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
    save(data0, file='cme_contracts_data.RData')
}

data0=get(load('cme_contracts_data.RData'))
#contracts = mon[unique(data0[, .(commodity, month, year)]), on='month'][order(year, num), ][, id:=1:.N][, idloc:=1:nrow(.SD), by='commodity'][, num:=NULL]
#data = contracts[, .(commodity, month, year, id, idloc)][data0, on=.(commodity, month, year)]

# comdtys=c('Feeder Cattle', 'Lean Hog'); lags=1:2; min_year=1990
# comdtys=c('Feeder Cattle', 'Lean Hog'); lags=2:3; min_year=1990
build_nplets = function(comdtys, data0, lags, min_year){
    data = data0[commodity%in%comdtys & year>=min_year, ]
    contracts = mon[unique(data[, .(commodity, month, year)]), on='month']
    contracts = contracts[order(year, num), ][, id:=1:.N][, idloc:=1:nrow(.SD), by='commodity'][, expiry:=as.Date(ISOdate(year,num,15))][, num:=NULL]
    data = contracts[, .(commodity, month, year, expiry, id, idloc)][data, on=.(commodity, month, year)]

    icols = c('month', 'expiry', 'id', 'idloc', 'Close', 'OI')
    data1 = data[, .SD, .SDcols=c(icols, 'commodity', 'Date')]
    colnames(data1) = c(paste0(icols,0), 'commodity', 'date')
    for(i in 1:length(lags)){
        idloci = paste0('idloc', i)
        data1[, eval(idloci):=idloc0+lags[i]]
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
    return(res)
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

#seasonal_metrics(c('Corn', 'Coffee', 'Chicago SRW Wheat', 'Soybean', 'Soybean Meal'), data0, 1:2, 1985, c(1,-2,1), Sys.Date(), 15, 0.05, 2)

spreads1 = calc_spreads(build_nplets(unique(comms$Name), data0, 1:2, 1985), c(1,-2,1))
spreads2 = calc_spreads(build_nplets(unique(comms$Name), data0, 2:3, 1985), c(1,-2,1))
spreads = rbind(spreads1, spreads2)
save(spreads, file=paste0('/home/', homedir, '/webhub/spreads_hist.RData'))

homedir <<- 'aslepnev' #; homedir = 'anton'
setwd(paste0('/home/', homedir, '/webhub'))
spreads = get(load('spreads_hist.RData'))
















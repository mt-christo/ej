homedir <<- 'aslepnev' #; homedir = 'anton'
setwd(paste0('/home/', homedir, '/webhub'))
Spreads <<- get(load('spreads_hist.RData'))
Spreads2 <<- get(load('spreads_hist2.RData'))
SpreadsData <<- get(load('cme_contracts_data.RData'))[order(Date), ][, tail(.SD, 750), by=.(commodity, month, year)][year>=2000, ]
SpreadsList <<- as.data.table(gs_read(gs_key('1s05m2xyRVXprFHyRVqY9qbEKl6OqXssymVsmvWnrpIk'), 'Spreads', range='A1:G3000', col_names=TRUE))
SpreadsCommodities = fread('Commodities.csv')
              
s = Spreads2
d = SpreadsData
i = 4055
y = 2007
b = SpreadsCommodities[SpreadsList, on=.(Code=Ticker)][, .(id=SpreadID, commodity=Name, year_lag=YearLag, month=Month, weight=Weight)][id==i, ]
d1 = d[b, on=.(commodity, month)][s[year==y & id==i, ], on=.(year=year, Date=date)][, ':='(year=NULL, Date=as.Date(Date))]

x = as.xts(data.table(dcast(d1, Date ~ month, value.var='OI')))
x_ma = rollapplyr(x, 5, FUN=mean, fill=NA)
x_lag = lag(x, 1)

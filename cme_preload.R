homedir <<- 'aslepnev' #; homedir = 'anton'
setwd(paste0('/home/', homedir, '/webhub'))
Spreads <<- get(load('spreads_hist.RData'))
Spreads2 <<- get(load('spreads_hist2.RData'))
setkey(Spreads2, id)
SpreadsData <<- get(load('cme_contracts_data.RData'))[order(Date), ][, tail(.SD, 750), by=.(commodity, month, year)][year>=2000, ]
SpreadsList <<- as.data.table(gs_read(gs_key('1s05m2xyRVXprFHyRVqY9qbEKl6OqXssymVsmvWnrpIk'), 'Spreads', range='A1:G3000', col_names=TRUE))
SpreadsCommodities = fread('Commodities.csv')
              

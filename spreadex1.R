# comdty='Feeder Cattle'; spread_months='F-H'; dt_start='2018-11-09'; dt_end='2019-01-04'; dt_center='2018-11-14' 
# source('/home/aslepnev/git/ej/rapache_eval.R')
spread_wnd_web = function(){
    save(GET, file='/home/aslepnev/webhub/last_GET_spread_wnd_web.RData')
    # GET = get(load('/home/aslepnev/webhub/last_GET_spread_wnd_web.RData'))
    comdty = GET$comdty
    spread_months = GET$spread_months
    dt_start = GET$dt_start
    dt_end = GET$dt_end
    dt_center = GET$dt_center
    dts_hist = unlist(GET[grep('dt_hist', names(GET))])
    
    res = get_spread_wnd(comdty, spread_months, dt_start, dt_end, dt_center)
    
    qres = res[, .(q15 = quantile(spread, 0.15, na.rm=TRUE),
                   q5 = quantile(spread, 0.5, na.rm=TRUE),
                   q85 = quantile(spread, 0.85, na.rm=TRUE),
                   c15 = quantile(spread_ctd, 0.15, na.rm=TRUE),
                   c5 = quantile(spread_ctd, 0.5, na.rm=TRUE),
                   c85 = quantile(spread_ctd, 0.85, na.rm=TRUE),
                   m107 = sum(ma10chg7>0)/.N),
               by='zero_date']

    zero_dates = unique(res$zero_date)
    h = rbindlist(foreach(dt_hist=dts_hist)%do%{
        dt = max(zero_dates[month(zero_dates)==month(dt_hist) & day(zero_dates)==day(dt_hist)])
        h_breaks = seq(res[, min(spread_ctd)], res[, max(spread_ctd)], len=10)
        h = hist(res[zero_date == dt, spread_ctd], plot=FALSE, breaks=h_breaks)
        h = as.data.table(h[c('mids', 'density')])[, date:=dt]
    })
    
    
    web_datatable(list('res'=res, 'qres'=qres))
}


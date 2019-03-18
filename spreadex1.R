# comdty='Feeder Cattle'; spread_months='F-H'; dt_start='2018-11-09'; dt_end='2019-01-04'; dt_center='2018-11-14' 
# source('/home/aslepnev/git/ej/rapache_eval.R')
spread_wnd_web = function(){
    save(GET, file='/home/aslepnev/webhub/last_GET_spread_wnd_web.RData')
    # GET = get(load('/home/aslepnev/webhub/last_GET_spread_wnd_web.RData'))
#    comdty = GET$comdty
#    spread_months = GET$spread_months
    spread_id = GET$spread_id
    
    dt_start_bunch = GET$dt_start_bunch
    dt_end_bunch = GET$dt_end_bunch
    dt_start_results = GET$dt_start_results
    dt_end_results = GET$dt_end_results
    years = as.numeric(strsplit(GET$years, '-')[[1]])
    
    wnd_bunch = as.Date(GET$dt_end_bunch) - as.Date(GET$dt_start_bunch)
    wnd_results = as.Date(GET$dt_end_results) - as.Date(GET$dt_start_results)
    
    h_bunch = get_spread_hist(spread_id, GET$dt_start_bunch, wnd_bunch)
    h_results = get_spread_hist(spread_id, GET$dt_start_results, wnd_results)

    ma = get_spread_ma_wnd(h_bunch, GET$dt_start_bunch, wnd_bunch)[year%in%years, ]
    results = get_spread_results(h_results, GET$dt_start_results, wnd_results)[year%in%years, ]

    qres = ma[, .(.N,
#                   q15 = quantile(spread, 0.15, na.rm=TRUE),
                   q5 = quantile(spread, 0.5, na.rm=TRUE),
#                   q85 = quantile(spread, 0.85, na.rm=TRUE),
#                   c15 = quantile(spread_ctd, 0.15, na.rm=TRUE),
#                   c5 = quantile(spread_ctd, 0.5, na.rm=TRUE),
#                   c85 = quantile(spread_ctd, 0.85, na.rm=TRUE),
                   t107 = sum(ma10chg7>0)/.N,
                   nt107 = sum(ma10chg7<0)/.N),
               by='zero_date']
    
    web_datatable(list('ma'=ma, 'qres'=qres, 'results'=results))
}


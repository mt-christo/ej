# comdty='Feeder Cattle'; spread_months='F-H'; dt_start='2018-11-09'; dt_end='2019-01-04'; dt_center='2018-11-14' 
# source('/home/aslepnev/git/ej/rapache_eval.R')
spread_wnd_mas = function(){
    save(GET, file='/home/aslepnev/webhub/last_GET_spread_wnd_web.RData')
    # GET = get(load('/home/aslepnev/webhub/last_GET_spread_wnd_web.RData'))
#    comdty = GET$comdty
#    spread_months = GET$spread_months
    spread_id = GET$spread_id
    
    dt_start_bunch = GET$dt_start_bunch
    dt_end_bunch = GET$dt_end_bunch
    years = as.numeric(strsplit(GET$years, '-')[[1]])
    
    # min_spread=4; max_spread=4.5; ival_dt1='1900-01-01'; ival_dt2='1901-12-31'; results_start_luft=4; results_end_luft=4
    min_spread = as.numeric(GET$min_spread)
    max_spread = as.numeric(GET$max_spread)
    ival_dt1 = to_zero_date(GET$ival_dt1, GET$ival_dt2)
    ival_dt2 = to_zero_date(GET$ival_dt2, GET$ival_dt2)
    
    wnd_bunch = as.Date(GET$dt_end_bunch) - as.Date(GET$dt_start_bunch)
    h_bunch = get_spread_hist(spread_id, dt_start_bunch, wnd_bunch)
    ma = get_spread_ma_wnd(h_bunch, dt_start_bunch, wnd_bunch)[year%in%years, ]

    years_thresh = ma[!is.na(spread), ][(spread<=min_spread | spread>=max_spread) & (zero_date>=ival_dt1 & zero_date<=ival_dt2), unique(year)]
    if(length(years_thresh) == 0){
        year(ival_dt1) = year(ival_dt1) + 1
        year(ival_dt2) = year(ival_dt2) + 1
        years_thresh = ma[!is.na(spread), ][(spread<=min_spread | spread>=max_spread) & (zero_date>=ival_dt1 & zero_date<=ival_dt2), unique(year)]
    }
    ma = ma[year%in%years_thresh, ]
   
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
    
    web_datatable(list('ma'=ma, 'qres'=qres))
}

spread_wnd_results = function(){
    save(GET, file='/home/aslepnev/webhub/last_GET_spread_wnd_web.RData')
    # GET = get(load('/home/aslepnev/webhub/last_GET_spread_wnd_web.RData'))
#    comdty = GET$comdty
#    spread_months = GET$spread_months
    spread_id = GET$spread_id
    
    results_start_luft = as.numeric(GET$results_start_luft)
    results_end_luft = as.numeric(GET$results_end_luft)
    dt_start_results = bizoff(as.Date(GET$dt_start_results), -results_start_luft, 'mycal')
    dt_end_results = bizoff(as.Date(GET$dt_end_results), results_end_luft, 'mycal')
    years = as.numeric(strsplit(GET$years, '-')[[1]])

    wnd_results = dt_end_results - dt_start_results
    h_results = get_spread_hist(spread_id, dt_start_results, wnd_results)
    results = get_spread_results(h_results, dt_start_results, wnd_results, results_start_luft, results_end_luft)[year%in%years, ]
    
    web_datatable(list('results'=results))
}


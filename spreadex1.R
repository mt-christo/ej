# comdty='Feeder Cattle'; spread_months='F-H'; dt_start='2018-11-09'; dt_end='2019-01-04'; dt_center='2018-11-14' 
# source('/home/aslepnev/git/ej/rapache_eval.R')
spread_wnd_web = function(){
    comdty=GET$comdty; spread_months=GET$spread_months; dt_start=GET$dt_start; dt_end=GET$dt_end; dt_center=GET$dt_center
    res = get_spread_wnd(s, comdty, spread_months, dt_start, dt_end)
    
    dt_center = max(res[date <= dt_center, date])
    center_value = res[date==dt_center, spread]
    res[, cspread := spread - center_value]
    qres = res[, .(q15=quantile(spread, 0.15),
                      q5=quantile(spread, 0.5),
                      q85=quantile(spread, 0.85),
                      c15=quantile(cspread, 0.15),
                      c5=quantile(cspread, 0.5),
                      c85=quantile(cspread, 0.85)),
               by='zerodate']
    
    web_datatable(list('res'=res, 'qres'=qres))
}


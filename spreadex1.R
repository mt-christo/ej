spread_wnd_web = function(){
    res = get_spread_wnd(s, GET$comdty, GET$spread_months, GET$dt_start, GET$dt_end)
    save(res, file='/home/aslepnev/webhub/res.RData')
    cat(paste(foreach(col=colnames(res),.combine='c')%do%paste(c(col, as.character(res[, get(col)])), collapse=';'), collapse='NEWCOL'))
}

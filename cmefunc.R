to_zero_date <- function(date, reldate){
    relres = as.Date(reldate)
    res = as.Date(date)
    year(res) = year(res) - year(relres) + 1990
    return(res)    
}

# Assumed that global 's' variable exists
spread_ival = function(data, dt_start, dt_end){
    ds = to_zero_date(dt_start, dt_start)
    de = to_zero_date(dt_end, dt_start)
    s1 = data[zerodate>ds & zerodate<de & year>=1990, ]
    return(s1)
}

spread_ival_tpdd = function(data, dt_start, dt_end){
    s1 = spread_ival(data, dt_start, dt_end)
    s2 = s1[, .(tp=max(spread)-spread[1], dd=min(spread)-spread[1]), by=.(commodity, months, year)]
    return(s2)
}

# dt_start=GET$dt_start_results; wnd= wnd_results
get_spread_hist <- function(spread_id, dt_start, wnd){
    s = Spreads2[id==spread_id, ]

    zero_year = 1901 + year(dt_start) - year(s[expiry > dt_start, min(expiry)])
    zero_start = as.Date(ISOdate(zero_year, month(dt_start), day(dt_start)))
    
    s = s[, zero_date:=as.Date(ISOdate(1901+year(date)-year(expiry), month(date), day(date)))][!is.na(zero_date), ]
    s = s[CJ(zero_date = unique(s$zero_date), year=unique(s$year)), on=.(zero_date, year)]
    s = s[order(zero_date), ][, spread := na.locf(spread, na.rm=FALSE), by=year]
    
    return(list(hist=s, zero_year=zero_year, zero_start=zero_start))
}

get_spread_results <- function(h, dt_start, wnd){
    s = h$hist[zero_date>=h$zero_start & zero_date<=h$zero_start+wnd, ][, !colnames(h$hist)%in%c('commodity','date','months','expiry'), with=FALSE]
    res = s[, .(max_dd = min(spread - head(spread, 1))
              , max_pf = max(spread - head(spread, 1))
              , end_pf = tail(spread, 1) - head(spread, 1)), by='year']

    return(res)
}

# comdty='Feeder Cattle'; spread_months='U-V-X'; dt_start='2014-03-09'; dt_end='2014-07-09'; dt_center=dt_start; wnd=as.Date(dt_end)-as.Date(dt_start)
# wnd=as.Date(dt_end)-as.Date(dt_start)
get_spread_ma_wnd <- function(h, dt_start, wnd){
    s = h$hist[, ':='(ma5=rollapply(spread, 5, FUN=mean, fill=NA, align='right')
                     ,ma10=rollapply(spread, 10, FUN=mean, fill=NA, align='right')
                      ), by=year]

    chgfun = function(x) { tail(x,1) - x[1] }
    s = s[, ':='(ma5chg3 = rollapply(ma5, 3, FUN=chgfun, fill=NA, align='right')
                ,ma5chg5 = rollapply(ma5, 5, FUN=chgfun, fill=NA, align='right')
                ,ma5chg7 = rollapply(ma5, 7, FUN=chgfun, fill=NA, align='right')
                ,ma10chg3 = rollapply(ma10, 3, FUN=chgfun, fill=NA, align='right')
                ,ma10chg5 = rollapply(ma10, 5, FUN=chgfun, fill=NA, align='right')
                ,ma10chg7 = rollapply(ma10, 7, FUN=chgfun, fill=NA, align='right')
                ), by=year]
    #s = s[, spread_ctd := spread - .SD[zero_date==h$zero_center, spread], by=year]
    res = s[zero_date>=h$zero_start & zero_date<=h$zero_start+wnd, ][, !colnames(s)%in%c('commodity','date','months','expiry'), with=FALSE]
    
    return(res)
}

#comdty='Feeder Cattle'; spread_months='U-V-X'; dt_start='2015-12-09'; dt_end='2016-01-04'; dt_center='2015-12-09'

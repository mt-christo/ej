to_zero_date <- function(date, reldate){

#    zero_year = 1901 + year(dt_end) - year(s[expiry > dt_end, min(expiry)])
#    zero_end = as.Date(ISOdate(zero_year, month(dt_end), day(dt_end)))
#    zero_start = as.Date(ISOdate(zero_year + year(dt_start) - year(dt_end), month(dt_start), day(dt_start)))


    relres = as.Date(reldate)
    res = as.Date(date)
    year(res) = year(res) - year(relres) + 1900
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
# dt_start=GET$dt_start_bunch; wnd= wnd_bunch
# dt_start=dt_start_bunch; wnd=wnd_bunch
# dt_start=dt_start_results; wnd=wnd_results
get_spread_hist <- function(spread_id, dt_start, wnd, hist_depth){
    s = Spreads2[id==spread_id & (expiry - date)<=hist_depth & (expiry - date)>=0, ]

    zero_year = 1901 + year(dt_start) - year(s[expiry > dt_start, min(expiry)])
    zero_start = as.Date(ISOdate(zero_year, month(dt_start), day(dt_start)))
    
    s = s[, zero_date:=as.Date(ISOdate(1901+year(date)-year(expiry), month(date), day(date)))][!is.na(zero_date), ]
    s = s[CJ(zero_date = unique(s$zero_date), year=unique(s$year)), on=.(zero_date, year)]
    s = s[order(zero_date), ][, spread := na.locf(spread, na.rm=FALSE), by=year]
    
    return(list(hist=s, zero_year=zero_year, zero_start=zero_start))
}

# dt_start=GET$dt_start_results; wnd= wnd_results
# dt_start=GET$dt_start_bunch; wnd= wnd_bunch
# dt_start=dt_start_bunch; wnd=wnd_bunch
# dt_start=dt_start_results; dt_end=dt_end_results; wnd=wnd_results
# dt_start=dt_start_bunch; dt_end=dt_end_bunch
get_spread_hist_endDate <- function(spread_id, dt_start, dt_end, hist_depth){
    s = Spreads2[id==spread_id & (expiry - date)<=hist_depth & (expiry - date)>=0, ]

    zero_year = 1901 + year(dt_end) - year(s[expiry > dt_end, min(expiry)])
    zero_end = as.Date(ISOdate(zero_year, month(dt_end), day(dt_end)))
    zero_start = as.Date(ISOdate(zero_year + year(dt_start) - year(dt_end), month(dt_start), day(dt_start)))
    
    s = s[, zero_date:=as.Date(ISOdate(1901 + year(date) - year(expiry), month(date), day(date)))]
    s = s[CJ(zero_date = unique(s$zero_date), year=unique(s$year)), on=.(zero_date, year)]
    s = s[order(zero_date), ][, spread := na.locf(spread, na.rm=FALSE), by=year][!is.na(zero_date), ]
    
    return(list(hist=s, zero_year=zero_year, zero_start=zero_start))
}

# h=h_results; dt_start=dt_start_results; wnd=wnd_results
get_spread_results <- function(h, dt_start, wnd, results_start_luft, results_end_luft){
    zero_start_2000 = h$zero_start; year(zero_start_2000) = year(zero_start_2000) + 100  # bizdays doesn't wotk with old dates
    zero_end_2000 = h$zero_start + wnd; year(zero_end_2000) = year(zero_end_2000) + 100
    
    zero_start1 = h$zero_start
    zero_start2 = bizoff(zero_start_2000, results_start_luft, 'mycal')
    year(zero_start2) = year(zero_start2) - 100
    zero_start3 = bizoff(zero_start_2000, 2*results_start_luft, 'mycal')
    year(zero_start3) = year(zero_start3) - 100
    
    zero_end1 = bizoff(zero_start_2000 + wnd, -2*results_end_luft, 'mycal')
    year(zero_end1) = year(zero_end1) - 100
    zero_end2 = bizoff(zero_start_2000 + wnd, -results_end_luft, 'mycal')
    year(zero_end2) = year(zero_end2) - 100
    zero_end3 = h$zero_start + wnd
    
    buys = h$hist[zero_date>=zero_start1 & zero_date<=zero_start3, .(buy=mean(spread)), by='year']
    sells = h$hist[zero_date>=zero_end1 & zero_date<=zero_end3, .(sell=mean(spread)), by='year']
    s = h$hist[zero_date>=zero_start2 & zero_date<=zero_end2, ][, !colnames(h$hist)%in%c('commodity','date','months','expiry'), with=FALSE]
    s = sells[buys[s, on='year'], on='year']
    res = s[, .(max_dd = min(spread - head(buy, 1))
              , max_pf = max(spread - head(buy, 1))
              , end_pf = tail(sell, 1) - head(buy, 1)), by='year']

    return(res)
}

# comdty='Feeder Cattle'; spread_months='U-V-X'; dt_start='2014-03-09'; dt_end='2014-07-09'; dt_center=dt_start; wnd=as.Date(dt_end)-as.Date(dt_start)
# h=h_bunch; dt_start=dt_start_bunch; wnd=wnd_bunch
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
#    res = s[zero_date>=h$zero_start & zero_date<=h$zero_start+wnd, ][, !colnames(s)%in%c('commodity','date','months','expiry'), with=FALSE]
    res = s[zero_date>=h$zero_start, ][, !colnames(s)%in%c('commodity','date','months','expiry'), with=FALSE]
    
    return(res)
}

# spread_id=4052; hist_depth=200; 
# ma_wnd=5
get_oi_analysis <- function(spread_id, hist_depth, yrs, ma_wnd){
    s = Spreads2[id==spread_id, ]
    s = s[year%in%yrs, ][(expiry - date)<=hist_depth & (expiry - date)>=0, ]
    d = SpreadsData
    b = SpreadsCommodities[SpreadsList, on=.(Code=Ticker)][, .(id=SpreadID, commodity=Name, year_lag=YearLag, month=Month, weight=Weight)][id==spread_id, ]
    
    d1 = d[b, on=.(commodity, month)][s, on=.(year=year, Date=date)][, .(year, commodity, month, date=Date-years(year(Date))+years(1901), OI=as.double(OI))]
    x = d1[order(date), ][d1[, .N, by=.(month, year)][N>ma_wnd, ], on=.(month, year)]
    x[, ':='(MA = rollapplyr(OI, ma_wnd, FUN=mean, fill=NA)
           , PREV = c(NA, OI[-.N])), by=.(month, year)][, ':='(vsMA = OI-MA
                                                             , relMA = OI/MA-1
                                                             , vsPREV = OI-PREV
                                                             , relPREV = OI/PREV-1)]

    head(x)
    x_med = x[, .(OI = median(OI, na.rm=TRUE)
                , MA = median(MA, na.rm=TRUE)
                , vsMA = median(vsMA, na.rm=TRUE)
                , relMA = median(relMA, na.rm=TRUE)
                , vsPREV = median(vsPREV, na.rm=TRUE)
                , relPREV = median(relPREV, na.rm=TRUE)), by=.(commodity, month, date)]
    
    return(list(all=x, median=x_med))
}

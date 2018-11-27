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

# comdty='Feeder Cattle'; spread_months='F-H'; dt_start='2018-11-09'; dt_end='2019-01-04'
get_spread_wnd <- function(data, comdty, spread_months, dt_start, dt_end){
    res = s[commodity==comdty & months==spread_months, ]
    res = spread_ival(res, dt_start, dt_end)[, .(zerodate, date, year, spread)]
    return(res)
}

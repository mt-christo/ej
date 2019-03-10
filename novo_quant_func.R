# params=list(window=20, maxvolwindow=10, level=0.01*14, max_weight=1.5); params=vc_params; 
# params = vc_params
volcontrol = function(r, params){
    w = sqrt(250)*rollapplyr(r, params$window, FUN=sd)
    if('maxvolwindow'%in%names(params))
        w = rollapplyr(w, params$maxvolwindow, FUN=function(x){ max(x) }) else
    if('avgvolwindow'%in%names(params))
        w = rollapplyr(w, params$avgvolwindow, FUN=function(x){ mean(x) })
    w[,1] = ifelse(is.na(w), 1, params$level/w)
    w[,1] = lag(ifelse(w > params$max_weight, params$max_weight, w), 1)
    res = log((exp(r) - 1)*w + 1)[-1,]
    return(res)
}

volcontrol_excess = function(r, params){
    rfr = params$rfr/252
    excess = params$excess/252
    return( -excess + volcontrol(-rfr + r, params) )
}


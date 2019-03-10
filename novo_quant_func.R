# params=list(window=20, maxvolwindow=10, level=0.01*14, max_weight=1.5); params=vc_params; 
# params = vc_params
volcontrol = function(r, params){
    w = sqrt(250)*rollapplyr(r, as.numeric(params$window), FUN=sd)
    if(params$type == 'max 10')
        w = rollapplyr(w, 10, FUN=function(x){ max(x) }) else
    if(params$type == 'avg 10')
        w = rollapplyr(w, 10, FUN=function(x){ mean(x) })

    params_level = as.numeric(params$level)
    max_weight = as.numeric(params$max_weight)
    
    w[,1] = ifelse(is.na(w), 1, params_level/w)
    w[,1] = lag(ifelse(w > max_weight, max_weight, w), 1)
    res = log((exp(r) - 1)*w + 1)[-1,]
    return(res)
}

# vc_params=params$vc_params; excess_rate=params$index_excess
volcontrol_excess = function(r, vc_params, excess_rate){
    rfr = as.numeric(vc_params$rfr)/252
    excess = as.numeric(excess_rate)/252
    return( -excess + volcontrol(-rfr + r, vc_params) )
}


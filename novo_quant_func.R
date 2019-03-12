# vc_params=list(window=20, type='max 10', level=0.01*14, max_weight=1.5)
# vc_params = params$vc_params
volcontrol = function(r, vc_params){
    w = sqrt(250)*rollapplyr(r, as.numeric(vc_params$window), FUN=sd)
    if(vc_params$type == 'max 10')
        w = rollapplyr(w, 10, FUN=function(x){ max(x) }) else
    if(vc_params$type == 'avg 10')
        w = rollapplyr(w, 10, FUN=function(x){ mean(x) })

    params_level = as.numeric(vc_params$level)
    max_weight = as.numeric(vc_params$max_weight)
    
    w[,1] = ifelse(is.na(w), 1, params_level/w)
    w[,1] = lag(ifelse(w > max_weight, max_weight, w), 1)
    w[1, 1] = 0
    res = log((exp(r) - 1)*w + 1)
    return(res)
}

# vc_params=params$vc_params; excess_rate=params$index_excess
volcontrol_excess = function(r, vc_params, libors){
    l3m = libors[index(libors)%in%index(r)]*0.01
    rfr = (if (vc_params$excess_type == 'rate-related excess') as.numeric(vc_params$excess) else 1.0) * l3m/252
    excess = if (vc_params$excess_type == 'simple excess') as.numeric(vc_params$excess)/252 else 0.0
    return( -excess + volcontrol(-rfr + r, vc_params) )
}


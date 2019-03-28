# vc_params=list(window=20, type='max 10', level=0.01*14, max_weight=1.5)
# vc_params = params$vc_params
# r=-rfr+r
volcontrol = function(r, vc_params){
    if(length(vc_params$window) == 1){
        print('Single window volcontrol')
    w = sqrt(250)*rollapplyr(r, as.numeric(vc_params$window), FUN=sd)
    if(vc_params$type == 'max 10')
        w = rollapplyr(w, 10, FUN=function(x){ max(x) }) else
    if(vc_params$type == 'max 5')
        w = rollapplyr(w, 5, FUN=function(x){ max(x) }) else
    if(vc_params$type == 'avg 10')
        w = rollapplyr(w, 10, FUN=function(x){ mean(x) })
    } else {
        print('Double window volcontrol')
        w1 = sqrt(250)*rollapplyr(r, as.numeric(vc_params$window[1]), FUN=sd)
        w2 = sqrt(250)*rollapplyr(r, as.numeric(vc_params$window[2]), FUN=sd)
        w = ifelse(w1>w2, w1, w2)
    }

    params_level = as.numeric(vc_params$level)
    max_weight = as.numeric(vc_params$max_weight)
    
    w[,1] = ifelse(is.na(w), 1, params_level/w)
    w[,1] = lag(ifelse(w > max_weight, max_weight, w), 1)
    w[1, 1] = 0
    res = log((exp(r) - 1)*w + 1)
    return(res)
}

# vc_params=params$vc_params; excess_rate=params$index_excess
# r=x; vc_params=list(window=c(20, 60), type='none', excess_type = 'simple excess', excess=3.5, level=0.12, max_weight=1.75)
volcontrol_excess = function(r, vc_params, libors){
    l3m = libors[index(libors)%in%index(r)]*0.01
    rfr = (if (vc_params$excess_type == 'rate-related excess') { print('rate-related excess'); as.numeric(vc_params$excess) } else 1.0) * l3m/252 
    excess = if (vc_params$excess_type == 'simple excess') { print('Simple excess'); as.numeric(vc_params$excess)*0.01/252 } else 0.0
    return( -excess + volcontrol(-rfr + r, vc_params) )
}


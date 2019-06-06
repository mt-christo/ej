# vc_params=list(window=20, type='max 10', level=0.01*14, max_weight=1.5)
# vc_params = params$vc_params
# x=x-rfr
volcontrol = function(x, vc_params){
    if(length(vc_params$window) == 1){
        print('Single window volcontrol')

        # w1 = sqrt(vc_params$vc_basis)*rollapplyr(x, as.numeric(vc_params$window), FUN=sd)
        # w2 = sqrt(vc_params$vc_basis)*vc_params$sd
        w = sqrt(vc_params$vc_basis)*if(vc_params$src == 'self')
                                         rollapplyr(x, as.numeric(vc_params$window), FUN=sd) else if(vc_params$src == 'precalc') vc_params$sd[[1]]
        
        if(vc_params$type == 'max 10'){
            w = rollapplyr(w, 10, FUN=function(y){ max(y) })
        } else if(vc_params$type == 'max 5'){
            w = rollapplyr(w, 5, FUN=function(y){ max(y) })
        } else if(vc_params$type == 'max 6'){
            w = rollapplyr(w, 6, FUN=function(y){ max(y) })
        } else if(vc_params$type == 'avg 10'){
            w = rollapplyr(w, 10, FUN=function(y){ mean(y) })
        }
    } else {
        print('Double window volcontrol')
        w1 = sqrt(vc_params$vc_basis)*if(vc_params$src == 'self')
                                         rollapplyr(x, as.numeric(vc_params$window[1]), FUN=sd) else if(vc_params$src == 'precalc') vc_params$sd[[1]]
        w2 = sqrt(vc_params$vc_basis)*if(vc_params$src == 'self')
                                         rollapplyr(x, as.numeric(vc_params$window[2]), FUN=sd) else if(vc_params$src == 'precalc') vc_params$sd[[2]]
        w = ifelse(w1>w2, w1, w2)
    }

    params_level = as.numeric(vc_params$level)
    max_weight = as.numeric(vc_params$max_weight)
    
    w[,1] = params_level/w
    w[,1] = lag(ifelse(w > max_weight, max_weight, w), 1)
    x = x[!is.na(w)]
    w = w[!is.na(w)]
    res = cbind(x, log((exp(x) - 1)*w + 1), w)
    colnames(res) = c('core_rt', 'rt', 'exposure')
    return(res)
}

# vc_params=params$vc_params; excess_rate=params$index_excess
# r=x; vc_params=list(window=c(20, 60), type='none', excess_type = 'simple excess', excess=3.5, level=0.12, max_weight=1.75)
# vc_params=list(window=20, type='none', excess_type = 'rate-related excess', excess=0, level=0.11, max_weight=1.25)
# vc_params=list(window=20, type='none', excess_type = 'libor plus', add_rate=1.5, excess=3.5, level=0.12, max_weight=1.75)
# x=r1; vc_params=list(window=20, type='max 5', excess_type = 'libor plus', add_rate=1, excess=3, level=0.08, max_weight=2.5, basis=365)
# x=r1; vc_params=list(window=20, type='none', excess_type = 'libor plus', add_rate=1.0, excess=3.5, level=0.14, max_weight=1.75, rate_basis=360, vc_basis=252)
# x=r_smidai; libors=u$libors
volcontrol_excess = function(x, vc_params, libors){
    l3m = na.locf(na.locf(merge.xts(x, libors)[, 2]))
    l3m = l3m[index(l3m)%in%index(x)]
    l3m = if(vc_params$excess_type == 'libor plus') { l3m + vc_params$add_rate } else if (vc_params$excess_type == 'rate-related excess') { l3m*(1 + as.numeric(vc_params$excess)) } else l3m

    l3m_days = c(1, as.numeric(diff(index(l3m))))
    l3m = l3m*0.01*l3m_days/vc_params$rate_basis
    rfr = l3m

    #excess = if (vc_params$excess_type != 'rate-related excess') { print('Simple excess'); as.numeric(vc_params$excess)*0.01*l3m_days/vc_params$rate_basis } else 0.0
    excess = if (vc_params$excess_type != 'rate-related excess') { print('Simple excess'); as.numeric(vc_params$excess)*0.01/252 } else 0.0
    res = volcontrol(x - rfr, vc_params)
    res$rt = -excess + res$rt
    return(res)
}






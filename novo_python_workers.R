calc_env_from_csv = function(filename){
    tab = fread(filename)
    names(tab) = c('id', 'name', 'value')
    tab = tab[!is.na(id), .(name, value)]
    return( sapply(1:nrow(tab), FUN=function(i) { res=list(); res[[tab[i, name]]] = tab[i, value]; res }) )
}

calc_env_split = function(env){
    prefixes = c('screen_', 'vc_', 'uni_')
    res = list()
    for(prefix in prefixes){
        x = paste0(prefix, 'params')
        y = env[startsWith(names(env), prefix)]
        names(y) = gsub(prefix, '', names(y))
        res[[x]] = y
#        do.call('<-', list(x, y))
    }
    
#    res = list(screen_params=screen_params, vc_params=vc_params, uni_params=uni_params)
    res = c(res, env[!foreach(x=prefixes, .combine='|')%do%startsWith(names(env), x)])
    
    return(res)
}

# filename = '/home/aslepnev/webhub/strtelestate_current.csv'
bot_report_to_python = function(filename){
    params = calc_env_split(calc_env_from_csv(filename))
    if(params$type == 'index' && params$uni_params$type == 'straight')
        botrep = process_index_straight_request(params)
    else if(params$type == 'index' && params$uni_params$type == 'stocks and etfs')
        botrep = process_index_stocks_etfs_request(params)
    else if(params$type == 'wo')
        botrep = process_wo_request(params)
    
    for(i in names(botrep))
        fwrite(as.data.table(botrep[[i]]), file=gsub('current_name', paste0('current_', i), R_STATE_DATA_MASK))
}

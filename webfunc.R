params_from_params = function(params){
    
}

params_via_file = function(get_var, filename){
    filepath = paste0('/home/aslepnev/webhub/rapache_params/', filename, '.txt')
    if(!is.na(get_var)){
        writeLines(gsub('NEWLINE','\n',get_var), file(filepath))
        res = read.csv(file=filepath, header=FALSE, sep=',', stringsAsFactors=FALSE)
        return(res)
    } else return(read.csv(filepath))
}

web_datatable = function(data_list){
    for(n in names(data_list)){
        data = data_list[[n]]
        cat(
            paste0('NEWTAB:', n, '=', paste(foreach(col=colnames(data),.combine='c')%do%paste(c(col, as.character(data[, get(col)])), collapse=';'), collapse='NEWCOL'))
        )
    }
}

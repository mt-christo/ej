params_from_params = function(params){
    
}

web_datatable = function(data_list){
    for(n in names(data_list)){
        data = data_list[[n]]
        cat(
            paste0('NEWTAB:', n, '=', paste(foreach(col=colnames(data),.combine='c')%do%paste(c(col, as.character(data[, get(col)])), collapse=';'), collapse='NEWCOL'))
        )
    }
}

params_from_params = function(params){
    
}

# get_var=POST$params; filename='optimize_wo_excel'
params_via_file = function(get_var, filename){
    filepath = paste0('/home/aslepnev/webhub/rapache_params/', filename, '.txt')
    if(!is.na(get_var)){
        cat(gsub('NEWLINE','\n',gsub('\"', '', get_var)), file=filepath)
        res = fread(file=filepath, sep=',', stringsAsFactors=FALSE)
        return(res)
    } else return(fread(filepath))
}

web_datatable = function(data_list){
    for(n in names(data_list)){
        data = data_list[[n]]
        cat(
            paste0('NEWTAB:', n, '=', paste(foreach(col=colnames(data),.combine='c')%do%paste(c(col, as.character(data[, get(col)])), collapse=';'), collapse='NEWCOL'))
        )
    }
}

web_simple_table = function(tab){
    return(paste(foreach(i=1:nrow(tab))%do%paste(tab[i,], collapse=';'), collapse='@'))
}

wait_pids <- cfunction(body='int wstat; while (waitpid(-1, &wstat, WNOHANG) > 0) {};', includes='#include <sys/wait.h>', convention='.C')

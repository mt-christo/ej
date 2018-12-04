library(data.table)

comms = fread('/home/anton/git/ej/Commodities.csv')
for(comm in unique(comms$Name))
    for(code in comms[Name==comm, unique(Code)])
        if(code%in%list.files('/home/anton/Downloads/FUT'))
           for(f in list.files(paste0('/home/anton/Downloads/FUT/', code), full.names=TRUE))
           {
               x = fread(f)
               if(nrow(x) > 0)
               {
                   idx = which(diff(as.Date(unlist(x[-1 ,1]),format='%m/%d/%Y'))>500)
                   if(length(idx) > 0)
                       fwrite(x[1:(idx[1]+1)], file = f)
               }
           }
           

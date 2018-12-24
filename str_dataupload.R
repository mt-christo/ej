t1=foreach(x=paste0('test_funds',0:9,'1.csv'))%do%fread(paste0('/home/anton/',x),header=FALSE)
t2=foreach(x=paste0('test_funds',0:9,'2.csv'))%do%fread(paste0('/home/anton/',x),header=FALSE)
t3=foreach(x=paste0('test_funds',0:9,'3.csv'))%do%fread(paste0('/home/anton/',x),header=FALSE)
t = rbindlist(foreach(i=1:length(t1))%do%cbind(t1[[i]], t3[[i]]))
colnames(t) = c('ticker','name','asset_class','strategy','region','geography','category','focus','niche','inverse','leveraged',
                'etn','underlying_index','provider','selection_criteria','weighting_scheme','actove_per_sec')
save(t, file='/home/anton/git/ej/etf_com_universe.RData')


pet = foreach(x=t$ticker,.errorhandling='pass')%do%{ Sys.sleep(1.1); print(i); i=i+1; get(getSymbols(x))[,paste0(x,'.Adjusted')] }
save(pet, file='/home/anton/git/ej/pet_yhoo.RData')
# pet = get(load('/home/anton/git/ej/pet_yhoo.RData'))

save(pet, file='/home/anton/git/ej/etf_com_hist.RData')

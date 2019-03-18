homedir <<- 'aslepnev' #; homedir = 'anton'
setwd(paste0('/home/', homedir, '/webhub'))
Spreads <<- get(load('spreads_hist.RData'))
Spreads2 <<- get(load('spreads_hist2.RData'))

x = uni_adapt(get(load('/home/aslepnev/webhub/grish_iter0.RData')))
fwrite(x$u, file='/home/aslepnev/webhub/grish_iter0_adapted_u.csv')
write.csv(x$h, file='/home/aslepnev/webhub/grish_iter0_adapted_h.csv', row.names=index(x$h))


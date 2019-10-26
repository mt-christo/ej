-- FASHION fixed  --  2019-05-21
source('/home/aslepnev/git/ej/strindexlib.R')
u = load_uni('data-20190506', c('equity', 'equity_metrics', 'h_usd', 'fxx', 'libors'), list(fixed_list=c('4911 JP Equity', 'EL US Equity', 'PG US Equity', 'TSCO LN Equity', 'PEP US Equity', 'BN FP Equity', 'NESN SW Equity', 'WMT US Equity')))

h = u$h_usd
b = data.table(id=1, weight=1, name='ALL')
f = data.table(ticker=u$equity$ticker)[, ':='(basket_id=1, name=ticker, weight=0.25)][, id:=1:nrow(u$equity)]

chart_data = list()
wnd = 80; vc_vt = 0.14
r = build_index_simple(h, get_rebals_h(h, 'month'), smidai_style_rebal, screen_params=list(funds=f, baskets=b, window=wnd, voltarget=0.08), start_date='2013-12-31')
r_smidai = foreach(x=r,.combine=rbind)%do%x$h
rvc_smidai = volcontrol_excess(r_smidai, list(window=20, type='none', excess_type = 'libor plus', add_rate=1, excess=3.5, level=vc_vt, max_weight=2, rate_basis=360, vc_basis=252), libors)
chart_data = c(chart_data, list(list(segment=paste('risk/return opt rebal,', wnd, vc_vt*100, ' vt/3.5 excess/1 fee/1.75 exp/252'), rt=rvc_smidai)))
tail(index_perf(rvc_smidai), 1)

#r_mt = foreach(x=build_index_simpler(u, 'month', screen_mixed_top, screen_params=list(perf_weight=3, top_n=8, price_window=wnd, voltarget=0.075), '2013-12-31'),.combine=rbind)%do%x$h
#rvc_mt = volcontrol_excess(r_mt, list(window=20, type='none', excess_type = 'libor plus', add_rate=1, excess=3.5, level=vc_vt, max_weight=2, rate_basis=360, vc_basis=252), libors)
#chart_data = c(chart_data, list(list(segment=paste('topN', wnd, 2, '8/3/1/2.5/365'), rt=rvc_mt)))


xly = load_uni('data-20190506', c('etf', 'h_usd'), list(fixed_list=c('XLY     US Equity')))$h_usd
chart_data = c(chart_data, list(list(segment='XLY', rt=xly)))

library(ggthemes)
save_data_as_chart(multi_plot_1, rt_to_chart_data(chart_data), 'novo_chart.png', 400)
a = index_perf(rvc_smidai); write.csv(a, file='/home/aslepnev/webhub/basket8_backtest.csv', row.names=index(a))


-- FASHION fixed  --  2019-06-10
source('/home/aslepnev/git/ej/strindexlib.R')
u = load_uni('data-20190506', c('equity', 'equity_metrics', 'h_usd', 'fxx', 'libors'), list(fixed_list=c('4911 JP Equity', 'EL US Equity', 'PG US Equity', 'TSCO LN Equity', 'PEP US Equity', 'BN FP Equity', 'NESN SW Equity', 'WMT US Equity')))

fashion_fixed = function(u, vc_params){
    h = u$h_usd[index(u$h_usd)>='2011-01-01']
    b = data.table(id=1, weight=1, name='ALL')
    f = data.table(ticker=colnames(h))[, ':='(basket_id=1, name=ticker, weight=0.30)][, id:=1:ncol(h)]
    
    screen_params = list(funds=f, baskets=b, window=89, voltarget=0.082)
    r = build_index_simple(h, get_rebals_h(h, 'month'), smidai_style_rebal, screen_params, start_date='2013-12-31', vc_params)
    r_smidai = foreach(x=r,.combine=rbind)%do%x$h
    vc_params$sd = foreach(i = 1:length(vc_params$window))%do%{ foreach(x=r,.combine=rbind)%do%{ x$r_sd[[i]] } }
    rvc_smidai = volcontrol_excess(r_smidai, vc_params, u$libors)

    return(rvc_smidai)
}

a4 = fashion_fixed(u, list(window=c(20, 60), src='precalc', type='none', excess_type = 'libor plus', add_rate=1, excess=4, level=0.115, max_weight=2, rate_basis=360, vc_basis=252))
print(unlist(basic_index_report(a4$rt, 252)))
out_res = rbind(xts(t(array(0, ncol(a4))), order.by=index(a4)[1]-1), a4)
colnames(out_res) = c('core_index', 'index', 'exposure')
out_res$core_index = index_perf(out_res$core_index, FALSE)
out_res$index = index_perf(out_res$index, FALSE)
out_res$exposure = lag(out_res$exposure, -1)
write.csv(out_res, file='/home/aslepnev/webhub/basket8_backtest.csv', row.names=index(out_res))






-- IT10
source('/home/aslepnev/git/ej/strindexlib.R')

u = load_uni('data-20190506', c('equity', 'equity_metrics', 'h_usd', 'libors'),
             list(field_filter=c('us', 'tech'), skip_filter=c('no_card'), rank_filter=c('top 30 mcap')))
libors = u$libors
u[['h']] = u[['h_usd']]
#u = load_uni(c('equity', 'equity_metrics', 'h', 'libors', 'p'),
#             list(field_filter=c('us', 'tech'), skip_filter=c('no_card'), rank_filter=c('top 30 mcap')))
r = build_index_simpler(u, 'month', screen_mixed_top, screen_params=list(perf_weight=0.5, top_n=10, price_window=135), '2012-11-30')  
r1 = foreach(x=r,.combine=rbind)%do%x$h


# libors=u$libors; r_in=r; r1_in=r1; terms_in=c(3, 5); vc_targets=c(0.12, 0.14); vc_types=c('max 10', 'simple');
#rvc = r1; print(sqrt(252)*sd(tail(rvc, 252))); print(tail(exp(cumsum(rvc)), 1))
rvc = volcontrol_excess(r1, list(window=20, src='self', type='none', excess_type = 'libor plus', add_rate=1.0, excess=3.5, level=0.14, max_weight=1.75, rate_basis=360, vc_basis=252), u$libors); print(sqrt(252)*sd(tail(rvc, 252))); print(tail(exp(cumsum(rvc)), 1))
exp(cumsum(rvc))
write.csv(1000*exp(cumsum(rvc)), file='/home/aslepnev/webhub/it10.csv', row.names=index(rvc))

rvc = volcontrol_excess(r1, list(window=20, type='none', excess_type = 'libor plus', add_rate=1.0, excess=3.5, level=0.14, max_weight=1.5, rate_basis=360, vc_basis=252), u$libors); print(sqrt(252)*sd(tail(rvc, 252))); print(tail(exp(cumsum(rvc)), 1))
exp(cumsum(rvc))
write.csv(1000*exp(cumsum(rvc)), file='/home/aslepnev/webhub/it_max_150.csv', row.names=index(rvc))

dt_start=as.Date('2012-12-29'); dt_end=as.Date('9999-03-01'); 
rvc = volcontrol_excess(r1, list(window=20, type='none', excess_type='libor plus', add_rate=0.5, excess=2, level=0.05, max_weight=2, basis=360), libors)
print(sqrt(252)*sd(tail(rvc, 252))); print(tail(exp(cumsum(rvc[index(rvc)>=dt_start & index(rvc)<=dt_end])), 1))


u = load_uni('data-20190506', c('equity', 'equity_metrics', 'h_usd', 'libors'), list(field_filter=c('us', 'tech'), skip_filter=c('no_card'), rank_filter=c('top 80 mcap')))
r = build_index_simpler(u, 'month', screen_mixed_top, screen_params=list(perf_weight=1.5, top_n=10, price_window=135), '2012-12-31')
r_merged = foreach(x=r,.combine=rbind)%do%x$h
bask = foreach(x=r,.combine=rbind)%do%t(x$basket$main$names)
r_vc = volcontrol_excess(r_merged, list(window=c(20, 60), src='self', type='none', excess_type = 'libor plus', add_rate=1, excess=3.5, level=0.14, max_weight=2, rate_basis=360, vc_basis=252), u$libors)$rt
basic_index_report(r_vc[index(r_vc)<='2019-05-06', ], 252)


r_vc = volcontrol_excess(r_merged, list(window=20, src='self', type='none', excess_type = 'libor plus', add_rate=1, excess=3.5, level=0.14, max_weight=2, rate_basis=360, vc_basis=252), u$libors)$rt

,
                   list(vc_params=list(window=20, type='simple', excess_type = 'libor plus', add_rate=1, excess=3.5, level=0.14, max_weight=1.75)),
                   u$libors)

send_files_to_email(c(save_data_as_csv(idx$baskets, 'top10it_baskets.csv'),
                      save_data_as_csv(idx$perf, 'top10it_perf.csv'),
                      save_data_as_chart(idx$perf, 'Top 10 IT companies, monthly', 'top10it.png'),
                      latex_pm_card(idx$orig_data, foreach(x=idx$orig_data,.combine=rbind)%do%x$h, c(3,5),
                                    vc_targets=c(0.12, 0.14), vc_types=c('max 10', 'simple'), list(exrate=3.5, add_rate=1, max_weight=1.75), u$libors, 'it10')),
                    'Top 10 IT index', 'aslepnev@novo-x.info')

write.csv(exp(cumsum(rvc)), file='/home/aslepnev/webhub/it10.csv', row.names=index(rvc))
a = u$p[,u$equity_metrics[dt=="2017-02-01",][order(mcap,decreasing=TRUE),ticker],with=FALSE]
write.csv(a, file='/home/aslepnev/webhub/it10_backtest.csv', row.names=index(a))


-- new
source('/home/aslepnev/git/ej/strindexlib.R')
u = load_uni('data-20190506', c('equity', 'equity_metrics', 'h_usd', 'libors'), list(field_filter=c('us', 'commtech'), skip_filter=c('no_card'), rank_filter=c('top 80 mcap')))
r = build_index_simpler(u, 'month', screen_mixed_top, list(freq='month', perf_weight=2, top_n=10, price_window=130), '2012-05-29')  
bask = foreach(x=r,.combine=rbind)%do%t(x$basket$main$names)
r_merged = foreach(x=r,.combine=rbind)%do%x$h

write.csv(cbind(r_merged, u$libors[index(r_merged)]), file='/home/aslepnev/webhub/it_max_130.csv', row.names=index(r_merged))
r_vc = volcontrol_excess(r_merged, list(window=c(20, 21), src='self', type='none', excess_type = 'libor plus', add_rate=1, excess=3.5, level=0.14, max_weight=2.0, rate_basis=365, vc_basis=252), u$libors)$rt
basic_index_report(r_vc[index(r_vc)<='2019-03-31', ], 252)

a = index_perf(r_vc$rt[index(r_vc$rt)>='2013-01-01' & index(r_vc$rt)<='2019-03-31', ]); head(a); tail(a)
write.csv(1000*a, file='/home/aslepnev/webhub/it_max_150.csv', row.names=index(a))
write.csv(tail(bask, 1), file='/home/aslepnev/webhub/it_max_150_basket.csv')

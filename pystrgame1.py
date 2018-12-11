from multiprocessing import Pool
import numpy as np
import random

#def f(x):
#    for i in range(5):
#        a=(np.sin([random.random() for i in range(1000000)])*np.cos([random.random() for i in range(1000000)])/(np.cos([random.random() for i in range(1000000)])+4))**4
#with Pool(7) as p:
#    print(p.map(f, [1,2,3,4,5,6,7,8,9]))
 
from scipy.optimize import minimize, fmin_cobyla
import matplotlib.pyplot as plt
import pandas as pd
import cvxopt as opt
from cvxopt import blas, solvers
COV_WND = 5
WND_SHORT, WND_LONG = 63, 126
MIN_VOL, MAX_VOL = 0.05, 0.075

#f0 = pd.read_excel('/home/anton/git/ej/smidai_hist.xlsx', 'Sheet4')
#s0 = pd.read_excel('/home/anton/git/ej/smidai_hist.xlsx', 'Sheet5')
#h0 = pd.read_excel('/home/anton/git/ej/smidai_hist.xlsx', 'Sheet1')
f0 = pd.read_excel('/home/aslepnev/git/ej/smidai_hist.xlsx', 'Sheet4').set_index('SubID')
s0 = pd.read_excel('/home/aslepnev/git/ej/smidai_hist.xlsx', 'Sheet5').set_index('SubID')
h0 = pd.read_excel('/home/aslepnev/git/ej/smidai_hist.xlsx', 'Sheet1')

h0['Dates'] = pd.to_datetime(h0['Dates'])
h0.columns = [x.replace(' Index', '').replace(' US Equity', '').replace(' INDEX', '') for x in h0.columns]
h0 = h0.sort_values('Dates').set_index('Dates').assign(idx=range(len(h0)))
cols = [x for x in h0.columns if x!='US0003M' and x!='idx']
h0r1, h0r5 = h0, h0
h0r1 = h0[cols].pct_change(1).assign(US0003M=h0['US0003M']*0.01/252).fillna(0)
h0r5 = h0[cols].pct_change(COV_WND).assign(US0003M=h0['US0003M']*0.07/252).fillna(0)

#plt.plot((res+1).cumprod())
#plt.show()

# h1 - 1-day returns in columns
# h5 - 5-day returns in columns
# wlim = sequence of weight upper bounds, aligned with return columns above
if False:
    h1, h5, wlim, relax_type = data['h1_long'], data['h5_long'], data['wlim'], 'var'
def get_optimal_weights(h1, h5, wlim, relax_type):
    if type(wlim.tolist())!=list:
        return 1.0  # the only possible weight for 1-element basket is 1.0

    h1idx = [i for i in range(n) if h1.iloc[:,i].abs().max()>0]
    bnds = [(0, wlim[i]) for i in h1idx]
    wlim0 = [wlim[i] for i in h1idx]
    h5 = h5.iloc[:, [i for i in h1idx]]
    h1 = h1.iloc[:, [i for i in h1idx]]
    n = len(wlim0)
    
    min_vol, max_vol = MIN_VOL, MAX_VOL
    def negtotret(x):  # our main functionto minimize - negative total return
        return -((h1.multiply(x).sum(axis=1)+1).prod()-1)
    def c1(x):
        return h5.multiply(x).cov().sum().sum()*250/COV_WND - (min_vol**2)
    def var_pos(x):
        return h5.multiply(x).cov().sum().sum()*250/COV_WND
    def var_neg(x):
        return -h5.multiply(x).cov().sum().sum()*250/COV_WND
    def c2(x):
        return (max_vol**2) - h5.multiply(x).cov().sum().sum()*250/COV_WND
    def c3(x):
        return 0.00001-round(abs(np.sum(x) - 1),6)
    def c4(x):
        return min(x)
    def c5(x):
        return min([wlim0[i]-x[i] for i in range(len(x))])

    
    cons = [{'type': 'ineq', 'fun': c1},
            {'type': 'eq', 'fun': c2}]   


    x0 = [(random.random()*wlim0[i]) for i in range(n)]
    for iii in range(20):
        x0 = x0/np.sum(x0)
        x0 = [x0[i] if x0[i]<=wlim0[i] else wlim0[i] for i in range(n)]
        if c2(x0)==0:
            break
    x = minimize(c1neg, x0, method='SLSQP', bounds=bnds, constraints=[{'type': 'eq', 'fun': c3}])

    x1 = minimize(var_pos, x0, method='COBYLA', constraints=[{'type': 'ineq', 'fun': c3}, {'type': 'ineq', 'fun': c4}, {'type': 'ineq', 'fun': c5}])
    x2 = minimize(var_neg, x0, method='COBYLA', constraints=[{'type': 'ineq', 'fun': c3}, {'type': 'ineq', 'fun': c4}, {'type': 'ineq', 'fun': c5}])


    fmin_cobyla(c1neg, x0, [c3, c4, c5], rhobeg=0.1, catol=0.000001)
    
    n, res, is_success = len(wlim), 0, False
    while not is_success:
        print('trying.. min_vol='+str(min_vol)+', max_vol='+str(max_vol))
        
        is_success, x0 = False, 0
        for ii in range(10000):
            if(ii%1000==0):
                print(ii)
            x0 = [(random.random()*wlim[i]) if h1sign[i]==1 else 0 for i in range(n)]
            for iii in range(20):
                x0 = x0/np.sum(x0)
                x0 = [x0[i] if x0[i]<=wlim[i] else wlim[i] for i in range(n)]
                if c2(x0)==0:
                    break
            is_success = (c1(x0) >= 0 and c2(x0)==0)
            if is_success:
                break;
            
        if is_success:
            res = minimize(negtotret, x0, bounds=bnds, constraints=cons)
            is_success = res['success']



        res1 = minimize(negtotret, x0, method='Nelder-Mead', bounds=bnds)
        res2 = minimize(negtotret, x0, method='Powell', bounds=bnds, constraints=cons)
        res3 = minimize(negtotret, x0, method='CG', bounds=bnds, constraints=cons)
        res4 = minimize(negtotret, x0, method='BFGS', bounds=bnds, constraints=cons)
        res5 = minimize(negtotret, x0, method='Newton-CG', bounds=bnds, constraints=cons)

        res6 = minimize(negtotret, x0, method='COBYLA', bounds=bnds, constraints=cons)
        
        res6 = minimize(negtotret, x0, method='COBYLA', bounds=bnds, constraints=cons, options={'catol':0.00000001}) 
        

        return h5.multiply(x).cov().sum().sum()*250/COV_WND - (min_vol**2)
        return (max_vol**2) - h5.multiply(x).cov().sum().sum()*250/COV_WND
        return np.sum(x) - 1
        return min([h1max[i] for i in range(len(x)) if x[i]!=0]) - 0.001
        def negtotret(x):
            if (h5.multiply(x).cov().sum().sum()*250/COV_WND < (min_vol**2)) or (h5.multiply(x).cov().sum().sum()*250/COV_WND > (max_vol**2)) or (np.sum(x)!=1) or (min([h1max[i] for i in range(len(x)) if x[i]!=0]) == 0):
                return 1
            else:
                return -((h1.multiply(x).sum(axis=1)+1).prod()-1)
        res7 = minimize(negtotret, x0, method='SLSQP', bounds=bnds, constraints=cons)




        def negtotret(x):  # our main functionto minimize - negative total return
            return -((h1.multiply(x).sum(axis=1)+1).prod()-1)
        def c1(x):  # constraint: 5-day annualized covariance to be higher than MIN_COV
            c = h5.multiply(x).cov().sum().sum()*250/COV_WND
            return 100*min(c - (min_vol**2), (max_vol**2) - c)
        def c2(x):  # weights have to sum up to 1
            return -100*abs(np.sum(x) - 1)
        def c3(x):  # weights have to be positive
            return min(x)
        def c4(x):  # weights have to be below caps
            return 10000*min(wlim-x)
        def c5(x):  # 
            return -100*sum([abs(x[i]) for i in range(len(x)) if h1max[i]==0])

        is_success, x0 = False, 0
        for ii in range(20000):
            x0 = [random.random()*wlim[i] if h1max[i]!=0 else 0 for i in range(len(wlim))]
            x0 = x0/np.sum(x0)
            is_success = (min([cons[i]['fun'](x0) for i in [0,1,3]]) >= 0)
            if is_success:
                break;
    
        x = fmin_cobyla(negtotret, x0, [c1, c2, c3, c4, c5], rhobeg=0.1, catol=0.000001)
#        x = fmin_cobyla(negtotret, [random.random()*wlim[i] if h1max[i]!=0 else 0 for i in range(len(wlim))], [c1, c2, c31, c32, c33, c4], catol=0.000000001)
        print([negtotret(x), c1(x), c2(x), c3(x), c4(x), c5(x)])
        print(x)
        print([c1(x0), c2(x0), c3(x0), c4(x0), c5(x0)])



        def negtotret(x):  # our main functionto minimize - negative total return
            return -((h1.multiply(x).sum(axis=1)+1).prod()-1)
        def c1(x):  # constraint: 5-day annualized covariance to be higher than MIN_COV
            c = h5.multiply(x).cov().sum().sum()*250/COV_WND
            return 100*min(c - (min_vol**2), (max_vol**2) - c)
        def c2(x):  # weights have to sum up to 1
            return -100*abs(np.sum(x) - 1)
        def c3(x):  # weights have to be positive
            return min(x)
        def c4(x):  # weights have to be below caps
            return 10000*min(wlim-x)
        def c5(x):  # 
            return -100*sum([abs(x[i]) for i in range(len(x)) if h1max[i]==0])



        
        res8 = minimize(negtotret, x0, method='dogleg', bounds=bnds, constraints=cons)
        res9 = minimize(negtotret, x0, method='trust-ncg', bounds=bnds, constraints=cons)


            
        if (not is_success) and relax_type=='var' and min_vol>0:
            min_vol = min_vol - 0.01
        elif (not is_success) and relax_type=='var' and max_vol<1.0:
            max_vol = max_vol + 0.025
        elif (not is_success) and relax_type=='cash' and bnds[-1][1]<1.0:  # here, we strictly assume that Cash is the last bound
            bnds[-1][1] = bnds[-1][1] + 0.1
        elif not is_success:  # this means we are out of options and fail
            res = -1
            is_success = True        
        
    return res['x']

def get_optimal_subbasket(data):
    print('dt: ' + str(data['dt']) + ', sub_id: ' + str(data['sub_id']))
    weights = 0.5 * (get_optimal_weights(data['h1_short'], data['h5_short'], data['wlim'], data['relax_type'])  # optimal weights over short window
                     + get_optimal_weights(data['h1_long'], data['h5_long'], data['wlim'], data['relax_type']))  # optimal weights over long window
    return {'sub_id': data['sub_id'],
            'weights': pd.DataFrame({'code': data['codes'], 'weight': weights if type(wlim.tolist())==list else [1.0]}),
            'hist': data['hist'].multiply(weights).sum(axis=1) if type(wlim.tolist())==list else data['hist']}
    
if False:
    subbaskets, returns_1d, returns_5d, dt = f0, h0r1, h0r5, '2006-04-24'  # assuming that Date is index in returns dataframes
#    sub_id = 1    
#    h1, h5 = h1_short, h5_short
#    negtotret(weights)
def get_optimal_subbaskets(subbaskets, returns_1d, returns_5d, dt):  # returns {weights by sub-basket, total return time series by sub-basket}
    # Transform to incremental indexing & find index of this rebal date
    h1_par = returns_1d.assign(idx=range(len(returns_1d)))
    idx = h1_par.loc[dt, 'idx'] - 4  # per index rules
    idx_short, idx_long = max(0, idx-WND_SHORT), max(0, idx-WND_LONG)
    h1_par = h1_par.set_index('idx')
    h5_par = returns_5d.assign(idx=range(len(returns_1d))).set_index('idx')

    # Parallelize slow optimization
    res = Pool(6).map(get_optimal_subbasket,
                      [{'sub_id': sub_id,
                        'dt': dt,
                        'relax_type': 'var',  # we are optimizing subbasket here, as per index rules
                        'h1_short': h1_par.loc[idx_short:idx, subbaskets.loc[sub_id, 'FundCode']],
                        'h5_short': h5_par.loc[idx_short:idx, subbaskets.loc[sub_id, 'FundCode']],
                        'h1_long': h1_par.loc[idx_long:idx, subbaskets.loc[sub_id, 'FundCode']],
                        'h5_long': h5_par.loc[idx_long:idx, subbaskets.loc[sub_id, 'FundCode']],
                        'wlim': np.array(subbaskets.loc[sub_id, 'FundMax']),
                        'hist': returns_1d[subbaskets.loc[sub_id, 'FundCode']],
                        'codes': subbaskets.loc[sub_id, 'FundCode']}
                       for sub_id in subbaskets.reset_index().SubID.unique()])
    
    res = [get_optimal_subbasket(x) for x in [{'sub_id': sub_id,
                        'dt': dt,
                        'relax_type': 'var',  # we are optimizing subbasket here, as per index rules
                        'h1_short': h1_par.loc[idx_short:idx, subbaskets.loc[sub_id, 'FundCode']],
                        'h5_short': h5_par.loc[idx_short:idx, subbaskets.loc[sub_id, 'FundCode']],
                        'h1_long': h1_par.loc[idx_long:idx, subbaskets.loc[sub_id, 'FundCode']],
                        'h5_long': h5_par.loc[idx_long:idx, subbaskets.loc[sub_id, 'FundCode']],
                        'wlim': np.array(subbaskets.loc[sub_id, 'FundMax']),
                        'hist': returns_1d[subbaskets.loc[sub_id, 'FundCode']],
                        'codes': subbaskets.loc[sub_id, 'FundCode']}
                       for sub_id in subbaskets.reset_index().SubID.unique()]]
    return {'weights': dict(zip([x['sub_id'] for x in res], [x['weights'] for x in res])),
            'hist': pd.DataFrame.from_dict(dict(zip([x['sub_id'] for x in res], [x['hist'] for x in res])))}

def get_optimal_hist(baskets, subbaskets, returns_1d, returns_5d, dates):  # assuming that (rebal)dates exist in history time series
    sub_weights = {}
    sub_hist = {}
    basket_weights = {}
    basket_hist = {}
    for dt in dates:
        s = get_optimal_subbaskets(subbaskets, returns_1d, returns_5d, dt)  # calculate optimal subbaskets

        # Histories in usual naming + extract weekly returns
        h1 = s['hist']
        h5 = (h1+1).cumprod().pct_change(COV_WND)

        # Transform to incremental indexing & find index of this rebal date
        h1 = h1.assign(idx=range(len(h1)))
        idx = h1.loc[dt, 'idx']
        h1 = h1.set_index('idx')
        h5 = h5.assign(idx=range(len(h5))).set_index('idx')

        # Extract short and long history for optimization
        h1_short = h1.loc[max(0, idx-WND_SHORT-4):(idx-4), baskets.SubID]
        h5_short = h5.loc[max(0, idx-WND_SHORT-4):(idx-4), baskets.SubID]
        h1_long = h1.loc[max(0, idx-WND_LONG-4):(idx-4), baskets.SubID]
        h5_long = h5.loc[max(0, idx-WND_LONG-4):(idx-4), baskets.SubID]
        weights = 0.5 * (get_optimal_weights(h1_short, h5_short, np.array(baskets.SubMax))  # optimal weights over short window
                         + get_optimal_weights(h1_long, h5_long, np.array(baskets.SubMax)))  # optimal weights over long window

        sub_weights[dt] = s['weights']
        basket_weights[dt] = weights

    r = returns_1d.assign(idx=range(len(returns_1d)))
    res = pd.concat([basket_hist[dates[i]][r.loc[dates[i], 'idx']:(r.loc[dates[i+1], 'idx'] if i<len(dates)-1 else len(r))]
                     for i in range(len(dates))])
    return res


if False:
    baskets, subbaskets, returns_1d, returns_5d, dates = s0, f0, h0r1, h0r5, ['2018-01-03', '2018-04-03']  # assuming that Date is index in returns dataframes

    # Extract calendar from given returns
    dates = pd.DataFrame({'date': returns_1d.reset_index()['Dates']})
    dates = dates.assign(month=dates.date.dt.month, year=dates.date.dt.year).groupby(['year', 'month']).agg({'date': 'min'}).reset_index()['date']
    dates = list(dates[1:(len(dates)-2)])

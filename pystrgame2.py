import pickle
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
MIN_STD, MAX_STD, STD_STEP_UP, STD_STEP_DOWN, OPT_ATTEMPTS = 0.05, 0.075, 0.025, 0.01, 10

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
h0r1 = h0[cols].pct_change(1).assign(US0003M=h0['US0003M']*0.01/252, idx=range(len(h0))).fillna(0)
h0r5 = h0[cols].pct_change(COV_WND).assign(US0003M=h0['US0003M']*0.07/252, idx=range(len(h0))).fillna(0)

#plt.plot((res+1).cumprod())
#plt.show()

# h1 - 1-day returns in columns
# h5 - 5-day returns in columns
# wlim = sequence of weight upper bounds, aligned with return columns above
# relax_type in ['vol', 'cash']
if False:
    h1_in, h5_in, wlim, relax_type = data['h1_long'], data['h5_long'], data['wlim'], 'vol'
    h1_in, h5_in, wlim, relax_type = h1_short, h5_short, np.array(baskets.SubMax), 'cash'
    res = get_optimal_weights(h1_in, h5_in, wlim, relax_type)
def get_optimal_weights(h1_in, h5_in, wlim, relax_type):

    # the only possible weight for 1-element basket is 1.0
    if type(wlim.tolist())!=list:
        return 1.0  

    eidx = [i for i in range(len(wlim)) if h1_in.iloc[:,i].abs().max()>0]  # column indices of existing returns
    wlime = [wlim[i] for i in eidx]  # weight caps of existing assets
    h5 = h5_in.iloc[:, [i for i in eidx]]
    h1 = h1_in.iloc[:, [i for i in eidx]]
    n, min_std, max_std, res  = len(eidx), MIN_STD, MAX_STD, -1
    
    def negtotret(x):  # our main functionto minimize - negative total return
        return -((h1.multiply(x).sum(axis=1)+1).prod()-1)
    def std_pos(x):  # standard deviation
        return np.sqrt(h5.multiply(x).cov().sum().sum()*250/COV_WND)
    def std_neg(x):  # negative standard deviation
        return -std_pos(x)
    def c_std1(x):  # constraint: for standard deviation above min_vol
        return std_pos(x) - min_std
    def c_std2(x):  # constraint: for standard deviation below max_vol
        return max_std - std_pos(x)
    def c_weight1(x):  # constraint: sum(x) == 1
        return 0.0000001-round(abs(np.sum(x) - 1.0), 6)
    def c_weight2(x):  # constraint: all weights are positive
        return min(x)
    def c_weight3(x):  # constraint: all weighte are below respective caps
        return min([wlime[i]-x[i] for i in range(len(x))])
    def attempt_minimize(params, attempts, do_tweak=True):
        for i in range(attempts):
#            print(i)
            res = minimize(params[0], initial_guess() if params[1]=='guess' else tweak_weights(params[1]) if do_tweak else params[1], method=params[2], constraints=params[3])
            if res['success']:
                return res
        return -1
    def flatten_weights(y):
        x = [abs(a) for a in y]
        for iii in range(100):
            x = x/np.sum(x)
            x = [x[i] if x[i]<=wlime[i] else wlime[i] for i in range(n)]
            if c_weight1(x)>=0.0000001 and c_weight2(x)>=0 and c_weight3(x)>=0:
                break
        return x
    def tweak_weights(y):
        return [x*(1-random.random()*0.000099) for x in y]
    def initial_guess():
        return flatten_weights(wlime/np.sum(wlime))
    def align_result(x):
        return [x[eidx.index(i)] if i in eidx else 0 for i in range(len(wlim))]

    weight_constraints = [{'type': 'ineq', 'fun': c_weight1}, {'type': 'ineq', 'fun': c_weight2}, {'type': 'ineq', 'fun': c_weight3}]
    std_constraints = [{'type': 'ineq', 'fun': c_std1}, {'type': 'ineq', 'fun': c_std2}]

    # Find weight-constrained initial guess (c_weight2 and c_weight3 met by design)
    std_low = attempt_minimize([std_pos, 'guess', 'COBYLA', weight_constraints], OPT_ATTEMPTS)
    if std_low!=-1:
        x0 = std_low['x']
    else:
        x0 = initial_guess()
    
    if relax_type=='vol':  # Find variance limits which surround volatility optimum
        if std_low!=-1 and std_low['fun'] >= min_std:  # => volatility low is above low cap
            x0 = std_low['x']
            if std_low['fun'] > max_std:  # => volatility condition should be relaxed upwards
                max_std = max_std + (int((std_low['fun']-max_std)/STD_STEP_UP)+1)*STD_STEP_UP
            res = attempt_minimize([negtotret, flatten_weights(x0), 'COBYLA', weight_constraints+std_constraints], OPT_ATTEMPTS)
            if res!=-1:
                return align_result(flatten_weights(res['x']))
            else:
                return align_result(flatten_weights(x0))  # This means something is definitely wrong
            
        std_high = attempt_minimize([std_neg, 'guess', 'COBYLA', weight_constraints], OPT_ATTEMPTS)
        if std_high!=-1 and std_high['fun'] < max_std:  # => volatility high is below high cap
            x0 = std_high['x']
            if std_high['fun'] < min_std:  # => volatility condition should be relaxed downwards
                min_std = min_std - (int((min_std-std_high['fun'])/STD_STEP_DOWN)+1)*STD_STEP_DOWN
            res = attempt_minimize([negtotret, flatten_weights(x0), 'COBYLA', weight_constraints+std_constraints], OPT_ATTEMPTS)
            if res!=-1:
                return align_result(flatten_weights(res['x']))
            else:
                return align_result(flatten_weights(x0))  # This means something is definitely wrong
#fmin_cobyla(c1neg, x0, [c3, c4, c5], rhobeg=0.1, catol=0.000001)
    else:  # Find valid Cash level    
        if std_low!=-1:  # => volatility low is above low cap
            while std_low['fun']>max_std and wlime[n-1]<1.0:
                wlime[n-1] = wlime[n-1] + 0.1
                std_low = attempt_minimize([std_pos, 'guess', 'COBYLA', weight_constraints], OPT_ATTEMPTS)
            if std_low!=-1 and std_low['fun']<max_std:
                x0 = std_low['x']
            else:
                return align_result(initial_guess())  # This means something is definitely wrong

    res = attempt_minimize([negtotret, flatten_weights(x0), 'COBYLA', weight_constraints+std_constraints], OPT_ATTEMPTS)
    if res!=-1:
        return align_result(flatten_weights(res['x']))
    else:
        return align_result(initial_guess())  # This means something is definitely wrong
             
def get_optimal_subbasket(data):
    print('dt: ' + str(data['dt']) + ', sub_id: ' + str(data['sub_id']))
    wlong = get_optimal_weights(data['h1_long'], data['h5_long'], data['wlim'], data['relax_type'])
    wshort = get_optimal_weights(data['h1_short'], data['h5_short'], data['wlim'], data['relax_type'])
    if wlong==-1 or wshort==-1:
        return -1
    weights = 0.5 * (np.array(wlong) + np.array(wshort))
    w1 = (type(weights.tolist())==list)  # for future 1-row dataframe processing
    return {'sub_id': data['sub_id'],
            'weights': pd.DataFrame({'code': data['codes'] if w1 else [data['codes']], 'weight': weights if w1 else [1.0]}),
            'vol_short': np.sqrt(data['h5_short'].multiply(weights).cov().sum().sum()*250/COV_WND) if w1 else [np.sqrt(data['h5_short'].var()*250/COV_WND)],
            'vol_long': np.sqrt(data['h5_long'].multiply(weights).cov().sum().sum()*250/COV_WND) if w1 else [np.sqrt(data['h5_long'].var()*250/COV_WND)],
            'hist': data['hist'].multiply(weights).sum(axis=1) if w1 else data['hist']}
    
if False:
    subbaskets, h1, h5, dt = f0, h0r1, h0r5, '2008-05-30'  # assuming that Date is index in returns dataframes
#    sub_id = 1    
#    h1, h5 = h1_short, h5_short
#    negtotret(weights)
def get_optimal_subbaskets(subbaskets, h1, h5, dt):  # returns {weights by sub-basket, total return time series by sub-basket}
    # Transform to incremental indexing & find index of this rebal date
    h1p = h1.assign(idx=range(len(h1)))
    h5p = h5.assign(idx=range(len(h1)))
    idx = h1p.loc[dt, 'idx'] - 4  # per index rules
    idx_short, idx_long = max(0, idx-WND_SHORT), max(0, idx-WND_LONG)
    h1p = h1p.set_index('idx')
    h5p = h5p.set_index('idx')
    
    h1_short = h1p.loc[idx_short:idx, :]
    h5_short = h5p.loc[idx_short:idx, :]
    h1_long = h1p.loc[idx_long:idx, :]
    h5_long = h5p.loc[idx_long:idx, :]

    # Parallelize slow optimization
    res = Pool(6).map(get_optimal_subbasket,
                      [{'sub_id': sub_id,
                        'dt': dt,
                        'relax_type': 'vol',  # we are optimizing subbasket here, as per index rules
                        'h1_short': h1_short.loc[:, subbaskets.loc[sub_id, 'FundCode']],
                        'h5_short': h5_short.loc[:, subbaskets.loc[sub_id, 'FundCode']],
                        'h1_long': h1_long.loc[:, subbaskets.loc[sub_id, 'FundCode']],
                        'h5_long': h5_long.loc[:, subbaskets.loc[sub_id, 'FundCode']],
                        'wlim': np.array(subbaskets.loc[sub_id, 'FundMax']),
                        'hist': h1p.loc[(idx_long-2*COV_WND):idx, subbaskets.loc[sub_id, 'FundCode']],
                        'codes': subbaskets.loc[sub_id, 'FundCode']}
                       for sub_id in subbaskets.reset_index().SubID.unique()])
    return {'weights': dict(zip([x['sub_id'] for x in res], [x['weights'] for x in res])),
            'vols_short': dict(zip([x['sub_id'] for x in res], [x['vol_short'] for x in res])),
            'vols_long': dict(zip([x['sub_id'] for x in res], [x['vol_long'] for x in res])),
            'hist': pd.DataFrame.from_dict(dict(zip([x['sub_id'] for x in res], [x['hist'] for x in res])))}

def get_optimal_hist(baskets, subbaskets, returns_1d, returns_5d, dates):  # assuming that (rebal)dates exist in history time series
    sub_weights = {}
    sub_hist = {}
    basket_weights = {}
    basket_hist = {}
    for dt in pd.to_datetime(dates):
        s = get_optimal_subbaskets(subbaskets, returns_1d, returns_5d, dt)  # calculate optimal subbaskets

        # Histories in usual naming + extract weekly returns
        h1p = s['hist']#.assign(idx=range(len(s['hist'])))
        h5p = (h1p+1).cumprod()\
                     .pct_change(COV_WND)#\
                     #.assign(idx=range(len(h1p)))

        # Transform to incremental indexing & find index of this rebal date
        idx = returns_1d.loc[dt, 'idx'] - 4  # per index rules
        idx_short, idx_long = max(0, idx-WND_SHORT), max(0, idx-WND_LONG)
#        h1p = h1p.set_index('idx')
#        h5p = h5p.set_index('idx')
        
        # Extract short and long history for optimization
        sub_ids = baskets.reset_index().SubID
        h1_short, h5_short = h1p.loc[idx_short:idx, sub_ids], h5p.loc[idx_short:idx, sub_ids]
        h1_long, h5_long = h1p.loc[idx_long:idx, sub_ids], h5p.loc[idx_long:idx, sub_ids]
        wlong = get_optimal_weights(h1_short, h5_short, np.array(baskets.SubMax), 'cash')  # optimal weights over short window
        wshort = get_optimal_weights(h1_long, h5_long, np.array(baskets.SubMax), 'cash')  # optimal weights over long window
        weights = 0.5 * (np.array(wlong) + np.array(wshort))

        res = {'dt_end': returns_1d[returns_1d.idx==idx].index.to_series()[0],
               'sub_weights': s['weights'],
               'sub_vols_short': s['vols_short'],
               'sub_vols_long': s['vols_long'],
               'basket_weights': weights}
        pickle.dump(res, open('/home/aslepnev/git/ej/pystr_weights/' + str(dt).split(" ")[0], 'wb'))
#        res1 = pickle.load(open('/home/aslepnev/git/ej/pystr_weights/' + str(dt).split(" ")[0], 'rb'))
        

    r = returns_1d.assign(idx=range(len(returns_1d)))
    res = pd.concat([basket_hist[dates[i]][r.loc[dates[i], 'idx']:(r.loc[dates[i+1], 'idx'] if i<len(dates)-1 else len(r))]
                     for i in range(len(dates))])
    return res


if False:
    baskets, subbaskets, returns_1d, returns_5d, dates = s0, f0, h0r1, h0r5, ['2018-01-03', '2018-04-03']  # assuming that Date is index in returns dataframes

    # Extract calendar from given returns
    dates = pd.DataFrame({'date': returns_1d.reset_index()['Dates']})
    dates = dates.assign(month=dates.date.dt.month, year=dates.date.dt.year).groupby(['year', 'month']).agg({'date': 'min'}).reset_index()['date']
    dates = list(dates[3:(len(dates)-2)])
#    dt = dates[0]
dt = pd.to_datetime('2008-05-23')
#    dt_start = dates[0]
#    dt_end = dates[1]
#    dates = [x for x in dates if x>=pd.to_datetime('2012-05-01')]


sub_ids = list(subbaskets.reset_index().SubID.unique())
def pnl_fromfile(dt_start, dt_end):
    res = pickle.load(open('/home/anton/git/ej/pystr_weights/' + str(dt_start).split(" ")[0], 'rb'))
#    res = pickle.load(open('/home/aslepnev/git/ej/pystr_weights/' + str(dt_start).split(" ")[0], 'rb'))
    sweights = res['sub_weights']
    pweights = res['basket_weights']
    w = pd.concat([x.assign(weight=x.weight*pweights[sub_ids.index(i)])
                   for i, x in zip(sweights.keys(), sweights.values())])\
          .groupby('code')\
          .agg({'weight': 'sum'})

    phist = returns_1d.reset_index()
    phist = phist[(phist.Dates>=dt_start) & (phist.Dates<dt_end)]
    pnl = phist[w.index].multiply(w['weight']).sum(axis=1)
    pnl = pd.DataFrame({'pnl': pnl, 'dt': phist['Dates']})
    for code in w.index:
        pnl[code] = w.loc[code, 'weight']
        
    return pnl


if False:
    x, voltarget, vol_wnd, roll_wnd, max_weight = H['pnl'], 0.085, 5, 20, 2.5
def vol_control(x, voltarget, roll_wnd, max_weight):
    res = voltarget/(np.log(1+x).rolling(roll_wnd).std()*np.sqrt(252))
    res.loc[res > max_weight] = max_weight
    return res

H = pd.concat([pnl_fromfile(dates[i], dates[i+1]) for i in range(len(dates)-1)])
H['weight_vc'] = vol_control(H['pnl'], 0.085, 20, 2.5)
H['pnl_vc'] = H['pnl']*H['weight_vc']


print(np.log(H.pnl+1).std()*np.sqrt(252))
print(np.log(H.pnl_vc+1).std()*np.sqrt(252))

print((H.pnl[H.dt>='2008-09-30']+1).prod())
print((H.pnl_vc[H.dt>='2008-09-30']+1).prod())

print((H.pnl_vc[H.dt>'2006-03-01']+1).prod())

H.to_csv('/home/anton/Documents/H.csv')




h = returns_5d
h = h.assign(idx=range(len(h)))
idx1, idx2, idx3 = h.loc['2007-12-21', 'idx'], h.loc['2008-02-22', 'idx'], h.loc['2008-05-23', 'idx']
h = h.reset_index().set_index('idx').loc[idx1:idx3, ['ROBOTR', 'IXP', 'SOXX', 'US0003M']]
w = [0.388, 0.505, 0.007, 0.1]
print(np.sqrt(h.multiply(w).cov().sum().sum()*252/5))



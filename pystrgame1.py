import random
from scipy.optimize import minimize
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import cvxopt as opt
from cvxopt import blas, solvers
COV_WND = 5
WND_SHORT, WND_LONG = 63, 126
MIN_COV, MAX_COV = 0.05**2, 0.075**2

f0 = pd.read_excel('/home/anton/git/ej/smidai_hist.xlsx', 'Sheet4')
s0 = pd.read_excel('/home/anton/git/ej/smidai_hist.xlsx', 'Sheet5')

h0 = pd.read_excel('/home/anton/git/ej/smidai_hist.xlsx', 'Sheet1')
h0['Dates'] = pd.to_datetime(h0['Dates'])
h0.columns = [x.replace(' Index', '').replace(' US Equity', '').replace(' INDEX', '') for x in h0.columns]
h0 = h0.sort_values('Dates').set_index('Dates').assign(idx=range(len(h0)))
cols = [x for x in h0.columns if x!='US0003M' and x!='idx']
h0r1, h0r5 = h0, h0
h0r1 = h0[cols].pct_change(1).assign(US0003M=h0['US0003M']*0.01/252).fillna(0)
h0r5 = h0[cols].pct_change(COV_WND).assign(US0003M=h0['US0003M']*0.07/252).fillna(0)



sub_id = 1
h1 = h0r1.loc['2018-01-01':'2018-04-01', f0[f0.SubID==sub_id].FundCode]
h5 = h0r5.loc['2018-01-01':'2018-04-01', f0[f0.SubID==sub_id].FundCode]
w = np.array(f0[f0.SubID==sub_id].FundMax)

mus = list()
vs = list()
for i in range(100000):
    wi = [random.random()*w[j] for j in range(len(w))]
    wi = wi/np.sum(wi)
    mus = mus + [(h1.multiply(wi).sum(axis=1)+1).prod()-1]
    vs = vs + [h5.multiply(wi).cov().sum().sum()*250/WND]
    
plt.plot(vs,mus,'o')
plt.show()

plt.plot((res+1).cumprod())
plt.show()

# h1 - 1-day returns in columns
# h5 - 5-day returns in columns
# wlim = sequence of weight upper bounds, aligned with return columns above
def get_optimal_weights(h1, h5, wlim):
    def negtotret(x):  # our main functionto minimize - negative total return
        return -((h1.multiply(x).sum(axis=1)+1).prod()-1)
    
    def constraint1(x):  # constraint: 5-day annualized covariance to be higher than MIN_COV
        return h5.multiply(x).cov().sum().sum()*250/WND - MIN_COV

    def constraint2(x):  # constraint: 5-day annualized covariance not to exceed MAX_COV
        return MAX_COV - h5.multiply(x).cov().sum().sum()*250/WND
    
    def constraint3(x):  # weights have to sum up to 1
        return np.sum(x) - 1
    
    def constraint4(x):  # constraint is OK only if non-zero returns exist in columns with non-zero weights
        return min([h1max[i] for i in range(len(x)) if x[i]!=0]) - 0.001
    
    x0 = [random.random()*x for x in wlim]
    x0 = x0/np.sum(x0)    
    bnds = [(0, x) for x in wlim]
    con1 = {'type': 'ineq', 'fun': constraint1}
    con2 = {'type': 'ineq', 'fun': constraint2}
    con3 = {'type': 'eq', 'fun': constraint3}
    con4 = {'type': 'ineq', 'fun': constraint4}
    cons = [con1, con2, con3, con4]
    h1max = [h1.iloc[:,i].abs().max() for i in range(len(wlim))]
    return minimize(negtotret, x0, method='SLSQP', bounds=bnds, constraints=cons)['x']

if False:
    baskets, returns_1d, returns_5d, dt = f0, h0r1, h0r5, '2006-04-24'  # assuming that Date is index in returns dataframes
    h1, h5 = h1_short, h5_short
    negtotret(weights)
def get_optimal_subbaskets(baskets, returns_1d, returns_5d, dt):  # returns {weights by sub-basket, total return time series by sub-basket}
    # Transform to incremental indexing & find index of this rebal date
    h1 = returns_1d.assign(idx=range(len(returns_1d)))
    idx = h1.loc[dt, 'idx']
    h1 = h1.set_index('idx')
    h5 = returns_5d.assign(idx=range(len(returns_1d))).set_index('idx')
    
    basket_weights = {}
    basket_hist = {}
    for sub_id in baskets.SubID.unique():
        print('dt: ' + str(dt) + ', sub_id: ' + str(sub_id))
        basket = baskets[baskets.SubID==sub_id]
        
        # Extract short and long history for optimization
        h1_short = h1.loc[max(0, idx-WND_SHORT):idx, basket.FundCode]
        h5_short = h5.loc[max(0, idx-WND_SHORT):idx, basket.FundCode]
        h1_long = h1.loc[max(0, idx-WND_LONG):idx, basket.FundCode]
        h5_long = h5.loc[max(0, idx-WND_LONG):idx, basket.FundCode]
        weights = 0.5 * (get_optimal_weights(h1_short, h5_short, np.array(basket.FundMax))  # optimal weights over short window
                         + get_optimal_weights(h1_long, h5_long, np.array(basket.FundMax)))  # optimal weights over long window
        
        basket_weights[sub_id] = pd.DataFrame({'code': basket.FundCode, 'weight': weights})
        basket_hist[sub_id] = returns_1d[basket.FundCode].multiply(weights).sum(axis=1)

    return {'weights': basket_weights, 'hist': pd.DataFrame.from_dict(basket_hist)}

if False:
    baskets, subbaskets, returns_1d, returns_5d, dates = s0, f0, h0r1, h0r5, ['2018-01-03', '2018-04-03']  # assuming that Date is index in returns dataframes
def get_optimal_hist(baskets, subbaskets, returns_1d, returns_5d, dates):  # assuming that (rebal)dates exist in history time series
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
        h1_short = h1.loc[max(0, idx-WND_SHORT):idx, baskets.SubID]
        h5_short = h5.loc[max(0, idx-WND_SHORT):idx, baskets.SubID]
        h1_long = h1.loc[max(0, idx-WND_LONG):idx, baskets.SubID]
        h5_long = h5.loc[max(0, idx-WND_LONG):idx, baskets.SubID]
        weights = 0.5 * (get_optimal_weights(h1_short, h5_short, np.array(baskets.SubMax))  # optimal weights over short window
                         + get_optimal_weights(h1_long, h5_long, np.array(baskets.SubMax)))  # optimal weights over long window
        
        basket_weights[dt] = pd.concat(s['weights'].values()).groupby('code').agg({'weight': 'sum'}).reset_index()
        basket_hist[dt] = h1.multiply(weights).sum(axis=1)

    r = returns_1d.assign(idx=range(len(returns_1d)))
    res = pd.concat([basket_hist[dates[i]][r.loc[dates[i], 'idx']:(r.loc[dates[i+1], 'idx'] if i<len(dates)-1 else len(r))]
                     for i in range(len(dates))])
    return res

# Extract calendar from given returns
dates = pd.DataFrame({'date': returns_1d.reset_index()['Dates']})
dates = dates.assign(month=dates.date.dt.month, year=dates.date.dt.year).groupby(['year', 'month']).agg({'date': 'min'}).reset_index()['date']
dates = list(dates[1:(len(dates)-2)])

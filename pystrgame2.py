import random
from scipy.optimize import minimize
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import cvxopt as opt
from cvxopt import blas, solvers
WND = 5
MIN_COV, MAX_COV = 0.05, 0.075

f0 = pd.read_excel('/home/anton/git/ej/smidai_hist.xlsx', 'Sheet4')
s0 = pd.read_excel('/home/anton/git/ej/smidai_hist.xlsx', 'Sheet5')

h0 = pd.read_excel('/home/anton/git/ej/smidai_hist.xlsx', 'Sheet1')
h0['Dates'] = pd.to_datetime(h0['Dates'])
h0.columns = [x.replace(' Index', '').replace(' US Equity', '').replace(' INDEX', '') for x in h0.columns]
h0 = h0.set_index('Dates').sort_values('Dates')
cols = [x for x in h0.columns if x!='US0003M']
h0r1, h0r5 = h0, h0
h0r1 = h0[cols].pct_change(1).assign(US0003M=h0['US0003M']*0.01/360)
h0r5 = h0[cols].pct_change(WND).assign(US0003M=h0['US0003M']*0.07/360)

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


def negtotret(x):
    return -((h1.multiply(x).sum(axis=1)+1).prod()-1)

def constraint1(x):
    return h5.multiply(x).cov().sum().sum()*250/WND - MIN_COV

def constraint2(x):
    return MAX_COV - h5.multiply(x).cov().sum().sum()*250/WND

def constraint3(x):
    return np.sum(x) - 1

x0 = [random.random()*w[j] for j in range(len(w))]
x0 = x0/np.sum(x0)

bnds = [(0, x) for x in w]
con1 = {'type': 'ineq', 'fun': constraint1}
con2 = {'type': 'ineq', 'fun': constraint2}
con3 = {'type': 'eq', 'fun': constraint3}
cons = [con1, con2, con3]
sol = minimize(negtotret, x0, method='SLSQP', bounds=bnds, constraints=cons)

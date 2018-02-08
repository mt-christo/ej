import Tkinter
import matplotlib.pyplot as plt
import pytz
import numpy as np
import pandas as pd
import datetime
from collections import OrderedDict
from zipline.api import order, symbol, record, order_target, set_benchmark
from zipline.algorithm import TradingAlgorithm
#from zipline.utils.factory import load_bars_from_yahoo


nhist = 15
prices = [1,1,1,1,1,1,1,1,1,1,1,1,1,2,1]
data = pd.DataFrame({"date" : np.array([datetime.datetime.today().date() - datetime.timedelta(days=nhist-x) for x in range(nhist)]),
                     "open" : prices,
                     "high" : prices,
                     "low" : prices,
                     "close" : prices,
                     "volume" : 1}).set_index('date')
data.index = pd.to_datetime(data.index)
data.tz_localize('UTC', level=0)


data = pd.Panel(OrderedDict({'A': data}))
data.minor_xs = ['open','high','low','close','volume']

def initialize(context):
    context.security = symbol('A')
    context.prev_price = -1
    set_benchmark(symbol('A'))


def handle_data(context, data):

#    MA1 = 1 #data[context.security].mavg(10)
    
    pp = context.prev_price
    p = data[context.security].price
    positions = context.portfolio.positions[symbol('A')].amount
    cash = context.portfolio.cash
    value = context.portfolio.portfolio_value
    pnl = context.portfolio.pnl
    
    date = str(data[context.security].datetime)[:10]
    print(p)

    if (pp > 0) & (p > pp) & (positions == 0):        
        number_of_shares = int(cash/p)
        order_target(context.security, number_of_shares)
        record(date=date, Price=p, status="buy", shares=number_of_shares, PnL=pnl, cash=cash, value=value)
    elif (pp > 0) & (p < pp) & (positions != 0):
        order_target(context.security, 0)
        record(date=date, Price=p, status="sell", shares="--", PnL=pnl, cash=cash, value=value)
    else:
        record(date=date, Price=p, status="--", shares="--", PnL=pnl, cash=cash, value=value)

    context.prev_price = p


algo_obj = TradingAlgorithm(initialize=initialize, handle_data=handle_data)
p = algo_obj.run(data)

p[["Price","PnL",'shares']].plot()
plt.show()


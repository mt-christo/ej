import Tkinter
import matplotlib.pyplot as plt
import pytz
import numpy as np
import pandas as pd
import datetime
from collections import OrderedDict
from zipline.api import order, symbol, record, order_target, set_benchmark, set_commission, set_slippage
from zipline.algorithm import TradingAlgorithm
from zipline.finance import slippage, commission
#from zipline.utils.factory import load_bars_from_yahoo

def data_piece(x):
    x.index = pd.to_datetime(data.index)
    x.tz_localize('UTC', level=0)
    return(x)
    

nhist = 15
prices = [1,1,1,1,1,1,1,2,2,2,1,1,1,1,1]
prices2 = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
#data = 
#data.index = pd.to_datetime(data.index)
#data.tz_localize('UTC', level=0)
#data = pd.Panel(OrderedDict({'A': data_piece(data)}))
data = pd.Panel(OrderedDict({'A': pd.DataFrame({"date" : pd.to_datetime(np.array([datetime.datetime.today().date() - datetime.timedelta(days=nhist-x) for x in range(nhist)])).tz_localize('UTC'),
                                                "open" : prices,
                                                "high" : prices,
                                                "low" : prices,
                                                "close" : prices,
                                                "volume" : 10000000}).set_index('date'),
                             'B': pd.DataFrame({"date" : pd.to_datetime(np.array([datetime.datetime.today().date() - datetime.timedelta(days=nhist-x) for x in range(nhist)])).tz_localize('UTC'),
                                                "open" : prices2,
                                                "high" : prices2,
                                                "low" : prices2,
                                                "close" : prices2,
                                                "volume" : 10000000}).set_index('date')}))

data.minor_xs = ['open','high','low','close','volume']

def initialize(context):
    context.security = symbol('A')
    context.prev_price = -1
    set_benchmark(symbol('B'))    
    #    context.set_commission(commission.PerShare(cost=.0075, min_trade_cost=1.0))
    
    # Use the fixed slippage model, which will just add/subtract a specified spread  
    # spread/2 will be added on buys and subtracted on sells per share  
    # commission will be charged per share  
    ##context.slippage = FixedSlippage(spread=0.05, commission=0.01)  
    # allow algo to buy up to half a single bar, scaling price impact with 10%  
    ##context.slippage = VolumeShareSlippage(volume_limit=.5, price_impact=0.1)
    
#    set_commission(commission.PerShare(cost=0, min_trade_cost=0))
    set_commission(PerShare(cost=0, min_trade_cost=0))
    set_slippage(FixedSlippage(spread=0))
    


def handle_data(context, data):
#    MA1 = 1 #data[context.security].mavg(10)
    date = str(data[context.security].datetime)[:10]
    pp = context.prev_price
    p = data[context.security].price
    positions = context.portfolio.positions[symbol('A')].amount
    cash = context.portfolio.cash
    value = context.portfolio.portfolio_value
    pnl = context.portfolio.pnl

    print(date)
    print(pp)
    print(p)
    print(positions)
    if (pp > 0) & (p > pp) & (positions == 0):        
#        number_of_shares = int(cash/p)
        order_target(context.security, 1)
    elif (pp > 0) & (p < pp) & (positions != 0):
        order_target(context.security, 0)

    record(date=date, price=p)
    context.prev_price = p


def analyze(context=None, results=None):
    fig = plt.figure()
    ax1 = fig.add_subplot(211)
    results.portfolio_value.plot(ax=ax1)
    plt.show()

algo_obj = TradingAlgorithm(initialize=initialize, handle_data=handle_data)
p = algo_obj.run(data)
p.portfolio_value.plot(); plt.show()

p[["Price","PnL",'shares']].plot()
plt.show()


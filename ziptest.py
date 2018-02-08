import pytz
import numpy as np
import pandas as pd
import datetime
from collections import OrderedDict
from zipline.api import order, symbol, record, order_target, set_benchmark
from zipline.algorithm import TradingAlgorithm
#from zipline.utils.factory import load_bars_from_yahoo


nhist = 1000
data = pd.DataFrame({"date" : np.array([datetime.datetime.today().date() - datetime.timedelta(days=nhist-x) for x in range(nhist)]),
                     "open" : 1+np.array(range(nhist)),
                     "high" : 1+np.array(range(nhist)),
                     "low" : 1+np.array(range(nhist)),
                     "close" : 1+np.array(range(nhist)),
                     "volume" : 1}).set_index('date')
data.index = pd.to_datetime(data.index)
data.tz_localize('UTC', level=0)


data = pd.Panel(OrderedDict({'A': data}))
data.minor_xs = ['open','high','low','close','volume']

def initialize(context):
    context.security = symbol('A')
    set_benchmark(symbol('A'))


def handle_data(context, data):

    MA1 = 1 #data[context.security].mavg(10)
    MA2 = 2 #data[context.security].mavg(20)
    
    current_price = data[context.security].price
    current_positions = context.portfolio.positions[symbol('A')].amount
    cash = context.portfolio.cash
    value = context.portfolio.portfolio_value
    current_pnl = context.portfolio.pnl
    
    date = str(data[context.security].datetime)[:10]
    print(date)
    print(current_pnl)

    if (MA1 > MA2) and current_positions == 0:
        number_of_shares = int(cash/current_price)
        order(context.security, number_of_shares)
        record(date=date, MA1=MA1, MA2=MA2, Price=current_price, status="buy", shares=number_of_shares, PnL=current_pnl, cash=cash, value=value)
    elif (MA1 < MA2) and current_positions != 0:
        order_target(context.security, 0)
        record(date=date, MA1=MA1, MA2=MA2, Price=current_price, status="sell", shares="--", PnL=current_pnl, cash=cash, value=value)
    else:
        record(date=date, MA1=MA1, MA2=MA2, Price=current_price, status="--", shares="--", PnL=current_pnl, cash=cash, value=value)


algo_obj = TradingAlgorithm(initialize=initialize, handle_data=handle_data)
p = algo_obj.run(data)

p[["MA1","MA2","Price"]].plot()

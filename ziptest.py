from zipline.api import order, record, symbol

def initialize(contect):
    pass


def handle_data(context, data):
    order(symbol('AAPL'), 10)
    record(AAPL = data.currect(symbol('AAPL'), 'price'))

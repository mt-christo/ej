import sys, tidexapi
import pandas as pd
import datetime

with tidexapi.KeyHandler('tidi.txt') as handler:
    if not handler.keys:
        print("No keys in key file.")
    else:
        for key in handler.keys:
            print("Printing info for key %s" % key)

            with tidexapi.Connection() as connection:
                t = tidexapi.TradeAPI(key, handler, connection)

                try:
                    th = t.transHistory()
                    for h in th:
                        print("\t\t        id: %r" % h.transaction_id)
                        print("\t\t      type: %r" % h.type)
                        print("\t\t    amount: %r" % h.amount)
                        print("\t\t  currency: %r" % h.currency)
                        print("\t\t      desc: %s" % h.desc)
                        print("\t\t    status: %r" % h.status)
                        print("\t\t timestamp: %r" % h.timestamp)
                        print()
                except Exception as e:
                    print("  An error occurred: %s" % e)


def setHistoryParams(params, from_number, count_number, from_id, end_id,
                     order, since, end):
    if from_number is not None:
        params["from"] = "%d" % from_number
    if count_number is not None:
        params["count"] = "%d" % count_number
    if from_id is not None:
        params["from_id"] = "%d" % from_id
    if end_id is not None:
        params["end_id"] = "%d" % end_id
    if order is not None:
        if order not in ("ASC", "DESC"):
            raise InvalidSortOrderException("Unexpected order parameter: %r" % order)
        params["order"] = order
    if since is not None:
        params["since"] = "%d" % since
    if end is not None:
        params["end"] = "%d" % end

        
t = tidexapi.TradeAPI(key, handler, connection)
params = {"method": "TradeHistory"}
setHistoryParams(params, None, None, None, None, None, None, None)
orders = list(t._post(params).items())
o = pd.DataFrame([x[1] for x in orders])
o.timestamp = [datetime.datetime.fromtimestamp(int(x)).strftime('%Y-%m-%d %H:%M:%S') for x in o.timestamp]
o = o.sort_values('timestamp')
o.to_excel('ttrades.xlsx')

def match_tickers(tickers):
    return [U[U.ticker.str.startswith(x.upper())].ticker.iloc[0]
            for x in tickers]
    
    
def save_basket(text):
    baskets = get_all_baskets()
    baskets[bname] = {'name': text.split(' ')[0],
                      'desc': '',
                      'tickers': match_tickers(text.split(' ')[1:])}
    save_all_baskets(baskets)
    return 'Basket '+bname+' saved'


# basket = dict{ name, desc, tickers[] }
def get_all_baskets():
    b = pickle.load(open(BASKETS_PATH, 'rb'))
    return b


def save_all_baskets(baskets):
    with open(BASKETS_PATH, 'wb') as tmp:
        pickle.dump(baskets, tmp, protocol=pickle.HIGHEST_PROTOCOL)
    return 'Baskets saved'


def print_baskets(baskets):
    return '\n'.join([x + ':\n' +
                      (b[x]['desc'] + '\n' if b[x]['desc'] != '' else '') +
                      '* ' + '\n* '.join(b[x]['tickers']) + '\n'
                      for x in baskets])


def delete_basket(text):
    baskets = get_all_baskets()
    del baskets[text.split(' ')[0]]
    save_all_baskets(baskets)
    return 'Basket ' + bname + ' deleted'


def my_who(text):
    res = U[U.ticker.str.startswith(text.upper())]
    if len(res) > 0:
        return '\n'.join(res.apply(lambda x: str(x.CODE) + '\n(' +
                                   str(x.NAME) + ')\nmcap: ' +
                                   str(x.MCAP) + '\n', axis=1))
    else:
        return 'No matching securities'



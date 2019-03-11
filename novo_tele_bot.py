import novo_tele_state
from novo_tele_state import init_state, state_path, save_state, get_state
from novo_tele_state import update_current_state, save_state_csv, run_current_r

BASKETS_PATH = '/home/aslepnev/git/ej/strbaskets.pickle'
U_PATH = '/home/aslepnev/webhub/grish_iter0_adapted_u.csv'
H_PATH = '/home/aslepnev/webhub/grish_iter0_adapted_h.csv'


def webhub_filename(filename):
    return '/home/aslepnev/webhub/' + str(random.randint(10000,99999)) + '_' + filename


def start(bot, update):
    """Send a message when the command /start is issued."""
    update.message.reply_text('Hi!')

    
def help(bot, update):
    """Send a message when the command /help is issued."""
    update.message.reply_text('Help!')


def get_csv_for_reply(data, filename):
    filepath = webhub_filename(filename + '.csv')
    data.to_csv(filepath, index=False)
    res = open(filepath, 'rb')
    return res

    
def smart_result(x):
    ustate = get_state()
    if ustate['type'] == 'index':
        if x == 'perf':
            res = run_current_r()
            return {'type': 'text',
                    'data': 'Performance since ' + res['index_start'] + ': **' + str(round(res['endPerf'].iloc[0, 0], 1)) + '%**'}
        elif x == 'backtest':
            res = run_current_r()
            return {'type': 'csv',
                    'name': 'backtest',
                    'text': 'Backtest since ' + res['index_start'],
                    'data': res['perf']}


def typewise_reply(data, update):
    if data['type'] == 'text':
        update.message.reply_text(data['data'])
    elif data['type'] == 'csv':
        update.message.reply_document(text=data['text'],
                                      document=get_csv_for_reply(data['data'], data['name']))


def my_response(bot, update):
    """Echo the user message."""
    text = update.message.text.lower()
    print(text)
    res = ''
    if text == 'help':
        res = 'Commands:'
        res = res + '\n: list our product types'
        res = res + '\n*who <text>: list securities starting with <text>'
        res = res + '\n*baskets: list your baskets'
        res = res + '\n*basket <name sec1 sec2 ..>: save basket with members'
        res = res + '\n*delete <name>: delete basket <name>'
        res = res + '\n*plot <name>: price chart for equal-weighted basket <name>'
        res = res + '\n\nIndicative pricing:'
        res = res + '\n*worstof <name> <n1-n2-...>: worst-of product, basket <name> with strikes n[i]'
        res = res + '\n*basketcall <name> <n1-n2-...>: ATM Call option, basket <name>'
        update.message.reply_text(res)
    else:
        typewise_reply(smart_result(text), update)
#        res = smart_result(text)
#        update.message.reply_text(str(res))
#    elif text.startswith('plot '):
#        print(text)
#        res = plot_basket(text[5:1000])
#        update.message.reply_text(text[5:1000] + ' price chart:')
#        update.message.reply_photo(open(res, 'rb'))
#    elif text.startswith('worstof '):
#        update.message.reply_text('Calculating..')
#        res = reply_wo(text[8:1000])
#        for x in res:
#            update.message.reply_text(x)

def error(bot, update, error):
    """Log Errors caused by Updates."""
    logger.warning('Update "%s" caused error "%s"', update, error)



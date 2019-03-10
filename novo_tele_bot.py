def start(bot, update):
    """Send a message when the command /start is issued."""
    update.message.reply_text('Hi!')

    
def help(bot, update):
    """Send a message when the command /help is issued."""
    update.message.reply_text('Help!')


def smart_recognize(x):
    ustate = get_state()
    if ustate['type'] == 'index':
        if x == 'perf':


def my_response(bot, update):
    """Echo the user message."""
    text = update.message.text.lower()
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
    elif text == 'products':
        res = "worstof:  pays off coupon if all stocks are above that year's strike\n\nbasketcall:  pays Call option payoff on equal-weighted basket"
        update.message.reply_text(res)
    elif text == 'baskets':
        res = get_baskets()
        update.message.reply_text(res)
    elif text.startswith('who '):
        res = my_who(text[4:1000])
        update.message.reply_text(res)
    elif text.startswith('basket '):
        res = save_basket(text[7:1000])
        update.message.reply_text(res)
        update.message.reply_text(get_baskets())
    elif text.startswith('delete '):
        res = delete_basket(text[7:1000])
        update.message.reply_text(res)
        update.message.reply_text(get_baskets())
    elif text.startswith('plot '):
        print(text)
        res = plot_basket(text[5:1000])
        update.message.reply_text(text[5:1000] + ' price chart:')
        update.message.reply_photo(open(res, 'rb'))
    elif text.startswith('worstof '):
        update.message.reply_text('Calculating..')
        res = reply_wo(text[8:1000])
        for x in res:
            update.message.reply_text(x)

def error(bot, update, error):
    """Log Errors caused by Updates."""
    logger.warning('Update "%s" caused error "%s"', update, error)



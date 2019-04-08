def toGridRange(sheet, cellsRange):
    sheetId = sheet['sheets'][0]['properties']['sheetId']
#    if sheetId is None:
#        raise SheetNotSetError()
    if isinstance(cellsRange, str):
        startCell, endCell = cellsRange.split(":")[0:2]
        cellsRange = {}
        rangeAZ = range(ord('A'), ord('Z') + 1)
    if ord(startCell[0]) in rangeAZ:
        cellsRange["startColumnIndex"] = ord(startCell[0]) - ord('A')
        startCell = startCell[1:]
    if ord(endCell[0]) in rangeAZ:
        cellsRange["endColumnIndex"] = ord(endCell[0]) - ord('A') + 1
        endCell = endCell[1:]
    if len(startCell) > 0:
        cellsRange["startRowIndex"] = int(startCell) - 1
    if len(endCell) > 0:
        cellsRange["endRowIndex"] = int(endCell)
        cellsRange["sheetId"] = sheetId
    return cellsRange


# bname, tail_days = 'fin1', 250
def plot_basket_bunch(bname, tail_days):
    b = get_all_baskets()[bname]
    h = H.loc[:, b['tickers']]
    h1 = 100*(np.exp(h.tail(tail_days).cumsum()) - 1).rolling(3).mean()
    
    matplotlib.style.use('ggplot')
    
    p = h.plot(kind='line', x='date', y='val', linewidth=4, title=bname+' basket performance, %', legend=False, color='green', antialiased=True)
    f = p.get_figure()
    filename = 'plot.png'
    f.savefig(filename)
    plt.close()   
#    client.send_file('coinsight_bot','plot.png')
    return filename
    
    
def plot_basket_single(bname):
    t = pickle.load(open('/home/aslepnev/git/ej/strbaskets.pickle', 'rb'))[bname]
    h = hist[hist.ticker.isin(t['tickers'])].groupby('dt').agg({'val': 'mean'}).reset_index()
#    h1 = (np.exp(h) - 1).mean(axis=1)
    
    h['date'] = pd.to_datetime(h.dt)
    h.val = (h.val.cumsum().rolling(20).mean()+1)*100
    
    matplotlib.style.use('ggplot')
    
    p = h.plot(kind='line', x='date', y='val', linewidth=4, title=bname+' basket performance, %', legend=False, color='green', antialiased=True)
    f = p.get_figure()
    filename = 'plot.png'
    f.savefig(filename)
    plt.close()   
#    client.send_file('coinsight_bot','plot.png')
    return filename

def calc_wo(basket,params):
    params_fn = '~/git/ej/wo_params.csv'
    quotes_fn = '~/git/ej/wo_quotes.csv'
    pd.DataFrame({'param':['coupon','strikes'], 'value':params}).to_csv(params_fn)
    hist[hist.ticker.isin(basket['tickers'])].to_csv(quotes_fn)
    return np.asarray(ro.r('wo_calculator_web("'+params_fn+'","'+quotes_fn+'")'))[0]


def report_wo(val, bname, basket, params):
    t = ses[ses.CODE.isin(basket['tickers'])]

#    credentials = ServiceAccountCredentials.from_json_keyfile_name('/home/aslepnev/a/gigi.json', ['https://spreadsheets.google.com/feeds', 'https://www.googleapis.com/auth/drive'])

    credentials = ServiceAccountCredentials.from_json_keyfile_name('/home/aslepnev/a/gigi.json', ['https://www.googleapis.com/auth/spreadsheets', 'https://www.googleapis.com/auth/drive'])
    httpAuth = credentials.authorize(httplib2.Http())
    service = apiclient.discovery.build('sheets', 'v4', http = httpAuth)

    spreadsheet = service.spreadsheets().create(body = {
        'properties': {'title': "product_" + str(random.randint(1,999999999999)), 'locale': 'ru_RU'},
        'sheets': [{'properties': {'sheetType': 'GRID',
                                   'sheetId': 0,
                                   'title': 'Product',
                                   'gridProperties': {'rowCount': 30, 'columnCount': 25}}}]}).execute()
    
    driveService = apiclient.discovery.build('drive', 'v3', http = httpAuth)
    shareRes = driveService.permissions().create(
        fileId = spreadsheet['spreadsheetId'],
        sendNotificationEmail = False, 
        body = {'type': 'user', 'role': 'writer', 'emailAddress': 'antonslepnev@gmail.com'},
        fields = 'id'
    ).execute()
    

    results = service.spreadsheets().batchUpdate(spreadsheetId = spreadsheet['spreadsheetId'], body = {
        'requests':[
            {'repeatCell': {
                'range': toGridRange(spreadsheet, "A"+str(6+len(t) + 3)+":A"+str(6+len(t) + 3)),
                'cell': {
                    'userEnteredFormat': {
                        'textFormat': {
                            'red': 1.0,
                            'green': 1.0,
                            'blue': 0.5,
                        },
                    },
                },
                'fields': 'userEnteredFormat(backgroundColor)'}},
            ]
    }).execute()

    results = service.spreadsheets().values().batchUpdate(spreadsheetId = spreadsheet['spreadsheetId'], body = {
        "valueInputOption": "USER_ENTERED",
        "data": [
            {"range": "Product!A1:A5",
             "majorDimension": "COLUMNS",
             "values": [['Worst-Of product indicative pricing report', '', 'Basket:', bname, basket['desc']]]},            
            {"range": "Product!A6:C" + str(6+len(t) + 1),
             "majorDimension": "COLUMNS",
             "values": [['CODE']+t.CODE.replace(np.nan,'').tolist(), ['NAME']+t.NAME.replace(np.nan,'No Data').tolist(), ['MARKET CAP']+t.MCAP.replace(np.nan,'').tolist()]},
            {"range": "Product!A"+str(6+len(t) + 2)+":A"+str(6+len(t) + 3),
             "majorDimension": "COLUMNS",
             "values": [['Product price:', str(val).replace('.',',')]]}, 
        ]
    }).execute()


    results = service.spreadsheets().batchUpdate(spreadsheetId = spreadsheet['spreadsheetId'], body = {
        'requests':[
            {'repeatCell': {
                'range': toGridRange(spreadsheet, "A"+str(6+len(t) + 3)+":A"+str(6+len(t) + 3)),
                'cell': {
                    'userEnteredFormat': {
                        'numberFormat': {
                            'type': 'NUMBER',
                            'pattern': '##.#%',
                        },
                    },
                },
                'fields': 'userEnteredFormat(numberFormat)'}},
            ]
    }).execute()

    return 'https://docs.google.com/spreadsheets/d/' + spreadsheet['spreadsheetId']


def reply_wo(text):
    items = text.split(' ')
    bname = items[0]
    params = items[1:3]
    basket = pickle.load(open('/home/aslepnev/git/ej/strbaskets.pickle', 'rb'))[bname] 
    val = calc_wo(basket,params)
    res = [str(val)+' %']
    if len(items) > 3:
        res = res + ['Please see product card on Google Drive: ' + report_wo(val, bname, basket, params)]
    return res



import pickle
import pandas as pd


def init_state(state_type):
    res = {'type': state_type}
    if state_type == 'index':
        res['rebal_type'] = 'fixed'
    elif state_type == 'wo':
        res['basket'] = ['MSFT', 'AAPL', 'CSCO']
        
    save_state(res)
    
    return res


def global_state_path():
    return '/home/aslepnev/webhub/strtelestate_current.pickle'


def global_r_state_path():
    return '/home/aslepnev/webhub/strtelestate_current.csv'


def global_r_state_data_mask():
    return '/home/aslepnev/webhub/strtelestate_current_name.csv'


def state_path(state_data, state_name=''):
    res = global_state_path()
    if state_name != '':
        res = res.replace('state_current', 'state_' + state_name)
        
    return res


def save_state(state_data, state_name=''):
    with open(state_path(state_data, state_name), 'wb') as tmp:
        pickle.dump(state_data, tmp, protocol=pickle.HIGHEST_PROTOCOL)


def get_state(state_name=''):
    return pickle.load(open(state_path(state_name) if state_name != ''
                            else global_state_path(), 'rb'))


def update_current_state(field_name, field_value):
    state_data = get_state()
    state_data[field_name] = field_value
    save_state(state_data)
    return state_data


def save_state_csv(state_data, filename):
    pd.DataFrame(state_data.items()).to_csv(filename)
    

def get_rdata_from_mask(item):
    return pd.read_csv(global_r_state_data_mask().replace('current_name', 'current_' + item))


def index_param_dict():
    res = [['uni_name', 'universe', 'Universe name', ''],
           ['screen_func','screen','Screen method', ''],
           ['screen_price_window', 'screen frame', 'Screen Frame', ''],
           ['index_start', 'start', 'Start date', ''],
           ['vc_window', 'vc window', 'VC window', ''],
           ['vc_level', 'vt', 'VC target', '100'],
           ['vc_max_weight', 'max exp', 'VC max exposure', '100'],
           ['vc_type', 'vc type', 'VC type', ''],
           ['vc_rfr', 'rfr', 'Cash rate', '100'],
           ['index_excess', 'excess', 'Excess Rate', '100']]
    res = pd.DataFrame(res)
    res.columns = ['field', 'code', 'title', 'mult']
    res = res.set_index('code')
    return res


# field_code, field_value ='vt', 16
def update_current_state_by_code(field_code, field_value):
    param_dict = index_param_dict()
    update_value = (float(field_value)/100.0) if param_dict.loc[field_code, 'mult'] == '100' else field_value
    return update_current_state(param_dict.loc[field_code, 'field'], update_value)


# value_phrase ='vt 17'
def update_current_state_with_phrase(value_phrase):
    param_dict = index_param_dict()
    res = {}
    for code in param_dict.index:
        if value_phrase.startswith(code + ' '):
            res = update_current_state_by_code(code, value_phrase.replace(code, '').strip())


def is_state_update_phrase(value_phrase):
    param_dict = index_param_dict()
    for code in param_dict.index:
        if value_phrase.startswith(code + ' '):
            return True
    return False
            
            
def index_param_display():
    s = get_state()
    res = 'Current inputs:'
    if s['type'] == 'index':
        param_dict = index_param_dict().reset_index().set_index('field')
        res = '<pre>\n</pre>'.join(['(' + param_dict.loc[x, 'code'] + ') <b>' + param_dict.loc[x, 'title'] +
                                    '</b>:  ' + (str(s[x]) if param_dict.loc[x, 'mult'] == ''
                                                 else str(round(float(s[x])*float(param_dict.loc[x, 'mult']), 2))) +
                                    ('%' if param_dict.loc[x, 'mult'] == '100' else '')
                                    for x in s if x in param_dict.index])
    return res


if False:
    state_data = {'type': 'index',
                  'uni_type': 'straight',
                  'uni_name': 'it10',                  
                  'screen_func': 'screen_mixed_top',
                  'screen_price_window': 40,
                  'index_start': '2012-12-31',
                  'vc_window': 20,
                  'vc_level': 0.14,
                  'vc_max_weight': 1.5,
                  'vc_type': 'max 10',
                  'vc_rfr': 0.02,
                  'index_excess': 0.035}
    state_data = {'type': 'index',
                  'uni_type': 'stocks and etfs',
                  'uni_name': 'asia',
                  'uni_etf_count': 15,
                  'uni_stock_count': 30,
                  'n_stocks': 3,
                  'n_etfs': 2,
                  'vt': 0.3,
                  'min_weight': 0.15,
                  'max_weight': 0.6}
    save_state(state_data)
    state_file = global_r_state_path()
    save_state_csv(state_data, state_file)
    
def run_current_r():
    state_data = get_state()
    state_file = global_r_state_path() + str(random.randint(10000,99999))
    save_state_csv(state_data, state_file)
    items = []
    res = {}
    if state_data['type'] == 'index' and state_data['uni_type'] == 'straight':
        ro.r('index_report_to_python("' + state_file + '")')
        items = ['perf', 'endPerf', 'volatility', 'baskets']
        res['index_start'] = state_data['index_start']
    elif state_data['uni_type'] == 'stocks and etfs':
        ro.r('index_report_to_python("' + state_file + '")')
        items = ['perf', 'endPerf', 'vol120', 'vol250', 'basket']

    for item in items:
        res[item] = get_rdata_from_mask(item)

    return res

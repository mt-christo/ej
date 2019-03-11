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
    res = [['uni_name', 'universe', 'Universe name'],
           ['screen_func','screen','Screening method'],
           ['screen_price_window', 'price window', 'Screening Frame'],
           ['index_start', 'start', 'Index start date'],
           ['vc_window', 'vc window', 'Volcontrol window'],
           ['vc_level', 'vt', 'Volatility target'],
           ['vc_max_weight', 'max exp', 'Voltarget max exposure'],
           ['vc_type', 'vc type', 'max 10'],
           ['vc_rfr', 'rfr', 'Cash rate'],
           ['index_excess', 'excess', 'Excess Rate']]
    res = pd.DataFrame(res)
    res.columns = ['field', 'code', 'title']
    res = res.set_index('code')
    return res


def index_param_display():
    s = get_state()
    res = 'Current inputs:'
    if s['type'] == 'index':
        param_dict = index_param_dict().set_index('field')
        res = '\n'.join([param_dict.loc[x, 'title'] + ': ' + str(s[x]) for x in s if x in param_dict.index])


if False:
    state_data = {'type': 'index',
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
def run_current_r():
    state_data = get_state()
    state_file = global_r_state_path() + str(random.randint(10000,99999))
    save_state_csv(state_data, state_file)
    items = []
    res = {}
    if state_data['type'] == 'index':
        ro.r('index_report_to_python("' + state_file + '")')
        items = ['perf', 'endPerf', 'volatility', 'baskets']
        res['index_start'] = state_data['index_start']

    for item in items:
        res[item] = get_rdata_from_mask(item)

    return res

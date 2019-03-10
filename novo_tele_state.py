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


def state_path(state_data, state_name):
    return STATE_PATH.replace('state_current', 'state_' + state_name)


def save_state(state_data, state_name):
    with open(state_path(state_data, state_name), 'wb') as tmp:
        pickle.dump(state_data, tmp, protocol=pickle.HIGHEST_PROTOCOL)


def get_state(state_name=''):
    return pickle.load(open(state_path(state_name) if state_name != ''
                            else STATE_PATH), 'rb')


def update_current_state(field_name, field_value):
    state_data = get_state()
    state_data[field_name] = field_value
    save_state(state_data)
    return state_data


def save_state_csv(state_data, filename):
    pd.DataFrame(state_data.items()).to_csv(filename)
    

def get_rdata_from_mask(item):
    return pd.read_csv(R_STATE_DATA_MASK.replace('current_name', 'current_' + item))
    

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
    state_file = R_STATE_PATH + str(random.randint(10000,99999))
    save_state_csv(state_data, state_file)
    items = []
    if state_data['type'] == 'index':
        ro.r('index_report_to_python("' + state_file + '")')
        items = ['perf', 'endPerf', 'volatility', 'baskets']

    res = {}
    for item in items:
        res[item] = get_rdata_from_mask(item)

    return res

import requests, json


addrs = pd.read_csv('/home/aslepnev/ca/tieholders')
URL = 'http://api.etherscan.io/api?module=account&action=txlist&address=%ADDRESS%&startblock=0&endblock=99999999&sort=asc&apikey=RPH54GB9BGS1G67EQFT39PFR34VEYGEY7N'

URL1 = 'https://api.etherscan.io/api?module=logs&action=getLogs&fromBlock=0&toBlock=latest&address=0x999967e2ec8a74b7c8e9db19e039d920b31d39d0&topic0=0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef'
r1 = json.loads(requests.get(URL1).text)['result']

dfs = pd.DataFrame()
addr = '0x2540bd52f7d342380158ce722e9fd79570283e01'

for addr in addrs.address[1]:
    r = pd.DataFrame(json.loads(requests.get(URL.replace('%ADDRESS%',addr)).text)['result'])
    r['address'] = addr
    dfs = pd.concat([dfs, r]) 

r = json.loads(requests.get(URL).text)['result']
    
URL1 = 'https://api.etherscan.io/api?module=transaction&action=getstatus&txhash=0x50ea0d0786e651a1fc3a208f1faf15767230174db27c424512ce3f792e7b6883&apikey=RPH54GB9BGS1G67EQFT39PFR34VEYGEY7N'
r1 = json.loads(requests.get(URL1).text)['result']

URL1 = 'https://api.etherscan.io/api?module=contract&action=getabi&address=0x999967e2ec8a74b7c8e9db19e039d920b31d39d0&apikey=RPH54GB9BGS1G67EQFT39PFR34VEYGEY7N'
r1 = json.loads(requests.get(URL1).text)['result']

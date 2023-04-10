#%%
from requests import get
from matplotlib import pyplot as plt
import json
from datetime import datetime

apy_key = 'F1YZPF3PNBR9CV5ZKRKUFG4MAVPHIYW4VQ'
address = '0xde0b295669a9fd93d5f28d9ec85e40f4cb697bae'
base_url = 'https://api.etherscan.io/api'
ether_value = 10**18


def make_api_url(module,action,address,**kwargs):
    url = base_url + f"?module={module}&action={action}&address={address}&apikey={apy_key}"

    for key, value in kwargs.items():
        url += f"&{key}={value}"
    
    return url

def get_account_balance(address):
    get_balance_url = make_api_url("account","balance",address, tag="latest")
    response = get(get_balance_url)
    data = response.json()

    value = int(data["result"])/ether_value
    return value
#%%
def get_transacctions(address):
    get_transacctions_url = make_api_url("account","txlist", address,startblock=0,endblock=99999999,page=1,offset=1000,sort="asc")
    response = get(get_transacctions_url)
    data = response.json()["result"]
    
    for tx in data:
        to = tx["to"]
        from_addr = tx["from"]
        value = int(tx["value"]) / ether_value
        gas = int(tx["gasUsed"]) * int(tx["gasPrice"]) / ether_value
        time = datetime.fromtimestamp(int(tx["timeStamp"]))
        print("--------------------------------")
        print("To",to)
        print("From",from_addr)
        print("Value",value)
        print("Gas Used",gas)
        print("Time",time)

#address = '0xde0b295669a9fd93d5f28d9ec85e40f4cb697bae'
address = '0x73bceb1cd57c711feac4224d062b0f6ff338501e'

get_transacctions(address)



# %%

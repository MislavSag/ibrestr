import requests
import json

# Define the URL of the endpoint
url = 'http://localhost:8000/liquidate'

# Define the data to be sent in the request body
# Replace these with actual values for testing
data = {
  "accountId": "your_account_id",
  "symbol": "your_symbol",
  "sectype": "your_sectype",
  "host": "your_host",     # Optional if default is set
  "port": 5000,            # Optional if default is set
  "strategy_name": None,   # Optional
  "email_config": {}       # Optional
}

# Make the POST request
response = requests.post(url, json=data)

# Check the response
print(response.text)

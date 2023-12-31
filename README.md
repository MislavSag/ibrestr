# IBClientPortal

## Overview
IBClientPortal is an R package that provides a comprehensive R6 class-based interface to the Interactive Brokers Client Portal API. It enables R users to interact with their Interactive Brokers account, facilitating tasks such as trading, monitoring, managing portfolios, and retrieving market data.

## Installation
You can install the development version from GitHub with:

```R
# install.packages("devtools")
devtools::install_github("your-github-username/IBClientPortal")
```

## Usage

Load the package and create an instance of the IB class:

```R
library(IBClientPortal)
ib <- IB$new(host = "localhost", port = 5000)
```

## Fetch Market Data
```R
market_data <- ib$get_market_data_snapshot(conids = "conid", fields = "fields")
```

## Manage Orders
```R
order_response <- ib$place_order(accountID = "your_account_id", orders = your_order_list)

```

## Get Portfolio Summary
```R
portfolio_summary <- ib$get_portfolio_summary(accountId = "your_account_id")

```

## Features

- Trade execution and management.
- Real-time and historical market data retrieval.
- Portfolio management functionalities.
- Email notifications for trade activities.
- Secure API connection handling.

## Contributing

Contributions are what make the open-source community such an amazing place to learn, inspire, and create. Any contributions you make are greatly appreciated.

## Fork the Project
Create your Feature Branch (git checkout -b feature/AmazingFeature)
Commit your Changes (git commit -m 'Add some AmazingFeature')
Push to the Branch (git push origin feature/AmazingFeature)
Open a Pull Request
License
Distributed under the [Your License]. See LICENSE for more information.

## Acknowledgements
Interactive Brokers


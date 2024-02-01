############SELL ALL FOR LEAST VOLATILE !!!! #################

library(ibrestr)
library(data.table)
library(lgr)
library(lgrExtra)
library(RPostgres)

library(httr)
library(checkmate)
library(jsonlite)
library(mailR)



# LOGGER ------------------------------------------------------------------
# create tables in database
table_name <- "minmax_logs"
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "defaultdb",
  host = "db-postgresql-fra1-54406-do-user-13776848-0.c.db.ondigitalocean.com",
  port = 25060L,
  user = "doadmin",
  password = "AVNS_7h0PktF6BbOHWOUK45K"
)

if (!(dbExistsTable(con, table_name))) {
  # create new table
  df = data.frame(
    level = integer(0),
    timestamp = lubridate::POSIXct(0),
    logger = character(0),
    caller = character(0),
    msg = character(0)
  )
  dbWriteTable(
    con,
    table_name,
    value = df,
    overwrite = TRUE,
    append = FALSE,
    row.names = FALSE
  )
}
dbDisconnect(con)

# start logger
lg = get_logger("db_logger_6")
lg$add_appender(name = "db",
                lgrExtra::AppenderDbi$new(
                  conn = dbConnect(
                    RPostgres::Postgres(),
                    dbname = "defaultdb",
                    host = "db-postgresql-fra1-54406-do-user-13776848-0.c.db.ondigitalocean.com",
                    port = 25060L,
                    user = "doadmin",
                    password = "AVNS_7h0PktF6BbOHWOUK45K"
                  ),
                  table = table_name
                ))


# IB ----------------------------------------------------------------------
# init IB
ib = IB$new(
  host = "cgspaperpra.eastus.azurecontainer.io",
  port = 5000,
  strategy_name = "Least Volatile Local",
  account_id = "DU6474915",
  email_config = list(
    email_from = "mislav.sagovac@contentio.biz",
    email_to = "mislav.sagovac@contentio.biz",
    smtp_host = "mail.contentio.biz",
    smtp_port = 587,
    smtp_user = "mislav.sagovac@contentio.biz",
    smtp_password = "s8^t5?}r-x&Q"
  ),
  logger = NULL
)
# ib = IB$new(
#   host = "cgspaperexuber.eastus.azurecontainer.io",
#   port = 5000,
#   strategy_name = "Exuber Inverse Live New API Test",
#   account_id = "DU8203010",
#   email_config = list(
#     email_from = "mislav.sagovac@contentio.biz",
#     email_to = "mislav.sagovac@contentio.biz",
#     smtp_host = "mail.contentio.biz",
#     smtp_port = 587,
#     smtp_user = "mislav.sagovac@contentio.biz",
#     smtp_password = "s8^t5?}r-x&Q"
#   ),
#   logger = NULL
# )
# ib = IB$new(
#   host = "cgslivepra.eastus.azurecontainer.io",
#   port = 5000,
#   strategy_name = "PRA CGS LIVE",
#   account_id = "U11177920",
#   email_config = list(
#     email_from = "mislav.sagovac@contentio.biz",
#     email_to = "mislav.sagovac@contentio.biz",
#     smtp_host = "mail.contentio.biz",
#     smtp_port = 587,
#     smtp_user = "mislav.sagovac@contentio.biz",
#     smtp_password = "s8^t5?}r-x&Q"
#   ),
#   logger = NULL
# )
# ib = IB$new(
#   host = "cgslive.eastus.azurecontainer.io",
#   port = 5000,
#   strategy_name = "MinMax CGS Live",
#   account_id = "U10539191",
#   email_config = list(
#     email_from = "mislav.sagovac@contentio.biz",
#     email_to = "mislav.sagovac@contentio.biz",
#     smtp_host = "mail.contentio.biz",
#     smtp_port = 587,
#     smtp_user = "mislav.sagovac@contentio.biz",
#     smtp_password = "s8^t5?}r-x&Q"
#   ),
#   logger = NULL
# )

ib$get()
ib$post()
# self = ib$clone()
# self$get_conid_by_symbol("AAPL", "CFD)

# data snapshot
snap = ib$get_market_data_snapshot("756733", fields = "31,84,86")
lubridate::with_tz(as.POSIXct(snap[[1]]$`_updated` / 1000), tzone = "America/New_York")
snap[[1]]$`86`
snap[[1]]$`84`

# list orders
ib$get_portfolio_accounts()
live_orders = ib$get_live_orders()
rbindlist(live_orders$orders, fill = TRUE)

# positions
positions = ib$get_account_positions(NULL, 0)
positions = lapply(positions, as.data.table)
positions = rbindlist(positions, fill = TRUE)[order(contractDesc )]
positions = positions[, .(acctId, conid, contractDesc, position, mktPrice,
                          mktValue, avgCost, unrealizedPnl, realizedPnl,
                          assetClass, ticker)]
positions = unique(positions)
positions
# LCID, ET, M, KMI, LBPH, LYFT, RF, TOST, VTRS
# ET, VTRS

# position by ID
ib$get_conid_by_symbol("SPY", "CFD", "AMEX")
position_id = ib$get_position_by_conid(NULL, 146224224)
position_id[[1]]$position

# trades
trades = ib$get_trades(days = 5)
trades = rbindlist(trades, fill = TRUE)

# cancel order
# ib$cancel_order("DU8203010", "-1")

# place order
# toJSON(list(orders = list(body)), auto_unbox = TRUE)
order = ib$place_and_confirm_order(
  "DU8203010",
  orders = list(
    acctId = "DU8203010",
    conid = as.integer("134770228"),
    sectype = paste0("134770228", ":CFD"),
    orderType = "MKT",
    outsideRTH = FALSE,
    side = "SELL",
    quantity = as.integer("1"),
    tif = "GTC",
    cOID = ids::random_id()
  )
)

# check order status and wait for 5 minute if it will be executed
ib$get_order_status_repeated(order$order[[1]]$order_id, 10)

# set holdings
order = ib$set_holdings(accountId = NULL,
                        symbol = "VTRS",
                        sectype = "CFD",
                        side = "BUY",
                        tif = "GTC",
                        weight = 0.034)

# liquidate
order = ib$liquidate(symbol = "SPY", sectype = "CFD")

# liquidate all
order = ib$liquidate_portfolio()

# coninds
conids = ib$get_conids_by_exchange("AMEX")
conids[ticker == "AAPL"]
contract = ib$search_contract_by_symbol("SPY")
con_stk = contract[[1]]$conid
contract = ib$get_sec_definfo(con_stk, "CFD")
contract[[1]]$conid


# market data
md = ib$get_historical_data(conid = "319856165", exchange = "SMART", period = "5d", bar = "1h", outsideRth = TRUE)
md = ib$get_historical_data(conid = "265598", exchange = "SMART", period = "5d", bar = "1h", outsideRth = TRUE, clean = TRUE)

dt_ = as.data.table(cbind.data.frame(symbol = md$symbol, rbindlist(md$data)))
dt_[, datetime := as.POSIXct(as.numeric(t) / 1000,
                             origin = "1970-01-01",
                             tz = "America/New_York")]

# portfolio performance
x = ib$get_account_performance("DU8203010", "Q")
x$nav$data

# portfolio transactions
x = ib$get_transaction_history("DU8203010", 134770228, "USD")
x$transactions


# TEST API ----------------------------------------------------------------
# globals
url = "http://127.0.0.1:8080/" # "cgspaperpra.eastus.azurecontainer.io", "cgslivepra.eastus.azurecontainer.io"
host = "cgspaperpra.eastus.azurecontainer.io"
strategy_name = "test"
account_id = "DU6474915"
email_config_ =list(
  email_from = "mislav.sagovac@contentio.biz",
  email_to = c("mislav.sagovac@contentio.biz"),
  smtp_host = "mail.contentio.biz",
  smtp_port = 587,
  smtp_user = "mislav.sagovac@contentio.biz",
  smtp_password = "s8^t5?}r-x&Q"
)

# send message
res = GET(paste0(url, "echo"), query = list(msg="I am trader"))
content(res)

# post sum
res = POST(paste0(url, "sum"), body = list(a = 1, b = 3), encode = "json")
content(res)

# ping
data <- list(
  host = host,
  port = 5000,
  strategy_name = strategy_name,
  account_id = account_id,
  email_config = email_config_,
  logger = NULL
)
response <- POST(paste0(HOST, "ping"), body = data, encode = "json")
p = content(response)
p

# post liquidate
data <- list(
  account_id = account_id,
  symbol = "AGNC",
  sectype = "CFD",
  host = host,
  port = 5000,
  strategy_name = strategy_name,
  email_config = email_config_,
  table_name = "least_volatile"
)
response <- POST(paste0(url, "liquidate"), body = data, encode = "json")
p = content(response)
p

# set holdings
data <- list(
  account_id = account_id,
  symbol = "SPY",
  sectype = "CFD",
  side = "BUY",
  tif = "GTC",
  weight = 0.02,
  host = host,
  port = 5000,
  strategy_name = "Least Vol",
  email_config = list(
    email_from = "mislav.sagovac@contentio.biz",
    email_to = c("mislav.sagovac@contentio.biz", "kontakt@contentio.biz"),
    smtp_host = "mail.contentio.biz",
    smtp_port = 587,
    smtp_user = "mislav.sagovac@contentio.biz",
    smtp_password = "s8^t5?}r-x&Q"
  ),
  table_name = "least_volatile"
)
# response <- POST("http://localhost:8080/set_holdings", body = data, encode = "json")
response <- POST(paste0(url, "set_holdings"), body = data, encode = "json")
p = content(response)
p



# TEST LOGER --------------------------------------------------------------
# lg$info("TEST dsgfd")
connec = dbConnect(
  RPostgres::Postgres(),
  dbname = "defaultdb",
  host = "db-postgresql-fra1-54406-do-user-13776848-0.c.db.ondigitalocean.com",
  port = 25060L,
  user = "doadmin",
  password = "AVNS_7h0PktF6BbOHWOUK45K"
)
DBI::dbListTables(connec)
table_ = dbReadTable(connec, "least_volatile")
dbDisconnect(connec)
table_ = as.data.table(table_)
table_[(nrow(table_)-99):nrow(table_), .(level, timestamp, caller, msg)]
table_[(nrow(table_)-198):(nrow(table_)-99), .(level, timestamp, caller, msg)]

# acctId     conid contractDesc position mktPrice  mktValue   avgCost unrealizedPnl realizedPnl assetClass ticker
# 1: DU6474915 147632654  AAL     CFD    -1614  14.0600 -22692.84 14.005500        -87.96           0        CFD    AAL
# 2: DU6474915 146224224  AGNC    CFD    -2096   9.7400 -20415.04  9.555500       -386.71           0        CFD   AGNC
# 3: DU6474915 481700548  CLSK    CFD    -2962   7.3100 -21652.22  6.808039      -1486.81           0        CFD   CLSK
# 4: DU6474915 120551117  F       CFD     3020  11.4100  34458.20 11.314781        287.56           0        CFD      F
# 5: DU6474915 120550981  GPS     CFD    -1445  18.9800 -27426.10 18.935465        -64.35           0        CFD    GPS
# 6: DU6474915 120550631  HBAN    CFD     2673  12.7900  34187.67 12.784500         14.70           0        CFD   HBAN
# 7: DU6474915 519075894  HOOD    CFD      -39  11.0000   -429.00 11.045128          1.76           0        CFD   HOOD
# 8: DU6474915 211076909  HPE     CFD    -1337  15.6000 -20857.20 15.539068        -81.47           0        CFD    HPE
# 9: DU6474915 290651557  IMGN    CFD      -99  29.4453  -2915.08 29.509899          6.40           0        CFD   IMGN   x
# 10: DU6474915 120552228  KEY     CFD     2400  14.4000  34560.00 14.234958        396.10           0        CFD    KEY
# 11: DU6474915 120550364  KMI     CFD     1999  17.1100  34202.89 17.084500         50.98           0        CFD    KMI
# 12: DU6474915 451397890  PLTR    CFD     1959  17.3900  34067.01 17.444500       -106.77           0        CFD   PLTR
# 13: DU6474915 268212237  SNAP    CFD     -968  16.4100 -15884.88 16.415500          5.32           0        CFD   SNAP
# 14: DU6474915 481702066  SOFI    CFD     -183   7.8300  -1432.89  8.024536         35.60           0        CFD   SOFI

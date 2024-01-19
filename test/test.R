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
ib = IB$new(
  host = "cgslive.eastus.azurecontainer.io",
  port = 5000,
  strategy_name = "MinMax CGS Live",
  account_id = "U10539191",
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

ib$get()
ib$post()

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
positions = rbindlist(positions, fill = TRUE)[position > 0][order(contractDesc )]
# LCID, ET, M, KMI, LBPH, LYFT, RF, TOST, VTRS
# ET, VTRS

# position by ID
ib$get_conid_by_symbol("SPY", "CFD", "AMEX")
ib$get_position_by_conid(NULL, 134770228)

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
HOST = "http://localhost:8080/"
host = "cgslivepra.eastus.azurecontainer.io"
strategy_name = "test"
email_config_ =list(
  email_from = "mislav.sagovac@contentio.biz",
  email_to = c("mislav.sagovac@contentio.biz"),
  smtp_host = "mail.contentio.biz",
  smtp_port = 587,
  smtp_user = "mislav.sagovac@contentio.biz",
  smtp_password = "s8^t5?}r-x&Q"
)

# send message
res = GET(paste0(HOST, "echo"), query = list(msg="I am trader"))
content(res)

# post sum
res = POST(paste0(HOST, "sum"), body = list(a = 1, b = 3), encode = "json")
content(res)

# ping
data <- list(
  host = host,
  port = 5000,
  strategy_name = strategy_name,
  account_id = "U11177920",
  email_config = email_config_,
  logger = NULL
)
response <- POST(paste0(HOST, "ping"), body = data, encode = "json")
p = content(response)
p

# post liquidate
data <- list(
  accountId = "DU8203010",
  symbol = "AAPL",
  sectype = "CFD",
  host = "cgspaperpra.eastus.azurecontainer.io",
  port = 5000,
  strategy_name = "Exuber 2",
  email_config = email_config_
)
response <- POST("https://ibapi.azurewebsites.net/liquidate", body = data, encode = "json")
p = content(response)
p

# set holdings
data <- list(
  accountId = "DU6474915",
  symbol = "AMD",
  sectype = "CFD",
  side = "BUY",
  tif = "GTC",
  weight = 0.01,
  host = "cgspaperpra.eastus.azurecontainer.io",
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
response <- POST("https://ibapi.azurewebsites.net/set_holdings", body = data, encode = "json")
p = content(response)
p



# TEST LOGER --------------------------------------------------------------
# lg$info("TEST dsgfd")
connec <- dbConnect(RPostgres::Postgres(),
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
tail(table_[, .(level, timestamp, caller, msg)], 99)



# library(ibrestr)
# library(jsonlite)
# library(data.table)
#
#
# ib = IB$new(
#   host = "cgspaperexuber.eastus.azurecontainer.io",
#   port = 5000,
#   strategy_name = "Exuber Inverse",
#   email_config = list(
#     email_from = "mislav.sagovac@contentio.biz",
#     email_to = "mislav.sagovac@contentio.biz",
#     smtp_host = "mail.contentio.biz",
#     smtp_port = 587,
#     smtp_user = "mislav.sagovac@contentio.biz",
#     smtp_password = "s8^t5?}r-x&Q"
#   )
# )
# ib$get()
# ib$post()
#
# # data snapshot
# snap = ib$get_market_data_snapshot("756733", fields = "31,84,86")
# lubridate::with_tz(as.POSIXct(snap[[1]]$`_updated` / 1000), tzone = "America/New_York")
# snap[[1]]$`86`
# snap[[1]]$`84`
#
# # list orders
# ib$get_portfolio_accounts()
# live_orders = ib$get_live_orders()
# rbindlist(live_orders$orders, fill = TRUE)
#
# # positions
# positions = ib$get_account_positions("DU8203010", 0)
# lapply(positions, as.data.table)
#
# # cancel order
# # ib$cancel_order("DU8203010", "-1")
#
# # place order
# # toJSON(list(orders = list(body)), auto_unbox = TRUE)
# order = ib$place_and_confirm_order(
#   "DU8203010",
#   orders = list(
#     acctId = "DU8203010",
#     conid = as.integer("134770228"),
#     sectype = paste0("134770228", ":CFD"),
#     orderType = "MKT",
#     outsideRTH = FALSE,
#     side = "SELL",
#     quantity = as.integer("1"),
#     tif = "GTC",
#     cOID = ids::random_id()
#   )
# )
#
# # check order status and wait for 5 minute if it will be executed
# ib$get_order_status_repeated(order$order[[1]]$order_id, 10)
#
# # set holdings
# order = ib$set_holdings("DU8203010", symbol = "SPY", sectype = "CFD", side = "BUY", tif = "GTC", weight = 0.02)
#
# # liquidate
# order = ib$liquidate("DU8203010", symbol = "SPY", sectype = "CFD")
#
# # coninds
# conids = ib$get_conids_by_exchange("AMEX")
# conids[ticker == "AAPL"]
# contract = ib$search_contract_by_symbol("SPY")
# con_stk = contract[[1]]$conid
# contract = ib$get_sec_definfo(con_stk, "CFD")
# contract[[1]]$conid
#
#
# # market data
# md = ib$get_historical_data(conid = "319856165", exchange = "SMART", period = "5d", bar = "1h", outsideRth = TRUE)
# md = ib$get_historical_data(conid = "265598", exchange = "SMART", period = "5d", bar = "1h", outsideRth = TRUE, clean = TRUE)
#
# dt_ = as.data.table(cbind.data.frame(symbol = md$symbol, rbindlist(md$data)))
# dt_[, datetime := as.POSIXct(as.numeric(t) / 1000,
#                              origin = "1970-01-01",
#                              tz = "America/New_York")]
#
#
# # TEST API ----------------------------------------------------------------
# library(httr)
#
# # send message
# res = GET("http://localhost:8080/echo", query = list(msg="I am trader"))
# content(res)
#
# # post sum
# res = POST("http://localhost:8080/sum", body = list(a = 1, b = 3), encode = "json")
# content(res)
#
# # post liquidate
# data <- list(
#   accountId = "DU8203010",
#   symbol = "BIL",
#   sectype = "CFD",
#   host = "cgspaperexuber.eastus.azurecontainer.io",
#   port = 5000,
#   strategy_name = "Exuber 2",
#   email_config = list(
#     email_from = "mislav.sagovac@contentio.biz",
#     email_to = c("mislav.sagovac@contentio.biz", "kontakt@contentio.biz"),
#     smtp_host = "mail.contentio.biz",
#     smtp_port = 587,
#     smtp_user = "mislav.sagovac@contentio.biz",
#     smtp_password = "s8^t5?}r-x&Q"
#   )
# )
# response <- POST("http://localhost:8080/liquidate", body = data, encode = "json")
# p = content(response)
# p
#
# # set holdings
# data <- list(
#   accountId = "DU8203010",
#   symbol = "AAPL",
#   sectype = "CFD",
#   side = "BUY",
#   tif = "GTC",
#   weight = 0.1,
#   host = "cgspaperexuber.eastus.azurecontainer.io",
#   port = 5000,
#   strategy_name = "Exuber 2",
#   email_config = list(
#     email_from = "mislav.sagovac@contentio.biz",
#     email_to = c("mislav.sagovac@contentio.biz", "kontakt@contentio.biz"),
#     smtp_host = "mail.contentio.biz",
#     smtp_port = 587,
#     smtp_user = "mislav.sagovac@contentio.biz",
#     smtp_password = "s8^t5?}r-x&Q"
#   )
# )
# response <- POST("http://localhost:8080/set_holdings", body = data, encode = "json")
# p = content(response)
# p
# p$order[[1]]$id[[1]]

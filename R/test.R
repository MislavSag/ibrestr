# library(data.table)
# library(httr)
# library(jsonlite)
# library(data.table)
#
#
# ib = IB$new(
#   host = "cgspaperexuber.eastus.azurecontainer.io",
#   port = 5000
# )
# ib$get()
# ib$post()
#
# # coninds
# conids = ib$get_conids_by_exchange("AMEX")
# conids[ticker == "AAPL"]
# ib$search_contract_by_symbol("SPY")
# ib$get_sec_definfo("756733", "CFD")
# ib$get_sec_definfo(conid = "265598", sectype = "OPT", month = "JAN24", strike = "195", right = "P")
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
# dt_
# md$
#
# md$t <- as.numeric(md$t)
# md[, datetime := as.POSIXct(t / 1000,
#                             origin = "1970-01-01",
#                             tz = "America/New_York")]
#
#
# # 4065
#
# # # market data beta
# # md = ib$get_historical_market_data(
# #   conid = "4065", period = "2d", bar = "1hrs", outsideRth = TRUE
# # )
#
# # CONTRACT_ID = 134770228
# # CONTRACT_ID_TLT = 134770764
# # CONTRACT_ID_BIL = 298404121
#
#

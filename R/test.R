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
# conids = ib$get_conids_by_exchange("AMEX")
# conids[ticker == "AAPL"]
# ib$search_contract_by_symbol("SPY")
# ib$get_sec_definfo("756733", "CFD")
# ib$get_sec_definfo(conid = "265598", sectype = "OPT", month = "JAN24", strike = "195", right = "P")
#
# # CONTRACT_ID = 134770228
# # CONTRACT_ID_TLT = 134770764
# # CONTRACT_ID_BIL = 298404121
#
#
# ib_api <- ibrestr::IB$new(host =  "cgspaperexuber.eastus.azurecontainer.io", port = 5000)
# contract = ib_api$search_contract_by_symbol("SPY")
# contract[[1]]$conid
# ib_api$get_sec_definfo(contract[[1]]$conid, "CFD")

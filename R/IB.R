#' @title IBREST Class
#'
#' @description
#' Get data data from IB Client Portal API.
#'
#' @export
IB = R6::R6Class(
  "IB",

  public = list(

    #' @field host Host, by default localhost
    host = NULL,

    #' @field port Port, by default 5000
    port = NULL,

    #' @field base_url Base url
    base_url = NULL,

    #' @description
    #' Create a new IB object.
    #'
    #' @param host Host, by default localhost
    #' @param port Port, by default 5000
    #'
    #' @return A new `IB` object.
    initialize = function(host = "localhost", port = 5000L) {
      # base url
      self$host = host
      self$port = port
      self$base_url = sprintf("https://%s:%d/v1/api", host, port)
    },

    #' @description
    #' Performs a GET request to the specified Interactive Brokers API endpoint.
    #'
    #' @param endpoint The endpoint path for the GET request.
    #' @param query A list of query parameters for the GET request.
    #' @return The response from the GET request.
    get = function(endpoint = "/sso/validate", query = NULL) {
      url <- paste0(self$base_url, endpoint)
      print(url)
      response <- RETRY(
        "GET",
        url,
        config = config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE),
        query = query,
        times = 5L
      )
      content(response)
    },

    #' @description
    #' Performs a POST request to the specified Interactive Brokers API endpoint.
    #'
    #' @param endpoint The endpoint path for the POST request.
    #' @param body The body of the POST request.
    #' @return The response from the POST request.
    post = function(endpoint = "/tickle", body = NULL) {
      url <- paste0(self$base_url, endpoint)
      response <- RETRY(
        "POST",
        url,
        config = config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE),
        body = body,
        add_headers('User-Agent' = 'Console', 'content-type' = 'application/json'),
        encode = "json",
        times = 5L
      )
      content(response)
    },


    #' @description
    #' Performs a DELETE request to the specified Interactive Brokers API endpoint.
    #'
    #' @param endpoint The endpoint path for the DELETE request.
    #' @return The response from the DELETE request.
    delete = function(endpoint) {
      url <- paste0(self$base_url, endpoint)
      response <- RETRY(
        "DELETE",
        url,
        config = config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE),
        times = 5L
      )
      content(response)
    },

    #' @description
    #' Retrieves all contracts available on a specified exchange.
    #' Note: This is only available for Stock contracts.
    #'
    #' @param exchange A string specifying the exchange.
    #' @return A data frame with columns.
    get_conids_by_exchange = function(exchange) {
      query = list(exchange = exchange)
      result = self$get("/trsrv/all-conids", query)
      rbindlist(result)
    },

    #' @description
    #' Searches by underlying symbol or company name and relays back what derivative contract(s) it has.
    #' This endpoint must be called before using /secdef/info.
    #'
    #' @param symbol String, required, underlying symbol of interest or company name if ‘name’ is set to true.
    #' @param name Boolean, optional, determines if symbol reflects company name or ticker symbol.
    #' @param secType String, optional, declares underlying security type.
    #'        Valid values: "STK", "IND", "BOND".
    #' @return A list containing details of the contract such as conid, companyName, symbol, etc.
    search_contract_by_symbol = function(symbol, name = NULL, secType = NULL) {
      query <- list(symbol = symbol, name = name, secType = secType)
      query <- query[!sapply(query, is.null)]
      self$get("/iserver/secdef/search", query)
    },

    #' @description
    #' Retrieves security definition information for a given contract identifier (conid).
    #' This method is applicable for various security types including futures, options, warrants, cash, and CFDs.
    #' For derivatives, /iserver/secdef/search must be called first.
    #'
    #' @param conid String, required, contract identifier of the underlying or the final derivative conid.
    #' @param sectype String, required, security type of the requested contract.
    #' @param month String, required for derivatives, expiration month for the given derivative.
    #' @param exchange String, optional, exchange to receive information for in relation to the contract.
    #' @param strike String, required for options and futures options, strike price for the requested contract.
    #' @param right String, required for options, "C" for Call or "P" for Put.
    #' @param issuerId String, required for bonds, issuerId for the given bond issuer.
    #' @return A list containing details of the contract such as conid, ticker, secType, etc.
    get_sec_definfo = function(conid, sectype, month = NULL, exchange = NULL,
                               strike = NULL, right = NULL, issuerId = NULL) {
      query <- list(conid = conid, secType = sectype, month = month,
                    exchange = exchange, strike = strike, right = right,
                    issuerId = issuerId)
      query = query[!sapply(query, is.null)]
      print(query) # DEBUG
      self$get("/iserver/secdef/info", query)
    },

    #' @description
    #' Retrieves the trading schedule up to a month for the requested contract.
    #'
    #' @param assetClass String, required, security type of the given contract.
    #'        Possible values: STK, OPT, FUT, CFD, WAR, SWP, FND, BND, ICS.
    #' @param symbol String, required, symbol for the contract.
    #' @param exchange String, optional, primary exchange of the contract.
    #' @param exchangeFilter String, optional, exchanges to retrieve data from.
    #' @return A list containing the trading schedule details.
    get_trading_schedule = function(assetClass, symbol, exchange = NULL,
                                    exchangeFilter = NULL) {
      query <- list(assetClass = assetClass, symbol = symbol,
                    exchange = exchange, exchangeFilter = exchangeFilter)
      query <- query[!sapply(query, is.null)]
      self$get("/trsrv/secdef/schedule", query)
    },

    #' @description
    #' Retrieves position details for the specified contract identifier (conid).
    #'
    #' @param accountId String, required, the account ID for which the position should be retrieved.
    #' @param conId String, required, the contract ID to receive position information on.
    #' @return A list containing position details for the specified conid.
    get_position_by_conid = function(accountId, conId) {
      endpoint <- sprintf("/portfolio/%s/position/%s", accountId, conId)
      self$get(endpoint)
    },

    #' @description
    #' Submits orders when connected to an IServer Brokerage Session.
    #' Supports various advanced order types and additional details.
    #'
    #' @param accountID String, required, the account ID for which the order should be placed.
    #' @param orders Array of Objects, required, used to specify the order content.
    #' @return A list containing details of the order status.
    place_order = function(accountID, orders) {
      endpoint <- sprintf("/iserver/account/%s/orders", accountID)
      body <- list(orders = orders)
      self$post(endpoint, body = body)
    },

    #' @description
    #' Confirms the order reply, agreeing or declining the message to transmit or discard the order.
    #'
    #' @param replyId String, required, the ID value from the prior order request.
    #' @param confirmed Boolean, required, pass TRUE to agree to the message and transmit the order, or FALSE to decline and discard the order.
    #' @return A list containing details of the order confirmation.
    confirm_order = function(replyId, confirmed = TRUE) {
      endpoint <- sprintf("/iserver/reply/%s", replyId)
      body <- list(confirmed = confirmed)
      self$post(endpoint, body = body)
    },

    #' @description
    #' Retrieves the status of an individual order using the orderId.
    #'
    #' @param orderId String, required, order identifier for the placed order.
    #' @return A list containing details of the order status.
    get_order_status = function(orderId) {
      endpoint <- sprintf("/iserver/account/order/status/%s", orderId)
      self$get(endpoint)
    },

    #' @description
    #' Places an order and handles the confirmation process.
    #'
    #' @param accountID String, required, the account ID for which the order should be placed.
    #' @param orders Array of Objects, required, used to specify the order content.
    #' @return A list containing details of the order placement and confirmation.
    place_and_confirm_order = function(accountID, orders) {
      placed_order <- self$placeOrder(accountID, orders)

      if (length(placed_order) > 0) {
        if (!is.null(placed_order$order_id)) {
          # Standard Order Response - order placed successfully without warnings
          return(list(order = placed_order, confirmation = "Order placed successfully"))
        } else if (!is.null(placed_order$id)) {
          # Alternate Response Object - order requires confirmation due to a warning
          replyId <- placed_order$id
          confirmation <- self$confirmOrder(replyId, confirmed = TRUE) # toJSON(list(confirmed = TRUE), auto_unbox = TRUE)
          return(list(order = placed_order, confirmation = confirmation))
        } else if (!is.null(placed_order$error)) {
          # Order Reject Object - order was rejected
          return(list(order = placed_order, confirmation = "Order rejected", error = placed_order$error))
        } else {
          return("Unexpected response received from the order placement.")
        }
      } else {
        return("Order placement failed or no response received.")
      }
    },

    # DOESNT WORK
    #' @description
    #' Retrieves a list of historical market data for a given contract identifier.
    #'
    #' @param conid String, required, contract identifier for which data should be requested.
    #' @param period String, required, duration for which data should be requested.
    #' @param bar String, required, bar size for which bars should be returned.
    #' @param outsideRth Boolean, optional, define if data should be returned for trades outside regular trading hours.
    #' @param startTime String, optional, specify the value from where historical data should be taken.
    #' @param direction String, optional, specify the direction from which market data should be returned.
    #' @param barType String, optional, returns valid bar types for which data may be requested.
    #' @return A list containing historical market data.
    get_historical_data_beta = function(conid, period, bar, outsideRth = NULL,
                                          startTime = NULL, direction = NULL,
                                          barType = NULL) {
      query <- list(conid = conid, period = period, bar = bar,
                    outsideRth = outsideRth, startTime = startTime,
                    direction = direction, barType = barType)
      query <- query[!sapply(query, is.null)]
      self$get("/hmds/history", query = query)
    },

    #' @description
    #' Retrieves historical market data for a given contract identifier.
    #'
    #' @param conid String, required, contract identifier for the ticker symbol of interest.
    #' @param period String, required, overall duration for which data should be returned.
    #' @param bar String, required, individual bars of data to be returned.
    #' @param exchange String, optional, exchange to receive data from.
    #' @param startTime String, optional, starting date of the request duration.
    #' @param outsideRth Boolean, optional, determine if data after regular trading hours is needed.
    #' @param clean Boolean, optional, if TRUE extract data object
    #' @return A list containing historical market data.
    get_historical_data = function(conid, period, bar, exchange = NULL,
                                   startTime = NULL, outsideRth = NULL,
                                   clean = FALSE) {
      query = list(conid = conid, period = period, bar = bar, exchange = exchange,
                   startTime = startTime, outsideRth = outsideRth)
      query <- query[!sapply(query, is.null)]
      md = self$get("/iserver/marketdata/history", query = query)
      if (clean) {
        md = as.data.table(cbind.data.frame(symbol = md$symbol, rbindlist(md$data)))
        md[, datetime := as.POSIXct(as.numeric(t) / 1000,
                                     origin = "1970-01-01",
                                     tz = "America/New_York")]
      }
      return(md)
    },


    #' @description
    #' Cancels an open order for the specified account and order ID.
    #'
    #' @param accountId String, required, the account ID for which the order should be canceled.
    #' @param orderId String, required, the order ID that should be canceled.
    #' @return A list containing the confirmation of the cancellation request.
    cancel_order = function(accountId, orderId) {
      endpoint <- sprintf("/iserver/account/%s/order/%s", accountId, orderId)
      self$delete(endpoint)
    },

    #' @description
    #' Retrieves a list of trades for the currently selected account for the current day and six previous days.
    #'
    #' @param days String, optional, specify the number of days to receive executions for, up to a maximum of 7 days.
    #' @param accountId String, optional, include a specific account identifier or allocation group to retrieve trades for.
    #' @return A list containing details of trades.
    get_trades = function(days = NULL, accountId = NULL) {
      query <- list(days = days, accountId = accountId)
      query <- query[!sapply(query, is.null)]
      self$get("/iserver/account/trades", query = query)
    },

    # PORTFOLIO ---------------------------------------------------------------
    #' @description
    #' Retrieves a list of accounts for which the user can view position and account information.
    #'
    #' @return A list containing details of portfolio accounts.
    get_portfolio_accounts = function() {
      endpoint <- "/portfolio/accounts"
      self$get(endpoint)
    },

    #' @description
    #' Retrieves a list of sub-accounts for which the user can view position and account-related information.
    #'
    #' @return A list containing details of portfolio subaccounts.
    get_portfolio_subaccounts = function() {
      endpoint <- "/portfolio/subaccounts"
      self$get(endpoint)
    },

    #' @description
    #' Retrieves information regarding settled cash, cash balances, etc., in the account’s base currency and other cash balances held in other currencies.
    #'
    #' @param accountId String, required, specify the account ID for which you require ledger information.
    #' @return A list containing details of the portfolio summary.
    get_portfolio_summary = function(accountId) {
      endpoint <- sprintf("/portfolio/%s/summary", accountId)
      self$get(endpoint)
    }
  )
)

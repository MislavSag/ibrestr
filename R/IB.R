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

      #' @field strategy_name Strategy name, optional.
      strategy_name = NULL,

      #' @field email_config Configuration for email notifications.
      #' @description
      #' List containing email configuration details. Includes fields for SMTP settings and email addresses.
      #' Required elements: email_from, email_to, smtp_host, smtp_port, smtp_user, smtp_password.
      email_config = list(),

      #' @description
      #' Create a new IB object.
      #'
      #' @param host Host, by default localhost
      #' @param port Port, by default 5000
      #' @param strategy_name Strategy name, optional.
      #' @param email_config List containing email configuration details.
      #'     Includes fields for SMTP settings and email addresses.
      #'     Required elements: email_from, email_to,
      #'     smtp_host, smtp_port, smtp_user, smtp_password.
      #'
      #' @return A new `IB` object.
      initialize = function(host = "localhost",
                            port = 5000L,
                            strategy_name = NULL,
                            email_config = list()) {
        # base url
        self$host = host
        self$port = port
        self$base_url = sprintf("https://%s:%d/v1/api", host, port)

        # other
        self$strategy_name = strategy_name

        # email notification
        if (length(email_config) > 0) {
          assert_names(names(email_config), "named",
                       must.include = c("email_from", "email_to", "smtp_host",
                                        "smtp_port", "smtp_user", "smtp_password"))
        }
        self$email_config = email_config
      },

      #' @description
      #' Performs a GET request to the specified Interactive Brokers API endpoint.
      #'
      #' @param endpoint The endpoint path for the GET request.
      #' @param query A list of query parameters for the GET request.
      #' @return The response from the GET request.
      get = function(endpoint = "/sso/validate", query = NULL) {
        url <- paste0(self$base_url, endpoint)
        response <- RETRY(
          "GET",
          url,
          config = config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE, timeout = 15),
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
          config = config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE, timeout = 15),
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
          config = config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE, timeout = 15),
          times = 5L
        )
        content(response)
      },

      #' @description
      #' Sends an email notification.
      #'
      #' @param subject The subject of the email.
      #' @param body The body content of the email.
      #' @param html Boolean indicating if the body content is HTML. Defaults to FALSE.
      send_email = function(subject, body, html = FALSE) {
        if (length(self$email_config) >= 6) {
          send.mail(
            from = self$email_config$email_from,
            to = self$email_config$email_to,
            subject = paste0(toupper(self$strategy_name), " - ", subject),
            body = body,
            encoding = "utf-8",
            smtp = list(
              host.name = self$email_config$smtp_host,
              port = self$email_config$smtp_port,
              user.name = self$email_config$smtp_user,
              passwd = self$email_config$smtp_password
            ),
            authenticate = TRUE,
            html = html
          )
        }
      },


      # CONTRACT ----------------------------------------------------------------
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


      # ORDERS ------------------------------------------------------------------
      #' @description
      #' Submits orders when connected to an IServer Brokerage Session.
      #' Supports various advanced order types and additional details.
      #'
      #' @param accountID String, required, the account ID for which the order should be placed.
      #' @param orders Array of Objects, required, used to specify the order content.
      #' @return A list containing details of the order status.
      place_order = function(accountID, orders) {
        assert_list(orders)
        assert_names(names(orders),
                     "named",
                     must.include = c("conid", "orderType", "side", "tif"))
        endpoint <- sprintf("/iserver/account/%s/orders", accountID)
        body <- toJSON(list(orders = list(orders)), auto_unbox = TRUE)
        print(body)
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
        body <- toJSON(list(confirmed = confirmed), auto_unbox = TRUE)
        self$post(endpoint, body = body)
      },

      #' @description
      #' Places an order and handles the confirmation process.
      #'
      #' @param accountID String, required, the account ID for which the order should be placed.
      #' @param orders Array of Objects, required, used to specify the order content.
      #' @return A list containing details of the order placement and confirmation.
      place_and_confirm_order = function(accountID, orders) {
        placed_order = self$place_order(accountID, orders)
        print(placed_order)

        if (length(placed_order) > 0) {
          if (!is.null(placed_order[[1]]$order_id)) {
            # Standard Order Response - order placed successfully without warnings
            return(list(order = placed_order, info = "Order placed successfully"))
          } else if (!is.null(placed_order[[1]]$id)) {
            # Alternate Response Object - order requires confirmation due to a warning
            print("Alternate Response Object - order requires confirmation")
            replyId <- placed_order[[1]]$id[[1]]
            confirmation <- self$confirm_order(replyId, confirmed = TRUE)
            return(list(order = confirmation, info = "Confirmation"))
          } else if (!is.null(placed_order$error)) {
            # Order Reject Object - order was rejected
            return(list(order = placed_order, info = "Order rejected", error = placed_order$error))
          } else {
            return("Unexpected response received from the order placement.")
          }
        } else {
          return("Order placement failed or no response received.")
        }
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


      # ORDER MONITORING --------------------------------------------------------
      #' @description
      #' Retrieves a list of live orders (cancelled, filled, submitted) for the given account.
      #'
      #' @param filters String, optional, filter orders by a unique status value (separated by commas for multiple filters).
      #' @param force Boolean, optional, force the system to clear saved information and make a fresh request for orders.
      #' @param accountId String, optional, for linked accounts, view orders on specified sub-accounts.
      #' @return A list containing details of live orders.
      get_live_orders = function(filters = NULL, force = NULL, accountId = NULL) {
        query = list(filters = filters, force = force, accountId = accountId)
        query <- query[!sapply(query, is.null)]
        self$get("/iserver/account/orders", query = query)
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
      #' Retrieves the status of an individual order using the orderId and checks repeatedly for a specified duration.
      #'
      #' @param orderId String, required, order identifier for the placed order.
      #' @param n Integer, required, number of seconds to keep checking the order status.
      #' @return Integer, 1L if the order status is 'filled' within the specified time, otherwise 0L.
      get_order_status_repeated = function(orderId, n) {
        for (i in 1:n) {
          status_response <- self$get_order_status(orderId)
          if (length(status_response) > 0 && status_response$order_status == "Filled") {
            return(1L)
          }
          Sys.sleep(1)  # Wait for 1 second before next check
        }
        return(0L)
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


      # MARKET DATA -------------------------------------------------------------
      #' @description
      #' Retrieves market data for the given contract identifiers.
      #'
      #' @param conids String, required, comma-separated contract identifiers.
      #' @param fields String, required, comma-separated series of field ids.
      #' @return A list containing market data for the specified contracts.
      get_market_data_snapshot = function(conids, fields) {
        # Validate inputs
        if (is.null(conids) || is.null(fields)) {
          stop("Both 'conids' and 'fields' are required.")
        }

        # Prepare the endpoint and query
        query <- list(conids = conids, fields = fields)

        # Perform the GET request
        response <- self$get("/iserver/marketdata/snapshot", query = query)
        return(response)
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
      },

      #' @description
      #' Retrieves a list of positions for the given account with pagination support.
      #'
      #' @param accountId String, required, the account ID for which positions should be retrieved.
      #' @param pageId String, required, the "page" of positions to be returned (pagination starts at 0).
      #' @param model String, optional, code for the model portfolio to compare against.
      #' @param sort String, optional, declare the table to be sorted by which column.
      #' @param direction String, optional, the order to sort by ('a' for ascending, 'd' for descending).
      #' @param period String, optional, period for PnL column (e.g., 1D, 7D, 1M).
      #' @return A list containing details of positions for the specified account and page.
      get_account_positions = function(accountId, pageId, model = NULL, sort = NULL,
                                       direction = NULL, period = NULL) {
        endpoint <- sprintf("/portfolio/%s/positions/%s", accountId, pageId)
        query <- list(model = model, sort = sort, direction = direction, period = period)
        query <- query[!sapply(query, is.null)]
        self$get(endpoint, query = query)
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


      # PORTFOLIO ANALYST -------------------------------------------------------
      #' @description
      #' Retrieves account performance (MTM) for the specified accounts.
      #'
      #' @param acctIds Array of Strings, required, account IDs to receive data for.
      #' @param freq String, required, frequency for analysis.
      #'       Available Values: "D" (Daily), "M" (Monthly), "Q" (Quarterly).
      #'
      #' @return A list containing the account performance data.
      get_account_performance = function(acctIds, freq = "D") {
        assert_character(acctIds, any.missing = FALSE, .var.name = "acctIds")
        assert_choice(freq, choices = c("D", "M", "Q"))
        body = toJSON(list(acctIds = list(acctIds), freq = freq), auto_unbox = TRUE)
        response = self$post("/pa/performance", body = body)
        return(response)
      },

      #' @description
      #' Retrieves transaction history for specified account IDs and contract IDs.
      #'
      #' @param acctIds Array of Strings, required, account IDs to receive data for.
      #' @param conids Array of Integers, required, contract IDs to receive data for.
      #' @param currency String, required, currency for displaying amounts.
      #' @param days Integer, optional, number of days for transaction data.
      #'
      #' @return A list containing the transaction history data.
      get_transaction_history = function(acctIds, conids, currency, days = 90) {
        # Validate input with checkmate
        assert_character(acctIds, any.missing = FALSE, .var.name = "acctIds")
        assert_integerish(conids, any.missing = FALSE, .var.name = "conids")
        assert_string(currency, .var.name = "currency")
        if (!is.null(days)) {
          assert_integerish(days, lower = 1, .var.name = "days")
        }

        # Endpoint and body
        body <- toJSON(list(acctIds = list(acctIds),
                            conids = list(conids),
                            currency = currency,
                            days = days), auto_unbox = TRUE)
        response <- self$post("/pa/transactions", body = body)
        return(response)
      },


      # UTILS -------------------------------------------------------------------
      #' @description
      #' Check for errors in gateway
      #'
      #' @param tries Number of tries - number of get requests and seconds.
      #' @return Boolean (error or no error).
      check_gateway = function(tries = 100) {
        # get and post requests to IB
        get_req_test <- tryCatch(self$get(), error = function(e) NULL)
        post_req_test <- tryCatch(self$post(), error = function(e) NULL)

        # test for errors
        try = 0
        repeat {
          if (is.null(get_req_test) | is.null(post_req_test)) {
            Sys.sleep(1L)
            try = try + 1
            get_req_test = tryCatch(self$get(), error = function(e) NULL)
            post_req_test = tryCatch(self$post(), error = function(e) NULL)

            if (try > tries) {
              return(FALSE)
            }
          } else {
            return(TRUE)
          }
        }

        # notify
        if (length(self$email_config) >= 6) {
          print("Notification - send email")
          self$send_email("Check gateway returned FALSE. Check ACI",
                          "Try to 1) restart ACI 2) ceck Ibema issues",
                          html = TRUE)
        }


        return(TRUE)
      },

      #' @description
      #' Performs a GET request to the specified Interactive Brokers API endpoint.
      #'
      #' @param order Filled order response
      #' @return Html character.
      order_result_to_html = function(order) {
        html_content <- "<html><body><h2>Order Details</h2><ul>"
        for (name in names(order)) {
          html_content <- paste0(html_content, "<li><b>", name, ":</b> ", order[[name]], "</li>")
        }
        html_content = paste0(html_content, "</ul></body></html>")
        html_content
      },

      #' @description
      #' Performs a GET request to the specified Interactive Brokers API endpoint.
      #'
      #' @param accountId String, required, the account ID for which the order should be placed.
      #' @param symbol String, required, underlying symbol of interest or company name if ‘name’ is set to true.
      #' @param sectype String, required, security type of the requested contract.
      #' @param side String, required, Valid Values: SELL or BUY.
      #' @param tif String, required, The Time-In-Force determines how long the order remains active on the market.
      #'     Valid Values: GTC, OPG, DAY, IOC, PAX (CRYPTO ONLY).
      #' @param weight Numeric, required, portfolio weight.
      #' @return It can return string if error or order info if everything is as expected.
      set_holdings = function(accountId, symbol, sectype, side, tif, weight) {
        # checks
        assert_string(accountId)
        assert_string(symbol)
        assert_choice(sectype, c("STK", "CFD", "OPT", "CASH", "WAR", "FUT"))
        assert_choice(side, c("BUY")) # DON'T ALLOW SELL (SHORT) YET
        assert_choice(tif, c("GTC", "OPG", "DAY", "IOC", "PAX"))
        assert_number(weight, lower = 0, upper = 10) # max leverage X 10

        # find con id by symbol
        print("Find conid by symbol for STK")
        contract_stk = self$search_contract_by_symbol(symbol)
        conid_stk = contract_stk[[1]]$conid

        # find conid for sectype
        print("Find conid by symbol for sectype")
        contract = self$get_sec_definfo(conid_stk, "CFD")
        conid = contract[[1]]$conid
        cat("Conid ", conid, "\n")

        # check if gateway is ready
        print("Check gateway")
        test_ib = self$check_gateway()
        if (!test_ib) return("Check gateway didn't pass.")

        # check if account id is in accounts
        print("Checks accounts")
        accounts = self$get_portfolio_accounts()
        accounts = unlist(lapply(accounts, `[`, "id"))
        if (!(accountId %in% accounts)) return("accountID not in accounts.")

        # get position
        print("Get position")
        positions = self$get_position_by_conid(accountId, conid)
        if (length(positions) == 0) {
          position = 0
        } else {
          position = positions[[1]]$position
        }
        cat("Position ", position, "\n")

        # get available cash
        print("Available cash")
        portfolio_summary = self$get_portfolio_summary(accountId)
        cash = portfolio_summary$totalcashvalue$amount
        cat("Cash ", cash, "\n")

        # order body
        print("Create body")
        if (side == "BUY" & position == 0) {
          # get price
          fmp_url = paste0(
            "https://financialmodelingprep.com/api/v3/quote-short/",
            symbol,
            "?apikey=15cd5d0adf4bc6805a724b4417bbaafc"
          )
          res = GET(fmp_url)
          p = content(res)
          if (length(p) == 0) {
            return("No data in FMP cloud.")
          }
          price = p[[1]]$price
          quantity = floor((cash * weight) / price)

          # order body
          body = list(
            acctId = accountId,
            conid = as.integer(conid),
            sectype = paste0(conid, "@", sectype),
            orderType = "MKT",
            outsideRTH = FALSE,
            side = side,
            quantity = quantity,
            tif = tif
          )
        # } else if (sign == "SELL" & position > 0) {
        #   # order body
        #   body = list(
        #     acctId = accountId,
        #     conid = as.integer(conid),
        #     sectype = paste0(conid, "@CFD"),
        #     orderType = "MKT",
        #     outsideRTH = FALSE,
        #     side = side,
        #     quantity = position,
        #     tif = tif
        #   )
        } else {
          return("We got sign but we don't buy or sell?")
        }

        # place order
        placed_order = self$place_and_confirm_order(accountId, body)
        print(placed_order)

        # check order
        if (is.null(placed_order$order[[1]]$order_id)) {
          print("There is no order id in placed_order return object")
          return(placed_order)
        } else {
          print("Check status")
          status = self$get_order_status_repeated(placed_order$order[[1]]$order_id, 60)
          if (status == 0) {
            # notify
            if (length(self$email_config) >= 6) {
              self$send_email("Order status not filled",
                              "Status is not filled",
                              html = FALSE)
            }
            return("Status is not filled")
          }
        }

        # notify
        if (length(self$email_config) >= 6) {
          self$send_email("Order with Set Holdings",
                          self$order_result_to_html(status$orders[[1]]),
                          html = TRUE)
        }

        return(placed_order)
      },

      #' @description
      #' Liquidate all positions by symbol.
      #'
      #' @param accountId String, required, the account ID for which the order should be placed.
      #' @param symbol String, required, underlying symbol of interest or company name if ‘name’ is set to true.
      #' @param sectype String, required, security type of the requested contract.
      #' @param side String, required, Valid Values: SELL or BUY.
      #' @param tif String, required, The Time-In-Force determines how long the order remains active on the market.
      #'     Valid Values: GTC, OPG, DAY, IOC, PAX (CRYPTO ONLY).
      #' @param weight Numeric, required, portfolio weight.
      #' @return It can return string if error or order info if everything is as expected.
      liquidate = function(accountId, symbol, sectype) {
        # debug
        # accountId = "DU8203010"
        # symbol = "SPY"
        # sectype = "CFD"
        # host = "cgspaperexuber.eastus.azurecontainer.io"
        # port = 5000
        # strategy_name = "Exuber 2"
        # self = IB$new(
        #   host = host,
        #   port = port,
        #   strategy_name = strategy_name,
        #   email_config = NULL
        # )

        # checks
        assert_string(accountId)
        assert_string(symbol)
        assert_choice(sectype, c("STK", "CFD", "OPT", "CASH", "WAR", "FUT"))

        # find con id by symbol
        print("Find conid by symbol for STK")
        contract_stk = self$search_contract_by_symbol(symbol)
        conid_stk = contract_stk[[1]]$conid

        # find conid for sectype
        print("Find conid by symbol for sectype")
        contract = self$get_sec_definfo(conid_stk, "CFD")
        conid = contract[[1]]$conid
        cat("Conid ", conid, "\n")

        # check if gateway is ready
        print("Check gateway")
        test_ib = self$check_gateway()
        if (!test_ib) return("Check gateway - didn't pass.")

        # check if account id is in accounts
        print("Checks accounts")
        accounts = self$get_portfolio_accounts()
        accounts = unlist(lapply(accounts, `[`, "id"))
        if (!(accountId %in% accounts)) return("accountID not in accounts.")

        # get position
        print("Get position")
        positions = self$get_position_by_conid(accountId, conid)
        if (length(positions) == 0) {
          position = 0
        } else {
          position = positions[[1]]$position
        }
        cat("Position ", position, "\n")

        # Check if position is greater than 0
        if (position == 0) {
          return("Position is 0")
        }

        # define body
        body = list(
          acctId = accountId,
          conid = as.integer(conid),
          sectype = paste0(conid, "@", sectype),
          orderType = "MKT",
          outsideRTH = FALSE,
          side = "SELL",
          quantity = position,
          tif = "GTC"
        )

        # place order
        placed_order = self$place_and_confirm_order(accountId, body)
        print(placed_order)

        # place order, confirm and heck status
        if (is.null(placed_order$order[[1]]$order_id)) {
          print("There is no order id in placed_order return object")
          return(placed_order)
        } else {
          print("Check status")
          status = self$get_order_status_repeated(placed_order$order[[1]]$order_id, 60)
          if (status == 0) {
            # notify
            if (length(self$email_config) >= 6) {
              self$send_email("Order status not filled",
                              "Status is not filled",
                              html = FALSE)
            }
            return("Status is not filled")
          }
        }

        # notify
        if (length(self$email_config) >= 6) {
          print("Notification - send email")
          self$send_email("Order with Liquidate",
                          self$order_result_to_html(status$orders[[1]]),
                          html = TRUE)
        }
        return(placed_order)
      }
    )
  )

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

      #' @field account_id Account ID.
      account_id = NULL,

      #' @field strategy_name Strategy name, optional.
      strategy_name = NULL,

      #' @field email_config Configuration for email notifications.
      #' @description
      #' List containing email configuration details. Includes fields for SMTP settings and email addresses.
      #' Required elements: email_from, email_to, smtp_host, smtp_port, smtp_user, smtp_password.
      email_config = list(),

      #' @field logger Logger object for logging messages.
      logger = NULL,

      #' @description
      #' Create a new IB object.
      #'
      #' @param host Host, by default localhost
      #' @param port Port, by default 5000
      #' @param strategy_name Strategy name, optional.
      #' @param account_id Account ID.
      #' @param email_config List containing email configuration details.
      #'     Includes fields for SMTP settings and email addresses.
      #'     Required elements: email_from, email_to,
      #'     smtp_host, smtp_port, smtp_user, smtp_password.
      #' @param logger A logger object for logging. If not provided,
      #'     a default no-operation logger is used.
      #'
      #' @return A new `IB` object.
      initialize = function(host = "localhost",
                            port = 5000L,
                            strategy_name = NULL,
                            account_id = NULL,
                            email_config = list(),
                            logger = NULL) {
        # base url
        self$host = host
        self$port = port
        self$base_url = sprintf("https://%s:%d/v1/api", host, port)
        self$account_id = account_id

        # other
        self$strategy_name = strategy_name

        # email notification
        if (length(email_config) > 0) {
          assert_names(names(email_config), "named",
                       must.include = c("email_from", "email_to", "smtp_host",
                                        "smtp_port", "smtp_user", "smtp_password"))
          private$notify = TRUE
        }
        self$email_config = email_config

        # loger
        if (!is.null(logger) && inherits(logger, "Logger")) {
          self$logger = logger
        } else {
          # Default to a no-operation logger
          self$logger = lgr::get_logger("NOP") # NOP logger does nothing
        }
      },

      #' @description
      #' Performs a GET request to the specified Interactive Brokers API endpoint.
      #'
      #' @param endpoint The endpoint path for the GET request.
      #' @param query A list of query parameters for the GET request.
      #' @param times The number of times to retry the GET request.
      #' @return The response from the GET request.
      get = function(endpoint = "/sso/validate", query = NULL, times = 5L) {
        url <- paste0(self$base_url, endpoint)
        response <- RETRY(
          "GET",
          url,
          config = config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE, timeout = 15),
          query = query,
          times = times
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
        if (private$notify) {
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
        self$logger$info(paste("Sending GET request to", "/trsrv/all-conids"))
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
        self$get("/iserver/secdef/info", query, times = 2L)
      },

      #' @description
      #' Retrieves stock contracts for given symbol(s).
      #'
      #' @param symbols String, required, comma-separated list of stock symbols.
      #'        Symbols must contain only capitalized letters.
      #' @return A list containing details of stock contracts.
      get_stocks_by_symbol = function(symbols) {
        if (is.null(symbols) || symbols == "") {
          stop("Symbols parameter is required and cannot be empty.")
        }
        # Construct query
        query = list(symbols = symbols)

        # Perform GET request
        response = self$get("/trsrv/stocks", query = query)
        return(response)
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
      #' @param account_id String, required, the account ID for which the order should be placed.
      #' @param orders Array of Objects, required, used to specify the order content.
      #' @return A list containing details of the order status.
      place_order = function(orders, account_id=NULL) {
        if (is.null(account_id)) account_id = self$account_id
        assert_true(self$check_account(account_id))
        assert_string(account_id)
        assert_list(orders)
        assert_names(names(orders),
                     "named",
                     must.include = c("conid", "orderType", "side", "tif"))
        endpoint <- sprintf("/iserver/account/%s/orders", account_id)
        body <- toJSON(list(orders = list(orders)), auto_unbox = TRUE)
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
      #' @param account_id String, required, the account ID for which the order should be placed.
      #' @param orders Array of Objects, required, used to specify the order content.
      #' @return A list containing details of the order placement and confirmation.
      place_and_confirm_order = function(account_id=NULL, orders) {
        if (is.null(account_id)) account_id = self$account_id
        assert_true(self$check_account(account_id))
        placed_order = self$place_order(orders, account_id)
        print(placed_order)

        if (length(placed_order) > 0) {
          if (!is.null(placed_order[[1]]$order_id)) {
            # Standard Order Response - order placed successfully without warnings
            return(list(order = placed_order, info = "Order placed successfully"))
          } else if (!is.null(placed_order[[1]]$id)) {
            # Alternate Response Object - order requires confirmation due to a warning
            self$logger$info("Alternate Response Object - order requires confirmation")
            Sys.sleep(1L)
            replyId =  placed_order[[1]]$id[[1]]
            confirmation = self$confirm_order(replyId, confirmed = TRUE)
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
      #' @param account_id String, required, the account ID for which the order should be canceled.
      #' @param orderId String, required, the order ID that should be canceled.
      #' @return A list containing the confirmation of the cancellation request.
      cancel_order = function(account_id=NULL, orderId) {
        if (is.null(account_id)) account_id = self$account_id
        assert_true(self$check_account(account_id))
        endpoint <- sprintf("/iserver/account/%s/order/%s", account_id, orderId)
        self$delete(endpoint)
      },


      # ORDER MONITORING --------------------------------------------------------
      #' @description
      #' Retrieves a list of live orders (cancelled, filled, submitted) for the given account.
      #'
      #' @param filters String, optional, filter orders by a unique status value (separated by commas for multiple filters).
      #' @param force Boolean, optional, force the system to clear saved information and make a fresh request for orders.
      #' @param account_id String, optional, for linked accounts, view orders on specified sub-accounts.
      #' @return A list containing details of live orders.
      get_live_orders = function(filters = NULL, force = NULL, account_id = NULL) {
        query = list(filters = filters, force = force, account_id = account_id)
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
          status_response = self$get_order_status(orderId)
          if (length(status_response) > 0 &&
              (status_response$order_status == "Filled" |
               status_response$cum_fill > 0)) {
            return(status_response)
          }
          self$logger$info("Check status %i ", as.integer(i))
          Sys.sleep(1)
        }
        return(0L)
      },

      #' @description
      #' Retrieves a list of trades for the currently selected account for the current day and six previous days.
      #'
      #' @param days String, optional, specify the number of days to receive executions for, up to a maximum of 7 days.
      #' @param account_id String, optional, include a specific account identifier or allocation group to retrieve trades for.
      #' @return A list containing details of trades.
      get_trades = function(days = NULL, account_id = NULL) {
        if (is.null(account_id)) account_id = self$account_id
        assert_true(self$check_account(account_id))
        query <- list(days = days, account_id = account_id)
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
      #' @param account_id String, required, specify the account ID for which you require ledger information.
      #' @return A list containing details of the portfolio summary.
      get_portfolio_summary = function(account_id=NULL) {
        if (is.null(account_id)) account_id = self$account_id
        assert_true(self$check_account(account_id))
        endpoint <- sprintf("/portfolio/%s/summary", account_id)
        self$get(endpoint)
      },

      #' @description
      #' Retrieves a list of positions for the given account with pagination support.
      #'
      #' @param account_id String, required, the account ID for which positions should be retrieved.
      #' @param pageId String, required, the "page" of positions to be returned (pagination starts at 0).
      #' @param model String, optional, code for the model portfolio to compare against.
      #' @param sort String, optional, declare the table to be sorted by which column.
      #' @param direction String, optional, the order to sort by ('a' for ascending, 'd' for descending).
      #' @param period String, optional, period for PnL column (e.g., 1D, 7D, 1M).
      #' @return A list containing details of positions for the specified account and page.
      get_account_positions = function(account_id=NULL, pageId, model = NULL, sort = NULL,
                                       direction = NULL, period = NULL) {
        if (is.null(account_id)) account_id = self$account_id
        self$logger$info("Get portfolio posistions for the account %s", account_id)
        endpoint = sprintf("/portfolio/%s/positions/%s", account_id, pageId)
        query = list(model = model, sort = sort, direction = direction, period = period)
        query = query[!sapply(query, is.null)]
        self$get(endpoint, query = query)
      },

      #' @description
      #' Retrieves position details for the specified contract identifier (conid).
      #'
      #' @param account_id String, required, the account ID for which the position should be retrieved.
      #' @param conId String, required, the contract ID to receive position information on.
      #' @return A list containing position details for the specified conid.
      get_position_by_conid = function(account_id=NULL, conId) {
        if (is.null(account_id)) account_id = self$account_id
        endpoint <- sprintf("/portfolio/%s/position/%s", account_id, conId)
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
        if (private$notify) {
          self$logger$info("Notification - send email")
          self$send_email("Check gateway returned FALSE. Check ACI",
                          "Try to 1) restart ACI 2) ceck Ibema issues",
                          html = FALSE)
        }

        return(TRUE)
      },

      #' @description
      #' Check if account id is in accounts
      #' @param account_id String, required, the account ID for which the order should be placed.
      #' @return Boolean (error or no error).
      #' @export
      check_account = function(account_id) {
        # check if account id is in accounts
        self$logger$info("Checks accounts")
        accounts = self$get_portfolio_accounts()
        accounts = unlist(lapply(accounts, `[`, "id"))
        if (!(account_id %in% accounts)) {
          self$logger$warn("account_id not in accounts.")
          return(FALSE)
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
      #' Get Conid by symbol
      #'
      #' @param symbol String, required, underlying symbol of interest or company name if ‘name’ is set to true.
      #' @param sectype String, required, security type of the requested contract.
      #' @param exchange String, required, exchange to receive information for in relation to the contract.
      #' @return Conid.
      get_conid_by_symbol = function(symbol, sectype, exchange) {
        # find con id by symbol
        conids = tryCatch({self$get_conids_by_exchange(exchange)},
                          error = function(e) NULL)
        tries = 0
        while (is.null(conids) & tries < 10) {
          conids = tryCatch({self$get_conids_by_exchange(exchange)},
                            error = function(e) NULL)
          Sys.sleep(0.5)
          tries = tries + 1
        }
        if (is.null(conids)) return("Can't get data for STK ids")
        conid_stk = conids[ticker == symbol, conid]

        # check if conid_stk is empty
        if (length(conid_stk) == 0) {
          return("Conid_stk is empty")
        }

        # UNSTABLE
        # find conid for sectype
        self$logger$info("Find conid by symbol for symbol %s", symbol)
        contract = self$get_sec_definfo(conid_stk, "CFD")
        conid = tryCatch({contract[[1]]$conid}, error = function(e) NULL)

        # check if conid is found
        if (is.null(conid)) {
          self$logger$info("ConID is NULL for the symbol %s", symbol)
          contracts_ = self$search_contract_by_symbol(symbol)
          index_ = which(lapply(contracts_, `[[`, "conid") == conid_stk)
          index_cfd = which(lapply(contracts_[[index_]]$sections, `[[`, "secType") == "CFD")
          conid = contracts_[[index_]]$sections[[index_cfd]]$conid
        }
        self$logger$info("ConId is %s", conid)

        return(conid)
      },

      #' @description
      #' Performs a GET request to the specified Interactive Brokers API endpoint.
      #'
      #' @param account_id String, required, the account ID for which the order should be placed.
      #' @param symbol String, required, underlying symbol of interest or company name if ‘name’ is set to true.
      #' @param sectype String, required, security type of the requested contract.
      #' @param side String, required, Valid Values: SELL or BUY.
      #' @param tif String, required, The Time-In-Force determines how long the order remains active on the market.
      #'     Valid Values: GTC, OPG, DAY, IOC, PAX (CRYPTO ONLY).
      #' @param weight Numeric, required, portfolio weight.
      #' @return It can return string if error or order info if everything is as expected.
      set_holdings = function(account_id=NULL, symbol, sectype, side, tif, weight) {
        # checks
        if (is.null(account_id)) account_id = self$account_id
        assert_true(self$check_account(account_id))
        self$logger$info("Checks")
        assert_string(account_id)
        assert_string(symbol)
        assert_choice(sectype, c("STK", "CFD", "OPT", "CASH", "WAR", "FUT"))
        assert_choice(side, c("BUY")) # DON'T ALLOW SELL (SHORT) YET
        assert_choice(tif, c("GTC", "OPG", "DAY", "IOC", "PAX"))
        assert_number(weight, lower = 0, upper = 10) # max leverage X 10

        # check if gateway is ready
        self$logger$info("Check gateway")
        test_ib = self$check_gateway()
        if (!test_ib) return("Check gateway didn't pass.")

        # get conid by symbol
        conid = self$get_conid_by_symbol(symbol, sectype = "CFD", "NYSE")

        # get position
        self$logger$warn("Get position for %s", symbol)
        positions = self$get_position_by_conid(account_id, conid)
        if (length(positions) == 0) {
          position = 0
        } else {
          position = positions[[1]]$position
        }
        self$logger$info("Position for %s is %d", symbol, position)

        # get available cash
        self$logger$info("Available cash")
        portfolio_summary = self$get_portfolio_summary(account_id)
        cash = portfolio_summary$totalcashvalue$amount
        self$logger$info("Cash %f", cash)

        # order body
        self$logger$info("Create body for %s", symbol)
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
            self$logger$warn("No data in FMP cloud for %s", symbol)
            return("No data in FMP cloud.")
          }
          price = p[[1]]$price
          quantity = floor((cash * weight) / price)
          self$logger$info("Quantity for %s is %d",
                           symbol, quantity)

          # order body
          body = list(
            acctId = account_id,
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
        #     acctId = account_id,
        #     conid = as.integer(conid),
        #     sectype = paste0(conid, "@CFD"),
        #     orderType = "MKT",
        #     outsideRTH = FALSE,
        #     side = side,
        #     quantity = position,
        #     tif = tif
        #   )
        } else {
          self$logger$warn("We got sign but we don't buy or sell for %s ?",
                           symbol)
          return("We got sign but we don't buy or sell?")
        }

        # place order
        placed_order = self$place_and_confirm_order(account_id, body)
        self$logger$info("Places order for %s", symbol)
        self$logger$info(placed_order)

        # check order
        if (is.null(placed_order$order[[1]]$order_id)) {
          self$logger$warn("There is no order id in placed_order return object for %s", symbol)
          return(placed_order)
        } else {
          self$logger$info("Check status for %s", symbol)
          status = self$get_order_status_repeated(placed_order$order[[1]]$order_id, 60)
          if (is.integer(status) && status == 0) {
            # notify
            if (private$notify) {
              self$send_email("Order status not filled",
                              "Status is not filled",
                              html = FALSE)
            }
            self$logger$warn("Status is not filled for %s", symbol)
            return("Status is not filled")
          }
        }

        # notify
        if (private$notify) {
          self$logger$info("Notification - send email for %s", symbol)
          self$send_email("Order with Set Holdings",
                          self$order_result_to_html(status),
                          html = TRUE)
        }

        return(placed_order)
      },

      #' @description
      #' Liquidate all positions by symbol.
      #'
      #' @param account_id String, required, the account ID for which the order should be placed.
      #' @param sectype String, required, security type of the requested contract.
      #' @param symbol String, required, underlying symbol of interest or company name if ‘name’ is set to true.
      #'     If symbol is set to NULL, the method will liquidate all positions.
      #' @return It can return string if error or order info if everything is as expected.
      liquidate = function(account_id=NULL, sectype=NULL, symbol=NULL) {
        # debug
        # symbol = "SPY"
        # sectype = "CFD"

        # checks
        if (is.null(account_id)) account_id = self$account_id
        assert_true(self$check_account(account_id))
        assert_string(account_id)
        assert_string(symbol, null.ok = TRUE)
        assert_choice(sectype,
                      choices = c("STK", "CFD", "OPT", "CASH", "WAR", "FUT"),
                      null.ok = TRUE)

        # check if gateway is ready
        self$logger$info("Check gateway")
        if (!self$check_gateway()) {
          self$logger$warn("Check gateway didn't pass.")
          return("Check gateway didn't pass.")
        }

        # check if account id is in accounts
        self$logger$info("Check accounts")
        accounts = self$get_portfolio_accounts()
        accounts = unlist(lapply(accounts, `[`, "id"))
        if (!(account_id %in% accounts)) {
          self$logger$warn("account_id not in accounts.")
          return("account_id not in accounts.")
        }

        # get conid by symbol
        conid = self$get_conid_by_symbol(symbol, sectype = "CFD", "NYSE")

        # get position
        self$logger$info("Get position for %s", symbol)
        positions = self$get_position_by_conid(account_id, conid)
        if (length(positions) == 0) {
          position = 0
        } else {
          position = positions[[1]]$position
        }
        self$logger$info("Position for %s is %d", symbol, position)

        # Check if position is greater than 0
        if (position == 0) {
          return("Position is 0")
        } else if (position > 0) {
          side_ = "SELL"
        } else {
          side_ = "BUY"
        }

        # define body
        body = list(
          acctId = account_id,
          conid = as.integer(conid),
          sectype = paste0(conid, "@", sectype),
          orderType = "MKT",
          outsideRTH = FALSE,
          side = side_,
          quantity = abs(position),
          tif = "GTC"
        )

        # place order
        placed_order = self$place_and_confirm_order(account_id, body)

        # place order, confirm and heck status
        if (is.null(placed_order$order[[1]]$order_id)) {
          self$logger$info("There is no order id in placed_order return object")
          return(placed_order)
        } else {
          self$logger$info("Check status")
          status = self$get_order_status_repeated(placed_order$order[[1]]$order_id, 60)
          if (is.integer(status) && status == 0) {
            # notify
            if (private$notify) {
              self$send_email("Order status not filled",
                              "Status is not filled",
                              html = FALSE)
            }
            return("Status is not filled")
          }
        }

        # notify
        if (private$notify) {
          print("Notification - send email")
          self$send_email("Order with Liquidate",
                          self$order_result_to_html(status),
                          html = TRUE)
        }
        return(placed_order)
      },

      #' @description
      #' Liquidate all positions for all symbols in the portfolio.
      #'
      #' @param account_id String, required, the account ID for which the order should be placed.
      #' @param sectype Character vector, required, security type we want to sell.
      #'     If it is NULL, the method will liquidate all positions.
      #' @return Boolean.
      liquidate_portfolio = function(account_id=NULL, sectype = NULL) {
        # DEBUG
        # account_id = "DU6474915"
        # sectype = NULL

        # checks
        if (is.null(account_id)) account_id = self$account_id
        assert_true(self$check_account(account_id))
        assert_string(account_id)
        assert_choice(sectype,
                      choices = c("STK", "CFD", "OPT", "CASH", "WAR", "FUT"),
                      null.ok = TRUE)

        # get all positions
        positions = self$get_account_positions(account_id, 0)
        if (length(positions) == 0) {
          self$logger$warn("No positions for the account %s", account_id)
          return(0L)
        }
        positions = lapply(positions, as.data.table)
        positions = rbindlist(positions, fill = TRUE)
        positions = positions[, .(acctId, conid, ticker, assetClass)]
        positions = unique(positions)

        # checks
        if (!check_true(positions[, account_id %in% unique(acctId)])) {
          self$logger$warn("account_id not in accounts.")
          return(0L)
        }
        if (!is.null(sectype)) {
          if (!test_character(intersect(positions[, unique(assetClass)], sectype),
                               min.len = 1)) {
            self$logger$warn("Sectype not in positions.")
            return(0L)
          }
        }

        # liquidate all symbols
        tickers_ = positions[, ticker]
        assets_ = positions[, assetClass]
        for (i in seq_along(tickers_)) {
          self$logger$info("Liquidate %s %s", tickers_[i], assets_[i])
          self$liquidate(account_id, assets_[i], tickers_[i])
        }

        self$logger$info("Liquidate portfolio finished")
        return(1L)
      }
    ),
    private = list(
      notify = NULL
    )
  )

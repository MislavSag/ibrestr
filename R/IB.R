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

    #' Generic GET request method
    #'
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

    #' Generic POST request method
    #'
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

    #' Get all contract identifiers by exchange
    #'
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

    #' Search Contract by Symbol
    #'
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

    #' Get Security Definition Information by Contract Identifier
    #'
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

    #' Get Trading Schedule by Symbol
    #'
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
    #' Get unadjusted market data.
    #'
    #' @param conid Contract ID.
    #' @param exchange Exchange.
    #' @param period Period.
    #' @param bar Bar.
    #' @param outsideRth Data outside trading hours.
    #' @param keep_nytime_10_16 If TRUE, timezone is changed to NY time and only
    #'     trading hours kept.
    #'
    #' @references \url{https://www.interactivebrokers.com/api/doc.html#tag/Market-Data/paths/~1iserver~1marketdata~1history/get}
    #' @return Data table with unadjusted market data.
    get_unadjusted_market = function(conid,
                                     exchange = NULL,
                                     period = "5d",
                                     bar = "1h",
                                     outsideRth = TRUE,
                                     keep_nytime_10_16 = TRUE) {

      # send GET request fro market data
      # print("Get unadjusted data from IB...")
      md <- self$ib_get(modify_url(self$baseurl, path = "v1/api/iserver/marketdata/history"),
                        list(conid = conid,
                             exchange = exchange,
                             period = period,
                             bar = bar,
                             outsideRth = outsideRth))
      md <- rbindlist(md$data)

      # convert timezone to New york time and keep trading hours
      if (keep_nytime_10_16) {
        # print("Change timezone to NY time.")
        # change timesone to NY
        # print(class(md$t))
        # print(md$t)
        md$t <- as.numeric(md$t)
        md[, datetime := as.POSIXct(t / 1000,
                                    origin = "1970-01-01",
                                    tz = Sys.timezone())]
        # print("Debug")
        attr(md$datetime, "tzone") <- "America/New_York"

        # keep trading hours
        # print("Keep trading hours.")
        md$datetime <- md$datetime + 60 * 60
        md <- md[format.POSIXct(datetime, format = "%H:%M:%S") %between% c("09:30:00", "16:00:00")]
      }

      return(md)
    },

    #' @description
    #' Get portfolio positions.
    #'
    #' @param account_id Account ID.
    #' @param con_id Contract id.
    #'
    #' @return list object with info on positions.
    get_position = function(account_id, con_id) {
      url <- paste0(modify_url(self$baseurl, path = "/v1/api/portfolio/"),
                    account_id,
                    "/position/",
                    con_id)
      positions <- self$ib_get(url)
      return(positions)
    },

    #' Place Order
    #'
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

    #' Confirm Order Reply
    #'
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

    #' Get Order Status
    #'
    #' Retrieves the status of an individual order using the orderId.
    #'
    #' @param orderId String, required, order identifier for the placed order.
    #' @return A list containing details of the order status.
    get_order_status = function(orderId) {
      endpoint <- sprintf("/iserver/account/order/status/%s", orderId)
      self$get(endpoint)
    },

    #' Place and Confirm Order
    #'
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

    #' Get Historical Market Data
    #'
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
    get_historical_market_data = function(conid, period, bar, outsideRth = NULL,
                                          startTime = NULL, direction = NULL,
                                          barType = NULL) {
      query <- list(conid = conid, period = period, bar = bar,
                    outsideRth = outsideRth, startTime = startTime,
                    direction = direction, barType = barType)
      query <- query[!sapply(query, is.null)]
      self$get("/hmds/history", query = query)
    },


    #' #' @description
    #' #' Place order.
    #' #'
    #' #' @param account_id Account ID.
    #' #' @param order_body Body of POST request which place order.
    #' #'
    #' #' @references \url{https://www.interactivebrokers.com/api/doc.html#tag/Order/paths/~1iserver~1account~1\%7Bacc€ountId\%7D~1orders/post}
    #' #' @return list object with info on positions.
    #' place_order = function(account_id, order_body) {
    #'
    #'   # order = function(contractid, side, quantity, coid) {
    #'   #   # debug
    #'   #   # contractid = CONTRACT_ID_BIL
    #'   #   # side = "SELL"
    #'   #   # quantity = bil_position
    #'   #   # coid = paste0(email_prefix, "Sell BIL CFD MANUALLY at ", Sys.time())
    #'   #
    #'   #   # ping or init sessions
    #'   #   print("Init sessions")
    #'   #   ib$ib_get()
    #'   #   ib$ib_post()
    #'   #   ib$ib_get(paste0(IB_URL, "/v1/api/iserver/accounts"))
    #'   #   ib$get_position(ACCOUNT_ID, CONTRACT_ID)
    #'   #   ib$get_portfolio_summary(ACCOUNT_ID)
    #'   #   Sys.sleep(1L)
    #'   #
    #'   #   # order body
    #'   #   print("Create body")
    #'   #   body = list(
    #'   #     acctId = ACCOUNT_ID,
    #'   #     conid = as.integer(contractid),
    #'   #     sectype = paste0(contractid, ":CFD"),
    #'   #     orderType = "MKT",
    #'   #     outsideRTH = FALSE,
    #'   #     side = side,
    #'   #     quantity = as.integer(quantity),
    #'   #     tif = "GTC",
    #'   #     cOID = ids::random_id()
    #'   #   )
    #'   #
    #'   #   # place orders order
    #'   #   print("Place order")
    #'   #   url <- paste0(modify_url(ib$baseurl, path = "/v1/api/iserver/account/"),
    #'   #                 ACCOUNT_ID, "/orders")
    #'   #   print(url)
    #'   #   body_json = toJSON(list(orders = list(body)), auto_unbox = TRUE)
    #'   #   print(body_json)
    #'   #
    #'   #   # POST(
    #'   #   #   url,
    #'   #   #   config = httr::config(ssl_verifypeer = FALSE,
    #'   #   #                         ssl_verifyhost = FALSE),
    #'   #   #   body = body_json,
    #'   #   #   add_headers(`User-Agent` = "Console", `content-type` = "application/json"), encode = "json"
    #'   #   # )
    #'   #
    #'   #   order_message <- ib$ib_post(url, body = body_json)
    #'   #   Sys.sleep(1L)
    #'   #   print("Reply")
    #'   #   print(order_message)
    #'   #   url <- paste0(modify_url(ib$baseurl, path = "/v1/api/iserver/reply/"),
    #'   #                 order_message[[1]]$id)
    #'   #   confirmed <- ib$ib_post(url, body = toJSON(list(confirmed = TRUE), auto_unbox = TRUE))
    #'   #   order_info = list(order_message = order_message, confirmed = confirmed)
    #'   #   # order_info = ib$buy_and_confirm(ACCOUNT_ID, body)
    #'   #   Sys.sleep(1L)
    #'   #
    #'   #   # check order status and wait for 5 minute if it will be executed
    #'   #   print("Check oders status")
    #'   #   n_trails = 40
    #'   #   repeat {
    #'   #     # stop if order executes or number of trias grater than 10
    #'   #     if (n_trails <= 0) {
    #'   #       send_email(sub = paste0(EMAIL_SUB, " - order problem"),
    #'   #                  message = "Order is not filled. Check why.")
    #'   #       break()
    #'   #     }
    #'   #
    #'   #     # get order status
    #'   #     url = paste0(
    #'   #       "https://",
    #'   #       parsed_iburl$hostname,
    #'   #       ":",
    #'   #       as.integer(parsed_iburl$port),
    #'   #       "/v1/api/iserver/account/orders"
    #'   #     )
    #'   #     orders <- ib$ib_get(url)
    #'   #     if (length(orders$orders) == 0) {
    #'   #       Sys.sleep(1L)
    #'   #       n_trails = n_trails - 1
    #'   #       next()
    #'   #     } else {
    #'   #       last_ = lapply(orders$orders, '[', "lastExecutionTime")
    #'   #       last_ = as.POSIXct(unlist(last_), format = "%y%m%d%H%M%S")
    #'   #       orders_last = orders$orders[which.max(last_)]
    #'   #       order_status = orders_last[[1]]$status
    #'   #
    #'   #       # check order status
    #'   #       if (order_status == "Filled") {
    #'   #         # send e-mail notification
    #'   #         confirmed = order_info$confirmed
    #'   #         if (length(confirmed) == 0) {
    #'   #           send_email(sub = EMAIL_SUB,
    #'   #                      message = "Order is not confirmed")
    #'   #         } else {
    #'   #           # Convert the list to an HTML formatted string
    #'   #           my_list = orders_last[[1]]
    #'   #           html_content <- "<html><body><h2>Order Details</h2><ul>"
    #'   #           for (name in names(my_list)) {
    #'   #             html_content <- paste0(html_content, "<li><b>", name, ":</b> ", my_list[[name]], "</li>")
    #'   #           }
    #'   #           html_content <- paste0(html_content, "</ul></body></html>")
    #'   #
    #'   #           # Send email
    #'   #           send_email(
    #'   #             sub = EMAIL_SUB,
    #'   #             message = html_content,
    #'   #             html = TRUE
    #'   #           )
    #'   #         }
    #'   #         break
    #'   #       } else {
    #'   #         Sys.sleep(1L)
    #'   #         n_trails = n_trails - 1
    #'   #       }
    #'   #     }
    #'   #   }
    #'   #   return(1L)
    #'   #
    #'   #   # cancel order
    #'   #   # url = paste0("https://", parsed_iburl$hostname, ":", as.integer(parsed_iburl$port),
    #'   #   #              "/v1/api/iserver/account/", ACCOUNT_ID,
    #'   #   #              "/order/", orders$orders[[2]]$orderId)
    #'   #   # DELETE(url, config = httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
    #'   # }
    #'
    #'   # place order
    #'   url <- paste0(modify_url(self$baseurl, path = "/v1/api/iserver/account/"),
    #'                 account_id,
    #'                 "/orders")
    #'   body_json = toJSON(list(orders = list(order_body)), auto_unbox = TRUE)
    #'   order_message <- self$ib_post(url, body = body_json)
    #'
    #'   # wait for 1 sec for order to be sent. Probably not necessary
    #'   Sys.sleep(1L)
    #'
    #'   # confirm order
    #'   url <- paste0(modify_url(self$baseurl, path = "/v1/api/iserver/reply/"),
    #'                 order_message[[1]]$id)
    #'   confirmed <- self$ib_post(url,
    #'                             body = toJSON(list(confirmed = TRUE),
    #'                                           auto_unbox = TRUE))
    #'   return(list(order_message = order_message, confirmed = confirmed))
    #' },

    #' @description
    #' Cancel order.
    #'
    #' @param account_id Account ID.
    #' @param order_id Order id from Place orders endpoint result.
    #'
    #' @return list object with info on positions.
    cancel_order =  function(account_id, order_id) {
      url <- paste0("https://localhost:5000/v1/api/iserver/account/",
                    account_id,
                    "/order/",
                    order_id)
      p <- DELETE(url, config = httr::config(ssl_verifypeer = FALSE,  ssl_verifyhost = FALSE))
      return(content(p))
    },

    #' @description
    #' Portfolio summary.
    #'
    #' @param account_id Account ID.
    #'
    #' @return list object with info on podrtfolio summary.
    get_portfolio_summary = function(account_id) {
      url <- paste0(modify_url(self$baseurl, path = "v1/api/portfolio/"),
                    account_id,
                    "/summary")
      positions <- self$ib_get(url)
      return(positions)
    }

    #' #' @description
    #' #' Get contract id by symbol.
    #' #'
    #' #' @param account_id Account ID.
    #' #'
    #' #' @return list object with info on podrtfolio summary.
    #' get_portfolio_summary = function(account_id) {
    #'   url <- modify_url(self$baseurl, path = "v1/api/portfolio/"),
    #'                 account_id,
    #'                 "/summary"))
    #'   positions <- self$ib_get(url)
    #'   return(positions)
    #' }
  )
)


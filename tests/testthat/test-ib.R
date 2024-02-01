library(testthat)




# Instantiate the IB object
ib_instance <- IB$new(host = Sys.getenv("HOST"),
                      port = 5000,
                      strategy_name = "Paper test",
                      account_id = Sys.getenv("ACCOUNT_ID"),
                      email_config = list(
                        "email_from" = "mislav.sagovac@contentio.biz",
                        "email_to" = "mislav.sagovac@contentio.biz",
                        "smtp_host" = Sys.getenv("EMAILHOST"),
                        "smtp_port" = 587,
                        "smtp_user" = Sys.getenv("EMAILUSER"),
                        "smtp_password" = Sys.getenv("EMAILPASS")
                      ),
                      logger = NULL)


test_that("GET method returns valid response", {
  # Test for a valid endpoint
  response = ib_instance$get()
  expect_type(response, "list")
  expect_true(length(response) > 0)
  expect_true("USER_ID" %in% names(response))
})

test_that("set_holdings returns checks", {
  # Call the method with parameters that lead to a successful operation
  result = ib_instance$set_holdings(
    account_id = NULL,
    symbol = "SPY",
    sectype = "CFD",
    side = "BUY",
    tif = "GTC",
    weight = 0.01
  )

  # Check if the result is a list
  expect_type(result, "list")
  expect_true(length(result) > 0)
  expect_true("order" %in% names(result))

  # Check the structure of the 'order' element
  expect_type(result$order, "list")
  expect_true(length(result$order) > 0)

  # Check the first order in the list (if applicable)
  expect_type(result$order[[1]], "list")
  expect_true("order_id" %in% names(result$order[[1]]))
  expect_true("order_status" %in% names(result$order[[1]]))
  expect_true("encrypt_message" %in% names(result$order[[1]]))

  # Check the types and values of elements in the first order
  expect_type(result$order[[1]]$order_id, "character")
  expect_type(result$order[[1]]$order_status, "character")
  expect_type(result$order[[1]]$encrypt_message, "character")

  # Check the 'info' element
  expect_type(result$info, "character")
})

test_that("get_live_orders returns a list with the correct structure", {
  # Call the method, possibly with specific parameters to get a predictable result
  result = ib_instance$get_live_orders()

  # Check if the result is a list
  expect_type(result, "list")

  # Check the structure of the 'orders' element
  expect_true("orders" %in% names(result))
  expect_type(result$orders, "list")

  # check orders structure if there are liv orders
  if (length(result$orders) == 0) {
    print("test")
  } else {
    expect_true(length(result$orders) > 0)

    # Check the structure and content of the first order in the list
    first_order <- result$orders[[1]]
    expect_type(first_order, "list")
    expect_true(all(c("acct", "conidex", "conid", "account", "orderId", "cashCcy", "sizeAndFills",
                      "orderDesc", "description1", "description2", "ticker", "secType",
                      "remainingQuantity", "filledQuantity", "totalSize", "companyName", "status",
                      "order_ccp_status", "avgPrice", "origOrderType", "supportsTaxOpt",
                      "lastExecutionTime", "orderType", "bgColor", "fgColor", "timeInForce",
                      "lastExecutionTime_r", "side") %in% names(first_order)))

    # Check the 'snapshot' element
    expect_true("snapshot" %in% names(result))
    expect_type(result$snapshot, "logical")
  }


})

ib_instance$get_live_orders()

test_that("Testing get_conids_by_exchange", {
  ib_api = ibrestr::IB$new(host =  "cgspaperexuber.eastus.azurecontainer.io", port = 5000)

  # Mocking a response or using an actual response
  # For actual API calls, ensure you have a valid host and port
  # For mocking, you can use `httr::with_mock_api()` or similar approaches
  result <- ib_api$get_conids_by_exchange("AMEX")

  # Test the structure of the result
  expect_true(is.list(result))
  # Add more specific tests, e.g., checking for expected fields in the result
})

test_that("liquidate method returns correct structure", {
  # Test when there is a position to liquidate
  result = ib_instance$liquidate(account_id = NULL,
                                 sectype = "CFD",
                                 symbol = "SPY")

  # Check if the result is a list and has at least one element
  expect_type(result, "list")
  expect_true(length(result) > 0)

  # Check the structure and content of the first item in the list
  first_order = result[[1]]
  expect_type(first_order, "list")
  expect_true(all(c("order_id", "order_status", "encrypt_message") %in% names(first_order[[1]])))

  # Check the types and values of elements in the first order
  expect_type(first_order[[1]]$order_id, "character")
  expect_type(first_order[[1]]$order_status, "character")
  expect_type(first_order[[1]]$encrypt_message, "character")
})

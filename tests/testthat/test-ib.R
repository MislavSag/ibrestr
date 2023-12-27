library(testthat)
library(httr)


test_that("Testing get_conids_by_exchange", {
  ib_api <- ibrestr::IB$new(host =  "cgspaperexuber.eastus.azurecontainer.io", port = 5000)

  # Mocking a response or using an actual response
  # For actual API calls, ensure you have a valid host and port
  # For mocking, you can use `httr::with_mock_api()` or similar approaches
  result <- ib_api$get_conids_by_exchange("AMEX")

  # Test the structure of the result
  expect_true(is.list(result))
  # Add more specific tests, e.g., checking for expected fields in the result
})

# test_that("Testing getSecDefInfo", {
#   ib_api <- yourpackage::IBApi$new(host = "localhost", port = 5000)
#
#   # Similar structure as above
#   result <- ib_api$getSecDefInfo(conid = "265598", sectype = "OPT", month = "JAN24", strike = "195", right = "P")
#
#   expect_true(is.list(result))
#   # Additional specific tests
# })
#
# test_that("Testing getTradingSchedule", {
#   ib_api <- yourpackage::IBApi$new(host = "localhost", port = 5000)
#
#   result <- ib_api$getTradingSchedule(assetClass = "STK", symbol = "AAPL", exchange = "ISLAND", exchangeFilter = "ISLAND,NYSE,AMEX")
#
#   expect_true(is.list(result))
#   # Additional specific tests
# })
#
# test_that("Testing searchContractBySymbol", {
#   ib_api <- yourpackage::IBApi$new(host = "localhost", port = 5000)
#
#   result <- ib_api$searchContractBySymbol(symbol = "Interactive Brokers", name = TRUE)
#
#   expect_true(is.list(result))
#   # Additional specific tests
# })
#
# # Run the tests
# test_dir("tests/testthat")
#

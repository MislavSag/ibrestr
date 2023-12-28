#' @title Utils Class
#'
#' @description
#' Utils method to work with IB data.
#'
#' @export
Utils = R6::R6Class(
  "Utils",

  public = list(
    #' @description
    #' Performs a GET request to the specified Interactive Brokers API endpoint.
    #'
    #' @param order Filled order response
    #' @return The response from the GET request.
    order_result_to_html = function(order) {
      html_content <- "<html><body><h2>Order Details</h2><ul>"
      for (name in names(order)) {
        html_content <- paste0(html_content, "<li><b>", name, ":</b> ", my_list[[name]], "</li>")
      }
      html_content = paste0(html_content, "</ul></body></html>")
      html_content
    }
  )
)

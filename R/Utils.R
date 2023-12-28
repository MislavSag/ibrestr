#' @title Utils Class
#'
#' @include IB.R
#'
#' @description
#' Utils method to work with IB data.
#'
#' @export
Utils = R6::R6Class(
  "Utils",
  inherit = IB,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param host Host, by default localhost
    #' @param port Port, by default 5000
    #'
    #' @return A new `Utils` object.
    initialize = function(host, port) {
      super$initialize(host = host, port = port)
    },

    #' @description
    #' Performs a GET request to the specified Interactive Brokers API endpoint.
    #'
    #' @param order Filled order response
    #' @return Html character.
    order_result_to_html = function(order) {
      html_content <- "<html><body><h2>Order Details</h2><ul>"
      for (name in names(order)) {
        html_content <- paste0(html_content, "<li><b>", name, ":</b> ", my_list[[name]], "</li>")
      }
      html_content = paste0(html_content, "</ul></body></html>")
      html_content
    },

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
      return(TRUE)
    }
  )
)

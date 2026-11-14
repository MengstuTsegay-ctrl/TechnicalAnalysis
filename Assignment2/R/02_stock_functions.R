
# BDA400 Assignment 2
# Student: Mengstu Tsegay
# Stock Data Import Function

library(quantmod)

load_stock_data <- function(
    portfolio_file = "portfolio.txt",
    start_date = Sys.Date() - 365,
    end_date = Sys.Date()) {

  # Read stock symbols from portfolio.txt
  symbols <- trimws(readLines(portfolio_file))
  symbols <- unique(symbols[nzchar(symbols)])

  if (length(symbols) == 0) {
    stop("No stock symbols found.")
  }

  stock_data <- list()

  # Download historical stock prices
  for (symbol in symbols) {

    cat("Downloading:", symbol, "\n")

    stock <- tryCatch(
      getSymbols(
        symbol,
        src = "yahoo",
        from = start_date,
        to = end_date,
        auto.assign = FALSE
      ),
      error = function(e) {
        warning(paste("Failed:", symbol, e$message))
        NULL
      }
    )

    if (!is.null(stock)) {
      stock_data[[symbol]] <- data.frame(
        Date = as.Date(index(stock)),
        coredata(stock),
        check.names = FALSE
      )
    }
  }

  if (length(stock_data) == 0) {
    stop("No stock data could be downloaded.")
  }

  return(stock_data)
}

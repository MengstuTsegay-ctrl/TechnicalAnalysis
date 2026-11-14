
# BDA400 Assignment 2
# Student: Mengstu Tsegay
# Display Imported Stock Data and Statistics

# Display data for every stock in the portfolio
display_stock_data <- function(stock_data,
                               summary_data,
                               rows_to_show = 5) {
  
  for (symbol in names(stock_data)) {
    
    cat("\n====================================\n")
    cat("STOCK SYMBOL:", symbol, "\n")
    cat("====================================\n")
    
    data <- stock_data[[symbol]]
    
    # Display dataset information
    cat("Total observations:", nrow(data), "\n")
    cat("Date range:",
        as.character(min(data$Date)), "to",
        as.character(max(data$Date)), "\n\n")
    
    # Display first observations
    cat("FIRST", rows_to_show, "ROWS:\n")
    print(head(data, rows_to_show))
    
    # Display latest observations
    cat("\nLATEST", rows_to_show, "ROWS:\n")
    print(tail(data, rows_to_show))
    
    # Display calculated statistics
    cat("\nCALCULATED STATISTICS:\n")
    
    stock_stats <- summary_data[
      summary_data$Symbol == symbol, ,
      drop = FALSE
    ]
    
    print(stock_stats, row.names = FALSE)
    cat("\n")
  }
  
  invisible(NULL)
}

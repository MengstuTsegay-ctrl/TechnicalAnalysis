
# BDA400 Assignment 2
# Student: Mengstu Tsegay
# Stock Price Statistical Analysis

library(TTR)

# Calculate the statistical mode of a numeric vector
numeric_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  frequency <- table(x)
  if (max(frequency) == 1) return(NA_real_)
  as.numeric(names(frequency)[which.max(frequency)])
}

# Calculate summary statistics for each stock
calculate_stock_statistics <- function(stock_data) {
  
  results <- lapply(names(stock_data), function(symbol) {
    
    data <- stock_data[[symbol]]
    
    # Extract the closing price column
    close_column <- grep(
      "\\.Close$",
      names(data),
      value = TRUE
    )
    
    if (length(close_column) != 1) {
      stop(paste("Closing price column missing:", symbol))
    }
    
    prices <- as.numeric(data[[close_column]])
    
    sma20 <- SMA(prices, n = 20)
    
    data.frame(
      Symbol = symbol,
      Observations = sum(!is.na(prices)),
      Mean = mean(prices, na.rm = TRUE),
      Median = median(prices, na.rm = TRUE),
      Mode = numeric_mode(prices),
      Standard_Deviation = sd(prices, na.rm = TRUE),
      Latest_Close = tail(na.omit(prices), 1),
      Latest_SMA20 = tail(na.omit(sma20), 1)
    )
  })
  
  do.call(rbind, results)
}

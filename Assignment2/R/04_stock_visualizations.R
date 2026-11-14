
# BDA400 Assignment 2
# Student: Mengstu Tsegay
# Stock Price Visualizations

library(TTR)

# Create an output folder
dir.create("outputs", showWarnings = FALSE)

# Chart 1: AAPL closing price and 20-day SMA
aapl <- stocks[["AAPL"]]

close_col <- grep("\\.Close$", names(aapl), value = TRUE)
aapl_close <- as.numeric(aapl[[close_col]])
aapl_sma20 <- SMA(aapl_close, n = 20)

png("outputs/AAPL_price_SMA20.png",
    width = 1200, height = 700)

plot(aapl$Date, aapl_close,
     type = "l",
     col = "steelblue",
     lwd = 2,
     main = "AAPL Closing Price and 20-Day Moving Average",
     xlab = "Date",
     ylab = "Price (USD)")

lines(aapl$Date, aapl_sma20,
      col = "firebrick",
      lwd = 2)

legend("topleft",
       legend = c("Closing Price", "20-Day SMA"),
       col = c("steelblue", "firebrick"),
       lty = 1, lwd = 2)

dev.off()

# Chart 2: Average closing price by stock
png("outputs/mean_stock_prices.png",
    width = 1200, height = 700)

barplot(stock_summary$Mean,
        names.arg = stock_summary$Symbol,
        col = "steelblue",
        main = "Mean Closing Price by Stock",
        xlab = "Stock Symbol",
        ylab = "Mean Closing Price")

dev.off()

# Save statistical results
write.csv(stock_summary,
          "outputs/stock_summary.csv",
          row.names = FALSE)

message("Charts and summary CSV saved in outputs folder.")

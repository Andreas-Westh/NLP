library(quantmod)

# Set date range (optional)
start_date <- as.Date("2019-01-01")
end_date <- Sys.Date()

# template for making into df
# PNDORA.CO <- data.frame(Date = index(PNDORA.CO), coredata(PNDORA.CO))

# Get stock data
getSymbols("NVO", src = "yahoo", from = start_date, to = end_date)         # Novo Nordisk (listed on NYSE)
getSymbols("PNDORA.CO", src = "yahoo", from = start_date, to = end_date)   # Pandora (listed on Copenhagen Stock Exchange)

# Plot closing prices
chartSeries(NVO, theme = chartTheme("black"), name = "Novo Nordisk")
chartSeries(PNDORA.CO, theme = chartTheme("black"), name = "Pandora")





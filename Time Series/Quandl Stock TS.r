## This function takes a Quandl code and generates a time series of closing stock prices from January 1, 2005 ##
## to the present. This function requires the use of the Quandl and ggplot2 packages                          ##
## Example: time_series("GOOG/NASDAQ_GOOGL")

time_series = function(symbol)
{
  # Download data set from Quandl
  data.series <- Quandl(symbol, start_date = "2005-01-01")[ , c(1, 5)] 
  
  # Apply initial layer using ggplot2
  my.plot <- ggplot(data = data.series, aes(x = Date, y = Close)) + geom_line(color = "#FAB521")
  
  # Apply theme for layer and specify element colors
  series.theme <- theme(panel.background = element_rect(fill = "#393939"), panel.grid.major.x = element_blank(), 
                      panel.grid.major.y = element_line(colour = "white", size = 0.1), 
                      panel.grid.minor = element_line(colour = "white", size = 0.1))
                     
  
  my.plot + series.theme +xlab("Date") + ylab("Closing Price") + ggtitle(symbol)
}

# Competitive Intelligence: Spirit Airlines Financial Dossier (R)

# install required packages

# bring packages into the workspace
library(RCurl)  # functions for gathering data from the web
library(XML)  # XML and HTML parsing
library(quantmod) # use for gathering and charting economic data
# online documentation for quantmod at <http://www.quantmod.com/> 
library(Quandl)  # extensive financial data online
# online documentation for Quandl at <http://www.quandl.com/> 
library(lubridate) # date functions
library(zoo)  # utilities for working with time series
library(xts)  # utilities for working with time series
library(ggplot2)  # data visualization

# ----------------------------------
# Text data acquisition and storage
# ----------------------------------
# get current working directory (commands for Mac OS or Linux)
cwd <- getwd()
# create directory for storing competitive intelligence data
ciwd <- paste(cwd, "/ci_data/", sep ="")
dir.create(ciwd)

# gather Wikipedia data using RCurl package
wikipedia_web_page <- 
    getURLContent('http://en.wikipedia.org/wiki/Spirit_Airlines')
# store data in directory ciwd for future processing    
sink(paste(ciwd, "/", "wikipedia_web_page", sep = "")) 
wikipedia_web_page
sink()

# similar procedures may be used for all acquired data
# for the Spirit Airlines competitive intelligence study
# use distinct file names to identify the data sources

# ---------------------------------------------------------------
# Yahoo! Finance for Spirit Airlines (NASDAQ stock symbol: SAVE)
# ---------------------------------------------------------------
# stock symbols for companies can be obtained from Yahoo! Finance
# <http://finance.yahoo.com/lookup>

# get Spirit Airlines stock price data
getSymbols("SAVE", return.class = "xts", src = "yahoo")
print(str(SAVE)) # show the structure of this xtx time series object
# plot the series stock price
chartSeries(SAVE,theme="white")

# examine the structure of the R data object
print(str(SAVE))
print(SAVE)

# convert character string row names to decimal date for the year
Year <- decimal_date(ymd(row.names(as.data.frame(SAVE))))
# Obtain the closing price of Spirit Airlines stock
Price <- as.numeric(SAVE$SAVE.Close)
# create data frame for Spirit Airlines Year and Price for future plots
SPIRIT_data_frame <- data.frame(Year, Price)

# similar procedures may be used for all airline competitors

# ---------------------------------------------------------------
# Google! Finance for Spirit Airlines (NASDAQ stock symbol: SAVE)
#   (basically the same data as from Yahoo! Finance)
# ---------------------------------------------------------------

# get Spirit Airlines stock price data
getSymbols("SAVE", return.class = "xts", src = "google")
print(str(SAVE)) # show the structure of this xtx time series object
# plot the series stock price
chartSeries(SAVE,theme="white")
# examine the structure of the R data object
print(str(SAVE))
print(SAVE)

# -----------------------------------------
# ggplot2 time series plotting, closing 
# price of Spirit Airlines common stock
# -----------------------------------------
# with a data frame object in hand... we can go on to use ggplot2 
# and methods described in Chang (2013) R Graphics Cookbook

# use data frame defined from Yahoo! Finance
# the Spirit Airlines closing price per share SPIRIT_data_frame
# now we use that data structure to prepare a time series plot
# again, let's highlight the Great Recession on our plot for this time series
plotting_object <- ggplot(SPIRIT_data_frame, aes(x = Year, y = Price)) +
    geom_line() +
    ylab("Stock Price (dollars/share)") +
    ggtitle("Spirit Airlines Stock Price")
print(plotting_object)    

# send the plot to an external file (sans title, larger axis labels)
pdf("fig_competitive_intelligence_spirit.pdf", width = 11, height = 8.5)
plotting_object <- ggplot(SPIRIT_data_frame, aes(x = Year, y = Price)) + 
    geom_line() + ylab("Stock Price (dollars/share)") +
    theme(axis.title.y = element_text(size = 20, colour = "black")) +
    theme(axis.title.x = element_text(size = 20, colour = "black")) 
print(plotting_object)
dev.off()

# ------------------------------------------
# FRED for acquiring general financial data 
# ------------------------------------------

# general financial data may be useful in understanding what is
# happening with a company over time... here is how to get those data

# demonstration of R access to and display of financial data from FRED
# requires a connection to the Internet
# ecomonic research data from the Federal Reserve Bank of St. Louis 
# see documentation of tags at http://research.stlouisfed.org/fred2/
# choose a particular series and click on it, a graph will be displayed
# in parentheses in the title of the graph will be the symbol 
# for the financial series some time series are quarterly, some monthly
# ... others weekly... so make sure the time series match up in time
# see the documentation for quantmod at
# <http://cran.r-project.org/web/packages/quantmod/quantmod.pdf>

# here we show how to download the Consumer Price Index
# for All Urban Consumers: All Items, Not Seasonally Adjusted, Monthly
getSymbols("CPIAUCNS",src="FRED",return.class = "xts")
print(str(CPIAUCNS)) # show the structure of this xtx time series object
# plot the series
chartSeries(CPIAUCNS,theme="white")

# Real Gross National Product in 2005 dollars
getSymbols("GNPC96",src="FRED",return.class = "xts")
print(str(GNPC96)) # show the structure of this xtx time series object
# plot the series
chartSeries(GNPC96,theme="white")

# National Civilian Unemployment Rate, 
#    not seasonally adjusted (monthly, percentage)
getSymbols("UNRATENSA",src="FRED",return.class = "xts")
print(str(UNRATENSA)) # show the structure of this xtx time series object
# plot the series
chartSeries(UNRATENSA,theme="white")

# University of Michigan: Consumer Sentiment, 
#    not seasonally adjusted (monthly, 1966 = 100)
getSymbols("UMCSENT",src="FRED",return.class = "xts")
print(str(UMCSENT)) # show the structure of this xtx time series object
# plot the series
chartSeries(UMCSENT,theme="white")

# New Homes Sold in the US, not seasonally adjusted (monthly, thousands)
getSymbols("HSN1FNSA",src="FRED",return.class = "xts")
print(str(HSN1FNSA)) # show the structure of this xtx time series object
# plot the series
chartSeries(HSN1FNSA,theme="white")

# ---------------------------------------
# Multiple time series plots 
# ---------------------------------------
# let's try putting consumer sentiment and new home sales on the same plot

# University of Michigan Index of Consumer Sentiment (1Q 1966 = 100)
getSymbols("UMCSENT", src="FRED", return.class = "xts")
ICS <- UMCSENT # use simple name for xts object
dimnames(ICS)[2] <- "ICS" # use simple name for index
chartSeries(ICS, theme="white")
ICS_data_frame <- as.data.frame(ICS)
ICS_data_frame$date <- ymd(rownames(ICS_data_frame))
ICS_time_series <- ts(ICS_data_frame$ICS, 
  start = c(year(min(ICS_data_frame$date)), month(min(ICS_data_frame$date))),
  end = c(year(max(ICS_data_frame$date)),month(max(ICS_data_frame$date))),
  frequency=12)
  
# New Homes Sold in the US, not seasonally adjusted (monthly, millions)
getSymbols("HSN1FNSA",src="FRED",return.class = "xts")
NHS <- HSN1FNSA
dimnames(NHS)[2] <- "NHS" # use simple name for index
chartSeries(NHS, theme="white")
NHS_data_frame <- as.data.frame(NHS)
NHS_data_frame$date <- ymd(rownames(NHS_data_frame))
NHS_time_series <- ts(NHS_data_frame$NHS, 
  start = c(year(min(NHS_data_frame$date)),month(min(NHS_data_frame$date))),
  end = c(year(max(NHS_data_frame$date)),month(max(NHS_data_frame$date))),
  frequency=12)
  
# define multiple time series object
economic_mts <- cbind(ICS_time_series,
  NHS_time_series) 
  dimnames(economic_mts)[[2]] <- c("ICS","NHS") # keep simple names 
modeling_mts <- na.omit(economic_mts) # keep overlapping time intervals only 

# examine the structure of the multiple time series object
# note that this is not a data frame object
print(str(modeling_mts))

# -----------------------------------------
# Prepare data frame for ggplot2 work
# -----------------------------------------
# for zoo examples see vignette at
# <http://cran.r-project.org/web/packages/zoo/vignettes/zoo-quickref.pdf>
modeling_data_frame <- as.data.frame(modeling_mts)
modeling_data_frame$Year <- as.numeric(time(modeling_mts))

# examine the structure of the data frame object
# notice an intentional shift to underline in the data frame name
# this is just to make sure we keep our object names distinct
# also you will note that programming practice for database work
# and for work with Python is to utilize underlines in variable names
# so it is a good idea to use underlines generally
print(str(modeling_data_frame)) 
print(head(modeling_data_frame))

# ----------------------------------------------
# ggplot2 time series plotting of economic data
# ----------------------------------------------
library(ggplot2)

# according to the National Bureau of Economic Research the 
# Great Recessionextended from December 2007 to June 2009
# using our Year variable this would be from 2007.917 to 2009.417
# let's highlight the Great Recession on our plot
plotting_object <- ggplot(modeling_data_frame, aes(x = Year, y = ICS)) +
    geom_line() +
    annotate("rect", xmin = 2007.917, xmax = 2009.417, 
        ymin = min(modeling_data_frame$ICS), 
        ymax = max(modeling_data_frame$ICS), 
        alpha = 0.3, fill = "red") +
    ylab("Index of Consumer Sentiment") +
    ggtitle("Great Recession and Consumer Sentiment")
print(plotting_object)    

# ---------------------------------------------------------------
# Quandl for Spirit Airlines (NASDAQ stock symbol: SAVE)
# obtain more extensive financial data for Spirit Airlines 
# more documentation at http://blog.quandl.com/blog/using-quandl-in-r/
# ---------------------------------------------------------------
Spirit_Price <- Quandl("GOOG/NASDAQ_SAVE", collapse="monthly", type="ts")
plot(stl(Spirit_Price[,4],s.window="periodic"), 
    main = "Time Series Decomposition for Spirit Airlines Stock Price")

# Suggestions for the student: Employ search and crawling code to access
# all of the competitive intelligence reports cited in the chapter.
# Save these data in the directory, building a text corpus for further study.
# Scrape and parse the text documents using XPath and regular expressions.
# Obtain stock price series for all the major airlines and compare those
# series with Spirit's. See if there are identifiable patterns in these
# data and if those patterns in any way correspond to economic conditions.
# (Note that putting the economic/monthly and stock price/daily data
#   into the same analysis will require a periodicity change using the
#   xts/zoo data for Spirit Airlines. Refer to documentation at
#   <http://www.quantmod.com/Rmetrics2008/quantmod2008.pdf>
#   or from Quandl at <http://blog.quandl.com/blog/using-quandl-in-r/>.)
# Conduct a study of ticket prices on a round trips between two cities
# (non-stop flights between Los Angeles and Chicago, perhaps).
# Crawl and scrape the pricing data to create a pricing database across
# alternative dates into the future, taking note of the day and time 
# of each flight and the airline supplying the service. Develop a
# competitive pricing model for airline travel between these cities.
# Select one of the competitive airlines as your client, and report
# the results of your competitive intelligence and competitive pricing
# research. Make strategic recommendations about what to do about
# the low-fare/amenities pricing approach of Spirit Airlines.
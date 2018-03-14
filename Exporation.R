library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(plotly)
library(ggplot2)  #using CRAN's, not GitHub's
library(ggthemes)


## Read in as Tibble, convert to Date type
RawStockData <- tbl_df(fread("Dated_stocks-us-adjClose.csv", fill=T))  ## Tibble.  Fill for uneven row length; it's not filling NA's
RawStockData$ObsDate <- as.Date(RawStockData$ObsDate, format = '%Y-%m-%d')


## Take a look
class(RawStockData)
head(RawStockData)


##  ~ 12k rows by 711 cols.  Over 5/8 of values are NA
dim(RawStockData)
mean(is.na(RawStockData)) 


### What does the distribution of NAs look like
options(scipen = 10)
SoManyNAs <- as.data.frame(sapply(RawStockData, function(x) round(sum(is.na(x))/length(x), 2) ))
if (dim(SoManyNAs)[2] == 1){
  setDT(SoManyNAs, keep.rownames = T)
  colnames(SoManyNAs) <- c("Ticker", "Count")
}
SoManyNAs <- SoManyNAs[SoManyNAs$Ticker != 'ObsDate', ]
p <-ggplot(SoManyNAs, aes(SoManyNAs$Count)) + 
  geom_histogram(breaks=seq(0, 1, .01)) + 
  labs(x="Percent NA by Stock", y="# of Stocks at Given Percentile of NAs") + 
  ggtitle("Distribution of NA/NULL Values in Dataset") +
  theme_stata()
ggplotly(p)
#ggplot(SoManyNAs, aes(SoManyNAs$count)) + stat_ecdf(geom = "step")


## Initial reshape: melt/gather
StockData <- gather(RawStockData, key=Ticker, value=Close, names(RawStockData)[-1]) %>%
  arrange(Ticker, ObsDate)


## check out DF size
sl <- object.size(StockData)
print(sl, units='auto')


## Convert tickers to factors; reduces given DF size by ~32 Mb
StockData$Ticker <- as.factor(StockData$Ticker)
sl <- object.size(StockData)
print(sl, units='auto')


## Quick data validation check to make sure we only have Mon - Fri
table(weekdays(StockData$ObsDate))


## NA's were so extensive, it seems inadvisable to fill, impute, or interpolate for them.
## TODO:  figure out why na.omit increases object size ???
StockData <- StockData %>% na.omit() 

sl <- object.size(StockData)
print(sl, units='auto')


## Get a distribution of records for all Tickers across Years.
RecordsPerYearAndStock <-
StockData %>%
  mutate(ObsYear = year(ObsDate)) %>%
  group_by(ObsYear, Ticker) %>%
  add_count() %>%
  rename(RecPerTickerPerYear=n) %>%
  select(ObsYear, Ticker, RecPerTickerPerYear) %>%
  distinct()


## Get a count of Tickers per Year.
RecordsPerYear <-
RecordsPerYearAndStock %>%
  ungroup() %>%
  select(-Ticker) %>%
  group_by(ObsYear) %>%
  mutate(StocksPerYear = n()) %>%
  select(-RecPerTickerPerYear) %>%
  distinct() %>%
  arrange(ObsYear)

p <- ggplot(RecordsPerYear, aes(ObsYear, StocksPerYear)) + 
  geom_point() + 
  stat_smooth(method = 'lm', se=F, size=.2) + 
  ggtitle("Stocks in the Dataset by Year") +
  labs(y ="# of Stocks With Trading Activity in Given Year") + 
  theme_stata()
ggplotly(p)



# TODO:  Nest.  One record per ticker.  
## Create Boolean columns (calculate in Shiny?) for above SMA(50, 100, 200).  
## Create col for RSI(14)
## Get company names, volume, call/put volume (important even if you ignore strike price)...Quandl, Quantmod, Google?








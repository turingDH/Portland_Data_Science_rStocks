library(data.table)
library(dplyr)
library(DT)
library(ggplot2)  #using CRAN's, not GitHub's
library(ggthemes)
library(lubridate)
library(plotly)
library(readr)
library(stringr)
library(tidyr)


##### Vector of dataframe names derived from files in repo home
AllCsvsInRepoHome <- dir()[grep('*.csv', dir())]
RelevantFileNames <- str_split(AllCsvsInRepoHome, 'us-')
RelevantFileNames <- gsub("adj", "Adj", gsub(".csv", "", sapply(RelevantFileNames, "[[", 2)))
RelevantFileNames


##### Create all the dataframes by import iteration
for(i in seq_along(RelevantFileNames)){
  TheName <- paste0("Raw", RelevantFileNames[i], sep="")
  #assign(TheName, tbl_df(fread(AllCsvsInRepoHome[i], fill=T)), envir = .GlobalEnv) 
  assign(TheName,  read_csv(AllCsvsInRepoHome[i]), envir = .GlobalEnv)  #fread cannot infer or take colTypes.  Important for ObsDate
}


##### Do all the dataframes have same dims and NAs
AllFileMeanNA <- vector()
AllFileDimCol <- vector()
AllFileDimRow <- vector()
AllFileName   <- vector()
for (i in seq_along(RelevantFileNames)){
  AllFileName[i]   <- eval(parse(text="paste0('Raw', RelevantFileNames)[i]"))
  AllFileMeanNA[i] <- round(mean(is.na(get(eval(parse(text="paste0('Raw', RelevantFileNames)[i]"))))), 4)
  AllFileDimCol[i] <- ncol(get(eval(parse(text="paste0('Raw', RelevantFileNames)[i]"))))
  AllFileDimRow[i] <- nrow(get(eval(parse(text="paste0('Raw', RelevantFileNames)[i]"))))
}
StatsOnEachDF <- tibble(AllFileName, AllFileDimRow, AllFileDimCol, AllFileMeanNA)
StatsOnEachDF


##### What does the distribution of NAs look like
options(scipen = 10)
ClosingNAs <- as.data.frame(sapply(RawAdjClose, function(x) round(sum(is.na(x))/length(x), 2) ))
if (dim(ClosingNAs)[2] == 1){
  setDT(ClosingNAs, keep.rownames = T)
  colnames(ClosingNAs) <- c("Ticker", "Percentile")
}
ClosingNAs <- ClosingNAs[ClosingNAs$Ticker != 'ObsDate', ]
p <-ggplot(ClosingNAs, aes(ClosingNAs$Percentile)) + 
  geom_histogram(bins=100, fill='darkgreen') + 
  labs(x="Percent NA by Stock", y="# of Stocks at Given Percentile of NAs") + 
  ggtitle("Distribution of NA/NULL Values in Closings Dataset") +
  theme_stata()
ggplotly(p)
#ggplot(ClosingNAs, aes(ClosingNAs$Percentile)) + stat_ecdf(geom = "step")



###################################################################
##### Initial reshape: melt/gather
###################################################################
TidyClosingStockData <- gather(RawAdjClose, key=Ticker, value=Close, names(RawAdjClose)[-1]) %>%
  arrange(Ticker, ObsDate)


##### check out DF size
sl <- object.size(TidyClosingStockData)
print(sl, units='auto')


##### Convert tickers to factors; reduces given DF size by ~32 Mb
TidyClosingStockData$Ticker <- as.factor(TidyClosingStockData$Ticker)
sl <- object.size(TidyClosingStockData)
print(sl, units='auto')


##### Quick data validation check to make sure we only have Mon - Fri
TidyClosingDaysTable <- table(weekdays(TidyClosingStockData$ObsDate))


##### NA's were so extensive, it seems inadvisable to fill, impute, or interpolate for them.
# TODO: how to group by Ticker, and find length of NA's using RLE, but only inside margins...only after 1st !NA or before last !NA.  Possible?
# DONT remove NA's until you compare NA's in 4 other CSV's.
TidyClosingStockData <- TidyClosingStockData %>% filter(!is.na(Close))  

sl <- object.size(TidyClosingStockData)
print(sl, units='auto')


##### Get a distribution of records for all Tickers across Years.
ClosingRecordsPerYearAndStock <-
TidyClosingStockData %>%
  mutate(ObsYear = year(ObsDate)) %>%
  group_by(ObsYear, Ticker) %>%
  add_count() %>%
  rename(RecPerTickerPerYear=n) %>%
  select(ObsYear, Ticker, RecPerTickerPerYear) %>%
  distinct()


##### Get a count of Tickers per Year.
ClosingRecordsPerYear <-
ClosingRecordsPerYearAndStock %>%
  ungroup() %>%
  select(-Ticker) %>%
  group_by(ObsYear) %>%
  mutate(StocksPerYear = n()) %>%
  select(-RecPerTickerPerYear) %>%
  distinct() %>%
  arrange(ObsYear)

p <- ggplot(ClosingRecordsPerYear, aes(ObsYear, StocksPerYear)) + 
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








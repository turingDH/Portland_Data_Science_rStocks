library(cowplot)
library(data.table)
library(dplyr)
library(DT)
library(ggplot2)  #using CRAN's, not GitHub's
library(ggthemes)
library(lubridate)
library(plotly)
library(purrr)
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

Distribution_of_NA <- function(DF, DataSetDescriptor) {
  TheNAs <- as.data.frame(sapply(DF, function(x) round(sum(is.na(x))/length(x), 2) ))
  if (dim(TheNAs)[2] == 1){
    setDT(TheNAs, keep.rownames = T)
    colnames(TheNAs) <- c("Ticker", "Percentile")
  }
  TheNAs <- TheNAs[TheNAs$Ticker != 'ObsDate', ]
  p <-ggplot(TheNAs, aes(TheNAs$Percentile)) + 
    geom_histogram(bins=100, fill='darkgreen') + 
    labs(x="Percent NA by Stock", y="# of Stocks at Given Percentile of NAs") + 
    ggtitle(paste("Distribution of NA/NULL Values in", DataSetDescriptor, "Dataset", sep=" ")) +
    theme_stata()
  
  return(ggplotly(p))
  #ggplot(TheNAs, aes(TheNAs$Percentile)) + stat_ecdf(geom = "step")
}

AdjHighNA_GGPlotly  <- Distribution_of_NA(RawAdjHigh,  "Highs")
AdjLowNA_GGPlotly   <- Distribution_of_NA(RawAdjLow,   "Lows")
AdjOpenhNA_GGPlotly <- Distribution_of_NA(RawAdjOpen,  "Openings")
AdjCloseNA_GGPlotly <- Distribution_of_NA(RawAdjClose, "Closings")
VolumeNA_GGPlotly   <- Distribution_of_NA(RawVolume,   "Volume")





##### Reshape: melt/gather
Tidy_all_the_raw_things <- function(RawDF, theMetric) {
  GatheredDF <- gather(RawDF, key=Ticker, value = Value, names(RawDF)[-1]) %>%
    arrange(Ticker, ObsDate)
  
  colnames(GatheredDF)[ncol(GatheredDF)] <- theMetric
  return(GatheredDF)
}

TidyAdjClose <- Tidy_all_the_raw_things(RawAdjClose, 'AdjClose')
TidyAdjHigh  <- Tidy_all_the_raw_things(RawAdjHigh, 'AdjHigh')
TidyAdjLow   <- Tidy_all_the_raw_things(RawAdjLow, 'AdjLow')
TidyAdjOpen  <- Tidy_all_the_raw_things(RawAdjOpen, 'AdjOpen')
TidyVolume   <- Tidy_all_the_raw_things(RawVolume, 'StockVolume')


##### Quick data validation check to make sure we only have Mon - Fri
table(weekdays(TidyAdjClose$ObsDate))


#####  Join them all; check NA's
TidyMasterDf <-
left_join(TidyAdjClose, TidyAdjHigh, by=c("ObsDate", "Ticker")) %>%
  left_join(TidyAdjLow, by=c("ObsDate", "Ticker")) %>%
  left_join(TidyAdjOpen, by=c("ObsDate", "Ticker")) %>%
  left_join(TidyVolume, by=c("ObsDate", "Ticker"))
TidyMasterDf


###### ObsDate, Ticker should have 0 NAs; subsequent 5 should have same # of NA's
NA_Table_MasterDF <- table(sapply(TidyMasterDf, function(x) sum(length(which(is.na(x))))))

if (NA_Table_MasterDF[1][[1]] == 2 && length(NA_Table_MasterDF) == 2){
  TidyMasterDf <- TidyMasterDf %>% filter(!is.na(StockVolume))
}


##### Save some resources:  Convert tickers to factors, remove joined DFs
TidyMasterDf$Ticker <- as.factor(TidyMasterDf$Ticker)
rm(TidyAdjClose, TidyAdjHigh, TidyAdjLow, TidyAdjOpen, TidyVolume)



TidyMasterDf





################################################################
#####  Decide what to do w/ this section.  Shiny?        
################################################################
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










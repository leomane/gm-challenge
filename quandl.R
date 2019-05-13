library(Quandl)
library(ggplot2)
library(tidyverse)
library(lubridate)

setwd('~/Code/intelligent-analytics/gm-challenge/')

Quandl.api_key("wE_NoJcpGHrzkajXSCcs")
data <- Quandl('EOD/TSLA', type = "xts")

head(data)
acf(data$Adj_Close)

# see what the time series looks like
plot(data$Adj_Close)

# create a 1-lag feature and use "yesterday's" price
# to predict "today's" price
data$Adj_Close_Prev = lag(data$Adj_Close, k=1)

# find the mean absolute error
mean(abs((data$Adj_Close[-1,] - data$Adj_Close_Prev[-1,])))

# look at the correlation betweeen the prediction and actual
cor(data$Adj_Close[-1,], data$Adj_Close_Prev[-1,]) # .9986

# create a new dataframe
lag_df <- data.frame(data$Adj_Close[-1,], data$Adj_Close_Prev[-1,])

# plot the prediction vs actual
ggplot(lag_df, aes(Adj_Close_Prev,Adj_Close)) +
  geom_point()+
  geom_smooth(method=lm)

# write.csv(data, 'TSLA.csv')

# get sentiment data
sentiment_data <- Quandl.datatable('IFT/NSA', ticker='TSLA', paginate=TRUE)
head(sentiment_data)
tail(sentiment_data)

# sort sentiment data
head(sentiment_data[order(sentiment_data$date),])
tail(sentiment_data[order(sentiment_data$date),])
sentiment_data <- sentiment_data[order(sentiment_data$date),]

tail(data, n=20)
tail(sentiment_data, n=20)

# interpolate missing weekends and holidays
merged_data <- merge(data, zoo(,seq(start(data),end(data),by='day')),all=TRUE)
head(data, n=20)
head(merged_data, n=20)

locf_data <- na.locf(merged_data)

holidays <- read_csv('holidays.csv', col_names=F)
names(holidays) <- c('name','date')

holidays$char_date <- holidays$date
holidays$date <- mdy(holidays$char_date)
holidays$is_holiday <- 1

head(holidays)

length(holidays$date)
length(unique(holidays$date))

double_dates <- table(holidays$date)[table(holidays$date) > 1]
holidays[holidays$date %in% as.Date(names(double_dates)),]

unique_holidays <- unique(holidays)
length(unique_holidays$date)

zoo_holidays <- zoo(unique_holidays, order.by = unique_holidays$date)
merged_holidays <- merge(zoo_holidays, zoo(,seq(start(zoo_holidays),end(zoo_holidays),by='day')),all=TRUE)
merged_holidays$is_holiday <- na.fill(merged_holidays$is_holiday,0)

filled_holidays <- zoo(merged_holidays$is_holiday)
start(filled_holidays)

data_and_holidays <- merge(x = locf_data, y = filled_holidays, join='left')
names(data_and_holidays)[14] <- 'is_holiday'

start(data_and_holidays)
end(data_and_holidays)

index(data_and_holidays)[1]
as.Date(index(data_and_holidays)[1],'%Y-%m-%d')
data_and_holidays$dow <- wday(as.Date(index(data_and_holidays),'%Y-%m-%d'))

head(data_and_holidays)

data_and_holidays$is_Sun <- ifelse(data_and_holidays$dow == 1, 1, 0)
data_and_holidays$is_Mon <- ifelse(data_and_holidays$dow == 2, 1, 0)
data_and_holidays$is_Tue <- ifelse(data_and_holidays$dow == 3, 1, 0)
data_and_holidays$is_Wed <- ifelse(data_and_holidays$dow == 4, 1, 0)
data_and_holidays$is_Thu <- ifelse(data_and_holidays$dow == 5, 1, 0)
data_and_holidays$is_Fri <- ifelse(data_and_holidays$dow == 6, 1, 0)
data_and_holidays$is_Sat <- ifelse(data_and_holidays$dow == 7, 1, 0)

clean_data <- data_and_holidays[,!(names(data_and_holidays) %in% c('dow','Sunday','Adj_Close_Prev'))]
length(is.na(clean_data)[is.na(clean_data)])

clean_data
zoo_sentiment <- zoo(sentiment_data,order.by = sentiment_data$date)

start(zoo_sentiment)
end(zoo_sentiment)

start(clean_data)
end(clean_data)

zoo_sent_clean <- zoo_sentiment[,names(zoo_sentiment) %in% c('sentiment','sentiment_high','sentiment_low','news_volume','news_buzz')]
head(zoo_sent_clean)
length(is.na(zoo_sent_clean)[is.na(zoo_sent_clean)])

tesla_data <- merge(x = clean_data, y = zoo_sent_clean, join='inner')

tesla_xts <- xts(tesla_data)

tesla_xts$Adj_Close_Diff <- diff(tesla_xts$Adj_Close)
head(tesla_xts[,c('Adj_Close','Adj_Close_Diff')])

tesla_xts$Direction <- ifelse(is.na(tesla_xts$Adj_Close_Diff)|tesla_xts$Adj_Close_Diff==0,0,ifelse(tesla_xts$Adj_Close_Diff>0,1,-1))

head(tesla_xts[,c('Adj_Close','Adj_Close_Diff','Direction')])

unique(tesla_xts$Dividend)
unique(tesla_xts$Split)

tesla_xts <- subset(tesla_xts, select=-c(Adj_Close_Diff,Open,High,Low,Close,Volume,Dividend,Split))
names(tesla_xts)

write.zoo(tesla_xts, sep=',' , file='TSLA_and_sentiment.csv')

four_week_lags = c('Adj_Close')
two_week_lags = c('Adj_Open','Adj_High','Adj_Low','Adj_Volume','sentiment','sentiment_high','sentiment_low','news_volume','news_buzz','Direction')
one_week_forward_and_back = c('is_holiday')

for(col_name in four_week_lags) {
  for(i in 1:28) {
    new_name <- paste0(col_name,'_tm', i)
    new_lag <- lag(tesla_xts[,col_name], i)
    names(new_lag) <- new_name
    tesla_xts <- merge(tesla_xts,new_lag,join='inner') 
  }
}

for(col_name in two_week_lags) {
  for(i in 1:14) {
    new_name <- paste0(col_name,'_tm', i)
    new_lag <- lag(tesla_xts[,col_name], i)
    names(new_lag) <- new_name
    tesla_xts <- merge(tesla_xts,new_lag,join='inner') 
  }
}

for(col_name in one_week_forward_and_back) {
  for(i in 1:7) {
    new_name <- paste0(col_name,'_tm', i)
    new_lag <- stats::lag(tesla_xts[,col_name], i)
    names(new_lag) <- new_name
    tesla_xts <- merge(tesla_xts,new_lag,join='inner') 
  }
  for(i in 1:7) {
    new_name <- paste0(col_name,'_tp', i)
    new_lag <- stats::lag(tesla_xts[,col_name], -i)
    names(new_lag) <- new_name
    tesla_xts <- merge(tesla_xts,new_lag,join='inner') 
  }
}

head(tesla_xts)
names(tesla_xts)

head(na.omit(tesla_xts))

tesla_xts <- na.omit(tesla_xts)
write.zoo(tesla_xts, sep=',' , file='TSLA_and_sentiment_lagged.csv')



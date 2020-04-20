setwd("/Users/a1120984690/Downloads/425")
test = read_csv('test.csv')
key = read_csv('key.csv')
weather = read_csv('weather.csv', na = c("M", "-"))
train = read_csv('train.csv')

library(tidyverse)
library(caret)
library(MASS)
library(data.table)
library(lubridate)
library(bnstruct)
library(RANN)
library(doParallel)
library(gridExtra)
library(glmnet)

weather$snowfall = weather$snowfall %>%
  replace(which(weather$snowfall == "	  T"), 0.01)
weather$preciptotal = weather$preciptotal %>%
  replace(which(weather$preciptotal == "	  T"), 0.01)

weather = weather%>%
  select(-codesum)%>%
  mutate(sunrise = as.numeric(sunrise),
         preciptotal = as.numeric(preciptotal),
         resultdir = as.numeric(resultdir),
         snowfall = as.numeric(snowfall),
         week = as.factor(wday(as.Date(date)))
  )


prepro_mod = preProcess(weather[, - c(1, 2, 20)], 
                        method = 'bagImpute',
                        na.remove = FALSE)

weather_all = weather
weather_all[, - c(1, 2, 20)] = predict(prepro_mod,  weather[, - c(1, 2, 20)])


trn = train %>%
  left_join(key, by = 'store_nbr') %>%
  left_join(weather_all, by = c('station_nbr', 'date'))

trn = trn %>%
  group_by(store_nbr,item_nbr) %>%
  mutate(sum = sum(units)) 

trn_n0 = trn %>%
  filter(sum != 0) %>%
  dplyr::select(-sum) 

trn_n0 = subset(trn_n0, select = -c(date, store_nbr, station_nbr))
trn_n0 = trn_n0 %>%
  mutate(item_nbr = as.factor(item_nbr))

trn_0 = trn %>%
  filter(sum == 0) %>%
  dplyr::select(-sum) 
trn_0 = subset(trn_0, select = -c(date, store_nbr, station_nbr))
trn_0 = trn_0 %>%
  mutate(item_nbr = as.factor(item_nbr))
  

trn_0_idx = trn %>%
  filter(sum != 0) %>%
  dplyr::select(c(store_nbr, item_nbr, units))

trn_0_idx = trn_0_idx[!duplicated(trn_0_idx),]

tst_0_idx = test %>%
  left_join(trn_0_idx, by = c('store_nbr','item_nbr'))%>% 
  mutate(item_nbr = as.factor(item_nbr))

tst_n0_idx = tst_0_idx[is.na(tst_0_idx$units),]

tst_n0 = tst_n0_idx %>%
  left_join(key)%>%
  left_join(weather_all)

ols_mod = lm(units ~ ., trn_n0)
trans_mod = lm(log(units+1) ~ ., trn_n0)

pred <- predict(trans_mod, tst_n0)
tst_n0$predicted = exp(pred) - 1

tst_n0$predicted = ifelse(tst_n0$predicted < 0, 0 ,tst_n0$predicted)
tst_n0$predicted = round(tst_n0$predicted)

tst_predicted = tst_0_idx %>%
  left_join(tst_n0[,c(1,2,3,24)], by=c("date", "store_nbr", "item_nbr") )
tst_predicted$units[is.na(tst_predicted$units)] = tst_predicted$predicted[is.na(tst_predicted$units)]
tst_predicted = tst_predicted[-5]





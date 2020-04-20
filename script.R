#loading libraries
library(dplyr)
library(tibble)
library(ggplot2)
library(chron)
library(caret)
library(MASS)
library(glmnet)
library(purrr)
library(lubridate)

setwd("/Users/a1120984690/Downloads/425")
# loading the original datasets
train <- read.csv("train.csv", stringsAsFactors = FALSE)
weather <- read.csv("weather.csv", stringsAsFactors = FALSE, na = c("M", "-"))
key <- read.csv("key.csv", stringsAsFactors = FALSE) #the relational mapping between stores and the weather stations that cover them
test <- read.csv("test.csv",stringsAsFactors = FALSE)
weather$snowfall[which(weather$snowfall == "  T")] <- 0.01
weather$preciptotal[which(weather$preciptotal == "  T")] <- 0.01
weather = weather %>%
  dplyr::select(-codesum) %>%
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
weather_all[, - c(1, 2, 20)] = predict(prepro_mod,  weather[, -c(1, 2, 20)])

#merge the training tables
table1 <- left_join(train, key)
table2 <- left_join(table1, weather)
# Not merge (inner join the test data coz the order may be changed)
#tst1 <- merge(test, key)
#tst2 <- merge(tst1, weather)


trn1 <- table1
bfs <- as.Date(c(BlackFriday = c("2012-11-20", "2012-11-21", "2012-11-23", "2012-11-24",
                                 "2012-11-25", "2012-11-26", "2013-11-26", "2013-11-27",
                                 "2013-11-28", "2013-11-29", "2013-11-30", "2013-12-01",
                                 "2013-12-02")))
interval <- as.integer(floor(julian((as.POSIXlt("2014-10-31"))))) - as.integer(floor(julian((as.POSIXlt("2012-01-01")))))
origin <- as.integer(floor(julian((as.POSIXlt("2012-01-01")))))
trn1$month <- factor(substr(trn1$date, 6, 7))
trn1$quarter <- factor(quarters(as.Date(as.Date(trn1$date))))
trn1$blackFriday <- factor(as.numeric(as.Date(trn1$date) %in% bfs))
trn1$weekday <- factor(weekdays(as.Date(as.Date(trn1$date))))
trn1$julian <- as.integer(floor(julian((as.POSIXlt(trn1$date))))) - origin
trn1$store_nbr <- factor(trn1$store_nbr)
trn1$item_nbr <- factor(trn1$item_nbr)
#trn1 <- trn1 %>% filter(date != set0_date)
comb <- trn1 %>% 
  group_by(store_nbr, item_nbr) %>% 
  summarize(m = max(units)) %>% 
  filter(m != 0) %>% 
  dplyr::select(store_nbr, item_nbr)

fits <- data.frame(julian = c(), store = c(), item = c())
mods <- list()
for (i in 1 : nrow(comb)) {
  store <- sapply(comb[i,1], as.character)
  item <- sapply(comb[i,2], as.character)
  data <- trn1 %>% 
    filter(store_nbr == store & item_nbr == item)
  mod <- ppr(log1p(units) ~ julian, data = data, nterms = 3, max.terms = 5)
  mods <- c(mods, list(mod))
  df <- data.frame(julian = 0:interval, store_nbr = store, item_nbr = item)
  df$fitted <- predict(mod, df)
  fits <- rbind(fits, df)
}
levels(fits$item_nbr) <- union(levels(fits$item_nbr), levels(trn1$item_nbr))
tb <- left_join(trn1, fits)
tb <- tb %>% na.omit
tb$loss <- log1p(tb$units) - tb$fitted
#x_vars <- model.matrix(~ item_nbr * month + store_nbr * month
#                       + item_nbr * blackFriday + store_nbr * blackFriday
#                       + item_nbr * weekday + store_nbr * weekday
#                       + item_nbr * quarter + store_nbr * quarter, tb)[,-1]
#y_var <- tb$loss
lm_loss <- lm(loss ~ item_nbr + store_nbr + month
              +  blackFriday + weekday + quarter, data = tb)
gbm_loss <- train(loss ~ item_nbr + store_nbr + month
                  +  blackFriday + weekday + quarter, data = tb,
                  method = "gbm", trControl = trainControl(method = "cv", number = 5),
                  verbose = FALSE)

tst0 <- test %>% 
  mutate_at(vars(matches("_nbr")),funs(factor)) 
tst0$julian <- as.integer(floor(julian((as.POSIXlt(tst0$date))))) - origin
tst0$month <- factor(substr(tst0$date, 6, 7))
tst0$quarter <- factor(quarters(as.Date(as.Date(tst0$date))))
tst0$blackFriday <- factor(as.numeric(as.Date(tst0$date) %in% bfs))
tst0$weekday <- factor(weekdays(as.Date(as.Date(tst0$date))))
tst0$julian <- as.integer(floor(julian((as.POSIXlt(tst0$date))))) - origin
tst0$store_nbr <- factor(tst0$store_nbr)
tst0$item_nbr <- factor(tst0$item_nbr)
levels(fits$item_nbr) <- union(levels(fits$item_nbr),levels(tst0$item_nbr))
levels(tst0$store_nbr) <- union(levels(tst0$store_nbr),levels(fits$store_nbr))
tst_tb <- left_join(x = tst0, y = fits, by = c("store_nbr", "item_nbr", "julian"))
tst_tb$fitted[is.na(tst_tb$fitted)] <- 0

add_lm_loss <- predict(lm_loss, newdata = tst_tb)
output2 <- as.integer(round(exp(add_lm_loss + tst_tb$fitted) - 1))
output2[ind]<- 0
write.table(output2, "ppr_loss.csv", row.names = F)
add_gbm_loss <- predict(gbm_loss, newdata = tst_tb)
output3<- as.integer(round(exp(add_gbm_loss + tst_tb$fitted) - 1))
output3[ind]<- 0
write.table(output3, "ppr_gbm.csv", row.names = F)

output <- as.integer(round(exp(tst_tb$fitted) - 1))
output[is.na(tst_tb$fitted)] <- 0
ind <- which(tst0$date == "2013-12-25" | tst0$date == "2012-12-25" | tst0$date == "2014-12-25")
output[ind] <- 0
write.table(output, "output.csv", row.names = F)


#knn models
knn_fits <- data.frame(julian = c(), store = c(), item = c())
knn_mods <- list()
for (i in 1 : nrow(comb)) {
  store <- sapply(comb[i,1], as.character)
  item <- sapply(comb[i,2], as.character)
  data <- trn1 %>% 
    filter(store_nbr == store & item_nbr == item)
  set.seed(42)
  knn_mod <- train(log1p(units) ~ julian, data = data, method="knn", 
                   trControl = trainControl(method = "cv", number = 5), tuneGrid = expand.grid(k = c(7, 14,21)))
  knn_mods <- c(knn_mods, list(knn_mod))
  df <- data.frame(julian = 0:interval, store_nbr = store, item_nbr = item)
  df$knn_fitted <- predict(knn_mod, df)
  
  knn_fits <- rbind(knn_fits, df)
}

levels(knn_fits$item_nbr) <- union(levels(knn_fits$item_nbr),levels(tst0$item_nbr))
levels(tst0$store_nbr) <- union(levels(tst0$store_nbr),levels(knn_fits$store_nbr))
knn_table <- left_join(x=tst0, y=knn_fits)
knn_table$knn_fitted[is.na(knn_table$knn_fitted)] <- 0
knn_outcome <- as.integer(round(exp(knn_table$knn_fitted)-1))
knn_outcome[ind] <- 0
write.table(knn_outcome, "knn_outcome.csv", row.names = F)



# EDA
library(rCharts)
data <- trn1 %>% 
  dplyr::select(-station_nbr, -date, -julian) %>% 
  group_by(blackFriday, weekday) %>% 
  summarise(s = sum(units)) %>% 
  filter(s != 0) 

nPlot(s ~ weekday, group = "blackFriday", data = data,
      type = "multiBarChart")

data2 <- trn1 %>% 
  mutate(units = log1p(units)) %>% 
  group_by(item_nbr, store_nbr) %>% 
  summarise(m = max(units)) %>% 
  filter(m != 0) 

rPlot(s ~ row.names(data2), data = data2, type = "point")
library(plotly)
plot_ly(data2, 
        x = ~store_nbr, 
        y = ~item_nbr,
        z = ~m)%>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = "#store", showgrid = FALSE),
         yaxis = list(title = "#item",showgrid = FALSE),
         zaxis = list(title = "log(units + 1)",showgrid = FALSE)))




# defining the month factor
table2$month <- substr(table2$date, 6, 7)

# what exactly are considered as holidays
holidays <- as.Date(c(NewYears = c("2014-01-01", "2014-12-31",
                                   "2013-01-01", "2013-12-31",
                                   "2012-01-01", "2012-12-31"), 
                      Christmas = c("2014-12-25", "2014-12-24", 
                                    "2013-12-25", "2014-12-24",
                                    "2012-12-25", "2014-12-24"), 
                      ValentinesDay = c("2014-02-14", "2013-02-14", "2012-02-14"),
                      IndDay = c("2014-07-04", "2013-07-04", "2012-07-04"),
                      Easter = c("2012-04-08", "2013-03-31", "2014-04-20"),
                      MothersDay = c("2012-05-13", "2013-05-12", "2014-05-11"),
                      FathersDay = c("2012-06-17", "2013-06-16", "2014-06-15"),
                      LaborDay = c("2012-09-03", "2013-09-02", "2014-09-01"),
                      Halloween = c("2012-10-31", "2013-10-31","2014-10-31"),
                      Thanksgiving = c("2014-11-27", "2013-11-28", "2012-11-22"),
                      BlackFriday = c("2012-11-20", "2012-11-21", "2012-11-23", "2012-11-24",
                                      "2012-11-25", "2012-11-26", "2013-11-26", "2013-11-27",
                                      "2013-11-28", "2013-11-29", "2013-11-30", "2013-12-01",
                                      "2013-12-02")))

dates <- as.Date(table2$date)
#defining holiday as a binary factor
table2$holiday <- as.numeric(dates %in% holidays)
#defining weekend as a binary factor
table2$weekend <- factor(ifelse(is.weekend(dates), 1, 0))


# picking up the low sales day(s)
set0_date <- table2 %>% 
  group_by(date) %>% 
  summarise(s = sum(units)) %>% 
  filter(s < 10) %>% 
  dplyr::select(date)


# there are 45 stores, 111 items


# deleting the low sales day in training date (it will be set to 0 as a result)
# omitting all the missing values

table2$preciptotal[which(table2$preciptotal == "  T" | table2$preciptotal == 'M')] <- 0
table2$tavg[which(tables$tavg == 'M')] <- 60
trn <- table2 %>% 
  filter(date != set0_date) %>% 
  dplyr::select(date, store_nbr, item_nbr, units, tavg, 
                dewpoint, wetbulb, preciptotal, avgspeed,  month, holiday, weekend) %>% 
  # mutate_each(funs(replace(., .== "M", NA))) %>% 
  na.omit()

# setting threshold that denotes the day is stormy
trn$storm <- as.numeric(as.numeric(trn$preciptotal) > 0.7)

# transfering the data structures
trn0 <- trn %>% 
  #mutate_at(vars(matches("_nbr")),funs(factor)) %>% 
  mutate_at(vars(holiday, storm, month, weekend),funs(factor)) %>% 
  mutate_at(2:10, as.numeric)

# only using the non-zero rows
trn_final <- trn0 %>% filter(units != 0)

#finding the store-item combinations that do have units sold.
comb0 <- trn_final %>% 
  group_by(store_nbr, item_nbr) %>% 
  summarize(m = max(units)) %>% 
  filter(m != 0) %>% 
  dplyr::select(store_nbr, item_nbr)
# there is 247 pairs since rows on 2013-12-25 have been deleted, 
# otherwise there would be 255 pairs


# processing the testing data [not merge--only item, store, date are contained]
test$holiday <- factor(as.numeric(as.Date(test$date) %in% holidays))
test$month <- substr(test$date, 6, 7)
test$weekend <- factor(ifelse(is.weekend(as.Date(test$date)), 1, 0))

tst0 <- test %>% 
  mutate_at(vars(matches("_nbr")),funs(factor)) 

# calculating how many distinct julian days in training data
interval <- as.integer(floor(julian((as.POSIXlt("2014-10-31"))))) - as.integer(floor(julian((as.POSIXlt("2012-01-01")))))
# setting the first julian day
origin <- as.integer(floor(julian((as.POSIXlt("2012-01-01")))))
# including the julian column indicating how many days away from the first day
trn_final$julian <- as.integer(floor(julian((as.POSIXlt(trn_final$date))))) - origin


#XGBoost
library(xgboost)
gbm_trn <- table2 %>% dplyr::select(-station_nbr)
gbm_trn$month <- as.numeric(substr(gbm_trn$date, 6, 7))
gbm_trn$day <- as.numeric(substr(gbm_trn$date, 9, 10))
#gbm_trn$julian <- as.integer(floor(julian((as.POSIXlt(gbm_trn$date))))) - origin
gbm_trn$weekday <- as.numeric(strftime(as.Date(gbm_trn$date), "%u"))
gbm_trn$days <- as.numeric(strftime(as.Date(gbm_trn$date), "%j"))
gbm_trn$units <- log1p(gbm_trn$units)
gbm_trn$preciptotal[which(gbm_trn$preciptotal == "  T" | gbm_trn$preciptotal == 'M')] <- 0
gbm_trn <- gbm_trn %>% 
  mutate_at(2:21, as.numeric)

gbm_trn$weekday <- as.factor(gbm_trn$weekday)
gbm_trn$day <- as.factor(gbm_trn$day)
gbm_trn$days <- as.factor(gbm_trn$days)
gbm_trn$month <- as.factor(gbm_trn$month)


gbm_trn1 <- gbm_trn %>% 
  na.omit() %>% 
  group_by(day, weekday) %>% 
  summarise(s = sum(units)) %>% 
  filter(s != 0)

gbm_trn2 <- gbm_trn %>% 
  na.omit() %>% 
  group_by(weekday, tavg) %>% 
  summarise(s = sum(units)) %>% 
  filter(s != 0)

library(rCharts)
nPlot(s ~ day, group = "weekday", data = gbm_trn1,
      type = "multiBarChart")

rPlot(s ~ tavg | weekday, data = gbm_trn2,
     color = 'weekday', type = 'point')







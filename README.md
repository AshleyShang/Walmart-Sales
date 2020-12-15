# Walmart-Sales
walmart sales forecast

dataset came from https://www.kaggle.com/c/walmart-recruiting-sales-in-stormy-weather


Task: to accurately predict the sales of 111 potentially weather-sensitive products (like umbrellas, bread, and milk) around the time of major weather events at 45 of their retail locations. 

**Train model**

1. Exclude item/stores whose units are all zeros.

2. For each item/stores, apply curve fitting by R ppr function (projection pursuit regression).
$$y = log1p_units, x = julian_dates$$

here, data on 2013-12-25 are excluded. (because units are almost all zeros)

3. Train lasso as bias reduction model.
y = log1p_units - ppr_fitted

features :
- A : weekday, is_weekend, is_holiday, is_holiday, is_stormy
- B : item_nbr
- C : store_nbr
- D : date
- E : year, month, day
- F : is_BlackFriday-3days, -2days, -1day, is_BlackFriday, +1day, +2days, +3days
- G : weather features (is preciptotal > 0.2, depart > 8, depart < -8)
- interactions A*B A*C B*E C*E B*F C*F



**Prediction on test set**

predicted_log1p = ppr_fitted(train-2) + lasso predicted(train-3)
predicted = exp(predicted_log1p) - 1

here, below are predicted as zero.
- item/stores whose units are all zeros.
- on 2013-12-25
- "too much zeros" 



As for features:
- weekday is the most important
- month periodicity is on some store/items
- around Black Friday sales fluctuates a lot
- weather features are not effective almost at all
   In the data, people go shopping as usual however much it rains.
   It's not natural, so I guess weather data came from different stations.


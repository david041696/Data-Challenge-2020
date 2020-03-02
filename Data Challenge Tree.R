data$location = as.factor(data$location)
data$wind_direction = as.factor(data$wind_direction)
data$event = as.factor(data$event)
data$Weekend = as.factor(data$Weekend)
data$Hour = as.factor(data$Hour)

##install.packages("randomForest")
library(randomForest)
set.seed(100)
train_insts <- sample(nrow(data), 0.7*nrow(data))
data_train <- data[train_insts,]
data_test <- data[-train_insts,]
summary(data_train)
summary(data_test)
attach(data)

#car tree on training set
tree_model_car <- randomForest(cars ~ event+location+Hour+Weekend+wind_direction+cloud_height+humidity+wind_speed+TEMP, data = data_train, importance = TRUE)
tree_model_car
importance(tree_model_car)

pred_test <- predict(tree_model_car, data_test)

RMSE_tree_car = sqrt(mean((pred_test - data_test$cars)**2))
RMSE_tree_car

#car tree on all data
tree_model_car <- randomForest(cars ~ event+location+Hour+Weekend+wind_direction+cloud_height+humidity+wind_speed+TEMP, data = data, importance = TRUE)
tree_model_car
importance(tree_model_car)

#car pred
car_pred = data.frame(data_pred)

car_pred$event = as.factor(car_pred$event)
car_pred$Hour = as.factor(car_pred$Hour)
car_pred$location = as.factor(car_pred$location)
car_pred$Weekend = as.factor(car_pred$Weekend)
car_pred$wind_direction = as.factor(car_pred$wind_direction)

levels(car_pred$event) = levels(data_train$event)
levels(car_pred$Hour) = levels(data_train$Hour)
levels(car_pred$location) = levels(data_train$location)
levels(car_pred$Weekend) = levels(data_train$Weekend)
levels(car_pred$wind_direction) = levels(data_train$wind_direction)


pred_matrix = matrix(predict(tree_model_car, newdata=car_pred), nrow=5, byrow=FALSE)
pred_df = as.data.frame(pred_matrix, row.names = c('main entrance', 'center campus', 'north entrance', 'south gate', 'southwest entrance'))

for (i in seq(1,length(pred_df))){
  names(pred_df)[i] = i
}

pred_df

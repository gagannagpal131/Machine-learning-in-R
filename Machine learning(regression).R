# Machine Learning Program.
# Implementation of machine learning by fitting a regression model on the data and
# performing predictive analysis of the data.

data <- attitude
print("Actual data is :-")
print(data)

index <- sample(1:nrow(data),0.75 * nrow(data))

# I have divided the original data set into two parts :- test and train
# train data set is used to fit the regression model 
# test data set is used to test the regression model and perform the predictive analysis 

train <- data[index,]
test <-data[-index,]

print("Fitting of regression model on the data")
lm_fit<- lm(rating~.,data = train)

plot(lm_fit)

pr_lm <- as.data.frame(predict(lm_fit,test))

finalOutput <- cbind(pr_lm,test$rating)
colnames(finalOutput) <- c("predicted values","actual values")

print(finalOutput)

plot.default(pr_lm,type = "b",col = "blue",ylim = c(0,100),ylab = "Ratings",main = "predicted value (blue) vs actual value (black)")
lines(test$rating,type = "b")

mape <- (mean(abs(test$rating - finalOutput$`predicted values`)/test$rating) * 100)
print("mean absolute percentage error is:- ")
print(mape)
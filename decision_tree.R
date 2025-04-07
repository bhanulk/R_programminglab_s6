
library(rpart)
library(rpart.plot)

model<-rpart(Species~Sepal.Width+Sepal.Length,data=iris,method = "class")
rpart.plot(model,type=2,extra=104,fallen.leaves=TRUE)


pred <- predict(model, iris, type = "class")

accuracy <- mean(pred == iris$Species)
cat("Model Accuracy:", accuracy, "\n")

len<-as.numeric(readline(prompt = "Enter the sepal length:"))
wid<-as.numeric(readline(prompt = "Enter the sepal width:"))
input<-data.frame(Sepal.Width=wid,Sepal.Length=len)
prediction<-predict(model,newdata = input,type = "class")
cat("prediction:",as.character(prediction))
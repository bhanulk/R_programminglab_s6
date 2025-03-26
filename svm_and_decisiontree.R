
library(caTools)
library(e1071)
library(ggplot2)
library(C50)

run_svm_program <- function() {
  
  data(iris)
  
  iris_subset <- iris[iris$Species %in% c("setosa", "versicolor"), ]
  iris_subset$Species <- factor(iris_subset$Species)
  
  iris_subset <- iris_subset[, c("Sepal.Length", "Sepal.Width", "Species")]
  
  set.seed(123)
  split <- sample.split(iris_subset$Species, SplitRatio = 0.8)
  training_set <- subset(iris_subset, split == TRUE)
  test_set <- subset(iris_subset, split == FALSE)
  

  classifier <- svm(Species ~ ., data = training_set, kernel = "radial")
  
  y_pred <- predict(classifier, newdata = test_set)
  
  cm <- table(test_set$Species, y_pred)
  print(cm)
  
  accuracy <- sum(diag(cm)) / sum(cm)
  cat("SVM Accuracy:", accuracy, "\n")

  plot_decision_boundary <- function(set, title) {
    X1 <- seq(min(set[, 1]) - 0.5, max(set[, 1]) + 0.5, by = 0.01)
    X2 <- seq(min(set[, 2]) - 0.5, max(set[, 2]) + 0.01, by = 0.01)
    grid_set <- expand.grid(X1, X2)
    colnames(grid_set) <- c('Sepal.Length', 'Sepal.Width')
    
    y_grid <- predict(classifier, newdata = grid_set)
    
    plot(set[, -3],
         main = title,
         xlab = "Sepal Length", ylab = "Sepal Width",
         col = ifelse(set$Species == "setosa", 'green4', 'red3'), pch = 21, bg = ifelse(set$Species == "setosa", 'green4', 'red3'))
    
    contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
  }
 
  plot_decision_boundary(training_set, "SVM Decision Boundary (Training Set)")
  
  plot_decision_boundary(test_set, "SVM Decision Boundary (Test Set)")
  
  cat("Enter Sepal Length: ")
  sepal_length <- as.numeric(readline())
  cat("Enter Sepal Width: ")
  sepal_width <- as.numeric(readline())
  
  user_data <- data.frame(Sepal.Length = sepal_length, Sepal.Width = sepal_width)
  user_prediction <- predict(classifier, user_data)
  cat("Predicted Species:", user_prediction, "\n")
}

run_decision_tree_program <- function() {
  
  play_tennis_data <- read.csv("D:/r_program_s6/tennis.csv")
  
  play_tennis_data$Outlook <- factor(play_tennis_data$Outlook, levels = c("Sunny", "Overcast", "Rain"))
  play_tennis_data$Temperature <- factor(play_tennis_data$Temperature, levels = c("Hot", "Mild", "Cool"))
  play_tennis_data$Humidity <- factor(play_tennis_data$Humidity, levels = c("High", "Normal"))
  play_tennis_data$Wind <- factor(play_tennis_data$Wind, levels = c("Weak", "Strong"))
  play_tennis_data$PlayTennis <- factor(play_tennis_data$PlayTennis, levels = c("No", "Yes"))
  
  set.seed(123)
  split <- sample.split(play_tennis_data$PlayTennis, SplitRatio = 0.8)
  training_set <- subset(play_tennis_data, split == TRUE)
  test_set <- subset(play_tennis_data, split == FALSE)
  
  model <- C5.0(PlayTennis ~ Outlook + Temperature + Humidity + Wind, data = play_tennis_data)
  
  y_pred <- predict(model, newdata = test_set)
  
  cm <- table(test_set$PlayTennis, y_pred)
  print(cm)
  
  accuracy <- sum(diag(cm)) / sum(cm)
  cat("Decision Tree Accuracy:", accuracy, "\n")
  
  get_user_input <- function() {
    cat("Enter the following options:\n")
    
    cat("1. Outlook (Sunny, Overcast, Rain): ")
    outlook <- readline()
    
    cat("2. Temperature (Hot, Mild, Cool): ")
    temperature <- readline()
    
    cat("3. Humidity (High, Normal): ")
    humidity <- readline()
    
    cat("4. Wind (Weak, Strong): ")
    wind <- readline()
    
    return(data.frame(
      Outlook = factor(outlook, levels = c("Sunny", "Overcast", "Rain")),
      Temperature = factor(temperature, levels = c("Hot", "Mild", "Cool")),
      Humidity = factor(humidity, levels = c("High", "Normal")),
      Wind = factor(wind, levels = c("Weak", "Strong"))
    ))
  }
  
  plot(model, main = "Decision Tree Visualization", type = "simple")
  
  cat("Provide input values for prediction:\n")
  test_data <- get_user_input()
  prediction <- predict(model, test_data)
  cat(paste("Predicted Class:", prediction, "\n"))
}

main_menu <- function() {
  while (TRUE) {
    
    cat("1. Run SVM Program\n")
    cat("2. Run Decision Tree Program\n")
    cat("3. Exit\n")
    choice <- as.numeric(readline(prompt = "Enter your choice (1, 2, or 3): "))
    
    if (choice == 1) {
      run_svm_program()
    } else if (choice == 2) {
      run_decision_tree_program()
    } else if (choice == 3) {
   
      break
    } else {
      cat("Invalid choice!")
    }
  }
}                          

main_menu()


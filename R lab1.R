
library(dplyr)
library(caret)
data <- read.csv("C:/Users/Admin/Downloads/oulad-students.csv")
factor_columns <- c("code_module", "code_presentation", "gender", "region", 
                    "highest_education", "imd_band", "age_band", "num_of_prev_attempts", 
                    "disability", "final_result")

data[factor_columns] <- lapply(data[factor_columns], as.factor)
data <- na.omit(data)

train_index <- createDataPartition(data$final_result, p = 0.8, list = FALSE)

train_data <- data[train_index, ]
test_data <- data[-train_index, ]

model <- glm(final_result ~ ., data = train_data, family = "binomial")

predictions <- predict(model, newdata = test_data, type = "response")

predicted_classes <- ifelse(predictions > 0.5, "Pass", "Fail")  # Adjust the threshold as needed

predicted_classes <- factor(predicted_classes, levels = levels(test_data$final_result))

confusionMatrix(data = predicted_classes, reference = test_data$final_result)

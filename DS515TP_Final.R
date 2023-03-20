# Data Acquisition
# Load required libraries
library(tidyverse)

# Load data
df <- read_csv('Data.csv')

# Check the data
head(df)

# Check the data type and missing values
str(df)


# Data Cleaning
# Check for missing values
sum(is.na(df))

# Replace missing values with mean
df[is.na(df)] <- mean(df, na.rm = TRUE)

# Replace missing values in df excluding the Influencer column
df[,-4] <- apply(df[,-4], 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# Check for missing values again
sum(is.na(df))


# Data Visualization

# Examine basic statistical metrics
summary(df)
mean(df$Sales)
median(df$Sales)
sd(df$Sales)

# Create histogram
ggplot(df, aes(x = Sales)) +
  geom_histogram(binwidth = 1000) +
  labs(x = "Sales", y = "Count") +
  ggtitle("Distribution of Sales")

# Histogram of Sales
ggplot(df, aes(x = Sales)) +
  geom_histogram(bins = 30, color = "white", fill = "#4C72B0") +
  ggtitle("Distribution of Sales") +
  xlab("Sales") +
  ylab("Frequency") +
  theme_bw()

png("salestv.png")

# Scatter plot of Sales vs TV
ggplot(df, aes(x = TV, y = Sales)) +
  geom_point(color = "#4C72B0") +
  ggtitle("Sales vs TV") +
  xlab("TV") +
  ylab("Sales") +
  theme_bw()

dev.off()

png("influencersales.png")

# Boxplot of Sales by Influencer 
ggplot(df, aes(x = Influencer, y = Sales, fill = Influencer)) +
  geom_boxplot() +
  ggtitle("Sales by Influencer Type") +
  xlab("Influencer Type") +
  ylab("Sales") +
  scale_fill_manual(values = c("#4C72B0", "#55A868", "#C44E52", "#8172B2")) +
  theme_bw()

dev.off()

# Data Analysis
# Split data into training and test sets
set.seed(123)
train_indices <- sample(nrow(df), nrow(df)*0.7)
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Linear Regression
# lm_model <- lm(Sales ~., data = df)
# lm_model$coef  - compare coefficients against full dataset

lr_regressor <- lm(Sales ~., data = train_data)
summary(lr_regressor)
lr_regressor$coef
  

# Prediction
y_pred_lr <- predict(lr_regressor, newdata = test_data)

# evaluate model performance
cat("R-squared: ", summary(lr_regressor)$r.squared, "\n")
cat("RMSE: ", sqrt(mean((test_data$Sales - y_pred_lr)^2)), "\n")

# Plot linear regression
library("car")
png("linear_reg.png")
avPlots(lr_regressor)
dev.off()

# Random Forest Regression

# Load the randomForest library
library(randomForest)

# Split the data into training and test sets
set.seed(123)
train_index <- sample(seq_len(nrow(df)), size = 0.7 * nrow(df))
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Train the model using Random Forest

# mtry <- floor(sqrt(ncol(train_data))) - determining mtry parameter

model_rf <- randomForest(Sales ~ . - Influencer - `Social Media`, data = train_data, ntree = 500, mtry = 2)

# Make predictions using the test data
y_pred_rf <- predict(model_rf, test_data)

# Evaluate the model's performance using R-squared and RMSE
library(caret)
# calculate R-squared value and RMSE
r2 <- R2(y_pred_rf, test_data$Sales)
rmse <- RMSE(y_pred_rf, test_data$Sales)

# print the results
print(paste0("R-squared: ", r2))
print(paste0("RMSE: ", rmse))

#train the model using a decision tree

# Load the required libraries
library(rpart)
library(rpart.plot)

# Split the data into training and test sets
set.seed(123)
train_index <- sample(seq_len(nrow(df)), size = 0.7 * nrow(df))
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

dt_regressor <- rpart(Sales ~ . - Influencer - `Social Media`, data = train_data)

# Plot the decision tree
rpart.plot(dt_regressor)

summary(dt_regressor)


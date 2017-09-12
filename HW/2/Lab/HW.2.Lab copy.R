---
title: "HW #2"
output: html_notebook
---
**Question 1**
* SVM and KNN models with cross validation and train/test/validation splits*


Loading the data and required packages

```{r}
require("kernlab")
require("kknn")

df <- read.table("https://d37djvu3ytnwxt.cloudfront.net/assets/courseware/v1/39b78ff5c5c28981f009b54831d81649/asset-v1:GTx+ISYE6501x+2T2017+type@asset+block/credit_card_data-headers.txt", header = TRUE)

print(dim(df))
head(df)
```
To compare multiple models, the data will be split into 3 sets: training (to fit the model), validation (to compare different models) and test (to compare between the best models evulated from the validation set). 

The dataset will be split into these parts as such:

**Training** - *60%*

**Validation** - *20%*

**Test** - *20*

```{r}
train_test_val_split <- function(df, train_split = .7, val_split = 0, test_split = .3){
  # Split a dataframe/matrix into training, validation and test splits
  
  # Args
  # df - dataframe/matrix - the orginal dataset to split
  # train_split - float - the percentage (as a decimal) of the df to retain as a training set.
  #                       Defaults to 70%
  # val_split - float - The percentage (as a decimal) of the df to retain as a validation set.
  #                     Defaults to 0%
  # test_split - float - The percentage (as a decimal) of the df to retain as a test set.
  #                     Defaults to 30%
  
  
  # Shuffling the rows of the dataframe to randomize before splitting
  df <- df[sample(nrow(df)),]
  
  
  # Forking into an if/else depending on whether a validation set is desired
  if (val_split != 0){
    # Finding the indexes to split our df at 
    train_split_end_index <- round(nrow(df)*train_split)
    
    test_split_start_index <- train_split_end_index + 1
    
    test_split_end_index <- test_split_start_index + round(nrow(df)*test_split)
    
    validation_split_start_index <- test_split_end_index + 1
    
    validation_split_end_index <- nrow(df)
    
    # Creating our df splits
    train <- df[0:train_split_end_index,]
    
    test <- df[test_split_start_index:test_split_end_index,]
    
    validation <- df[validation_split_start_index:validation_split_end_index,]
    
    # Must wrap our df in a list as R doesn't allow multiple values to be returned
    output <- list(train, test, validation)
  } else {
    # Finding the indexes to split our df at 
    train_split_end_index <- round(nrow(df)*train_split)
    
    test_split_start_index <- train_split_end_index + 1
    
    test_split_end_index <- nrow(df)
    
    # Creating our df splits
    train <- df[0:train_split_end_index,]
    
    test <- df[test_split_start_index:test_split_end_index,]
    
    # Must wrap our df in a list as R doesn't allow multiple values to be returned
    output <- list(train, test)
  }
  
  return(output)
}
```
Testing the function
```{r}
splits = train_test_val_split(df, train_split = .6, val_split = .2, test_split = .2)
print(class(splits[[1]]) )
print(dim(splits[[1]]))
print(class(splits[[2]]) )
print(dim(splits[[2]]))
print(class(splits[[3]]) )
print(dim(splits[[3]]))

splits = train_test_val_split(df, train_split = .6, test_split = .2)
print(class(splits[[1]]) )
print(dim(splits[[1]]))
print(class(splits[[2]]) )
print(dim(splits[[2]]))

# There shouldn't be a validation split in this instance
#print(class(splits[[3]]) )
#print(dim(splits[[3]]))
```
Creating dataframes for training, validation and test sets
**Note to grader**
Because my train/test/validation function shuffles the df prior to splitting, the exact accuracy and potentially parameters of the following models may not exactly match when you run this code.
```{r}
splits = train_test_val_split(df, train_split = .6, val_split = .2, test_split = .2)

# Our Splits from indexing the returned list
train <- splits[[1]]
test <- splits[[2]]
validation <- splits[[3]]

# Sanity Check
print(class(splits[[1]]) )
print(dim(splits[[1]]))
print(class(splits[[2]]) )
print(dim(splits[[2]]))
print(class(splits[[3]]) )
print(dim(splits[[3]]))
```
Now to train and test various models on the validation set
```{r}
# One potential model
# This is looking through many different kernsl and has a max k value of 250

# train.knn automatically removes each data point when predicting its response variable
# This avoids the problem of using data observation as a neighbour of itself when predicting 

# Hypothetically, this should come up with our best parameters and accuracy on the train and test set
# However, this could be a recipe for overfitting

knn_model <- train.kknn(R1 ~ ., 
                        # Train on training set
                        data = train, 
                        # Test on validation set
                        test= validation, 
                        # Use 5 fold cross validation
                        kcv = 5,
                        # Scale our data
                        scaled=TRUE, 
                        # Try multiple k values with the max being 100
                        kmax = 100,
                        # Try multiple kernels
                        kernel = c("optimal", "triangular", "rectangular",
           "epanechnikov","cos","inv", "gaussian","triweight","biweight"))
# Our model summary
summary(knn_model)

```
The best model (on my splits) uses a k of 20 and the 'inv' kernel.

Testing on the validation set
```{r}
# Predicting on our validation set
pred_knn <- round(predict(knn_model, validation[,1:10]))
print("Prediction % =")
# Getting our simple accuracy - correct predictions/total predictions
print(sum(pred_knn == validation[,11]) / nrow(validation))

```
Let's build an SVM model
```{r}
# ksvm requires matrices as inputs
# Transforming our data splits into matrices
ksvm_train <- as.matrix(train)
ksvm_validation <- as.matrix(validation)
ksvm_test <- as.matrix(test)
```
Training a model
```{r}
# Training a ksvm model with cross-validation enabled
ksvm_model <- ksvm(
                  # Our data to train on (all variables)  
                  ksvm_train[,1:10], 
                  # Our target variable
                  ksvm_train[,11], 
                  # 5 fold cross validation
                  cross=5, 
                  # A classification SVM
                  type="C-svc", 
                  # Linear kernel (the best one I found after trying many)
                  kernel= "vanilladot", 
                  # Default C value
                  C=1, 
                  # Scaled data
                  scaled=TRUE)

# predicitions using the svm model on the dataset
pred_svm <- predict(ksvm_model, ksvm_validation[,1:10])

accuracy <- sum(pred_svm == validation[,11])/nrow(ksvm_validation)
print("Accuracy:")
print(round(accuracy, 6))
```
After trying multiple models the vanilladot, polydot, and laplace all returned an accuracy of roughly 87% (on my splits). For simplicity sake, I'll stick with the vanilladot linear kernel.

Both models scored similar accuracy. Let's see which one performs better on the test (hold out) set
```{r}
# Predictions with our SVM model on the test set
pred_svm_test <- predict(ksvm_model, ksvm_test[,1:10])

accuracy <- sum(pred_svm_test == test[,11])/nrow(ksvm_validation)
print("Accuracy for SVM:")
print(round(accuracy, 6))

# Predictions with our knn model on the test set
pred_knn_test <- round(predict(knn_model, test[,1:10]))
print("Accuracy for KNN:")
# Getting our simple accuracy - correct predictions/total predictions
print(sum(pred_knn_test == test[,11]) / nrow(test))
```
SVM outperformed the KNN model with a 90% accuracy (on my splits) vs. 83%. 90% is great and 83% is not bad either. In this case it would be worth digging a bit deeper into other metrics (confusion matrix, precision, recall, etc) and determine which model is better for the bank's goals. All things held equal and looking solely at accuracy of predictions, the SVM model outperforms the KNN model.


**Question 2**
*Real Life Clustering Situation*

In my job I've used different clustering algorithms on text documents to try and find latent topics within them. A common example of this is the Reuters news dataset, which you can cluster into seperate topics like 'news', 'international', 'financial' clusters among others.

In my own life I could cluster on something like drives from my home. Some of the predictors could be travel time, day of the week, time of day, direction, and average speed. The day could be clustered around these variables and seperate into clusters like 'daily commute', 'errands', 'weekend trip' etc.

**Question 3**
*K-Means on the Iris dataset*

Loading the dataset
```{r}
url <- "https://d37djvu3ytnwxt.cloudfront.net/assets/courseware/v1/26886db51f665dbde534f8c6326694b5/asset-v1:GTx+ISYE6501x+3T2017+type@asset+block/iris.txt"

df <- read.table(url, header = TRUE)

head(df)
df$Species
```
To find the best k means:
Train/test split the data
Combinations of the x variables (optional)
For each combination (start with all 4):
  Loop through k values and record accuracy for predicting species
  for k in range (1,50):
  
K means them 
Splitting data
```{r}
kmeans_splits = train_test_val_split(df, train_split = .7, val_split = 0, test_split = 3)

# Our Splits from indexing the returned list
kmeans_train <- splits[[1]]
kmeans_test <- splits[[2]]
```

```{r}
set.seed(1)
kmeans_cls <- kmeans(
                    # Only using Petal Length and Width Variables  
                    df[,3:4], 
                    # 3 clusters to match number of species
                    centers = 3, 
                    # To ensure convergence
                    iter.max = 100, 
                    # Number of random samples to use as starting points
                    nstart = 50)
#kmeans.predict(kmeans_cls, kmeans_test[,1:4])

# A table showing our clusters and how the species fall into them
table(kmeans_cls$cluster, df$Species)

# Our clusters (x) as predictions of Species (y)
x <- kmeans_cls$cluster
y <- as.numeric(df$Species)
class(y)
correct_pred <- sum(x==y)
total_pred <- nrow(df)
acc <- correct_pred/total_pred
acc

# Another way of measuring accuracy
correct_predictions <- nrow(df) - 6 
accuracy <- correct_predictions/nrow(df)
accuracy

# Accuracy of these clusters is 96% for correctly predicting flower species
# Best variables are Petal length and width
# Number of clusters is 3 (the number of species)

# Plotting our data and clusters
plot(df[,3:4], col =(kmeans_cls$cluster +1) , main="K-Means result with 3 clusters", pch=20, cex=2)
```
```{r}

x <- fitted(kmeans_cls, method="classes")
class(x)
#x <- kmeans_cls$cluster
y <- as.numeric(df$Species)
class(y)
correct_pred <- sum(x==y)
total_pred <- nrow(df)
acc <- correct_pred/total_pred
acc
plot(x,y)
attributes(kmeans_cls)
#plot(x, col=kmeans_cls$cluster)
#print(y)
```

So if I'm understanding this right, I want to figure out the kmeans clusters and have these clusters predict the species. How do I do that? Make a numeric version of the species (e.g. versicola = 3) and match the predicted cluster to the species?

Or am I running k-means to find centroids and then using the centroids to predict the species?

Figuring out how to use kmeans function
```{r}
#Elbow Method for finding the optimal number of clusters
#set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- df[3:4]
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
```


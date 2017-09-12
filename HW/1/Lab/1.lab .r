
# Loading kernlab package
require(kernlab)

# Reading in data from txt file URL
data <- read.table("https://d37djvu3ytnwxt.cloudfront.net/assets/courseware/v1/39b78ff5c5c28981f009b54831d81649/asset-v1:GTx+ISYE6501x+2T2017+type@asset+block/credit_card_data-headers.txt", header = TRUE)

# Transforming dataframe to matrix
data <- as.matrix(data)

# Fitting a model to the data
# Classifer model with Linear kernel
ksvm(data[ , 1:10], data[,11], type="C-svc", kernel= "vanilladot", C=.01, scaled=TRUE)

# Fitting a model to the data
# Classifer model with Linear kernel
ksvm(data[ , 1:10], data[,11], type="C-svc", kernel= "vanilladot", C=.1, scaled=TRUE)

# Fitting a model to the data
# Classifer model with Linear kernel
ksvm(data[ , 1:10], data[,11], type="C-svc", kernel= "vanilladot", C=1, scaled=TRUE)

# Fitting a model to the data
# Classifer model with Linear kernel
ksvm(data[ , 1:10], data[,11], type="C-svc", kernel= "vanilladot", C=10, scaled=TRUE)

# Fitting a model to the data
# Classifer model with Linear kernel
ksvm(data[ , 1:10], data[,11], type="C-svc", kernel= "vanilladot", C=100, scaled=TRUE)

# Fitting a model to the data
# Classifer model with Linear kernel
ksvm(data[ , 1:10], data[,11], type="C-svc", kernel= "vanilladot", C=500, scaled=TRUE)

model <- ksvm(data[ , 1:10], data[,11], type="C-svc", kernel= "vanilladot", C=1, scaled=TRUE)

# Calculating our coefficients coefficients 
a <- colSums(data[model@SVindex,1:10] * model@coef[[1]])
print("a =")
print(a)


# Calculating a0
a0 <- sum(a*data[1,1:10]) - model@b
print("a0 =")
print(a0)

# predicitions using the svm model on the dataset
pred <- predict(model, data[,1:10])

print("pred =")
print(pred)

# Counting up the matches between what our model predicted and the correct label
# Our simple accuracy (correct predictions/total observations)

print("Prediction % =")
print(sum(pred == data[,11]) / nrow(data))

# kknn package
require(kknn)

# kknn requires data as a dataframe and not matrix
# Reloading data as a df
df <- read.table("https://d37djvu3ytnwxt.cloudfront.net/assets/courseware/v1/39b78ff5c5c28981f009b54831d81649/asset-v1:GTx+ISYE6501x+2T2017+type@asset+block/credit_card_data-headers.txt", header = TRUE)

train.kknn(R1~., kmax=100, df, scale=TRUE)

# Training a k-nearest neighbours on the whole dataset (no train/test/validation split)
# Scaled data with a max k value of 100 testing a variety of kernel options

knn_model <- train.kknn(R1 ~ ., data = df, scaled=TRUE, kmax = 100,
           kernel = c("optimal", "triangular", "rectangular",
           "epanechnikov","cos","inv","gaussian","triweight","biweight"))

summary(knn_model)

?kknn

# load
data(ionosphere)

# set to fit model
ionosphere.learn <- ionosphere[1:200,]
dim(ionosphere.learn)

# Set to validate model
ionosphere.valid <- ionosphere[-c(1:200),]
dim(ionosphere.valid)

# fitting model
fit.kknn <- kknn(class ~ ., ionosphere.learn, ionosphere.valid)

#Confustion matrix?
table(ionosphere.valid$class, fit.kknn$fit)

table(ionosphere.valid$class, fit.kknn$fit)

?fit.kknn

# Model # 1
(fit.train1 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15, kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 1))

# Confusion Matrix
table(predict(fit.train1, ionosphere.valid), ionosphere.valid$class)

#Model #2

(fit.train2 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15, kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 2))

# Confusion Matrix
table(predict(fit.train2, ionosphere.valid), ionosphere.valid$class)



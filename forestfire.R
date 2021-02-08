forestfire<-read.csv(file.choose())
View(forestfire)
table(forestfire$area)
forestfire1<-forestfire[,-c(1,2,31)]
View(forestfire1)
forestfire1_train <- forestfire[1:361,]
forestfire1_test  <- forestfire[362:517,]

##Training a model on the data ----
# begin by training a simple linear SVM
library(kernlab)
install.packages()
library(caret)
forestfire_model1<- ksvm(area ~ ., data = forestfire1_train,
                         kernel = "vanilladot")

?ksvm 
help(kvsm)
#different types of kernel
#'rbfdot',polydot',tanhdot','vanilladot','laplacedot',besseldot',anovadot',splinedot','matrixdot'
# basic information about the model
forestfire_model1

## Evaluating model performance ----
# predictions on testing dataset
forestfire_pred <- predict(forestfire_model1, forestfire1_test)

head(forestfire_pred)

table(forestfire1_pred, forestfire1_test$area)


agreement <- forestfire_pred==forestfire_test$area
table(agreement)
prop.table(table(agreement))
mean(forestfire_pred==forestfire_test$area)

## Improving model performance ----
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)

agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))
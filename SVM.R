#####Support Vector Machines -------------------
#For SVM all the features must be in numeric
#all the feature values should be in same range, if not we shd normalise
#SVM model will perform rescalling automatically
##  Optical Character Recognition ----
#load letterdata as letters
# divide into training and test data
letters<-read.csv(file.choose())
View(letters)
table(letters$letter)
letters_train <- letters[1:16000, ]
letters_test  <- letters[16001:20000, ]

##Training a model on the data ----
# begin by training a simple linear SVM
library(kernlab)
install.packages()
library(caret)
letter_classifier<- ksvm(letter ~ ., data = letters_train,
                          kernel = "vanilladot")

?ksvm 
help(kvsm)
#different types of kernel
#'rbfdot',polydot',tanhdot','vanilladot','laplacedot',besseldot',anovadot',splinedot','matrixdot'
# basic information about the model
letter_classifier

## Evaluating model performance ----
# predictions on testing dataset
letter_predictions <- predict(letter_classifier, letters_test)

head(letter_predictions)

table(letter_predictions, letters_test$letter)


agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))
mean(letter_predictions==letters_test$letter)

## Improving model performance ----
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)

agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))
?ksvm

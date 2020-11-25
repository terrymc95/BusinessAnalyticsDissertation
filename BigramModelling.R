### Loading Data ############################################################
data <- read.csv(paste0(getwd(),"/TwitterData/SampleData.csv"))
data$text <- as.character(data$text) ##  Changing to character
levels(data$Classification)[1] <- "Y" ## Changing Hate to value Y
levels(data$Classification)[2] <- "N" ## Changing Neutral to value N
data$ID <- 1:nrow(data)
### Creating DTM ############################################################
TextBigram <- data %>% 
                unnest_tokens(bigram, text, token = "ngrams", n = 2)
data_dtm <- TextBigram %>% 
                count(ID, bigram) %>% 
                cast_dtm(document = ID, term = bigram, value = n, weighting = 
                           tm::weightTfIdf)
data_dtm ## 55,466 bigrams need to reduce dimensionality

## Removing sparse bigrams
data_dtm <- removeSparseTerms(data_dtm, sparse = 0.99)
## This leaves 70 terms for prediction
### Structuring in data frame ##################################################
TermDataIDs <- data_dtm$dimnames$Docs
Termdf <- as.data.frame( as.matrix(data_dtm))
TermClass <- data %>% 
              filter(ID %in% TermDataIDs) %>% 
              select(Classification)

TermData <- data.frame(ID = TermDataIDs, Termdf, Classification = TermClass)

## creating train/test data 75/25 split 
set.seed(40126429)
trainID <- sample(TermData$ID, size = nrow(TermData)*0.75)
testID <- TermData %>% 
  filter(!ID %in% trainID) %>% 
  select(ID)
testID <- testID$ID

## creating training and testing datasets
trainData <- TermData %>% 
  filter(ID %in% trainID)
testData <- TermData %>% 
  filter(ID %in% testID)
## Saving Test and training data for evaluating models
write.csv(trainData, paste0(getwd(),"/twitterData/ BigramTrain.csv"), row.names = F)
write.csv(testData, paste0(getwd(),"/twitterData/ BigramTest.csv"), row.names = F)
### Building CV Settings #######################################################
fitControl<- trainControl(
  method = "repeatedcv",
  number=10,
  repeats=10,
  classProbs = T,
  savePredictions = T
)
fitControl2<- trainControl(
  method = "repeatedcv",
  number=10,
  repeats=10
)
set.seed(40126429) ## this is needed for reproducibility
### SVM Model ##############################################################
tic();cv_train_svm <- train(x = as.matrix(trainData[2:71]),
                      y= trainData$Classification, method = "svmLinear2",
                      trControl = fitControl); toc();
cv_train_svm
## Saving Model 
saveRDS(cv_train_svm, file = paste0(getwd(), "/Models/BigramModels/SVM_Model.RDS"))
### Random Forest ###########################################################
rangerGrid <- expand.grid(.mtry = c(2:10), .splitrule = "gini",
                          .min.node.size = c(1:20))
## this takes 1hrs 23 mins to run 
tic();cv_train_rf <- train(x = as.matrix(trainData[2:71]),
                           y = trainData$Classification, method = "ranger",
                           num.trees = 200, trControl = fitControl, tuneGrid 
                           = rangerGrid); toc();
cv_train_rf
## Save Model
saveRDS(cv_train_rf, paste0(getwd(),"/Models/BigramModels/rf_model.RDS"))
### Gradient Boosting ########################################################

gbmGrid <- expand.grid(interaction.depth = 3, n.trees = (0:50)*50, shrinkage = 0.1, 
                       n.minobsinnode = 10)
## 26mins
tic();cv_train_gbm <- train(x = as.matrix(trainData[2:71]), y = trainData$Classification,
                         method = "gbm", trControl = fitControl, 
                         tuneGrid = gbmGrid,  
                         verbose = F);toc();
cv_train_gbm
## Save Model
saveRDS(cv_train_gbm, paste0(getwd(), "/Models/BigramModels/gbm_Model.rds"))
### Extreme Gradient Boosting #################################################
xgbGrid <- expand.grid(nrounds = c(2:10)*50, max_depth = c(1:5), eta = 0.3,
                       gamma = 0, colsample_bytree = 0.6, min_child_weight = 1,
                       subsample = c(0.5,0.75,1))
tic();cv_xgb_train_2 <- train(x = as.matrix(trainData[2:71]), 
                              y = trainData$Classification,
                              method = "xgbTree", trControl = fitControl,
                              tuneGrid = xgbGrid,
                              verbose = F);toc()
cv_xgb_train_2
## Saving Model 
saveRDS(cv_xgb_train_2, paste0(getwd(),"/Models/BigramModels/cv_xgb_train_2.rds"))

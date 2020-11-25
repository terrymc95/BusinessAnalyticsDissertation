### Loading Data ############################################################
data <- read.csv(paste0(getwd(),"/TwitterData/SampleData.csv"))
data$text <- as.character(data$text) ##  Changing to character
levels(data$Classification)[1] <- 1 ## Changing Hate to value 1
levels(data$Classification)[2] <- 0 ## Changing Neutral to value 0
data$ID <- 1:nrow(data)
### Creating TermMatrix ######################################################
## Need to unnest tokens 
TextToken <- data %>% 
                unnest_tokens(word, text) %>% 
                anti_join(stop_words)
data_dtm <- TextToken %>% 
                count(ID, word) %>% 
                cast_dtm(document = ID, term = word, value = n, weighting = 
                           tm::weightTfIdf)
data_dtm
## Need to remove Sparse features to reduce dimensionality 
data_dtm <- removeSparseTerms(data_dtm, sparse = 0.99) ## This means that a term
## needs to make up at least 1% of all the tokens or it will be removed 

## Some tweets will be lost due to the tokenisation and sparsity so need to correct data

data_slice <- slice(data, as.numeric(data_dtm$dimnames$Docs))

### Creating A DTM for the unseen data for future testing of models#############
unseen <- read.csv(paste0(getwd(),"/TwitterData/RemainingData.csv"))
unseen$text <- as.character(unseen$text)
levels(unseen$Classification)[1] <- 1 ## Changing Hate to value 1
levels(unseen$Classification)[2] <- 0 ## Changing Neutral to value 0
unseen$ID <- 1:nrow(unseen)
control <- Terms(data_dtm)
unseenToken <- unseen %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words)
unseen_DTM <- unseenToken %>% 
  count(ID,word) %>% 
  cast_dtm(document = ID, term = word, value = n, weighting = tm::weightTfIdf)
## create DF
unseenID <- unseen_DTM$dimnames$Docs
unseendtm.df <- as.data.frame(as.matrix(unseen_DTM))
unseenClass <- unseen %>% 
  filter(ID %in% unseenID) %>% 
  select(Classification)
unseenSparse <- unseendtm.df %>% 
  select(!!control)
UnseenData <- data.frame(ID = unseenID, unseenSparse, Classification = unseenClass)
write.csv(UnseenData, paste0(getwd(),"/TwitterData/UnSeenDTM.csv", row.names = F))
### Test/Train Model #########################################################
## Making the term matrix into a dataframe 
TermDataIDs <- data_dtm$dimnames$Docs
TermMat <- as.matrix(data_dtm)
Termdf <- as.data.frame(TermMat)
TermClassification <- data_slice$Classification
TermData <- data.frame(ID = TermDataIDs, Termdf, Classification = TermClassification)
## Creating a train/test data 75/25 split
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
## Saving Data to file 
write.csv(trainData, paste0(getwd(),"/TwitterData/TrainData.csv"), row.names = F)
write.csv(testData, paste0(getwd(),"/TwitterData/TestData.csv"), row.names = F)
### Building cross validation settings ########################################
## K-fold cross validation where K = 10
fitControl<- trainControl(
  method = "repeatedcv",
  number=10,
  repeats=10)

fitControl2<- trainControl( ## this is to save probabilities and predictions
                          		        ## for producing ROC curve 
  method = "repeatedcv",
  number=10,
  repeats=10, 
  classProbs = TRUE, 
  savePredictions = TRUE)
set.seed(40126429) ## this is needed for reproducibility
### SVM CV Model ###########################################################
svmOptimal <- expand.grid(cost = 0.5)
tic(); SVM_named_class <- train(x = as.matrix(trainData[2:154]),
                  		      	y= namedClass_train, method = "svmLinear2",
                         			trControl = fitControl2, tuneGrid = svmOptimal);toc();
## Saving Model 
saveRDS(SVM_named_class, file = paste0(getwd(), "/Models/SingleTerm/CV_Train_SVM.RDS"))
### CV Random Forest ########################################################
rf_optimal <- expand.grid(.mtry = 5, .splitrule = "gini", .min.node.size = 7)

tic();rf_named_class <- train(x = as.matrix(trainData[2:154]), y = namedClass_train, 
                        method = "ranger",num.trees = 200, trControl = fitControl2, tuneGrid = 
                        rf_optimal);toc();
## Save Model
saveRDS(rf_named_class, paste0(getwd(),"/Models/SingleTerm/RF_named_class.RDS"))
### CV Gradient Boosted Machine ##########################################
optimal_gbm <- expand.grid(n.trees=150, interaction.depth = 3, shrinkage = 0.1,
                           n.minobsinnode = 10)
tic();gbm_optimal_model <- train(x = as.matrix((trainData[2:154])), y = namedClass_train, 
                           method = "gbm", trControl = fitControl2, tuneGrid =
                           optimal_gbm, verbose = F);toc();
## Save Model
saveRDS(gbm_optimal_model, paste0(getwd(), "/Models/SingleTerm/Optimal_GBM.rds"))
### CV Extreme gradient Boosting ###############################################
xgb_optimal <- expand.grid(nrounds = 150, max_depth = 2, eta = 0.3, gamma = 0,  colsample_bytree = 0.6, min_child_weight = 1, subsample = 1)


tic();xgb_optimal_model <- train(x = as.matrix(trainData[2:154]), 
                           y = namedClass_train,
                           method = "xgbTree", trControl = fitControl2,
                           tuneGrid = xgb_optimal,
                           verbose = F);toc()
## Saving Model 
saveRDS(xgb_optimal_model, paste0(getwd(),"/Models/SingleTerm/cv_xgb_optimal.rds"))

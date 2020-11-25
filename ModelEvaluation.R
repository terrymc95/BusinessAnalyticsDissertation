### Loading Models ##########################################################
svm <- readRDS(paste0(getwd(),"/Models/SingleTerm/cv_Train_SVM.RDS"))
rf <- readRDS(paste0(getwd(),"/Models/SingleTerm/rf_named_class.RDS"))
gbm <- readRDS(paste0(getwd(), "/Models/SingleTerm/Optimal_GBM.rds"))
xgb <- readRDS(paste0(getwd(), "/Models/SingleTerm/cv_xgb_optimal.rds"))
svm_bi <- readRDS(paste0(getwd(),"/Models/BigramModels/SVM_Model.rds"))
rf_bi  <- readRDS(paste0(getwd(),"/Models/BigramModels/rf_model.rds"))
gbm_bi <- readRDS(paste0(getwd(),"/Models/BigramModels/gbm_model.rds"))
xgb_bi <- readRDS(paste0(getwd(),"/Models/BigramModels/cv_xgb_train_2.rds"))
### Loading Unigram Testing Data ###############################################
trainData <- read.csv(paste0(getwd(),"/TwitterData/trainData.csv"))
trainData$Classification <- as.factor(trainData$Classification)
levels(trainData$Classification)[1] <- "N"
levels(trainData$Classification)[2]<- "Y"
## Reordering so they are the same as the models
trainData$Classification <- factor(trainData$Classification, levels = c("Y","N"))
testData <- read.csv(paste0(getwd(),"/TwitterData/testData.csv"))
testData$Classification <- as.factor(testData$Classification)
levels(testData$Classification)[1] <- "N"
levels(testData$Classification)[2]<- "Y"
testData$Classification <- factor(testData$Classification, levels = c("Y","N"))
### Loading Unseen Data ######################################################
UnseenData <- read.csv(paste0(getwd(),"/TwitterData/UnSeenDTM.csv"))
UnseenData$Classification <- as.factor(UnseenData$Classification)
levels(UnseenData$Classification)[1] <- "N"
levels(UnseenData$Classification)[2] <- "Y"
UnseenData$Classification <- factor(UnseenData$Classification, levels = c("Y","N"))
### Loading Bigram Testing Data ################################################
biTrain <-  read.csv(paste0(getwd(), "/twitterData/ BigramTrain.csv"))
biTest <- read.csv(paste0(getwd(), "/twitterData/ BigramTest.csv"))
biTrain$Classification <- factor(biTrain$Classification, levels = c("Y","N"))
biTest$Classification <- factor(biTest$Classification, levels = c("Y","N"))
### Confusion Matrices #######################################################
svm_pred <- predict(svm, newdata = testData)
svm_CM <- confusionMatrix(svm_pred, testData$Classification, mode = "everything")
svm_CM$overall[1:2];svm_CM$byClass[5:7]

rf_pred <- predict(rf, newdata = testData)
rf_CM <- confusionMatrix(rf_pred, testData$Classification, mode = "everything")
rf_CM$overall[1:2];rf_CM$byClass[5:7]

gbm_pred <- predict(gbm, newdata = testData)
gbm_CM <- confusionMatrix(gbm_pred, testData$Classification, mode = "everything")
gbm_CM$overall[1:2];gbm_CM$byClass[5:7]

xgb_pred <- predict(xgb, newdata = testData)
xgb_CM <- confusionMatrix(xgb_pred, testData$Classification, mode = "everything")
xgb_CM$overall[1:2];xgb_CM$byClass[5:7]

x <- data.frame(SVM = c(svm_CM$overall[1:2],svm_CM$byClass[5:7]), 
                 RF = c( rf_CM$overall[1:2], rf_CM$byClass[5:7]),
                GBM = c(gbm_CM$overall[1:2],gbm_CM$byClass[5:7]),
                XGB = c(xgb_CM$overall[1:2],xgb_CM$byClass[5:7]))
x
write.csv(x, paste0(getwd(),"/Models/SingleTerm/UnigramModelStats.csv"))
### Evaluating Model Performance ##############################################
svm_eval <- evalm(svm)
svm_plot <- svm_eval$roc
svm_auc <- svm_eval$stdres$`Group 1`$Score[13]
svm_plot+theme(text = element_text(size = 20),
           axis.text.x = element_text(angle=0, hjust = 0.5))+
  xlab("False-Positive Rate")+ylab("True-Positive Rate")+ 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  geom_text(aes(x = 0.75, y = 0.1, label = paste("SVM AUC = ",svm_auc) ),
            colour = "black", size = 10)+
  theme(legend.position = "none")
svm_plot_final <- last_plot()



rf_eval <- evalm(rf)
rf_auc <- rf_eval$stdres$`Group 1`$Score[13]
rf_plot <- evalm(rf)$roc
rf_plot+theme(text = element_text(size = 20),
               axis.text.x = element_text(angle=0, hjust = 0.5))+
  xlab("False-Positive Rate")+ylab("True-Positive Rate")+ 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  geom_text(aes(x = 0.75, y = 0.1, label = paste("RF AUC = ",rf_auc)),colour = "black", size = 10)+
  theme(legend.position = "none")
rf_plot_final <- last_plot()


gbm_eval <- evalm(gbm)
gbm_auc <- gbm_eval$stdres$`Group 1`$Score[13]
gbm_plot <- gbm_eval$roc

gbm_plot+theme(text = element_text(size = 20),
               axis.text.x = element_text(angle=0, hjust = 0.5))+
  xlab("False-Positive Rate")+ylab("True-Positive Rate")+ 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  geom_text(aes(x = 0.75, y = 0.1, label = paste("GBM AUC = ",gbm_auc))
            ,colour = "black", size = 10)+
  theme(legend.position = "none")
gbm_plot_final <- last_plot()

xgb_eval <- evalm(xgb)
xgb_auc <- xgb_eval$stdres$`Group 1`$Score[13]
xgb_plot <- xgb_eval$roc
xgb_plot+theme(text = element_text(size = 20),
               axis.text.x = element_text(angle=0, hjust = 0.5))+
  xlab("False-Positive Rate")+ylab("True-Positive Rate")+ 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  geom_text(aes(x = 0.75, y = 0.1, label = paste("XGB AUC = ",xgb_auc))
            ,colour = "black", size = 10)+
  theme(legend.position = "none")
xgb_plot_final <- last_plot()
evalm(list(svm, gbm,xgb,rf), gnames = c("SVM", "Gradient Boost", "Extreme Gradient Boost", "Random Forest"))$roc
all_plot <- last_plot()

all_plot+theme(text = element_text(size = 30),
               axis.text.x = element_text(angle=0, hjust = 0.5, size = 15))+
  xlab("False-Positive Rate")+ylab("True-Positive Rate")+ 
  theme(axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  theme(legend.position = c(0.75, 0.3), legend.title = element_text(size = 30))

ggsave(paste0(getwd(),"/Models/SingleTerm/Plots/svm_roc.png"),plot =svm_plot_final ,
       width = 16, height = 13.14)
ggsave(paste0(getwd(),"/Models/SingleTerm/Plots/rf_roc.png"),plot = rf_plot_final,
       width = 16, height = 13.14)
ggsave(paste0(getwd(),"/Models/SingleTerm/Plots/gbm_roc.png"),plot = gbm_plot_final,
       width = 16, height = 13.14)
ggsave(paste0(getwd(),"/Models/SingleTerm/Plots/xgb_roc.png"),plot = xgb_plot_final,
       width = 16, height = 13.14)
ggsave(paste0(getwd(),"/Models/SingleTerm/Plots/All.png"),plot = all_plot,
       width = 16, height = 13.14)
### Bigram Confusion Matricies #################################################
svm_bi_pred <- predict(svm_bi, newdata = biTest)
svm_bi_CM <- confusionMatrix(svm_bi_pred, biTest$Classification, mode = "everything")
svm_bi_CM$overall[1:2];svm_bi_CM$byClass[5:7]

rf_bi_pred <- predict(rf_bi, newdata = biTest)


rf_bi_CM <- confusionMatrix(rf_bi_pred, biTest$Classification, mode = "everything")

rf_bi_CM$overall[1:2];rf_bi_CM$byClass[5:7]

gbm_bi_pred <- predict(gbm_bi, newdata = biTest)

gbm_bi_CM <- confusionMatrix(gbm_bi_pred, biTest$Classification, mode = "everything")

gbm_bi_CM$overall[1:2];gbm_bi_CM$byClass[5:7]

xgb_bi_pred <- predict(xgb_bi, newdata = biTest)

xgb_bi_CM <- confusionMatrix(xgb_bi_pred, biTest$Classification, mode = "everything")

xgb_bi_CM$overall[1:2];xgb_bi_CM$byClass[5:7]


y <- data.frame(SVM = c(svm_bi_CM$byClass[5:7], svm_bi_CM$overall[1:2]), 
                RF = c( rf_bi_CM$byClass[5:7],  rf_bi_CM$overall[1:2]),
                GBM = c(gbm_bi_CM$byClass[5:7], gbm_bi_CM$overall[1:2]),
                XGB = c(xgb_bi_CM$byClass[5:7], xgb_bi_CM$overall[1:2]))
y

write.csv(y, paste0(getwd(),"/Models/BigramModels/BigramModelStats.csv"))
### Bigram Comparison #######################################################
svm_compare <- evalm(list(svm_bi,svm), gnames = c("Bigram", "Unigram"))
svm_bigram_auc <- svm_compare$stdres$Bigram$Score[13]
svm_comp_plot <- svm_compare$roc
svm_comp_plot+theme(text = element_text(size = 20),
                       axis.text.x = element_text(angle=0, hjust = 0.5))+
  xlab("False-Positive Rate")+ylab("True-Positive Rate")+ 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))
svm_compare_plot <- last_plot()

rf_compare <- evalm(list(rf_bi,rf), gnames = c("Bigram", "Unigram"))
rf_bigram_auc <- rf_compare$stdres$Bigram$Score[13]
rf_compare_plot <- rf_compare$roc
rf_compare_plot+theme(text = element_text(size = 20),
                      axis.text.x = element_text(angle=0, hjust = 0.5))+
  xlab("False-Positive Rate")+ylab("True-Positive Rate")+ 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))
rf_comp_plot <- last_plot()

gbm_compare <- evalm(list(gbm_bi,gbm), gnames = c("Bigram", "Unigram"))
gbm_bigram_auc <- gbm_compare$stdres$Bigram$Score[13]
gbm_compare_plot <- gbm_compare$roc
gbm_compare_plot+theme(text = element_text(size = 20),
                       axis.text.x = element_text(angle=0, hjust = 0.5))+
  xlab("False-Positive Rate")+ylab("True-Positive Rate")+ 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))
gbm_comp_plot <- last_plot()
xgb_compare <- evalm(list(xgb_bi,xgb), gnames = c("Bigram", "Unigram"))
xgb_bigram_auc <- xgb_compare$stdres$Bigram$Score[13]
xgb_compare_plot <- xgb_compare$roc
xgb_compare_plot+theme(text = element_text(size = 20),
                       axis.text.x = element_text(angle=0, hjust = 0.5))+
  xlab("False-Positive Rate")+ylab("True-Positive Rate")+ 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))
xgb_comp_plot <- last_plot()
allCompare <- evalm(list(svm,svm_bi,rf,rf_bi,gbm,gbm_bi,xgb,xgb_bi),
                    gnames = c("SVM", "SVM Bigram", "RF", "RF Bigram","GBM",
                               "GGM Bigram", "XGB", "XGB Bigram"))$roc
ggsave(paste0(getwd(),"/Models/BigramModels/Plots/svm_roc.png"),
       plot =svm_compare_plot ,width = 16, height = 13.14)
ggsave(paste0(getwd(),"/Models/BigramModels/Plots/rf_roc.png"),
       plot = rf_comp_plot,width = 16, height = 13.14)
ggsave(paste0(getwd(),"/Models/BigramModels/Plots/gbm_roc.png"),
       plot = gbm_comp_plot,width = 16, height = 13.14)
ggsave(paste0(getwd(),"/Models/BigramModels/Plots/xgb_roc.png"),
       plot = xgb_comp_plot,width = 16, height = 13.14)
ggsave(paste0(getwd(),"/Models/BigramModels/Plots/All.png"),
       plot = allCompare,width = 16, height = 13.14)
### Predictions with Unseen Data ###############################################
svm_unseen <- predict(svm, newdata = UnseenData)
 rf_unseen <- predict( rf, newdata = UnseenData)
gbm_unseen <- predict(gbm, newdata = UnseenData)
xgb_unseen <- predict(xgb, newdata = UnseenData)
CM_US_SVM <- confusionMatrix(svm_unseen, UnseenData$Classification, mode = "everything" )
CM_US_RF <- confusionMatrix( rf_unseen, UnseenData$Classification, mode = "everything" )
CM_US_GBM <- confusionMatrix(gbm_unseen, UnseenData$Classification, mode = "everything" )
CM_US_XGB <- confusionMatrix(xgb_unseen, UnseenData$Classification, mode = "everything" )
z <- data.frame(SVM = c(CM_US_SVM$overall[1:2], CM_US_SVM$byClass[5:7]),
                 RF = c( CM_US_RF$overall[1:2],  CM_US_RF$byClass[5:7]),
                GBM = c(CM_US_GBM$overall[1:2], CM_US_GBM$byClass[5:7]),
                XGB = c(CM_US_XGB$overall[1:2], CM_US_XGB$byClass[5:7]))
z
write.csv(z,paste0(getwd(),"/Models/SingleTerm/UnseenStats.csv")) 

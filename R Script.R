if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(smotefamily)) install.packages("smotefamily", repos = "http://cran.us.r-project.org")
if(!require(C50)) install.packages("C50", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(ROSE)) install.packages("ROSE", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(naivebayes)) install.packages("naivebayes", repos = "http://cran.us.r-project.org")
if(!require(rattle)) install.packages("rattle", repos = "http://cran.us.r-project.org")
if(!require(AUC)) install.packages("AUC", repos = "http://cran.us.r-project.org")
if(!require(ROCR)) install.packages("ROCR", repos = "http://cran.us.r-project.org")
if(!require(OneR)) install.packages("OneR", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(lubridate)
library(smotefamily)
library(C50)
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(ROSE)
library(randomForest)
library(xgboost)
library(e1071)
library(naivebayes)
library(rattle)
library(AUC)
library(ROCR)
library(OneR)
#read the csv, correct the Handcap column and then set it as logical
kaggle <- read_csv("KaggleV2-May-2016.csv",
                   col_types = cols(PatientId = col_character(),Alcoholism = col_logical(), 
                                    Diabetes = col_logical(),Hipertension = col_logical(), SMS_received = col_logical(), 
                                    Scholarship = col_logical()))
kaggle %>%
  mutate(Handcap, Handcap = ifelse(Handcap == 0, 0, 1))
kaggle$Handcap <- as.logical(kaggle$Handcap)
kaggle$Handcap <- as.numeric(kaggle$Handcap)
mean(kaggle$Handcap)
str(kaggle)
kaggle$Age <- abs(kaggle$Age)

#correct the column name to remove hyphen
names(kaggle)[14]<-"No_Show"

#mutate the no shows to a logical vector
kaggle <- kaggle %>%
  mutate(clean_No_Show = No_Show == "Yes")


#remove the old no show column and rename the new column
kaggle$No_Show <- NULL
names(kaggle)[14]<-"No_Show"

#split dates
kaggle$appointmentmonth <- month(ymd(kaggle$AppointmentDay))

#take distinct patient Ids and get their earliest appointment
distinctpatID <- kaggle %>%
  distinct(PatientId, .keep_all = TRUE)
mindates <- distinctpatID %>%
  group_by(PatientId) %>%
  summarise(mindate = min(AppointmentDay))

#join the distinct pateints back to the main table to end up with a distinct list of patients who have no-showed
mindates <- mindates %>%
  left_join(kaggle, by = c("PatientId" = "PatientId", "mindate" = "AppointmentDay")) %>%
  distinct(PatientId, .keep_all = TRUE) %>%
  mutate(previousnoshow = No_Show) %>%
  select(PatientId, mindate, previousnoshow)
mindates$No_Show <- NULL

#join it back to main table to give the clean dataset ready for analysis
kaggle <- kaggle %>%
  left_join(mindates)
kaggle <- kaggle %>%
  mutate(clean_previous = if_else(AppointmentDay == mindate, FALSE, previousnoshow))
kaggle$previousnoshow <- NULL
names(kaggle)[17]<-"Previous_No_Show"


#develop lead time from scheduling to actual appointment
kaggle$leadtime <- as.integer(abs(ceiling(difftime(ymd(kaggle$AppointmentDay), kaggle$ScheduledDay , units = "days"))))


#Clean Up for Modelling
kaggle$No_Show <- as.factor(kaggle$No_Show)


kaggle$rownum <- row(kaggle)
kaggle$Gender <- as.factor(kaggle$Gender)
kaggle$Previous_No_Show <- as.factor(kaggle$Previous_No_Show)
kaggle$Scholarship <- as.factor(kaggle$Scholarship)
kaggle$Neighbourhood <- as.factor(kaggle$Neighbourhood)
kaggle$Hipertension <- as.factor(kaggle$Hipertension)
kaggle$Diabetes <- as.factor(kaggle$Diabetes)
kaggle$Alcoholism <- as.factor(kaggle$Alcoholism)
kaggle$Handcap <- as.factor(kaggle$Handcap)
kaggle$SMS_received <- as.factor(kaggle$SMS_received)


kaggle_ml <- kaggle%>%
  select(No_Show,Gender,Age,Neighbourhood,Scholarship,Hipertension,Diabetes,Alcoholism,Handcap,SMS_received,Previous_No_Show,leadtime,rownum)

kaggle_ml$BinnedAge <- bin(kaggle_ml$Age, nbins = 10)
levels(kaggle_ml$BinnedAge)
levels(kaggle_ml$BinnedAge) = c("0to11.5","11.5to23","23to34.5","34.5to46","46to57.5","57.5to69","69to80.5","80.5to92","92to104","104to115")

save(kaggle_ml, file = "eda.Rdata")
#exploration
ggplot(kaggle_ml, aes(x = No_Show, fill = No_Show) )+
  geom_bar()
ggplot(kaggle_ml, aes(x = No_Show, fill = No_Show) )+
  geom_bar()+
  facet_wrap(~Neighbourhood)
ggplot(kaggle_ml, aes(x = Age))+
  geom_density()
ggplot(kaggle_ml, aes(x = Age))+
  geom_histogram(bins = 30)
ggplot(kaggle_ml, aes(x = Gender, fill = Gender) )+
  geom_bar()
ggplot(kaggle_ml, aes(x = leadtime < 5, fill = leadtime < 5))+
  geom_bar()
ggplot(kaggle_ml, aes(x = leadtime))+
  geom_density()
kaggle_ml %>%
  filter(Alcoholism == TRUE) %>%
  ggplot(aes(x = No_Show, fill = No_Show))+
  geom_bar()
kaggle_ml %>%
  filter(Scholarship == TRUE) %>%
  ggplot(aes(x = No_Show, fill = No_Show))+
  geom_bar()
kaggle_ml %>%
  filter(Hipertension == TRUE | Diabetes == TRUE) %>%
  ggplot(aes(x = No_Show, fill = No_Show))+
  geom_bar()
kaggle_ml %>%
  filter(SMS_received == TRUE) %>%
  ggplot(aes(x = No_Show, fill = No_Show))+
  geom_bar()
kaggle_ml %>%
  filter(Previous_No_Show == TRUE) %>%
  ggplot(aes(x = No_Show, fill = No_Show))+
  geom_bar()

mean(kaggle_ml$Age)
mean(kaggle_ml$Alcoholism == TRUE)
mean(kaggle_ml$Scholarship == TRUE)
mean(kaggle_ml$Hipertension == TRUE)
mean(kaggle_ml$Diabetes == TRUE)
mean(kaggle_ml$SMS_received == TRUE)
mean(kaggle_ml$Previous_No_Show == TRUE)
mean(kaggle_ml$leadtime)
median(kaggle_ml$leadtime)
mean(kaggle_ml$Gender == "F")
mean(kaggle_ml$No_Show == TRUE)
#build initial model to assess important independent variables
testrf <- randomForest(No_Show ~ Gender + Age + Scholarship + Hipertension + Diabetes + Alcoholism + SMS_received + Previous_No_Show + leadtime, data = kaggle_ml, type = "class",importance = TRUE, ntree = 100)
unbinned <- varImpPlot(testrf)

testrfbinned <- randomForest(No_Show ~ Gender + BinnedAge + Scholarship + Hipertension + Diabetes + Alcoholism + SMS_received + Previous_No_Show + leadtime, data = kaggle_ml, type = "class",importance = TRUE, ntree = 100)
binned <- varImpPlot(testrfbinned)

save(testrf, testrfbinned, file = "rfplots.Rdata")

var

#Training Set Creation
set.seed(123)
kaggle_ML1 <- kaggle_ml %>%
  filter(No_Show == TRUE)
kaggle_ML2 <- kaggle_ml %>%
  filter(No_Show == FALSE) %>%
  sample_n(size = nrow(kaggle_ML1), replace = FALSE)

kaggle_ml_final <- rbind(kaggle_ML1,kaggle_ML2)

mean(kaggle_ml_final$No_Show == TRUE)

TestIndex <- createDataPartition(kaggle_ml_final$No_Show, p = 0.7, list = FALSE)

TrainingSet <- kaggle_ml_final[TestIndex,]
TestSet <- kaggle_ml_final[-TestIndex,]


#Modelling

decisiontree <- C5.0(No_Show ~ Gender + Age + Scholarship + Hipertension + Diabetes + Alcoholism + SMS_received + Previous_No_Show + leadtime, data = TrainingSet, type = "class")
plot(decisiontree)
decisiontreeboost <- C5.0(No_Show ~ Gender + Age + Scholarship + Hipertension + Diabetes + Alcoholism + SMS_received + Previous_No_Show + leadtime, data = TrainingSet, type = "class", trials = 5)
plot(decisiontreeboost)
decisiontree2 <- C5.0(No_Show~ Gender + BinnedAge + Scholarship + Hipertension + Diabetes + Alcoholism + SMS_received + leadtime, data = TrainingSet, type = "class")
plot(decisiontree2)
lmmodel <- lm(No_Show~ Gender + Age + Scholarship + Hipertension + Diabetes + Alcoholism + SMS_received + Previous_No_Show + leadtime, data = TrainingSet,family = binomial)

randforestmodel <- randomForest(No_Show~ Gender + Age + Scholarship + Hipertension + Diabetes + Alcoholism + SMS_received + Previous_No_Show + leadtime, data = TrainingSet, type = "class", ntree = 200)
randforestmodel2 <- randomForest(No_Show~ Gender + BinnedAge + Scholarship + Hipertension + Diabetes + Alcoholism + SMS_received + Previous_No_Show + leadtime, data = TrainingSet, type = "class", ntree = 200)

nbmodel <- naive_bayes(No_Show~ Gender + Age + Scholarship + Hipertension + Diabetes + Alcoholism + SMS_received + Previous_No_Show + leadtime, data = TrainingSet)
nbmodel2 <- naive_bayes(No_Show~ Gender + BinnedAge + Scholarship + Hipertension + Diabetes + Alcoholism + SMS_received + Previous_No_Show + leadtime, data = TrainingSet)

rparttree <- rpart(No_Show~ Gender + Age + Scholarship + Hipertension + Diabetes + Alcoholism + SMS_received + Previous_No_Show + leadtime, data = TrainingSet, minsplit = 2, minbucket = 1, cp=-1)
#plot(rparttree)
#text(rparttree)
save(decisiontreeboost, nbmodel, TrainingSet, file = "modelsforplots.RData")

#Generate Predictions
C50Predictions <- predict(decisiontree, newdata = TestSet, type = "class")
C50Predictions2 <- predict(decisiontree2, newdata = TestSet, type = "class")
C50Predictionsboost <- predict(decisiontreeboost, newdata = TestSet, type = "class")

rfpreds <- predict(randforestmodel, TestSet, type = "class")
rfpreds2 <- predict(randforestmodel2, TestSet, type = "class")
nbpreds <- predict(nbmodel, TestSet, type = "class")
nbpreds2 <- predict(nbmodel2, TestSet, type = "class")

lmpreds <- predict(lmmodel, TestSet, type = "response")
lmpreds <- ifelse(lmpreds >1.5,2L,1L)
lmpreds <- factor(lmpreds,levels=c(1L,2L),labels =c("FALSE","TRUE"))

rpartpreds <- predict(rparttree, newdata = TestSet, type = "class")

#Generate Confusion!
confusionMatrix(rfpreds, TestSet$No_Show, positive = "TRUE")
confusionMatrix(rfpreds2, TestSet$No_Show, positive = "TRUE")
confusionMatrix(C50Predictions, TestSet$No_Show, positive = "TRUE")
confusionMatrix(C50Predictions2, TestSet$No_Show, positive = "TRUE")
confusionMatrix(C50Predictionsboost, TestSet$No_Show, positive = "TRUE")
confusionMatrix(lmpreds, TestSet$No_Show, positive = "TRUE")
confusionMatrix(nbpreds2, TestSet$No_Show, positive = "TRUE")
confusionMatrix(nbpreds, TestSet$No_Show, positive = "TRUE")
confusionMatrix(rpartpreds, TestSet$No_Show, positive = "TRUE")

#Ensemble Build
C50Predictionsprob <- predict(decisiontree, newdata = TestSet, type = "prob")

C50Predictions2prob <- predict(decisiontree2, newdata = TestSet, type = "prob")

rfpredsprob <- predict(randforestmodel, TestSet, type = "response")

nbpreds2prob <- predict(nbmodel2, TestSet, type = "prob")

lmpredsprob <- predict(lmmodel, TestSet, type = "response")

rpartpredsprob <- predict(rparttree, newdata = TestSet, type = "prob")


pred_C50_prob <- as.data.frame(predict(decisiontree, TrainingSet, type = "prob"))$'TRUE'
pred_rf_prob <- as.data.frame(predict(randforestmodel, TrainingSet, type = "prob"))$'TRUE'
pred_nb_prob <- as.data.frame(predict(nbmodel2, TrainingSet, type = "prob"))$'TRUE'
pred_lm_prob <- as.data.frame(predict(lmmodel, TrainingSet, type = "response"))
names(pred_lm_prob)[1] <- "pred_lm_prob"
buildresults <- NULL
buildresults <- as.data.frame(TrainingSet$No_Show)
buildresults <- cbind.data.frame(buildresults, pred_C50_prob, pred_rf_prob, pred_nb_prob, pred_lm_prob)
names(buildresults)[1] <- "No_Show"


ensembletree <- C5.0(No_Show ~ pred_C50_prob + pred_rf_prob + pred_nb_prob + pred_lm_prob, data = buildresults, type = "class", trials = 20)
ensembleforest <- randomForest(No_Show ~ pred_C50_prob + pred_rf_prob + pred_nb_prob + pred_lm_prob, data = buildresults, type = "class", ntree = 200)
plot(ensembletree)



testpred_c50_prob  <- as.data.frame(predict(decisiontree, newdata = TestSet, type = "prob"))$'TRUE'
testpred_rf_prob <- as.data.frame(predict(randforestmodel, TestSet, type = "prob"))$'TRUE'
testpred_nb_prob <- as.data.frame(predict(nbmodel2, TestSet, type = "prob"))$'TRUE'
testpred_lm_prob <- as.data.frame(predict(lmmodel, TestSet, type = "response"))
ensembletest <- as.data.frame(cbind(testpred_c50_prob,testpred_rf_prob, testpred_nb_prob, testpred_lm_prob))
names(ensembletest) <- names(buildresults[2:5])

ensembletreeresults <- predict(ensembletree, ensembletest, type = "class")
ensembleforestresults <- predict(ensembleforest, ensembletest, type = "class")

confusionMatrix(ensembletreeresults, TestSet$No_Show, positive = "TRUE")
confusionMatrix(ensembleforestresults, TestSet$No_Show, positive = "TRUE")


#Ensemble Pair - C50 and NB - Highest Sensitivity & Specificity

pairpred_C50_prob <- as.data.frame(predict(decisiontree, newdata = TrainingSet, type = "prob"))$'TRUE'
pairpred_nb_prob <- as.data.frame(predict(nbmodel, TrainingSet, type = "prob"))$'TRUE'

pairbuildresults <- as.data.frame(TrainingSet$No_Show)
pairbuildresults <- cbind.data.frame(pairbuildresults, pairpred_C50_prob, pairpred_nb_prob)
names(pairbuildresults)[1] <- "No_Show"
head(pairbuildresults)

pairensembletree <- C5.0(No_Show ~ pairpred_C50_prob + pairpred_nb_prob, data = pairbuildresults, type = "class", trials = 10)
plot(pairensembletree)

pairtestpred_c50_prob  <- as.data.frame(predict(decisiontree, newdata = TestSet, type = "prob"))$'TRUE'
pairtestpred_nb_prob <- as.data.frame(predict(nbmodel2, newdata = TestSet, type = "prob"))$'TRUE'
pairensembletest <- as.data.frame(cbind(pairtestpred_c50_prob, pairtestpred_nb_prob))
names(pairensembletest) <- names(pairbuildresults[2:3])

pairensembletreeresults <- predict(pairensembletree, pairensembletest, type = "class")


confusionMatrix(pairensembletreeresults, TestSet$No_Show, positive = "TRUE")


pred1 <- prediction(ifelse(pairensembletreeresults == TRUE,1,0), TestSet$No_Show)
perf1 <- performance(pred1,"tpr","fpr")
plot(perf1)


save(ensembletree, pairensembletree,buildresults,pairbuildresults, file= "ensemble.Rdata")


#Neural Net
crs$dataset <- kaggle_ml_final
crs$nobs     <- nrow(crs$dataset)
crs$train    <- TrainingSet
crs$validate <- NULL
crs$test     <- TestSet

# The following variable selections have been noted.

crs$input     <- c("Gender", "Age", "Scholarship", "Hipertension",
                   "Diabetes", "Alcoholism", "Handcap", "SMS_received",
                   "Previous_No_Show", "leadtime")

crs$numeric   <- c("Age", "leadtime")

crs$categoric <- c("Gender", "Scholarship", "Hipertension", "Diabetes",
                   "Alcoholism", "Handcap", "SMS_received", "Previous_No_Show")

crs$target    <- "No_Show"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("Neighbourhood", "rownum", "BinnedAge")
crs$weights   <- NULL

if(!require(nnet)) install.packages("nnet", repos = "http://cran.us.r-project.org")
library(nnet)
crs$nnet <- nnet(as.factor(No_Show) ~ .,
                 data=TrainingSet[,c(crs$input, crs$target)],
                 size=10, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

crs$nnetpr <- predict(crs$nnet, newdata=TestSet[, c(crs$input, crs$target)], type="class")

confusionMatrix(as.factor(crs$nnetpr), TestSet$No_Show, positive = "TRUE")


#XGBOOST
crs$ada <- xgboost(No_Show ~ .,
                   data              = TrainingSet[,c(crs$input, crs$target)],
                   max_depth         = 6,
                   eta               = 0.3, 
                   num_parallel_tree = 1, 
                   nthread           = 2, 
                   nround            = 10,
                   metrics           = 'error',
                   objective         = 'binary:logistic')

lvls <- levels(as.factor(crs$dataset[[crs$target]]))
crs$xgpr <- factor(ifelse(predict(crs$ada, TestSet[, c(crs$input, crs$target)]) > 0.5,
                        lvls[2], lvls[1]))
confusionMatrix(as.factor(crs$xgpr), TestSet$No_Show, positive = "TRUE")


#SVM - this takes a long time to build ( ~ 6 mins )
if(!require(kernlab)) install.packages("kernlab", repos = "http://cran.us.r-project.org")
library(kernlab)

crs$ksvm <- ksvm(as.factor(No_Show) ~ .,
                 data=TrainingSet[,c(crs$input, crs$target)],
                 kernel="rbfdot",
                 prob.model=TRUE)

crs$svmpr <- kernlab::predict(crs$ksvm, newdata=na.omit(TestSet[, c(crs$input, crs$target)]))

confusionMatrix(as.factor(crs$svmpr), TestSet$No_Show, positive = "TRUE")



# Save Confusion Matrix Results to a table for the report
rfcm <- confusionMatrix(rfpreds2, TestSet$No_Show, positive = "TRUE")
c50dt <-confusionMatrix(C50Predictionsboost, TestSet$No_Show, positive = "TRUE")
lmcm <- confusionMatrix(lmpreds, TestSet$No_Show, positive = "TRUE")
nbcm <- confusionMatrix(nbpreds2, TestSet$No_Show, positive = "TRUE")
EnsembleC50cm <- confusionMatrix(ensembletreeresults, TestSet$No_Show, positive = "TRUE")
ensemblerfcm <- confusionMatrix(ensembleforestresults, TestSet$No_Show, positive = "TRUE")
ensemble2C50cm <- confusionMatrix(pairensembletreeresults, TestSet$No_Show, positive = "TRUE")
artneuralnetcm <- confusionMatrix(as.factor(crs$nnetpr), TestSet$No_Show, positive = "TRUE")
xgboostcm <- confusionMatrix(as.factor(crs$xgpr), TestSet$No_Show, positive = "TRUE")
svmcm <- confusionMatrix(as.factor(crs$svmpr), TestSet$No_Show, positive = "TRUE")

confmatrices <- list(rfcm, c50dt, lmcm, nbcm, EnsembleC50cm, ensemblerfcm, ensemble2C50cm,
                     artneuralnetcm, xgboostcm, svmcm)

row_names <- c("C50 Decison Tree", "Logistic Regression", "Random Forest", "Naive Bayes",
                "Ensemble Model 1 - C50", "Ensemble Model 1 - Random Forest",
                "Ensemble Model 2 - C50 boosted", "Artifical Neural Network",
                "XGBoost Model", "Support Vector Machine")

as.matrix(rfcm, what = "classes")[1]

tmp <- lapply(confmatrices,function(x) {
  Sensitivity <- as.matrix(x, what = "classes")[1]
  Specificity <- as.matrix(x, what = "classes")[2]
  Kappa <- as.matrix(x, what = "overall")[2]
  Balanced_Accuracy <- as.matrix(x, what = "classes")[11]
  Results <- cbind.data.frame(Sensitivity, Specificity, Kappa, Balanced_Accuracy)
}
)
names(tmp) <- row_names

final_results_table <- bind_rows(tmp, .id = "Model")

save(final_results_table, file = "results.RData")

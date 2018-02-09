library(data.table)
library(dplyr)
library(mlr)
library(Amelia)
library(MASS)
library(sqldf)
library(RSQLite)

setwd("C:/Users/tpurohit/Desktop/AV/Black Friday")
train<-read.csv("train.csv",header=T)
str(train)
train1<- train 
test<-read.csv("test.csv", header=T)
str(test)


#create variable no of useres per useri id

#mean_purc_price_percat<- sqldf("select User_ID , Product_Category_1, avg(Purchase) from train
#                            group by User_ID , Product_Category_1
#                          ")
#train_f<-merge(train, mean_purc_price_percat, by = c("User_ID" ,"Product_Category_1") )

summarizeColumns(train)
summarizeColumns(test)
test$Purchase <- 0
combi<- rbind(train, test)

#Convert gender to number
attach(combi)
combi$Gender <- as.numeric(as.factor(Gender)) 
#convt age to number
combi$Age <- as.numeric(as.factor(Age)) 

##convt 4+ t0 4
combi$Stay_In_Current_City_Years<- ifelse(combi$Stay_In_Current_City_Years == '4+' , '4',  combi$Stay_In_Current_City_Years)
combi$Stay_In_Current_City_Years <-  as.numeric(combi$Stay_In_Current_City_Years)

##product cat , make the blanks as -999
combi$Product_Category_2 <- ifelse(is.na(Product_Category_2) == TRUE, -999,  Product_Category_2)
#combi$Product_Category_2 <- as.numeric(as.factor(Product_Category_2)) 
combi$Product_Category_3 <- ifelse(is.na(Product_Category_3) == TRUE, -999,  Product_Category_3)
#combi$Product_Category_3 <- as.numeric(as.factor(Product_Category_3)) 

#city category:
combi$City_Category <- as.numeric(as.factor(City_Category)) 

#product tio number
#combi$Product_ID <- gsub('P', '99', combi$Product_ID)
#combi$Product_ID<- as.numeric(combi$Product_ID)


#TRAETING MISSING VALUES
summarizeColumns(combi)
combi[combi == ""] <- NA
missmap(combi, main = "Missing values vs observed")
sum(is.na(combi)) #37347 values
sort(sapply(combi,function(x){sum(is.na(x))/length(x)}),decreasing =T) #percetage
summarizeColumns(combi)

##SEP TEST AND TRAIN
#train_F<- sqldf("select * FROM combi where purchase <>0 ")
train_F <- combi[1:nrow(train),]
str(train_F)
test_F <- combi[-(1:nrow(train)),]
#test_F <- sqldf("select count(*) FROM combi where purchase = 0")
str(test_F)

###creating features
##Train Freatures
no_dist_cat<-
  sqldf("select User_ID, count(distinct Product_Category_1) as cnt_pc, count(distinct Product_ID) as cnt_pd
        from train_F group by User_ID")

train_F1 <- merge(train_F, no_dist_cat, by = "User_ID" )

no_of_pdt_per_cat <- sqldf("select count(Product_ID) as cnt_pid_pc , Product_Category_1  from train_F
                           group by  Product_Category_1
                           ")
train_F1 <- merge(train_F1, no_of_pdt_per_cat, by = "Product_Category_1" )
###Test features

no_dist_cat<-
  sqldf("select User_ID, count(distinct Product_Category_1) as cnt_pc, count(distinct Product_ID) as cnt_pd
        from test_F group by User_ID")

test_F1 <- merge(test_F, no_dist_cat, by = "User_ID" )


no_of_pdt_per_cat <- sqldf("select count(Product_ID) cnt_pid_pc , Product_Category_1  from test_F
                           group by  Product_Category_1
                           ")
test_F1 <- merge(test_F1, no_of_pdt_per_cat, by = "Product_Category_1" )

##model

library(h2o)
h2o.shutdown()
h2o.server <- h2o.init( nthreads= -1)
train.hex <- as.h2o(train_F1)
features=c("User_ID","Product_ID")


##---test
gbmF_model_1 = h2o.gbm( x=features,
                        y = "Purchase",
                        training_frame =train.hex ,
                        #validation_frame =testHex ,
                        max_depth = 3,
                        distribution = "gaussian",
                        ntrees =500,
                        learn_rate = 0.05,
                        nbins_cats = 5891
)

gbmF_model_2 = h2o.gbm( x=features,
                        y = "Purchase",
                        training_frame =train.hex ,
                        #validation_frame =testHex ,
                        max_depth = 5,
                        distribution = "gaussian",
                        ntrees =500,
                        learn_rate = 0.01,
                        nbins_cats = 5891
)


dl_model_1 = h2o.deeplearning( x=features,
                               y = "Purchase",
                               training_frame =train.hex ,
                               #validation_frame =testHex ,
                               activation="Rectifier",
                               hidden=6,
                               epochs=60,
                               adaptive_rate =F
)


dl_model_2 = h2o.deeplearning( x=features,
                               # x=features,
                               y = "Purchase",
                               training_frame =train.hex ,
                               #validation_frame =testHex ,
                               activation="Rectifier",
                               hidden=60,
                               epochs=40,
                               adaptive_rate =F
)


dl_model_3 = h2o.deeplearning( x=features,
                               y = "Purchase",
                               training_frame =train.hex ,
                               #validation_frame =testHex ,
                               activation="Rectifier",
                               hidden=6,
                               epochs=120,
                               adaptive_rate =F
)

testPurchase_gbm_1 = as.data.frame(h2o.predict(gbmF_model_1, newdata = test.hex) )
testPurchase_gbm_2 = as.data.frame(h2o.predict(gbmF_model_2, newdata = test.hex) )

testPurchase_dl_model_1 = as.data.frame(h2o.predict(dl_model_1, newdata = test.hex) )
testPurchase_dl_model_2 = as.data.frame(h2o.predict(dl_model_2, newdata = test.hex) )
testPurchase_dl_model_3 = as.data.frame(h2o.predict(dl_model_3, newdata = test.hex) )


predict=0 *(testPurchase_dl_model_1$predict)+
  0.0*(testPurchase_dl_model_2$predict)+
  0.0*(testPurchase_dl_model_3$predict)+
  1*(testPurchase_gbm_1$predict)+
  0.0*(testPurchase_gbm_2$predict)

sub_reg <- data.frame(User_ID = test_F1$User_ID, Product_ID = 
                        test_F1$Product_ID, Purchase =  predict)

write.csv(sub_reg, file = "sub_reg1.csv", row.names = F)

## try stacking
h2o.removeAll() 
library(h2o)
h2o.init(nthreads = -1)
features=c("User_ID","Product_ID")

my_gbm <- h2o.gbm(x=features,
                  y = "Purchase",
                  training_frame = train.hex,
                  distribution = "AUTO",
                  ntrees = 10,
                  max_depth = 3,
                  min_rows = 2,
                  learn_rate = 0.2,
                  nfolds = 3,
                  fold_assignment = "Stratified",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)
gc()
# Train & Cross-validate a RF
?h2o.randomForest
my_rf <- h2o.randomForest(x=features,
                          y = "Purchase",
                          training_frame = train.hex,
                          ntrees = 10,
                          nfolds = 3,
                          fold_assignment = "Stratified",
                          keep_cross_validation_predictions = TRUE,
                          seed = 1)


ensemble <- h2o.stackedEnsemble(x=features,,
                                y = "Purchase",
                                training_frame =train.hex ,
                                model_id = "my_ensemble_binomial",
                                base_models = list(my_gbm, my_rf))
test.hex <- as.h2o(test_F1)
perf <- h2o.performance(ensemble, newdata = test.hex)


perf_gbm_test <- h2o.performance(my_gbm, newdata = test.hex)
perf_rf_test <- h2o.performance(my_rf, newdata = test.hex)
baselearner_best_auc_test <- max(h2o.auc(perf_gbm_test), h2o.auc(perf_rf_test))
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))

# Generate predictions on a test set (if neccessary)

library(data.table)
pred <- h2o.predict(ensemble, newdata = test.hex)

class(pred)
class(test.hex)
Purchase<- as.data.frame(pred)
test_F2 <- as.data.frame(test.hex)
test_F2$P<- Purchase

sub_reg1 <- data.frame(User_ID = test_F2$User_ID, Product_ID = 
                        test_F2$Product_ID, Purchase =  test_F2$P)

write.csv(sub_reg, file = "sub_reg1.csv", row.names = F)
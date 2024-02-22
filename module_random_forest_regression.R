
#Random Forest regression for identifying drivers of late summer SRP among gaged watersheds
#see script 'gaged_watersheds_data_processing_and_analysis_script.R' for preceeding steps to create input data 


#################
# LOAD PACKAGES #
#################

library(tidyverse)
library(stringr)
library(data.table) #note: make sure it works ok to load this, if it conflicts
library(tidymodels)
library(workflows)
library(tune)
library(ranger)
library(missRanger) 


####################
# REGRESSION MODELS #
####################


#RANDOM FOREST MODEL
#Split into training and testing datasets
#80% for model training, 20% for model testing

#Dataset is late summer low flow SRP (Aug-Oct, <20% EP) for 142 gages
#Note, using NHD estimates for Ws area, rather than delineated WS

#mean low flow SRP during each season is from legacy_P_script_clean_11_8_23.R
setwd(output_dir)
lowflow.att2<-read.table("Lowflow_meanSRP_by_gage_and_Season_with_attributes.csv", sep=",", header=TRUE)
head(lowflow.att2)

names(lowflow.att2) 

#check for columns that are >20% empty (ie, 'NA')
colnames(lowflow.att2)[ colSums(is.na(lowflow.att2))/nrow(lowflow.att2) > 0.20 ]

#Create model dataset: mean SRP concentrations during late summer low flows for gaged watersheds
#restrict to watersheds with >=3 SRP samples collected during late summer low flows

DATA<-lowflow.att2 %>%
  filter(Season=="Late Summer") %>% #select season 
  filter(n>=3) %>% #select sites with >=3 SRP samples in a season
  select(!c(sd.SRP, Station_number, Season, station, COMID, Station_name, station_ALL_clean, name_ALL_clean,
            Area_Km2, Behavior)) %>%  ##omit unneeded ID columns
  select(where(~ any(. != 0))) %>% #exclude columns that are all 0s
  select(!c(NPDESDensCatRp100, PctWaterCat, PctEolCrsCat, PctEolCrsWs, CoalMineDensWs)) %>%  #Omit columns that are almost entirely 0s
  select(!c(MWST_2008, MWST_2009, MWST_2014)) %>% #omit columns that are >20% missing values
  select(!c(Impacted, WWTP_none, WWTP_lim, n)) %>% #omit additional columns you made for other analyses
  select(!c(Phos_Ag_BalanceCat, Phos_Ag_BalanceWs)) #omit as these are not currently defined in StreamCat variable list  
head(DATA)
nrow(DATA) #128 sites 
names(DATA)


#Check for outliers
#Histogram of P
#circle to annotate outlier

library(ggforce)
  
P.hist<-
  ggplot(DATA, aes(mean.SRP))+
  geom_histogram(binwidth = 0.001,colour = "darkgray", fill = "darkgrey") +
  geom_vline(aes(xintercept=mean(mean.SRP)), linetype="dashed", color="black", linewidth=1)+
  xlim(0,0.9)+
  xlab("mean SRP (mg/L)") +
  ylab ("Frequency") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_mark_circle(aes(x = 0.8, y = 0.5), 
                   radius = unit(5, "mm"), linewidth = 0.5, 
                  con.colour = NA, label.fill = NA,
                   label.buffer = unit(5, "mm"))
P.hist

#write fig to file
setwd(output_dir)
jpeg("./Figures/meanSRP_dist.jpeg", units="in", width=5, height=4, res=300)
P.hist
dev.off()

#Identify sites with very high mean SRP
ggplot(droplevels(lowflow.att2 %>% filter(Season=="Late Summer" & mean.SRP>0.2)))+
         geom_point(aes(WWTPAllDensWs, mean.SRP, color=factor(Station_name)))

#Very high mean.SRP at Buffalo Creek nr Glencoe
#largest tributary to South Fork Crow River 
#note there are 3 samples for Late Summer for this gage (small n)
#actually this is a good candidate for instream release? or WWTP influence
#Buffalo Creek Stressor ID report: https://www.pca.state.mn.us/sites/default/files/wq-ws5-07010205a.pdf
#Nothing specific to explain extremely high SRP
#If it ends up in the test dataset, will affect measure of model performance, but not model function 
#Exclude as it's outside the range of training data

#Exclude very high site from dataset
DATA<-DATA %>% 
  filter(mean.SRP<0.7)

samplesize = 0.70*nrow(DATA)
set.seed(100)
index = sample(seq_len(nrow(DATA)), size = samplesize)
#Creating training and test set 
datatrain = DATA[index,]
datatest = DATA[-index,]



ntrain<-nrow(datatrain) #n=88
ntrain
ntest<-nrow(datatest)  #n=39
ntest

#Check P distribution for training and test datasets

summary(datatrain$mean.SRP)
summary(datatest$mean.SRP) 

library(gridExtra)
train.plot<-ggplot(datatrain, aes(mean.SRP))+
  geom_histogram(binwidth = 0.001,colour = "lightgray", fill = "darkgrey") +
  geom_vline(aes(xintercept=mean(mean.SRP)), linetype="dashed", color="black", linewidth=1)+
  xlab("mean SRP (mg/L)") +
  ylab ("Frequency") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlim(0, 0.9)
test.plot<-ggplot(datatest, aes(mean.SRP))+
  geom_histogram(binwidth = 0.001,colour = "lightgray", fill = "darkgrey") +
  geom_vline(aes(xintercept=mean(mean.SRP)), linetype="dashed", color="black", linewidth=1)+
  xlab("mean SRP (mg/L)") +
  ylab ("Frequency") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlim(0, 0.9)
grid.arrange(train.plot, test.plot, ncol=1, nrow=2)



#ID columns with missing data 
#All MAST and MWST columns are missing some values
#impute or exclude? mean annual summer temp could be important, so impute

###
#IMPUTE MISSING VALUES FOR REMAINING COVARIATES
#use missRanger to interpolate
#see https://cran.r-project.org/web/packages/missRanger/vignettes/missRanger.html

#By default missRanger uses all columns in the data set to impute all columns with missings. 

#exclude latesummer_mean_SRP from imputation
#first impute training dataset
#then use imputed training dataset to impute test dataset (to prevent data leakage)

#Note that the only variables with missing values
#are all temperature: MAST & MWST

#Separate out predictor variables:
names(datatrain)
Predictors<-datatrain %>% 
  select(-mean.SRP)

#R with parameters set to run faster
#no pmm.k, num.trees=100;

start_time <- Sys.time()
start_time
dataImputed <- missRanger(Predictors,
                          formula = . ~ . ,
                          num.trees = 100, 
                          verbose = 1, seed = 111)
head(dataImputed)

end_time <- Sys.time()
end_time - start_time

nrow(dataImputed)

#rematch to latesummer SRP
names(datatrain)
P.DATA<-datatrain[,c(1)]
P.DATA
P.Predictors<-cbind(P.DATA, dataImputed)
head(P.Predictors)

summary(P.Predictors)

#WRITE IMPUTED DATA TO FILE
#(note this is training data only)
setwd(output_dir)
write.table(P.Predictors, "./latesummer_SRP_Predictors_imputed_25pct.csv", sep=",", row.names=FALSE)
#read in as needed:
P.Predictors<-read.table("./latesummer_SRP_Predictors_imputed_25pct.csv", sep=",", header=TRUE)


###
#Impute *testing data* separately (for use in model testing later)

names(datatest)

#recreate dataImputed
dataImputed<-P.Predictors %>% 
  select(-P.DATA)
names(dataImputed)
nrow(datatest)

Test.impute<-rbind(
  (datatest %>% 
     select(-mean.SRP)),
  dataImputed)
nrow(Test.impute)


#Impute missing values for test data (combined with imputed training data)
#Is this the correct way to do this?
start_time <- Sys.time()
start_time
data.Test.Imputed <- missRanger(Test.impute,
                                formula = . ~ . ,
                                num.trees = 100, 
                                verbose = 1, seed = 111)
head(data.Test.Imputed)
nrow(data.Test.Imputed)
end_time <- Sys.time()
end_time - start_time

#select out test data; and attach to outcome variable (SRP)
names(data.Test.Imputed)
test<-data.Test.Imputed[c(1:ntest),]
P.test<-datatest[,c(1)]
P.test
test.prep<-cbind(P.test, test)
nrow(test.prep)
head(test.prep)

setwd(output_dir)
write.table(test.prep, "./latesummer_SRP_TEST_imputed_25pct.csv", sep=",", row.names=FALSE)
#read in table as needed:
test.prep<-read.table("./latesummer_SRP_TEST_imputed_25pct.csv", sep=",", header=TRUE)


#Input datasets are:
#1. Training Data: 'P.Predictors'
#2. Test Data: 'test.prep'


###
#DEFINE A RECIPE 
#Notes:
#standardize predictors
#So far, only have numeric predictors (StreamCat attributes + tile density)

#Note: best practice is to scale only using the training dataset - 
#eg: https://datascience.stackexchange.com/questions/39932/feature-scaling-both-training-and-test-data
#If you use the whole dataset to figure out the feature mean and variance, 
#you're using knowledge about the distribution of the test set to set the scale of the training set - 'leaking' information.
#then you can "bake" out the centered/scaled test data later, when you need to test independent randomForest models.

#rename P column to match for train and test
names(P.Predictors)
names(test.prep)
colnames(P.Predictors)[1]<- "mean.SRP"
colnames(test.prep)[1] <-"mean.SRP"

P_recipe <- 
  # which consists of the formula (outcome ~ predictors)
  recipe(mean.SRP ~ ., data = P.Predictors) %>% 
#option to exclude centering and scaling for now, to see how it affects results %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric())
#Other options excluded for now, eg:
#%>%
#step_dummy(all_nominal()) #%>%
#step_impute_bag(all_predictors(), trees=100) #option to impute missing values, using all other predictors
P_recipe


###
#EXTRACT THE PREPROCESSED DATASET
P_train_preprocessed <- P_recipe %>%
  # apply the recipe to the training data
  prep(P.Predictors) %>%
  # extract the pre-processed training dataset
  juice()
P_train_preprocessed
names(P_train_preprocessed)
#write pre-processed training data to file
setwd(output_dir)
write.table(as.data.frame(P_train_preprocessed), "./latesummerSRP_train_preprocessed_25pct.csv", sep=",", row.names=FALSE)
names(P_train_preprocessed)

#Important: How you prep the test data:
#The bake() function takes a prepped recipe (one that has had all quantities estimated from training data) 
#and applies it to new_data. 
#That new_data could be the training data again...
#Or it could be the testing data. In this case, the column means from the training data are applied to the testing data;
#To do otherwise is data leakage.

#So need to bake() and not juice
P_test_preprocessed <- P_recipe %>%
  # apply the recipe to the training data
  prep(P.Predictors) %>%
  # extract the pre-processed testing dataset
  bake(new_data=test.prep)
nrow(P_test_preprocessed)
#View(P_test_preprocessed)

#write pre-processed testing data to file, so can use later for model evaluation
write.table(as.data.frame(P_test_preprocessed), "./latesummerSRP_test_preprocessed_25pct.csv", sep=",", row.names=FALSE)


###
#SPECIFY THE MODEL

#example from Julia Silge: 
#https://juliasilge.com/blog/sf-trees-random-tuning/

#note: tuning the number of trees is not necessary
#see https://stats.stackexchange.com/questions/348245/do-we-have-to-tune-the-number-of-trees-in-a-random-forest
#"Tuning the number of trees is unnecessary; instead, simply set the number of trees to a large, computationally feasible number, and let the asymptotic behavior of LLN do the rest."

#Use parallel processing to make tuning go faster
#install.packages("doParallel", repos="https://cloud.r-project.org")
#install.packages("foreach", repos="https://cloud.r-project.org")

setwd(output_dir)

library(doParallel)
library(foreach)
library(ranger)

doParallel::registerDoParallel()

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("ranger")

#specify cross validation for the training dataset, note that default is k=10 folds
set.seed(234)
P_folds <- vfold_cv(P.Predictors)

###
#PUT EVERYTHING TOGETHER IN A WORKFLOW

tune_wf <- workflow() %>%
  add_recipe(P_recipe) %>%
  add_model(tune_spec)

###
#TUNE THE PARAMETERS

#Check start time for initial tuning
start_time <- Sys.time()
start_time

set.seed(345)
tune_res <- tune_grid(
  tune_wf,
  resamples = P_folds,
  grid = 20,
  control = control_grid(verbose = TRUE)
)
tune_res

#Check end time for initial tuning: 
end_time <- Sys.time()
end_time - start_time #took ~3 min

#save the tune results
saveRDS(tune_res, "./rf_tune_results_INITIAL_ranger_25pct_latesummer.rds")

#load in tune results (as needed, if skipping ahead to this step)
tune_res<-readRDS("./rf_tune_results_INITIAL_ranger_25pct_latesummer.rds")
tune_res

#Check out initial tuning results
tune_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter") %>%
  arrange(mean) %>%
  print(n=40) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rsq")


#find the best set:
best_rmse1<-select_best(tune_res, "rmse")
best_rmse1


####
#FINALIZE THE WORKFLOW

final_rf <- finalize_model(
  tune_spec,
  best_rmse1
)

final_rf

final_wf <- workflow() %>%
  add_recipe(recipe(mean.SRP ~ ., data = P_train_preprocessed)) %>%
  add_model(final_rf)

###
#FIT THE MODEL BASED ON TRAINING DATA
rf_fit <- final_wf %>%
  fit(P_train_preprocessed)
rf_fit

#Save the final model 
saveRDS(rf_fit, "./rf_fit.ranger_MODEL_training_25pct_latesummer.rds")
rf_fit<-readRDS("./rf_fit.ranger_MODEL_training_25pct_latesummer.rds")
rf_fit
###
#APPLY MODEL TO TEST DATA
#predict soil P for the test dataset
rf_pred <-predict(rf_fit, new_data=P_test_preprocessed)
rf_pred  
#add predictions to test data:
test.pred<-as.data.frame(cbind(P_test_preprocessed, rf_pred))
head(test.pred)


#unscale by multiplying values by the standard deviation and adding the mean of the training dataset
test.pred$latesummer_mean_SRP_unscaled<-test.pred$mean.SRP* sd(P.Predictors$mean.SRP) + mean(P.Predictors$mean.SRP)
test.pred$pred_unscaled<-test.pred$.pred* sd(P.Predictors$mean.SRP) + mean(P.Predictors$mean.SRP)
head(test.pred)
nrow(test.pred)

#check R2 value for actual vs predicted
#(using unscaled data)
reg1<- with(test.pred,lm(latesummer_mean_SRP_unscaled~pred_unscaled))
summary(reg1)
#excluding outlier 
reg<- with(test.pred[test.pred$latesummer_mean_SRP_unscaled<0.25,],lm(latesummer_mean_SRP_unscaled~pred_unscaled))
summary(reg)

#View actual vs predicted soil P for test data
ActualvsPredict.plot<-
  #ggplot(test.pred %>% filter(latesummer_mean_SRP_unscaled<0.25))+
  ggplot(test.pred)+
  geom_point(aes(latesummer_mean_SRP_unscaled, pred_unscaled))+
  #scaled and centered data:
  #geom_point(aes(P_mgkg, .pred))+
  geom_abline(intercept=0, slope=1)+
  xlim(0,0.6)+
  ylim(0,0.6)+
  theme_bw()+
  xlab("Actual SRP (mg/L)") +
  ylab ("Predicted SRP (mg/L)") +
  theme_bw()+
  theme(axis.title=element_text(size=18), axis.text=element_text(size=18),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ActualvsPredict.plot

#write fig to file
setwd(output_dir)
jpeg("./Figures/Actual_vs_model_predicted_P_test_data_EP50_128_1_outlier_excluded.jpeg", units="in", width=4, height=4, res=300)
ActualvsPredict.plot
dev.off()

#calculate rmse
RMSE = mean((test.pred$latesummer_mean_SRP_unscaled - test.pred$pred_unscaled)^2) %>% sqrt()
RMSE
mean(test.pred$latesummer_mean_SRP_unscaled)

#calculate RMSE as percentage:
#https://search.r-project.org/CRAN/refmans/forestmangr/html/rmse_per.html
library(forestmangr)
names(test.pred)
class(test.pred)
rmse_per(test.pred, "latesummer_mean_SRP_unscaled", "pred_unscaled", na.rm = TRUE)

# STEP 9: VARIABLE IMPORTANCE (using permimp)

#see https://cran.r-project.org/web/packages/permimp/vignettes/permimp-package.html

setwd(output_dir)

library(party)
library(permimp)

#need a randomForest object
#need to set hyperparameters to those tuned by ranger previously 
#note in tidy models for engine rand_forest:
#min_n = An integer for the minimum number of data points in a node that are required for the node to be split further.
#is this equivalent in randomForest to nodesize = Minimum size of terminal nodes. Setting this number larger causes smaller trees to be grown (and thus take less time). Note that the default values are different for classification (1) and regression (5).
#note: need pre-processed (centered, scaled) train and test data (created in recipe above):

#Run Model in randomForest
library(randomForest)

start_time <- Sys.time()
start_time

#check parameter values
best_rmse1

set.seed(542863)
rfP_model <- randomForest(mean.SRP ~ ., data = as.data.frame(P_train_preprocessed), 
                          mtry = best_rmse1$mtry, ntree=1000, replace = FALSE, #specify mtry and nodesize(same as min_n)
                          nodesize = best_rmse1$min_n,
                          keep.forest = TRUE, keep.inbag = TRUE)
end_time <- Sys.time()
end_time - start_time

summary(rfP_model)

#SAVE FINAL RANDOM FOREST MODEL!
saveRDS(rfP_model, "./FINALMODEL_randomForest_latesummer.rds")

#LOAD IN MODEL (if needed)
rfP_model<-readRDS("./FINALMODEL_randomForest_latesummer.rds")
rfP_model

#Check performance of the model (should match performance of the random_Forest() model tuned above, using tidymodels?)
rf.pred<-predict(rfP_model, newdata=P_test_preprocessed)
head(rf.pred)

#Match actual to predicted data:
predicted.P<-cbind(as.data.frame(P_test_preprocessed), rf.pred)
head(predicted.P)

#actual vs predicted
reg.check<-with(predicted.P, lm(mean.SRP~rf.pred))
summary(reg.check)
#calculate rmse
RMSE = mean((predicted.P$mean.SRP - predicted.P$rf.pred)^2) %>% sqrt()
RMSE

names(predicted.P)
names(P.Predictors)


##RUN CONDITIONAL PERMUTATION IMPORTANCE:
#read in saved model if needed;
setwd(output_dir)
rfP_model<-readRDS("./FINALMODEL_randomforest_latesummer.rds")
rfP_model

#Note: permimp does require training data to run!

start_time <- Sys.time()
start_time

CPI_permimpRF <- permimp(rfP_model, 
                         conditional = TRUE, progressBar = TRUE, do_check=FALSE)

CPI_permimpRF
end_time <- Sys.time()
end_time - start_time #took 14 minutes

#save permimp object!
saveRDS(CPI_permimpRF, "./permimp_results_latesummer.rds")


#read in permimp results if needed
CPI_permimpRF<-readRDS("./permimp_results_latesummer.rds")


#Rank importance values and plot a subset:
#Make a dot plot:
#library(dplyr)
#library(ggplot2)
VarImp<-as.data.frame(sort(CPI_permimpRF$values, decreasing=TRUE), optional=T)
colnames(VarImp)[1]<-"Imp"
rownames(VarImp)
VarImp2 <- cbind(rownames(VarImp), data.frame(VarImp, row.names=NULL))
colnames(VarImp2)[1]<-"Var"
head(VarImp2)
View(VarImp)
VarImp2[1:75,]

#Appendix table - list of all model predictors, by importance
setwd(output_dir)
write.table(VarImp2, "Importance_values_table.csv", sep=",", row.names=FALSE)

#rename 30m grid scale NLCD attribute so it doesn't get confused with catchment-scale attributes
VarImp3<-
  VarImp2 %>% 
  mutate(Var=ifelse(Var=="NLCD06Cat", "NLCD06", Var))
head(VarImp3)

#Subset variables for plot
Imp.forPlot<-droplevels(VarImp2[1:15,])
#Make nice labels for plot
levels(factor(Imp.forPlot$Var))
Imp.forPlot$Var<-factor(Imp.forPlot$Var,
                        labels=c("Mean Soil Erodibility KF Factor on ag soils in catchment",
                                 "% Clay in soils of watershed",
                                 "Mean rate of synthetic fertilizer application to ag land (kg N/ha/yr) in watershed",
                                 "Mean Soil Erodibility KF Factor in catchment",
                                 "Predicted mean annual stream temp - 2013",
                                 "% Cropland in riparian areas in catchment",
                                 "% Cropland in riparian areas in watershed",
                                 "% Grassland cover in watershed",
                                 "% Mixed forest in riparian areas in watershed",
                                 "% Urban open land use in riparian areas in catchment",
                                 "% Woody wetland cover in riparian areas in watershed",
                                 "Mean permeability (cm/hour) of soils in catchment",
                                 "Pesticide use in watershed - 1997",
                                 "Phorphorus uptake by crops in the watershed",
                                 "Precipitation minus evaporation in watershed"))
                                 
                                 
                                
                               
                                 
                                 
                                 

Importance.plot<-
  #VarImp2[1:15,] %>% 
  Imp.forPlot %>% 
  #slice(25, (Imp)) %>% 
  ggplot() + aes(x=reorder(Var, Imp), y=(Imp)) + geom_point()+coord_flip()+
  ylab("Importance")+
  xlab("Covariate")+
  theme_bw()+
  theme(panel.grid=element_blank())+
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"))
Importance.plot


#write fig to file
jpeg("./Figures/Covariate_importance_plot_EP50_128sites_1outliersexcluded.jpeg", 
     units="in", width=17, height=8, res=300)
Importance.plot
dev.off()

###
#Partial dependence plots
#https://rdrr.io/cran/randomForest/man/partialPlot.html
###

library(randomForest)

#rfp model
#read in saved model if needed;
setwd(output_dir)
rfP_model<-readRDS("./FINALMODEL_randomForest_latesummer.rds")
rfP_model

#training data:
setwd(output_dir)
P_train_preprocessed<-read.table("./latesummerSRP_train_preprocessed_25pct.csv", sep=",", header=TRUE)
head(P_train_preprocessed)
names(P_train_preprocessed)

#load in pre-scaled, pre-centered data:
setwd(output_dir)
P.Predictors<-read.table("./latesummer_SRP_Predictors_imputed_25pct.csv", sep=",", header=TRUE)
names(P.Predictors)
colnames(P.Predictors)[1]<- "latesummer_mean_SRP"
head(P.Predictors)
names(P.Predictors)

#Top variables:


ggplot(lowflow.att2 %>% filter(Season=="Late Summer"))+
  ylim(0,0.6)+
  #geom_point(aes(ClayWs, mean.SRP))+
  #geom_text(aes(ClayWs, mean.SRP, label=Station_name))
 
plot1<-partialPlot(rfP_model, P_train_preprocessed, PctCrop2019CatRp100)
plot1
plot2<-partialPlot(rfP_model, P_train_preprocessed, PctCrop2019WsRp100)
plot3

plot3<-partialPlot(rfP_model, P_train_preprocessed, KffactCat)
plot3

plot4<-partialPlot(rfP_model, P_train_preprocessed, PermCat)
plot4

plot5<-partialPlot(rfP_model, P_train_preprocessed, AgKffactWs)
plot5

plot6<-partialPlot(rfP_model, P_train_preprocessed, PctMxFst2019WsRp100)
plot6

plot7<-partialPlot(rfP_model, P_train_preprocessed, PctUrbOp2019CatRp100)
plot7

plot8<-partialPlot(rfP_model, P_train_preprocessed, Precip_Minus_EVTWs)
plot8

plot9<-partialPlot(rfP_model, P_train_preprocessed, ClayWs)
plot9
plot10<-partialPlot(rfP_model, P_train_preprocessed, PctWdWet2019WsRp100)
plot10

plot11<-partialPlot(rfP_model, P_train_preprocessed, PctGrs2019Ws)
plot11
plot12<-partialPlot(rfP_model, P_train_preprocessed, Pestic97Cat)
plot12

plot13<-partialPlot(rfP_model, P_train_preprocessed, MAST_2013)
plot13

plot14<-partialPlot(rfP_model, P_train_preprocessed, Phos_Crop_UptakeWs)
plot14

plot15<-partialPlot(rfP_model, P_train_preprocessed, FertWs)
plot15

#Make nicer plots with formatting and put in one figure:
library(ggtext)
P1.df<-as.data.frame(plot1)
plot1a<-ggplot(P1.df)+
  geom_line(aes(x,y))+
  xlab("% Cropland <br> in riparian areas - Cat")+
  ylab("Partial dependence")+
  ylab("")+
  ylim(0, 0.12)+
  theme(panel.grid=element_blank(), 
        axis.text=element_text(size=14),
        axis.title.x=element_markdown(size=14))
plot1a

P2.df<-as.data.frame(plot2)
plot2a<-ggplot(P2.df)+
  geom_line(aes(x,y))+
  xlab("% Cropland <br> in riparian areas - Ws")+
  ylab("")+
  ylim(0, 0.12)+
  theme(panel.grid=element_blank(), 
        axis.text=element_text(size=14),
        axis.title.x=element_markdown(size=14))
plot2a

P3.df<-as.data.frame(plot3)
plot3a<-ggplot(P3.df)+
  geom_line(aes(x,y))+
  xlab("Mean Soil Erodibility - Cat")+
  ylab("")+
  ylim(0, 0.12)+
  theme(panel.grid=element_blank(), 
        axis.text=element_text(size=14),
        axis.title.x=element_markdown(size=14))
plot3a

P4.df<-as.data.frame(plot4)
plot4a<-ggplot(P4.df)+
  geom_line(aes(x,y))+
  xlab("Mean soil permeability - Cat")+
  ylab("")+
  ylim(0, 0.12)+
  theme(panel.grid=element_blank(), 
        axis.text=element_text(size=14),
        axis.title.x=element_markdown(size=14))
plot4a

P5.df<-as.data.frame(plot5)
plot5a<-ggplot(P5.df)+
  geom_line(aes(x,y))+
  xlab("Mean Soil Erodibility <br> on ag soils - Cat")+
  ylab("Partial dependence")+
  ylab("")+
  ylim(0, 0.12)+
  theme(panel.grid=element_blank(), 
        axis.text=element_text(size=14),
        axis.title.x=element_markdown(size=14))
plot5a

P6.df<-as.data.frame(plot6)
plot6a<-ggplot(P6.df)+
  geom_line(aes(x,y))+
  xlab("% Mixed forest <br> in riparian areas - Ws")+
  ylab("")+
  ylim(0, 0.12)+
  theme(panel.grid=element_blank(), 
        axis.text=element_text(size=14),
        axis.title.x=element_markdown(size=14))
plot6a

P7.df<-as.data.frame(plot7)
plot7a<-ggplot(P7.df)+
  geom_line(aes(x,y))+
  xlab("% developed, open land <br> in riparian areas - Ws")+
  ylab("Partial dependence")+
  ylab("")+
  ylim(0, 0.12)+
  theme(panel.grid=element_blank(), 
        axis.text=element_text(size=14),
        axis.title.x=element_markdown(size=14))
plot7a

P8.df<-as.data.frame(plot8)
plot8a<-ggplot(P8.df)+
  geom_line(aes(x,y))+
  xlab("Precipitation minus <br> evaporation - Ws")+
  ylab("")+
  ylim(0, 0.12)+
  theme(panel.grid=element_blank(), 
        axis.text=element_text(size=14),
        axis.title.x=element_markdown(size=14))
plot8a

P9.df<-as.data.frame(plot9)
plot9a<-ggplot(P9.df)+
  geom_line(aes(x,y))+
  xlab("% Clay in soils - Ws")+
  ylab("Partial dependence")+
  ylab("")+
  ylim(0, 0.12)+
  theme(panel.grid=element_blank(), 
        axis.text=element_text(size=14),
        axis.title.x=element_markdown(size=14))
plot9a

P10.df<-as.data.frame(plot10)
plot10a<-ggplot(P10.df)+
  geom_line(aes(x,y))+
  xlab("% Woody wetland cover <br> in riparian areas - Ws")+
  ylab("Partial dependence")+
  ylab("")+
  ylim(0, 0.12)+
  theme(panel.grid=element_blank(), 
        axis.text=element_text(size=14),
        axis.title.x=element_markdown(size=14))
plot10a

P11.df<-as.data.frame(plot11)
plot11a<-ggplot(P11.df)+
  geom_line(aes(x,y))+
  xlab("% grassland - Ws")+
  ylab("Partial dependence")+
  ylab("")+
  ylim(0, 0.12)+
  theme(panel.grid=element_blank(), 
        axis.text=element_text(size=14),
        axis.title.x=element_markdown(size=14))
plot11a

P12.df<-as.data.frame(plot12)
plot12a<-ggplot(P12.df)+
  geom_line(aes(x,y))+
  xlab("Mean pesticide <br>use (kg/km<sup>2</sup>) <br> in yr. 1997 - Cat")+
  ylab("")+
  ylim(0, 0.12)+
  theme(panel.grid=element_blank(), 
        axis.text=element_text(size=14),
        axis.title.x=element_markdown(size=14))
plot12a

P13.df<-as.data.frame(plot13)
plot13a<-ggplot(P13.df)+
  geom_line(aes(x,y))+
  xlab("Predicted mean annual <br>stream temperature \u00b0C <br> 2013")+
  ylab("")+
  ylim(0, 0.12)+
  theme(panel.grid=element_blank(), 
        axis.text=element_text(size=14),
        axis.title.x=element_markdown(size=14))
plot13a

P14.df<-as.data.frame(plot14)
plot14a<-ggplot(P14.df)+
  geom_line(aes(x,y))+
  xlab("Phosphorus uptake <br> by crops - Ws")+
  ylab("Partial dependence")+
  ylab("")+
  ylim(0, 0.12)+
  theme(panel.grid=element_blank(), 
        axis.text=element_text(size=14),
        axis.title.x=element_markdown(size=14))
plot14a

P15.df<-as.data.frame(plot15)
plot15a<-ggplot(P15.df)+
  geom_line(aes(x,y))+
  xlab("Mean rate of fertilizer <br> application to ag land - Ws")+
  ylab("")+
  ylim(0, 0.12)+
  theme(panel.grid=element_blank(), 
        axis.text=element_text(size=14),
        axis.title.x=element_markdown(size=14))
plot15a


library(gridExtra)
grid.arrange(plot1a, plot2a, plot3a, plot4a, plot5a,
             plot6a, plot7a, plot8a, plot9a, plot10a, plot11a,
             plot12a, plot13a, plot14a, plot15a, ncol=3)

#write fig to file
setwd(output_dir)
jpeg("./Figures/Partial_dependence_plot.jpeg", units="in", width=9, height=12, res=300)
grid.arrange(plot1a, plot2a, plot3a, plot4a, plot5a,
             plot6a, plot7a, plot8a, plot9a, plot10a, plot11a,
             plot12a, plot13a, plot14a, plot15a, ncol=3)
dev.off()


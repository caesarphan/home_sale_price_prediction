library(ISLR)
library(glmnet)
library(pls)
library(dplyr)
library(tidyverse)
library(leaps)
library(boot)
library(MASS)
library(DAAG)
library(standardize)
df <- read.csv("~/OneDrive - UC Irvine/BANA - Predictive Analytics /Project/train_bnry.csv")
##Factor 
df$FullBath <- as.factor(df$FullBath)
df$BedroomAbvGr <- as.factor(df$BedroomAbvGr)
df$KitchenAbvGr <- as.factor(df$KitchenAbvGr)
df$TotRmsAbvGrd <- as.factor(df$TotRmsAbvGrd)
df$Fireplaces <- as.factor(df$Fireplaces)
df$GarageCars <- as.factor(df$GarageCars)
##Standardize 
df$LotFrontage <- scale(df$LotFrontage)[, 1]
df$LotArea <- scale(df$LotArea)[, 1]
df$MasVnrArea <- scale(df$MasVnrArea)[, 1]
df$BsmtFinSF1 <- scale(df$BsmtFinSF1)[, 1]
df$BsmtFinSF2 <- scale(df$BsmtFinSF2)[, 1]
df$BsmtUnfSF <- scale(df$BsmtUnfSF)[, 1]
df$TotalBsmtSF <- scale(df$TotalBsmtSF)[, 1]
df$X1stFlrSF<- scale(df$X1stFlrSF)[, 1]
df$X2ndFlrSF <- scale(df$X2ndFlrSF)[, 1]
df$LowQualFinSF <- scale(df$LowQualFinSF)[, 1]
df$GrLivArea <- scale(df$GrLivArea)[, 1]
df$GarageArea <- scale(df$GarageArea)[, 1]
df$WoodDeckSF <- scale(df$WoodDeckSF)[, 1]
df$OpenPorchSF <- scale(df$OpenPorchSF)[, 1]
df$EnclosedPorch <- scale(df$EnclosedPorch)[, 1]
df$X3SsnPorch <- scale(df$X3SsnPorch)[, 1]
df$ScreenPorch <- scale(df$ScreenPorch)[, 1]
df$PoolArea <- scale(df$PoolArea)[, 1]
df$MiscVal <- scale(df$MiscVal)[, 1]





####WHOLE DATA 
#all variables
reg1<- lm(SalePrice~.,df)
summary(reg1)
RSS1<- sum(reg1$residuals^2)
MSE1<- RSS1/(nrow(df)-1)
RMSE1<- MSE1^0.5
RMSE1 #RMSE->20592.38 (nothing standardize), 0.2502147 (standardize saleprice), 19877, 0.9374(X variable standardize)


#subset
regsubset1 <- regsubsets(SalePrice~. ,nvmax=350,df,
                         method = "forward",really.big=TRUE)
regsummary1<-summary(regsubset1)
Rsquare=which.max(regsummary1$adjr2)
cp=which.min(regsummary1$cp)
bic=which.min(regsummary1$bic)
Rsquare #183
cp #116
bic #59
regsummary1$which[65,]

#LotArea+MasVnrArea+BsmtFinSF1+TotalBsmtSF+X1stFlrSF+GrLivArea+FullBath2+FullBath3+HalfBath+BedroomAbvGr5+KitchenAbvGr1
#+TotRmsAbvGrd10+Fireplaces2+Fireplaces3+GarageCars3+GarageArea+WoodDeckSF+ScreenPorch+PoolArea+MSZoning_RM+Street_Pave
#+LotConfig_CulDSac+Neighborhood_Crawfor+Neighborhood_Edwards+Neighborhood_NoRidge+Neighborhood_NridgHt+Neighborhood_Somerst
#+Neighborhood_StoneBr+Condition1_Feedr+Condition1_RRAe+Condition2_PosN+BldgType_Twnhs+BldgType_TwnhsE+RoofMatl_Tar.Grv
#+RoofMatl_WdShngl+Exterior1st_BrkFace+ExterQual_Fa+ExterQual_Gd+ExterQual_TA+Foundation_PConc+BsmtQual_Fa+BsmtQual_Gd
#+BsmtQual_TA+BsmtExposure_Gd+BsmtFinType1_GLQ+BsmtFinType2_GLQ+HeatingQC_Gd+HeatingQC_TA+CentralAir_Y+KitchenQual_Fa
#+KitchenQual_Gd+KitchenQual_TA+Functional_Typ+GarageYrBlt_1918.0+GarageYrBlt_1972.0+GarageYrBlt_2008.0+GarageYrBlt_2009.0
#+PoolQC_Fa+PoolQC_Gd+PoolQC_NA+GarageCond_TA+MiscFeature_TenC+SaleType_New+SaleCondition_AdjLand+SaleCondition_Normal

regsub=lm(SalePrice~LotArea+MasVnrArea+BsmtFinSF1+TotalBsmtSF+X1stFlrSF+GrLivArea+FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr
          +TotRmsAbvGrd+Fireplaces+GarageCars+GarageArea+WoodDeckSF+ScreenPorch+PoolArea+MSZoning_RM+Street_Pave
          +LotConfig_CulDSac+Neighborhood_Crawfor+Neighborhood_Edwards+Neighborhood_NoRidge+Neighborhood_NridgHt+Neighborhood_Somerst
          +Neighborhood_StoneBr+Condition1_Feedr+Condition1_RRAe+Condition2_PosN+BldgType_Twnhs+BldgType_TwnhsE+RoofMatl_Tar.Grv
          +RoofMatl_WdShngl+Exterior1st_BrkFace+ExterQual_Fa+ExterQual_Gd+ExterQual_TA+Foundation_PConc+BsmtQual_Fa+BsmtQual_Gd
          +BsmtQual_TA+BsmtExposure_Gd+BsmtFinType1_GLQ+BsmtFinType2_GLQ+HeatingQC_Gd+HeatingQC_TA+CentralAir_Y+KitchenQual_Fa
          +KitchenQual_Gd+KitchenQual_TA+Functional_Typ+GarageYrBlt_1918.0+GarageYrBlt_1972.0+GarageYrBlt_2008.0+GarageYrBlt_2009.0
          +PoolQC_Fa+PoolQC_Gd+PoolQC_NA+GarageCond_TA+MiscFeature_TenC+SaleType_New+SaleCondition_AdjLand+SaleCondition_Normal,df)
#Fullbath2,fullbath3, HalfBath, BedroomAbvGr5, KitchenAbvGr1,TotalRmsAbvGrd10 
summary(regsub) #r2<-0.911
RSSsub<- sum(regsub$residuals^2)
MSEsub<- RSSsub/(nrow(df)-1)
RMSEsub<- MSEsub^0.5
RMSEsub  #RMSE=23704

#before standardize subset
regsub1=lm(SalePrice~LotArea+MasVnrArea+BsmtUnfSF+TotalBsmtSF+LowQualFinSF+GrLivArea+GarageCars+Fireplaces+TotRmsAbvGrd+KitchenAbvGr+BedroomAbvGr+FullBath
          +WoodDeckSF+ScreenPorch+PoolArea+Street_Pave+MSZoning_RM+LotConfig_CulDSac+Neighborhood_Crawfor+Neighborhood_Edwards+Neighborhood_NAmes+Neighborhood_NoRidge
          +Neighborhood_NridgHt+Neighborhood_SWISU+Neighborhood_Somerst+Condition1_Norm+Neighborhood_StoneBr+BldgType_Twnhs+Condition2_PosN+BldgType_TwnhsE+RoofMatl_Tar.Grv
          +RoofMatl_WdShngl+Exterior1st_BrkFace+Exterior2nd_VinylSd+ExterQual_Fa+ExterQual_Gd +ExterQual_TA+BsmtQual_Fa+BsmtQual_Gd+BsmtQual_TA +BsmtExposure_Gd+BsmtExposure_NA
          +BsmtExposure_No+BsmtFinType1_GLQ+CentralAir_Y+KitchenQual_Fa+KitchenQual_Gd+KitchenQual_TA+Functional_Typ+GarageYrBlt_1918.0+GarageYrBlt_2008.0+GarageYrBlt_2009.0
          +GarageCond_Fa +PoolQC_Gd+PoolQC_Fa+PoolQC_NA+MiscFeature_TenC+SaleType_New +SaleCondition_Normal, df)
summary(regsub1) #r2= 0.9102 (after standardize)
RSSsub1<- sum(regsub1$residuals^2)
MSEsub1<- RSSsub1/(nrow(df)-1)
RMSEsub1<- MSEsub1^0.5
RMSEsub1 #24596.95 , 23803.91(after standardize)

####We want to see how it performs in the testing, so we decided to split the data 70% train 30% test 
####Split data with train and test 
set.seed(970711)
nrows.train <- 1022
train <- sample(1:nrow(df),nrows.train)
train1 <- df[train,]
test1 <- df[-train,]

#all variables
reg2<- lm(SalePrice~.,train1)
summary(reg2) #r2-> 0.9464
RSS2<- sum(reg2$residuals^2)
MSE2<- RSS2/(nrow(train1)-350-1)
RMSE2<- MSE2^0.5
RMSE2 #RMSE-> 23728.68
new.y2hat <- predict(reg2, test1)
RSS2test <- sum((test1$SalePrice-new.y2hat)^2)
MSE2test <- RSS2test/(nrow(test1))
RMSE2test <- MSE2test^0.5
RMSE2test #RMSE->44863.49
##OVERFITTING, turns out, if we use all the variables, there is an overfitting problem. 
#RMSE in training is 24797.44 and training is 46483.09
##Let's try to how the the testing set perform using the subset model we had. 

#subset
regsubset2= regsubsets(SalePrice~. ,nvmax=350,train1,
                       method = "forward",really.big=TRUE)
regsummary2<-summary(regsubset2)
Rsquare=which.max(regsummary2$adjr2)
cp=which.min(regsummary2$cp)
bic=which.min(regsummary2$bic)
Rsquare #167
cp #141
bic #54
regsummary2$which[54,]

#LotArea+BsmtFinSF1+TotalBsmtSF+GrLivArea+FullBath+BedroomAbvGr+KitchenAbvGr+TotRmsAbvGrd+GarageCars+GarageArea+WoodDeckSF
#+X3SsnPorch+ScreenPorch+PoolArea+MSZoning_RM+Street_Pave+Neighborhood_Crawfor+Neighborhood_Edwards+Neighborhood_NoRidge
#+Neighborhood_NridgHt+Neighborhood_Somerst+Neighborhood_StoneBr+Condition1_Norm+Condition2_PosN+BldgType_Twnhs+BldgType_TwnhsE
#+RoofMatl_Tar.Grv+RoofMatl_WdShngl+Exterior1st_BrkFace+Exterior2nd_VinylSd+ExterQual_Fa+ExterQual_Gd+ExterQual_TA+BsmtQual_Fa
#+BsmtQual_Gd+BsmtQual_TA+BsmtExposure_Gd+BsmtExposure_No+BsmtFinType1_GLQ+BsmtFinType2_NA+CentralAir_Y+KitchenQual_Fa
#+KitchenQual_Gd+KitchenQual_TA+Functional_Typ+FireplaceQu_NA+GarageCond_TA+PoolQC_Fa+PoolQC_Gd+PoolQC_NA+MiscFeature_TenC
#+SaleType_New+SaleCondition_AdjLand+SaleCondition_Normal
##Fullbath3, BedroomAbvGr5,KitchenAbvGr1,TotRmsAbvGrd10,GarageCars3 

regsub2=lm(SalePrice~LotArea+BsmtFinSF1+TotalBsmtSF+GrLivArea+FullBath+BedroomAbvGr+KitchenAbvGr+TotRmsAbvGrd+GarageCars+GarageArea+WoodDeckSF
           +X3SsnPorch+ScreenPorch+PoolArea+MSZoning_RM+Street_Pave+Neighborhood_Crawfor+Neighborhood_Edwards+Neighborhood_NoRidge
           +Neighborhood_NridgHt+Neighborhood_Somerst+Neighborhood_StoneBr+Condition1_Norm+Condition2_PosN+BldgType_Twnhs+BldgType_TwnhsE
           +RoofMatl_Tar.Grv+RoofMatl_WdShngl+Exterior1st_BrkFace+Exterior2nd_VinylSd+ExterQual_Fa+ExterQual_Gd+ExterQual_TA+BsmtQual_Fa
           +BsmtQual_Gd+BsmtQual_TA+BsmtExposure_Gd+BsmtExposure_No+BsmtFinType1_GLQ+BsmtFinType2_NA+CentralAir_Y+KitchenQual_Fa
           +KitchenQual_Gd+KitchenQual_TA+Functional_Typ+FireplaceQu_NA+GarageCond_TA+PoolQC_Fa+PoolQC_Gd+PoolQC_NA+MiscFeature_TenC
           +SaleType_New+SaleCondition_AdjLand+SaleCondition_Normal, train1)
summary(regsub2) #r2<-0.907, r2=0.9122
RSSsub2<- sum(regsub2$residuals^2)
MSEsub2<- RSSsub2/(nrow(train1)-1)
RMSEsub2<- MSEsub2^0.5
RMSEsub2 #21209.39, 24634.49
new.y2bhat <- predict(regsub2, test1)
RSS2btest <- sum((test1$SalePrice-new.y2bhat)^2)
MSE2btest <- RSS2btest/(nrow(test1))
RMSE2btest <- MSE2btest^0.5
RMSE2btest #25063, 26424.43


# This installs the packages if you don't have them, and loads them into your environment.
# If you need more packages, add them to the packages vector.
packages <- c("tidyverse", "faux", "DataExplorer", "randomForest", "caret", "corrplot", "modelr", "stats", "rpart.plot", "mlbench", "imputeMissings", "ggplot2", "cowplot")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if(!require(x, character.only = TRUE)){
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)



# Define our RMSE and RMSLE functions to evaluate our model
r2 <- function(actual, predicted){
  TSS <- sum((actual - mean(actual))^2)
  RSS <- sum((actual - predicted)^2)
  1 - RSS/TSS
}

rmse <- function(actual, fitted){
  sqrt(mean((actual - fitted)^2))
}

rmsle <- function(actual, fitted) {
  sqrt(mean((log(fitted+1) - log(actual+1))^2))
}


calc_mode <- function(x){
  # List the distinct / unique values
  distinct_values <- na.omit(unique(x))
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}



# Determine how many NAs there are in the data
read.csv("train.csv") %>%
  sapply(function(x) sum(is.na(x)))

read.csv("test.csv") %>%
  sapply(function(x) sum(is.na(x)))


# Clean our data
cleaner <- function(dirty_data) {
  clean_data <- dirty_data %>%
    mutate(
      MSSubClass = factor(MSSubClass, level = c(20, 30, 40, 45, 50, 60, 70, 75, 80, 85, 90, 120, 150, 160, 180, 190)),
      MSZoning = MSZoning %>% replace_na("NoZone"), # Train data: No NAs; Test data: HAS NAs
      MSZoning = factor(MSZoning, levels = c("A", "C (all)", "FV", "I", "RH", "RL", "RP", "RM", "NoZone")),
      
      LotFrontage = LotFrontage %>% replace_na(0), # Train data: HAS NAs; Test data: HAS NAs
      LotFrontage = log(LotFrontage + 1),
      
      # Logging LotArea in the cleaner function rather than the read csv function
      LotArea = log(LotArea +1),
      
      Street = factor(Street),
      Alley = Alley %>% replace_na("NoAcc"), # Replace NAs with NoAcc # HAS NAs
      Alley = factor(Alley),
      LotShape = factor(LotShape),
      LandContour = factor(LandContour),
      Utilities = Utilities %>% replace_na("Unknown"),
      Utilities = factor(Utilities, levels = c("AllPub", "NoSewr", "NoSeWa", "ELO", "Unknown")),
      LotConfig = factor(LotConfig),
      LandSlope = factor(LandSlope),
      Neighborhood = factor(Neighborhood),
      Condition1 = factor(Condition1),
      Condition2 = factor(Condition2),
      BldgType = factor(BldgType),
      HouseStyle = factor(HouseStyle),
      OverallQual = factor(OverallQual),
      OverallCond = factor(OverallCond),
      
      #These should not be factors
      #   YearBuilt = factor(YearBuilt, levels = factor(YearBuilt) %>% levels),
      #   YearRemodAdd = factor(YearRemodAdd, levels = factor(YearRemodAdd) %>% levels),
      
      RoofStyle = factor(RoofStyle),
      RoofMatl = factor(RoofMatl),
      
      #Replacing NA with mode
      Exterior1st = factor(Exterior1st), # Train data: No NAs; Test data: HAS 1 NA
      Exterior1st = Exterior1st %>% replace_na(calc_mode(levels(Exterior1st))),
      
      Exterior2nd = Exterior2nd %>% replace_na("Other"), #Train data: No NAs; Test data: HAS 1 NA
      Exterior2nd = factor(Exterior2nd),
      MasVnrType = MasVnrType %>% replace_na("None"), # Train data: HAS NAs; Test data: HAS NAs
      MasVnrType = factor(MasVnrType),
      MasVnrArea = MasVnrArea %>% replace_na(0), # Train data: HAS NAs; Test data: No NAs
      
      # MasVnrArea - log(MasVnrArea), # No benefit in logging it
      
      ExterQual = factor(ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex")),
      ExterCond = factor(ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex")),
      Foundation = factor(Foundation),
      BsmtQual = BsmtQual %>% replace_na("NoBsmt"), # Train data: HAS NAs; Test data: HAS NAs
      BsmtQual = factor(BsmtQual, levels = c("NoBsmt", "Po", "Fa", "TA", "Gd", "Ex")), # HAS NAs
      BsmtCond = BsmtCond %>% replace_na("NoBsmt"), # HAS NAs
      BsmtCond = factor(BsmtCond, levels = c("NoBsmt", "Po", "Fa", "TA", "Gd", "Ex")), # HAS NAs
      BsmtExposure = BsmtExposure %>% replace_na("NoBsmt"), # HAS NAs
      BsmtExposure = factor(BsmtExposure, levels = c("NoBsmt", "No", "Mn", "Av", "Gd")), # HAS NAs
      BsmtFinType1 = BsmtFinType1 %>% replace_na("NoBsmt"), # HAS NAs
      BsmtFinType1 = factor(BsmtFinType1, levels = c("NoBsmt", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")), # HAS NAs
      BsmtFinType2 = BsmtFinType2 %>% replace_na("NoBsmt"), # HAS NAs
      BsmtFinType2 = factor(BsmtFinType2, levels = c("NoBsmt", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")),
      
      # Replacing NA with 0
      BsmtFinSF1 = BsmtFinSF1 %>% replace_na(0), # Train data: No NAs, Test data: HAS 1 NA
      BsmtFinSF2 = BsmtFinSF2 %>% replace_na(0), # Test data: HAS 1 NA
      BsmtUnfSF = BsmtUnfSF %>% replace_na(0), # Test data: HAS 1 NA
      TotalBsmtSF = TotalBsmtSF %>% replace_na(0), # Test data: HAS 1 NA
      
      TotalSF = TotalBsmtSF + GrLivArea,
      TotalSF = log(TotalSF +1),
      
      # Logging
      BsmtFinSF1 = log(BsmtFinSF1 +1), # Highly skewed right with most values at 0
      BsmtFinSF2 = log(BsmtFinSF2 +1), # Most values at 0

      #BsmtUnfSF = log(BsmtUnfSF), # Negative beneift when logged
      TotalBsmtSF = log(TotalBsmtSF +1), 
      
      Heating = factor(Heating),
      HeatingQC = factor(HeatingQC, levels = c("Po", "Fa", "TA", "Gd", "Ex")),
      CentralAir = factor(CentralAir),
      Electrical = Electrical %>% replace_na("Mix"), # Train data: HAS NA; Test data: No NAs
      Electrical = factor(Electrical), # HAS NA
      
      # Logging X1stFlrSF in cleaner function rather than read csv function
      # X1stFlrSF = log(X1stFlrSF), # No benefit in logging
      X2ndFlrSF = log(X2ndFlrSF +1),
      LowQualFinSF = log(LowQualFinSF +1),
      
      # Logging in cleaner function rather than read csv function
      GrLivArea = log(GrLivArea +1),
      
      BsmtFullBath  = BsmtFullBath %>% replace_na(0), # Test data: HAS 2 NAs
      BsmtHalfBath  = BsmtHalfBath %>% replace_na(0), # Test data: HAS 2 NAs
      
      #    FullBath = factor(), # Probably doesn't need any mutations
      #    HalfBath = factor(), # Probably doesn't need any mutations
      #    BedroomAbvGr = factor(), # Probably doesn't need any mutations
      #    KitchenAbvGr = factor(), # Probably doesn't need any mutations
      
      KitchenQual = KitchenQual %>% replace_na("TA"), # Test data: has 1 NA
      KitchenQual = factor(KitchenQual, levels = c("Po", "Fa", "TA", "Gd", "Ex")),
      
      #    KitchenQual = as.numeric(KQ_factor), #might need to be as.numeric(KitchenQual)
      #    TotRmsAbvGrd = factor(), # Probably doesn't need any mutations
      
      Functional = Functional %>% replace_na("Typ"), # Test data: has 2 NAs
      Functional = factor(Functional, levels = c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ")),
      
      #    Fireplaces = factor(), # Probably doesn't need any mutations
      
      FireplaceQu = FireplaceQu %>% replace_na("NoFp"), # NAs should specify no fireplace # HAS NAs
      FireplaceQu = factor(FireplaceQu, levels = c("NoFp", "Po", "Fa", "TA", "Gd", "Ex")),  # HAS NAs
      
      GarageType = GarageType %>% replace_na("NoGrge"), #NAs should specify no garage # HAS NAs
      GarageType = factor(GarageType), # HAS NAs
      
      GarageYrBlt = GarageYrBlt %>% replace_na(0), # NAs should use the median [1980] to not mess with the data # HAS NAs
      
      # Logging
      GarageYrBlt = log(GarageYrBlt +1),
      
      GarageFinish = GarageFinish  %>% replace_na("NoGrge"), # HAS NAs
      GarageFinish = factor(GarageFinish), # NAs should specify no garage # HAS NAs
      
      GarageCars = GarageCars %>% replace_na(0), # Test data: HAS 1 NA
      
      GarageArea = GarageArea %>% replace_na(0), # Test data: HAS 1 NA
      
      # Logging
      #   GarageArea = log(GarageArea),
      
      GarageQual = GarageQual %>% replace_na("NoGrge"), # NAs should specify no garage # HAS NAs
      GarageQual = factor(GarageQual, levels = c("NoGrge", "Po", "Fa", "TA", "Gd", "Ex")), # HAS NAs
      
      GarageCond = GarageCond %>% replace_na("NoGrge"), # NAs should specify no garage # HAS NAs
      GarageCond = factor(GarageCond, levels = c("NoGrge", "Po", "Fa", "TA", "Gd", "Ex")), # HAS NAs
      
      PavedDrive = factor(PavedDrive),
      
      # Logging
      #   WoodDeckSF = log(WoodDeckSF), # No beneift in logging
      OpenPorchSF = log(OpenPorchSF +1),
      EnclosedPorch = log(EnclosedPorch +1),
      X3SsnPorch = log(X3SsnPorch +1),
      #   ScreenPorch = log(ScreenPorch), # No benefit in logging
      #   PoolArea = log(PoolArea), # No benfit in logging
      
      PoolQC = PoolQC %>% replace_na("NoPool"), #NAs should specify no pool # HAS NAs
      PoolQC = factor(PoolQC, levels = c("NoPool", "Fa", "TA", "Gd", "Ex")), # HAS NAs
      
      Fence = Fence %>% replace_na("NoFnc"), #NAs should specify no fence # HAS NAs
      Fence = factor(Fence), # HAS NAs
      
      MiscFeature = MiscFeature %>% replace_na("NoMiscF"), #NAs should specify no amenities # HAS NAs
      MiscFeature = factor(MiscFeature), # HAS NAs
      
      #    MiscVal = factor(), # Probably doesn't need any mutations
      MoSold = factor(MoSold),
      #    YrSold = factor(), # Probably doesn't need any mutations
      
      SaleType = factor(SaleType),
      SaleType = SaleType %>% replace_na(calc_mode(levels(SaleType))), # Test data has 1 NA
      
      SaleCondition = factor(SaleCondition),
      
      # New Predictors
      YrBltAndRemod = YearBuilt + YearRemodAdd,
      TotalSF = TotalBsmtSF + X1stFlrSF + X2ndFlrSF,
      Total_Sq_Footage = (BsmtFinSF1 + BsmtFinSF2 + X1stFlrSF + X2ndFlrSF),#Kept by Ridge Regression
      Total_Bathrooms = (FullBath + (.5 * HalfBath) + BsmtFullBath + (.5 * BsmtHalfBath)),#Kept by Ridge Regression
      Total_Porch_SF = (OpenPorchSF + X3SsnPorch + EnclosedPorch + ScreenPorch + WoodDeckSF),#Kept by Ridge Regression
      HasPool = ifelse(PoolArea > 0, 1, 0),
      Has2ndFloor = ifelse(X2ndFlrSF > 0, 1, 0),
      HasGarage = ifelse(GarageArea > 0, 1, 0),#Kept by Ridge Regression
      HasBsmt = ifelse(TotalBsmtSF > 0, 1, 0),    
      HasFireplace = ifelse(Fireplaces > 0, 1, 0)#Kept by Ridge Regression
    )
  return(clean_data)
}


# Read in the data from CSV to RStudio
train_data <- read.csv("train.csv") %>%
  cleaner() %>%
  select(-Utilities, -Street, -PoolQC) %>%
  mutate(SalePrice = log(SalePrice))

complete.cases(train_data) %>% all

test_data <- read.csv("test.csv") %>%
  cleaner() %>% 
  select(-Utilities, -Street, -PoolQC)


complete.cases(test_data) %>% all


###### There are some outliers that we might consider removing from our model. Anything more than 4,000 SF GrLivArea or 6,000 SF TotalSF



# Create linaer model with GLMNET using CARET
set.seed(123)
c_lm <- train(SalePrice ~
                MSSubClass +
                MSZoning +
                LotFrontage +
                LotArea +
                Alley +
                LotShape +
                LandContour +
                LotConfig +
                LandSlope +
                Condition1 +
                Condition2 +
                BldgType +
                HouseStyle +
                GrLivArea * OverallQual  +
                GrLivArea * Neighborhood +
                TotalSF * Neighborhood +
                OverallCond * Neighborhood +
                OverallQual * Neighborhood + 
                YearBuilt +
                YearRemodAdd +
                RoofStyle +
                RoofMatl +
                Exterior1st +
                Exterior2nd +
                MasVnrType +
                MasVnrArea +
                ExterQual +
                ExterCond +
                Foundation +
                BsmtQual +
                BsmtCond +
                BsmtExposure +
                BsmtFinType1 +
                BsmtFinSF1 + 
                BsmtFinType2 +
                BsmtFinSF2 +
                BsmtUnfSF + 
                TotalBsmtSF +
                Heating +
                HeatingQC +
                CentralAir +
                Electrical +
                #   X1stFlrSF +  # Highly correlated to 'TotalBsmtSF'
                #   X2ndFlrSF +
                LowQualFinSF +
                BsmtFullBath +
                BsmtHalfBath +
                FullBath +
                HalfBath +
                BedroomAbvGr +
                KitchenAbvGr +
                KitchenQual +
                #   TotRmsAbvGrd +  # Highly correlated to 'GrLivArea'
                Functional +
                Fireplaces +
                FireplaceQu +
                GarageType +
                #   GarageYrBlt +
                GarageFinish +
                GarageCars +
                GarageCond +
                GarageQual +
                #   GarageArea +  # Highly correlated to 'GarageCars'
                PavedDrive +
                WoodDeckSF +
                OpenPorchSF +
                EnclosedPorch +
                X3SsnPorch +
                ScreenPorch +
                PoolArea +
                Fence +
                MiscFeature +
                MiscVal +
                MoSold +
                YrSold +
                SaleType +
                SaleCondition +
                #   TotalSF + #Duplicate because interaction
                Total_Sq_Footage +
                Total_Bathrooms +
                Total_Porch_SF +
                HasPool +
                Has2ndFloor +
                HasGarage +
                HasBsmt +
                HasFireplace,
                data = train_data,
                preProcess = c("center", "scale"),
                method = "glmnet")


#In-sample performance
#R^2
r2(exp(train_data$SalePrice), exp(fitted(c_lm)))
#RSME
rmse(exp(train_data$SalePrice), exp(fitted(c_lm)))
#RSMLE
rmsle(exp(train_data$SalePrice), exp(fitted(c_lm)))

#Out-of-sample performance
best_alpha <- c_lm$results[c_lm$results$alpha==c_lm$bestTune$alpha, ]
best_alpha_lambda <- best_alpha[best_alpha$lambda==c_lm$bestTune$lambda, ]
#R^2
best_alpha_lambda$Rsquared
#RSMLE
best_alpha_lambda$RMSE

#90% Ridge Regression and 10# Lasso Regression Coefficients Removed
c_lm_coeffs <- coef(c_lm$finalModel, c_lm$bestTune$lambda)
c_lm_Rmvd_Coef <- c_lm_coeffs[c_lm_coeffs[,1]==0,0]
c_lm_Rmvd_Coef
nrow(c_lm_Rmvd_Coef)
c_lm_Kept_Coef <- c_lm_coeffs[c_lm_coeffs[,1]!=0,0]
c_lm_Kept_Coef
nrow(c_lm_Kept_Coef)


submission_data <- test_data %>%
  select(Id) %>%
  mutate(SalePrice = exp(predict(c_lm, test_data)))

write.csv(submission_data, "submission_file.csv", row.names=FALSE)


#Kaggle submission results:

#All predictors, no transformations 
# Result := Log RSME 0.20247

#All predictors, log transformations on:
#   SalePrice
#   LotFrontage
#   LotArea
#   X1stFlrSF
#   GrLivArea
# Result := Log RSME 0.15104

#All predictors, scaled and centered
# Result := Log RSME 0.19524

#All predictors, scaled and centered, log transformations on:
#   SalePrice
#   LotFrontage
#   LotArea
#   X1stFlrSF
#   GrLivArea
# Result := Log RSME 0.15104

#All predictors, ridge/lasso, log transformations on:
#   SalePrice
#   LotArea
#   X1stFlrSF
#   GrLivArea
# Kaggle Result := Log RSME 0.14007
#   alpha       lambda      RMSE  Rsquared        MAE      RMSESD RsquaredSD       MAESD
#3  0.10 0.0583203499 0.1421900 0.8742627 0.08719893 0.013919797 0.02815391 0.003560103

#All predictors, ridge/lasso, scaled and centered, log transformations on:
#   SalePrice
#   LotArea
#   X1stFlrSF
#   GrLivArea
#   alpha       lambda      RMSE  Rsquared        MAE      RMSESD RsquaredSD       MAESD
#3  0.10 0.0583203499 0.1402955 0.8790267 0.08679432 0.012036779 0.02125528 0.003616432
# Kaggle Result := Log RSME 0.14007

#All predictors minus highly correlated predictors (X1stFlrSF, TotRmsAbvGrd, and GarageArea), and multiplying OverallQual * by GrLivArea
# Kaggle Result: = Log RMSE 0.13953

#Rubric score	Kaggle score (log RMSE)
# 10  <.12
# 9	  <.13 <- Current Score
# 8	  <.14 
# 7	  <.15
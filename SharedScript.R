# This installs the packages if you don't have them, and loads them into your environment.
# If you need more packages, add them to the packages vector.

packages <- c("tidyverse", "faux", "DataExplorer", "randomForest", "caret", "corrplot", "modelr", "stats", "rpart.plot", "mlbench")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if(!require(x, character.only = TRUE)){
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

rmse <- function(actual, fitted){
  sqrt(mean((actual - fitted)^2))
}

rmsle <- function(actual, fitted) {
  sqrt(mean((log(fitted+1) - log(actual+1))^2))
}

rsq <- function (actual, fitted) {
  cor(actual, fitted) ^ 2
}

calc_mode <- function(x){
  # List the distinct / unique values
  distinct_values <- na.omit(unique(x))
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}


read.csv("train.csv") %>%
  sapply(function(x) sum(is.na(x)))

read.csv("test.csv") %>%
  sapply(function(x) sum(is.na(x)))

cleaner <- function(dirty_data) {
  clean_data <- dirty_data %>%
    mutate(
      MSSubClass = factor(MSSubClass, level = c(20, 30, 40, 45, 50, 60, 70, 75, 80, 85, 90, 120, 150, 160, 180, 190)),
      MSZoning = MSZoning %>% replace_na("NoZone"), # Train data: No NAs; Test data: HAS NAs
      MSZoning = factor(MSZoning, levels = c("A", "C (all)", "FV", "I", "RH", "RL", "RP", "RM", "NoZone")),
      
      #I think this should be 0 is there is an NA Kaggle score went from 0.14012 to 0.14007
      LotFrontage = LotFrontage %>% replace_na(0), # Train data: HAS NAs; Test data: HAS NAs
      
      #    LotArea = factor(), # Doesn't need any mutations
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
      
      #These should not be factors, it's a year and they're be way too many coefficients
      # YearBuilt = factor(YearBuilt, levels = factor(YearBuilt) %>% levels),
      # YearRemodAdd = factor(YearRemodAdd, levels = factor(YearRemodAdd) %>% levels),
      
      RoofStyle = factor(RoofStyle),
      RoofMatl = factor(RoofMatl),
      
      #Replacing NA with mode
      Exterior1st = factor(Exterior1st), # Train data: No NAs; Test data: HAS 1 NA
      Exterior1st = Exterior1st %>% replace_na(calc_mode(levels(Exterior1st))),
      
      Exterior2nd = Exterior2nd %>% replace_na("Other"), #Train data: No NAs; Test data: HAS 1 NA
      Exterior2nd = factor(Exterior2nd),
      MasVnrType = MasVnrType %>% replace_na("None"), # Train data: HAS NAs; Test data: HAS NAs
      MasVnrType = factor(MasVnrType),
      MasVnrArea = MasVnrArea %>% replace_na(0), # Train data: HAS NAs; Test data: No NAs # NAs should be replaced with 0 since we don't know what NA means in this context
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
      BsmtFinSF1 = BsmtFinSF1 %>% replace_na(0), # Train data: No NAs, Test data: HAS 1 NA
      BsmtFinType2 = BsmtFinType2 %>% replace_na("NoBsmt"), # HAS NAs
      BsmtFinType2 = factor(BsmtFinType2, levels = c("NoBsmt", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")),
      BsmtFinSF2 = BsmtFinSF2 %>% replace_na(0), # Test data: HAS 1 NA
      BsmtUnfSF = BsmtUnfSF %>% replace_na(0), # Test data: HAS 1 NA
      TotalBsmtSF = TotalBsmtSF %>% replace_na(0), # Test data: HAS 1 NA
      Heating = factor(Heating),
      HeatingQC = factor(HeatingQC, levels = c("Po", "Fa", "TA", "Gd", "Ex")),
      CentralAir = factor(CentralAir),
      Electrical = Electrical %>% replace_na("Mix"), # Train data: HAS NA; Test data: No NAs
      Electrical = factor(Electrical), # HAS NA
      #   1stFlrSF = factor(), # Probably doesn't need any mutations
      #   2ndFlrSF = factor(), # Probably doesn't need any mutations
      #   LowQualFinSF = factor(), # Probably doesn't need any mutations
      #    GrLivArea   = factor(), # Probably doesn't need any mutations
      
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
      
      GarageYrBlt = GarageYrBlt %>% replace_na(1980), # NAs should use the median [1980] to not mess with the data # HAS NAs
      
      GarageFinish = GarageFinish  %>% replace_na("NoGrge"), # HAS NAs
      GarageFinish = factor(GarageFinish), # NAs should specify no garage # HAS NAs
      
      GarageCars = GarageCars %>% replace_na(0), # Test data: HAS 1 NA
      
      GarageArea = GarageArea %>% replace_na(0),
      
      GarageQual = GarageQual %>% replace_na("NoGrge"), # NAs should specify no garage # HAS NAs
      GarageQual = factor(GarageQual, levels = c("NoGrge", "Po", "Fa", "TA", "Gd", "Ex")), # HAS NAs
      
      GarageCond = GarageCond %>% replace_na("NoGrge"), # NAs should specify no garage # HAS NAs
      GarageCond = factor(GarageCond, levels = c("NoGrge", "Po", "Fa", "TA", "Gd", "Ex")), # HAS NAs
      
      PavedDrive = factor(PavedDrive),
      
      #    WoodDeckSF = factor(), # Probably doesn't need any mutations
      #    OpenPorchSF = factor(), # Probably doesn't need any mutations
      #    EnclosedPorch = factor(), # Probably doesn't need any mutations
      #    3SsnPorch = factor(), # Probably doesn't need any mutations
      #    ScreenPorch = factor(), # Probably doesn't need any mutations
      #    PoolArea = factor(), # Probably doesn't need any mutations
      
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
      
      SaleCondition = factor(SaleCondition)
    )
  return(clean_data)
}

train_data <- read.csv("train.csv") %>%
  cleaner() %>%
  select(-Id) %>%
  mutate(SalePrice = log(SalePrice),
         LotArea = log(LotArea),
         X1stFlrSF = log(X1stFlrSF),
         GrLivArea = log(GrLivArea))

test_data <- read.csv("test.csv") %>%
  cleaner() %>%
  mutate(LotArea = log(LotArea),
         X1stFlrSF = log(X1stFlrSF),
         GrLivArea = log(GrLivArea))

set.seed(123)
c_lm <- train(SalePrice ~ MSSubClass +
                MSZoning +
                LotFrontage +
                LotArea +
                Street +
                Alley +
                LotShape +
                LandContour +
                Utilities +
                LotConfig +
                LandSlope +
                Neighborhood +
                Condition1 +
                Condition2 +
                BldgType +
                HouseStyle +
                OverallQual * # A discussion post recommended using the GrLivArea multiplied by the OverallQual
                GrLivArea +
                OverallCond +
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
                # X1stFlrSF +  # Highly correlated to 'TotalBsmtSF'
                X2ndFlrSF +
                LowQualFinSF +
                BsmtFullBath +
                BsmtHalfBath +
                FullBath +
                HalfBath +
                BedroomAbvGr +
                KitchenAbvGr +
                KitchenQual +
                # TotRmsAbvGrd +  # Highly correlated to 'GrLivArea'
                Functional +
                Fireplaces +
                FireplaceQu +
                GarageType +
                GarageYrBlt +
                GarageFinish +
                GarageCars +
                GarageCond +
                GarageQual +
                # GarageArea +  # Highly correlated to 'GarageCars'
                PavedDrive +
                WoodDeckSF +
                OpenPorchSF +
                EnclosedPorch +
                X3SsnPorch +
                ScreenPorch +
                PoolArea +
                PoolQC +
                Fence +
                MiscFeature +
                MiscVal +
                MoSold +
                YrSold +
                SaleType +
                SaleCondition, 
              data = train_data,
              preProcess = c("center", "scale"),
              method = "glmnet")


#In-sample performance
rsq(exp(train_data$SalePrice), exp(fitted(c_lm)))
rmse(exp(train_data$SalePrice), exp(fitted(c_lm)))
rmsle(exp(train_data$SalePrice), exp(fitted(c_lm)))

#Out-of-sample performance
c_lm$results

###############################################################

# Identify Highly correlated predictors

# Numeric Correlation Coefficients

t <- train_data %>%
  select(where(is.numeric))

v <- t %>%
  select(-SalePrice)

o <- cor(t$SalePrice, v)


# Highly correlated to each other
correlMatrix <- cor(v[,2:31])
(highCorrel <- findCorrelation(correlMatrix, cutoff=0.75, names = TRUE, verbose = TRUE))
print(correlMatrix[,highCorrel])

cor(train_data$GrLivArea, train_data$TotRmsAbvGrd)
train_data %>%
  ggplot(aes(GrLivArea, TotRmsAbvGrd)) +
  geom_point()

cor(train_data$GrLivArea, train_data$SalePrice)
cor(train_data$TotRmsAbvGrd, train_data$SalePrice) # Remove TotRmsAbvGrd from model?

cor(train_data$X1stFlrSF, train_data$TotalBsmtSF)
train_data %>%
  ggplot(aes(X1stFlrSF, TotalBsmtSF)) +
  geom_point()

cor(train_data$X1stFlrSF, train_data$SalePrice) # Remove X1stFlrSF from model?
cor(train_data$TotalBsmtSF, train_data$SalePrice)


cor(train_data$GarageArea, train_data$GarageCars)
train_data %>%
  ggplot(aes(GarageArea, GarageCars)) +
  geom_point()

cor(train_data$GarageArea, train_data$SalePrice) # Remove Garage Area from model?
cor(train_data$GarageCars, train_data$SalePrice)


###############################################################

# Attempting a Random Forest Model
set.seed(123)
inTrain <- createDataPartition(train_data$SalePrice, p = 0.8, list = FALSE)
rf.train <- train_data[inTrain,]
rf.test <- train_data[-inTrain,]

set.seed(123)
c_rf <- train(SalePrice ~ OverallQual * GrLivArea +
                OverallCond +
                YrSold +
                SaleType +
                SaleCondition,
              data = train_data,
              method = "rf",
              importance = TRUE,
              trControl = trainControl(method = "cv", number = 5))

print(c_rf)

rf_model <- randomForest(SalePrice ~ OverallQual * GrLivArea +
                      OverallCond +
                      YrSold +
                      SaleType +
                      SaleCondition,
                         data = train_data,
                         mtry = 2,
                         importance = TRUE,
                         na.action = na.omit)

print(rf_model)
plot(rf_model)
(rf_pred <- predict(c_rf, data.test))
# (confusionMatrix(table(rf.test[,"SalePrice"],pred)))

###############################################################



# We need to do all the manipulations to the data we are going to be testing as we do to the train data. Once that dataframe is defined, we can apply the predict function with the linear model of our choosing.


# predict_prices <- predict(m_lm, test_data)
# submission_data <- data.frame('Id' = test_data$Id, 'SalePrice' = predict_prices)

# submission_data <- add_predictions(test_data, m_lm, var = "SalePrice")


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
# 9	  <.13
# 8	  <.14 
# 7	  <.15 <- Current Score
# 6	  <.16 
# 5	  <.17
#this is a test for nick github
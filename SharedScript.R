# This installs the packages if you don't have them, and loads them. If you need more packages, add them to the packages vector.

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

calc_mode <- function(x){
  # List the distinct / unique values
  distinct_values <- na.omit(unique(x))
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}

<<<<<<< HEAD
=======

read.csv("train.csv") %>%
  sapply(function(x) sum(is.na(x)))

read.csv("test.csv") %>%
  sapply(function(x) sum(is.na(x)))


>>>>>>> c686dca6a945304901509e58d3fc18e53ea038e6
cleaner <- function(dirty_data) {
  clean_data <- dirty_data %>%
    mutate(
      MSSubClass = factor(MSSubClass, level = c(20, 30, 40, 45, 50, 60, 70, 75, 80, 85, 90, 120, 150, 160, 180, 190)),
      MSZoning = MSZoning %>% replace_na("NoZone"),
      MSZoning = factor(MSZoning, levels = c("A", "C (all)", "FV", "I", "RH", "RL", "RP", "RM", "NoZone")),
      
      #I think this should be 0 is there is an NA Kaggle score went from 0.14012 to 0.14007
      LotFrontage = LotFrontage %>% replace_na(0), # HAS NAs
      
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
      Exterior1st = factor(Exterior1st),
      Exterior1st = Exterior1st %>% replace_na(calc_mode(levels(Exterior1st))),
      
      Exterior2nd = Exterior2nd %>% replace_na("Other"),
      Exterior2nd = factor(Exterior2nd),
      MasVnrType = MasVnrType %>% replace_na("None"), # HAS NAs
      MasVnrType = factor(MasVnrType), # HAS NAs
      MasVnrArea = MasVnrArea %>% replace_na(0), # HAS NAs # NAs should be replaced with 0 since we don't know what NA means in this context
      ExterQual = factor(ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex")),
      ExterCond = factor(ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex")),
      Foundation = factor(Foundation),
      BsmtQual = BsmtQual %>% replace_na("NoBsmt"), # HAS NAs
      BsmtQual = factor(BsmtQual, levels = c("NoBsmt", "Po", "Fa", "TA", "Gd", "Ex")), # HAS NAs
      BsmtCond = BsmtCond %>% replace_na("NoBsmt"), # HAS NAs
      BsmtCond = factor(BsmtCond, levels = c("NoBsmt", "Po", "Fa", "TA", "Gd", "Ex")), # HAS NAs
      BsmtExposure = BsmtExposure %>% replace_na("NoBsmt"), # HAS NAs
      BsmtExposure = factor(BsmtExposure, levels = c("NoBsmt", "No", "Mn", "Av", "Gd")), # HAS NAs
      BsmtFinType1 = BsmtFinType1 %>% replace_na("NoBsmt"), # HAS NAs
      BsmtFinType1 = factor(BsmtFinType1, levels = c("NoBsmt", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")), # HAS NAs
      BsmtFinSF1 = BsmtFinSF1 %>% replace_na(0), # HAS NAs
      BsmtFinType2 = BsmtFinType2 %>% replace_na("NoBsmt"), # HAS NAs
      BsmtFinType2 = factor(BsmtFinType2, levels = c("NoBsmt", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")),
      BsmtFinSF2 = BsmtFinSF2 %>% replace_na(0),
      BsmtUnfSF = BsmtUnfSF %>% replace_na(0),
      TotalBsmtSF = TotalBsmtSF %>% replace_na(0),
      Heating = factor(Heating),
      HeatingQC = factor(HeatingQC, levels = c("Po", "Fa", "TA", "Gd", "Ex")),
      CentralAir = factor(CentralAir),
      Electrical = Electrical %>% replace_na("Mix"), # HAS NAs
      Electrical = factor(Electrical), # HAS NAs
      #   1stFlrSF = factor(), # Probably doesn't need any mutations
      #   2ndFlrSF = factor(), # Probably doesn't need any mutations
      #   LowQualFinSF = factor(), # Probably doesn't need any mutations
      #    GrLivArea   = factor(), # Probably doesn't need any mutations
      
      BsmtFullBath  = BsmtFullBath %>% replace_na(0),
      BsmtHalfBath  = BsmtHalfBath %>% replace_na(0),
      
      #    FullBath = factor(), # Probably doesn't need any mutations
      #    HalfBath = factor(), # Probably doesn't need any mutations
      #    BedroomAbvGr = factor(), # Probably doesn't need any mutations
      #    KitchenAbvGr = factor(), # Probably doesn't need any mutations
      
      KitchenQual = KitchenQual %>% replace_na("TA"),
      KitchenQual = factor(KitchenQual, levels = c("Po", "Fa", "TA", "Gd", "Ex")),
      
      #    KitchenQual = as.numeric(KQ_factor), #might need to be as.numeric(KitchenQual)
      #    TotRmsAbvGrd = factor(), # Probably doesn't need any mutations
      
      Functional = Functional %>% replace_na("Typ"),
      Functional = factor(Functional, levels = c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ")),
      
      #    Fireplaces = factor(), # Probably doesn't need any mutations
      
      FireplaceQu = FireplaceQu %>% replace_na("NoFp"), # NAs should specify no fireplace # HAS NAs
      FireplaceQu = factor(FireplaceQu, levels = c("NoFp", "Po", "Fa", "TA", "Gd", "Ex")),  # HAS NAs
      
      GarageType = GarageType %>% replace_na("NoGrge"), #NAs should specify no garage # HAS NAs
      GarageType = factor(GarageType), # HAS NAs
      
      GarageYrBlt = GarageYrBlt %>% replace_na(1980), # NAs should use the median [1980] to not mess with the data # HAS NAs
      
      GarageFinish = GarageFinish  %>% replace_na("NoGrge"), # HAS NAs
      GarageFinish = factor(GarageFinish), # NAs should specify no garage # HAS NAs
      
      GarageCars = GarageCars %>% replace_na(0),
      
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
      SaleType = SaleType %>% replace_na(calc_mode(levels(SaleType))),

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

<<<<<<< HEAD
# Categorical Correlation Coefficients

f_d <- train_data %>%
  select(where(is.character))

f_o <- data.frame("Test", .12)
names(f_o) <- c("Feature", "R2")
f_o

for (i in colnames(f_d)) {
  (f_lm <- lm(train_data$SalePrice ~ train_data[[i]]) %>% summary)
  f_o <- f_o %>% add_row(Feature = i, R2 = round(f_lm$adj.r.squared,2))
}

f_o %>%
  arrange(f_o$R2)


# GarageFinish 0.27
# Alley 0.28
# BsmtQual 0.45
# KitchenQual 0.46
# ExterQual 0.48
# Neighborhood 0.54


#.662
(m_lm <- lm(SalePrice ~ YearBuilt + LotArea + Neighborhood + HouseStyle + GarageArea, data = train_data)) %>% summary


#.7556
(m_lm <- lm(SalePrice ~ OverallQual + BsmtQual + KitchenQual + ExterQual + Neighborhood, data = train_data)) %>%
  summary

#.7603
(m_lm <- lm(SalePrice ~ OverallQual + GrLivArea + TotalBsmtSF + GarageCars + GarageArea, data = train_data)) %>%
  summary

#.8042 # Check out this model
(m_lm <- lm(log(SalePrice) ~ OverallQual * GrLivArea + TotalBsmtSF + GarageCars + Neighborhood, data = train_data)) %>%
  summary

#.8073
(m_lm <- lm(SalePrice ~ OverallQual + GrLivArea + TotalBsmtSF + ExterQual + Neighborhood, data = train_data)) %>%
  summary

#.8094
(m_lm <- lm(log(SalePrice) ~ OverallQual * GrLivArea + KitchenQual + ExterQual + Neighborhood, data = train_data)) %>%
  summary

#.8243
train_x <- train_data %>%
  select(OverallQual, GrLivArea, KitchenQual, ExterQual, Neighborhood) %>%
  mutate(GrLivArea = log(GrLivArea))


# getFeat

c_lm <- train(train_x, log(train_data$SalePrice), method = "lm")

#In-sample performance, R^2 = 0.8424
summary(c_lm)$r.squared
=======
test_data <- read.csv("test.csv") %>%
  cleaner() %>%
  mutate(LotArea = log(LotArea),
         X1stFlrSF = log(X1stFlrSF),
         GrLivArea = log(GrLivArea))


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
              OverallQual +
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
              X1stFlrSF +
              X2ndFlrSF +
              LowQualFinSF +
              GrLivArea +
              BsmtFullBath +
              BsmtHalfBath +
              FullBath +
              HalfBath +
              BedroomAbvGr +
              KitchenAbvGr +
              KitchenQual +
              TotRmsAbvGrd +
              Functional +
              Fireplaces +
              FireplaceQu +
              GarageType + 
              GarageYrBlt +
              GarageFinish +
              GarageCars +
              GarageCond +
              GarageQual +
              GarageArea +
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
summary(c_lm)
>>>>>>> c686dca6a945304901509e58d3fc18e53ea038e6
rmse(train_data$SalePrice, exp(fitted(c_lm)))
rmsle(train_data$SalePrice, exp(fitted(c_lm)))

#Out-of-sample performance
c_lm$results


<<<<<<< HEAD
# From this most recent model ^
# 0.7885 without OverallQual
# 0.7473 without grLivArea
# 0.7983 without KitchenQual
# 0.8038 without ExterQual
# 0.7596 without neighborhood

###############################################################

# Attempting a Random Forest Model
set.seed(123)
inTrain <- createDataPartition(train_data[,"SalePrice"],p=0.8, list=FALSE)
rf.train <- train_data[train,]
rf.test <- train_data[-train,]

ctrl <- trainControl(method = "cv", number = 10)

set.seed(123)
c_rf <- randomForest(SalePrice ~ OverallQual + GrLivArea + KitchenQual + ExterQual + Neighborhood,
                     data = train_data,
                     mtry = 2,
                     importance = TRUE,
                     na.action = na.omit)

print(c_rf)
plot(c_rf)
(rf_pred <- predict(c_rf, data.test))
# (confusionMatrix(table(rf.test[,"SalePrice"],pred)))

###############################################################

set.seed(123)
correlat


# We need to do all the manipulations to the data we are going to be testing as we do to the train data. Once that dataframe is defined, we can apply the predict function with the linear model of our choosing.


# predict_prices <- predict(m_lm, test_data)
# submission_data <- data.frame('Id' = test_data$Id, 'SalePrice' = predict_prices)

# submission_data <- add_predictions(test_data, m_lm, var = "SalePrice")

text_x <- test_data %>%
  select(OverallQual, GrLivArea, KitchenQual, ExterQual, Neighborhood) %>%
  mutate(GrLivArea = log(GrLivArea))

=======
>>>>>>> c686dca6a945304901509e58d3fc18e53ea038e6
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

#Rubric score	Kaggle score (log RMSE)
# 10  <.12
# 9	  <.13
# 8	  <.14 
# 7	  <.15 <- Current Score
# 6	  <.16 
# 5	  <.17

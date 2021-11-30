# This installs the packages if you don't have them, and loads them. If you need more packages, add them to the packages vector.

packages <- c("tidyverse", "faux", "DataExplorer", "randomForest", "caret", "corrplot", "modelr", "stats")

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

cleaner <- function(dirty_data) {
  clean_data <- dirty_data %>%
    mutate(
      MSSubClass = factor(MSSubClass, level = c(20, 30, 40, 45, 50, 60, 70, 75, 80, 85, 90, 120, 150, 160, 180, 190)),
      MSZoning = MSZoning %>% replace_na("NoZone"),
      MSZoning = factor(MSZoning, levels = c("A", "C (all)", "FV", "I", "RH", "RL", "RP", "RM", "NoZone")),
      LotFrontage = LotFrontage %>% replace_na(69), #median was 69 with NA values
      #    LotArea = factor(), # Doesn't need any mutations
      Street = factor(Street),
      Alley = Alley %>% replace_na("NoAcc"), # Replace NAs with NoAcc
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
      YearBuilt = factor(YearBuilt, levels = factor(YearBuilt) %>% levels),
      YearRemodAdd = factor(YearRemodAdd, levels = factor(YearRemodAdd) %>% levels),
      RoofStyle = factor(RoofStyle),
      RoofMatl = factor(RoofMatl),
      Exterior1st = Exterior1st %>% replace_na("Other"),
      Exterior1st = factor(Exterior1st),
      Exterior2nd = Exterior2nd %>% replace_na("Other"),
      Exterior2nd = factor(Exterior2nd),
      MasVnrType = MasVnrType %>% replace_na("None"),
      MasVnrType = factor(MasVnrType),
      MasVnrArea = MasVnrArea %>% replace_na(0), # NAs should be replaced with median (0) since we don't know what NA means in this context
      ExterQual = factor(ExterQual, ordered = TRUE, levels = c("Po", "Fa", "TA", "Gd", "Ex")),
      ExterCond = factor(ExterCond, ordered = TRUE, levels = c("Po", "Fa", "TA", "Gd", "Ex")),
      Foundation = factor(Foundation),
      BsmtQual = BsmtQual %>% replace_na("NoBsmt"),
      BsmtQual = factor(BsmtQual, levels = c("NoBsmt", "Po", "Fa", "TA", "Gd", "Ex")),
      BsmtCond = BsmtCond %>% replace_na("NoBsmt"),
      BsmtCond = factor(BsmtCond, levels = c("NoBsmt", "Po", "Fa", "TA", "Gd", "Ex")),
      BsmtExposure = BsmtExposure %>% replace_na("NoBsmt"),
      BsmtExposure = factor(BsmtExposure, levels = c("NoBsmt", "No", "Mn", "Av", "Gd")),
      BsmtFinType1 = BsmtFinType1 %>% replace_na("NoBsmt"),
      BsmtFinType1 = factor(BsmtFinType1, levels = c("NoBsmt", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")),
      BsmtFinSF1 = BsmtFinSF1 %>% replace_na(0),
      BsmtFinType2 = BsmtFinType2 %>% replace_na("NoBsmt"),
      BsmtFinType2 = factor(BsmtFinType2, levels = c("NoBsmt", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")),
      BsmtFinSF2 = BsmtFinSF2 %>% replace_na(0),
      BsmtUnfSF = BsmtUnfSF %>% replace_na(0),
      TotalBsmtSF = TotalBsmtSF %>% replace_na(0),
      Heating = factor(Heating),
      HeatingQC = factor(HeatingQC, ordered = TRUE, levels = c("Po", "Fa", "TA", "Gd", "Ex")),
      CentralAir = factor(CentralAir),
      Electrical = Electrical %>% replace_na("Mix"),
      Electrical = factor(Electrical),
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
      KitchenQual = factor(KitchenQual, ordered = TRUE, levels = c("Po", "Fa", "TA", "Gd", "Ex")),
      #    KitchenQual = as.numeric(KQ_factor), #might need to be as.numeric(KitchenQual)
      #    TotRmsAbvGrd = factor(), # Probably doesn't need any mutations
      Functional = Functional %>% replace_na("Typ"),
      Functional = factor(Functional, levels = c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ")),
      #    Fireplaces = factor(), # Probably doesn't need any mutations
      FireplaceQu = FireplaceQu %>% replace_na("NoFp"), # NAs should specify no fireplace
      FireplaceQu = factor(FireplaceQu, levels = c("NoFp", "Po", "Fa", "TA", "Gd", "Ex")), 
      GarageType = GarageType %>% replace_na("NoGrge"), #NAs should specify no garage
      GarageType = factor(GarageType),
      GarageYrBlt = GarageYrBlt %>% replace_na(1980), # NAs should use the median [1980] to not mess with the data
      GarageFinish = GarageFinish  %>% replace_na("NoGrge"),
      GarageFinish = factor(GarageFinish), # NAs should specify no garage
      GarageCars = GarageCars %>% replace_na(0),
      GarageArea = GarageArea %>% replace_na(0),
      GarageQual = GarageQual %>% replace_na("NoGrge"), # NAs should specify no garage
      GarageQual = factor(GarageQual, levels = c("NoGrge", "Po", "Fa", "TA", "Gd", "Ex")),
      GarageCond = GarageCond %>% replace_na("NoGrge"), # NAs should specify no garage
      GarageCond = factor(GarageCond, levels = c("NoGrge", "Po", "Fa", "TA", "Gd", "Ex")),
      PavedDrive = factor(PavedDrive),
      #    WoodDeckSF = factor(), # Probably doesn't need any mutations
      #    OpenPorchSF = factor(), # Probably doesn't need any mutations
      #    EnclosedPorch = factor(), # Probably doesn't need any mutations
      #    3SsnPorch = factor(), # Probably doesn't need any mutations
      #    ScreenPorch = factor(), # Probably doesn't need any mutations
      #    PoolArea = factor(), # Probably doesn't need any mutations
      PoolQC = PoolQC %>% replace_na("NoPool"), #NAs should specify no pool
      PoolQC = factor(PoolQC, levels = c("NoPool", "Fa", "TA", "Gd", "Ex")),
      Fence = Fence %>% replace_na("NoFnc"), #NAs should specify no fence
      Fence = factor(Fence),
      MiscFeature = MiscFeature %>% replace_na("NoMiscF"), #NAs should specify no amenities
      MiscFeature = factor(MiscFeature),
      #    MiscVal = factor(), # Probably doesn't need any mutations
      MoSold = factor(MoSold),
      #    YrSold = factor(), # Probably doesn't need any mutations
      SaleType = SaleType %>% replace_na("Other"),
      SaleType = factor(SaleType),
      SaleCondition = factor(SaleCondition)
    )
  return(clean_data)
}

train_data <- read.csv("train.csv") %>%
  cleaner()
test_data <- read.csv("test.csv") %>%
  cleaner()

train_data %>%
  summary

# Mean > Median => Skew
train_data$SalePrice %>%
  summary

range(train_data$SalePrice)

quantile(train_data$SalePrice)

# EDA

hist(train_data$SalePrice) # Right Skewed

train_data %>%
  ggplot(aes(OverallQual, SalePrice)) +
  geom_point()

train_data %>%
  ggplot(aes(GrLivArea, SalePrice)) +
  geom_point() +
  stat_smooth(method = "lm")

train_data %>%
  ggplot(aes(KitchenQual, SalePrice)) +
  geom_boxplot()

train_data %>%
  ggplot(aes(ExterQual, SalePrice)) +
  geom_boxplot()

train_data %>%
  ggplot(aes(Neighborhood, SalePrice)) +
  geom_boxplot()


train_data %>%
  ggplot(aes(GrLivArea, SalePrice, col = Neighborhood)) +
  geom_point()



# Numeric Correlation Coefficients

#Gets numeric columns as BOOLEAN
(nums <- unlist(lapply(train_data, is.numeric)))

t <- train_data %>%
  select(where(is.numeric))

v <- t %>%
  select(-SalePrice)

(o <- cor(t$SalePrice, v))

round(o,2)

#OverallQual = .79
#GrLivArea = .71
#Total BsmtSF = .61
#X1stFlrSF = .61
#GarageCars = .64
#GarageArea = .62

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

#.8042
(m_lm <- lm(SalePrice ~ OverallQual + GrLivArea + TotalBsmtSF + GarageCars + Neighborhood, data = train_data)) %>%
  summary

#.8073
(m_lm <- lm(SalePrice ~ OverallQual + GrLivArea + TotalBsmtSF + ExterQual + Neighborhood, data = train_data)) %>%
  summary

#.8094
(m_lm <- lm(SalePrice ~ OverallQual + GrLivArea + KitchenQual + ExterQual + Neighborhood, data = train_data)) %>%
  summary

#.8243
train_x <- train_data %>%
  select(OverallQual, GrLivArea, KitchenQual, ExterQual, Neighborhood) %>%
  mutate(GrLivArea = log(GrLivArea))



c_lm <- train(train_x, log(train_data$SalePrice), method = "lm")

#In-sample performance, R^2 = 0.8424
summary(c_lm)$r.squared
rmse(train_data$SalePrice, exp(fitted(c_lm)))
rmsle(train_data$SalePrice, exp(fitted(c_lm)))

#Out-of-sample performance, R^2 = 0.8261, RMSE = 0.1669746
c_lm$results


# From this most recent model ^
# 0.7885 without OverallQual
# 0.7473 without grLivArea
# 0.7983 without KitchenQual
# 0.8038 without ExterQual
# 0.7596 without neighborhood


# We need to do all the manipulations to the data we are going to be testing as we do to the train data. Once that dataframe is defined, we can apply the predict function with the linear model of our choosing.


# predict_prices <- predict(m_lm, test_data)
# submission_data <- data.frame('Id' = test_data$Id, 'SalePrice' = predict_prices)

# submission_data <- add_predictions(test_data, m_lm, var = "SalePrice")

text_x <- test_data %>%
  select(OverallQual, GrLivArea, KitchenQual, ExterQual, Neighborhood) %>%
  mutate(GrLivArea = log(GrLivArea))

submission_data <- test_data %>%
  select(Id) %>%
  mutate(SalePrice = exp(predict(c_lm, text_x)))

write.csv(submission_data, "submission_file.csv", row.names=FALSE)

# main
source("src//dummy_function.R")
source("src//clean_data_functions.R")

# read data
data <- read.csv("data/data_complete.csv"
                    , stringsAsFactors = TRUE)

# get rid of possible duplicates
data_unique <- clean_duplicates(data)

# remove constants
data_unique_no_constants <- janitor::remove_constant(data_unique, na.rm= TRUE)

# get rid of multicollinearity
data_unique_no_constants_without_multicol <- 
  data_unique_no_constants[, !(colnames(data_unique_no_constants) 
                               %in% c("YearBuilt"
                                      , "YrSold"
                                      , "GarageCars"
                                      , "GrLivArea"
                                      , "GarageYrBlt"))]

ggplot(data=data_unique_no_constants_without_multicol, aes(x=SalePrice, y=after_stat(density))) +
  geom_histogram(color="black", fill="blue") +
  geom_density(color = "orange", fill = "orange", alpha = 0.6) +
  labs(x = "Immobilienpreis", 
       y = "Dichte", 
       title = paste("Verteilung von Immobilienpreisen in den USA"),
       caption = "Quelle: R, ggplot2") +
  apatheme

ggsave("Immobilienpreise.pdf", dpi=300)

# create dummies
data_unique_no_constants_without_multicol_dummied <- 
  create_dummies(data_unique_no_constants_without_multicol)

# train and test set
train_rows <- caret::createDataPartition(
  data_unique_no_constants_without_multicol_dummied$SalePrice, p=0.8, list=FALSE)

train_y <- as.matrix(data_unique_no_constants_without_multicol_dummied[train_rows, ][, 32])
train_x <- as.matrix(data_unique_no_constants_without_multicol_dummied[train_rows, ][, -32])

train_complete <- xgboost::xgb.DMatrix(data=train_x, label=train_y)

test_y <- as.matrix(data_unique_no_constants_without_multicol_dummied[-train_rows, ][, 32])
test_x <- as.matrix(data_unique_no_constants_without_multicol_dummied[-train_rows, ][, -32])

test_complete <- xgboost::xgb.DMatrix(data=test_x, label=test_y)


# fit xgboost
# cv
xgbcv <- xgboost::xgb.cv(data = train_complete, max.depth = 3, nrounds = 100, nfold = 5)
min(xgbcv$evaluation_log$test_rmse_mean)
which.min(xgbcv$evaluation_log$test_rmse_mean)

# train with optimal nrounds
xgb_train_1 <- xgboost::xgboost(data = train_complete, max.depth = 3, nrounds = 32)
xgb_train <- xgboost::xgb.train(data = train_complete, max.depth = 3, nrounds = 32)

# visualize nrounds
ggplot(data=xgb_train_1$evaluation_log) +
  geom_line(aes(x=iter, y=train_rmse), color="darkblue", fill="darkblue") +
  labs(x = "Anzahl Trees", 
       y = "RMSE (Training)", 
       title = paste("Tuning Anzahl Bäume in XGBoost"),
       caption = "Quelle: R, xgboost, ggplot2") +
  apatheme



sqrt(mean((test_y - predict(xgb_train, test_complete))^2))


# tuning

caret::modelLookup("xgbTree")

xgb_grid_1 = expand.grid(nrounds = 32
                         , max_depth = 2
                         , eta = 0.27
                         , gamma = 0
                         , colsample_bytree = 0.4
                         , min_child_weight = 5
                         , subsample = 0.6)
xgb_grid_1


xgb_trcontrol_1 = caret::trainControl(method = "cv",
                               number = 5,
                               verboseIter = TRUE,
                               returnData = FALSE,
                               returnResamp = "all", 
                               allowParallel = TRUE)

train_y <- data_unique_no_constants_without_multicol_dummied[train_rows, ][, 32]

xgb_train_1 = caret::train(x = train_x,
                    y = train_y,
                    trControl = xgb_trcontrol_1,
                    tuneGrid = xgb_grid_1,
                    method = "xgbTree")

xgb_train_1$bestTune

# fit tuned model
xgb_train_tuned <- xgboost::xgb.train(data = train_complete
                                      , nrounds = 32
                                      , max_depth = 2
                                      , eta = 0.27
                                      , gamma = 0
                                      , colsample_bytree = 0.4
                                      , min_child_weight = 5
                                      , subsample = 0.6)


sqrt(mean((test_y - predict(xgb_train_tuned, test_complete))^2))

# Tuning improvement in RMSE
sqrt(mean((test_y - predict(xgb_train_tuned, test_complete))^2)) / sqrt(mean((test_y - predict(xgb_train, test_complete))^2))

importance_matrix_tuned <- as.data.frame(xgboost::xgb.importance(model = xgb_train_tuned))
importance_matrix_tuned$Feature <- factor(importance_matrix_tuned$Feature
                                    , levels=importance_matrix_tuned$Feature[order(importance_matrix_tuned$Gain, decreasing=FALSE)])

# visualize importance
ggplot(data=importance_matrix_tuned[1:20, ]) +
  geom_bar(aes(x=Feature, y=Gain), stat="identity", fill="darkblue", color="darkblue") +
  coord_flip() +
  labs(x = "Feature Importance", 
       y = "Feature", 
       title ="Top 20 Feature Importance",
       caption = "Quelle: R, xgboost, ggplot2") +
  apatheme

ggsave("visualize_importance.pdf", dpi=300)

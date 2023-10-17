# data wrangling

# join and cocatenate data

original_train <- read.csv(path_data_train)
original_test <- read.csv(path_data_test)
original_sample_submission <- read.csv(path_data_sample_submission)

original_test_joined <- cbind(original_test, original_sample_submission$SalePrice)
names(original_test_joined)[names(original_test_joined) == 'original_sample_submission$SalePrice'] <- 'SalePrice'

data_complete <- rbind(original_train, original_test_joined)

View(data_complete)

colSums(is.na(data_complete))[colSums(is.na(data_complete)) > 0]

# delete columns with a lot of NAs

data_complete$Id <- NULL
data_complete$Alley <- NULL
data_complete$FireplaceQu <- NULL
data_complete$PoolQC <- NULL
data_complete$Fence <- NULL
data_complete$MiscFeature <- NULL

# imputate missing values with random forest

data_complete_imputated <- missRanger::missRanger(data_complete, pmm.k=3)

# write to hdd
write.csv(data_complete_imputated,file = "data/data_complete_imputated.csv"
          , row.names=FALSE)


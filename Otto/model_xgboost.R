toxgbmatrix <- function(train, test){
  # Make a copy
  train_copy <- train
  test_copy <- test
  train_copy$id <- NULL
  test_copy$id <- NULL
  
  # Obtain numerical values as class starting from 0
  target <- train_copy$target
  num_target <- as.numeric(unlist(strsplit(as.character(train$target),"_"))[c(FALSE, TRUE)]) - 1
  
  # Remove the target column and transform to matrix
  train_copy$target <- NULL
  
  # XGBoost only takes numeric values as data input
  train_copy[] <- lapply(train_copy, as.numeric)
  test_copy[] <- lapply(test_copy, as.numeric)
  train_matrix <- as.matrix(train_copy)
  test_matrix <- as.matrix(test_copy)
  return(list(train_matrix = train_matrix, test_matrix = test_matrix))
}


myxgb <- function(train_matrix, test_matrix, eta, gamma, max_depth, min_child_weight, subsample, colsample_bytree, nrounds){
  # Simple XGBoost
  xgboost_submission <- data.frame(id=test$id, Class_1=NA, Class_2=NA, Class_3=NA, Class_4=NA, Class_5=NA, Class_6=NA, Class_7=NA, Class_8=NA, Class_9=NA)
  
  # XGBoost
  param <- list(booster = "gbtree", objective = "multi:softprob", eval_metric = "mlogloss",  eta = eta, gamma = gamma, max_depth = max_depth, min_child_weight = min_child_weight, subsample = subsample, colsample_bytree = colsample_bytree, num_class = 9)
  xgb_first_tune <- xgboost(param = param, data = train_matrix, label = num_target, nrounds = nrounds)
  xgboost_submission[,2:10] <- matrix(predict(xgb_first_tune, test_matrix), ncol = 9, byrow = T)
  
  write.csv(xgboost_submission, file="xgboost_first_tune_submission.csv", row.names = FALSE)
  # Example # 
  #param <- list(booster = "gbtree", objective = "multi:softprob", eval_metric = "mlogloss",  eta = 0.05, gamma = 1, max_depth = 9, min_child_weight = 5, subsample=0.8, colsample_bytree=0.8, num_class = 9)
}

myxgb_cv <- function(train_matrix, test_matrix, lab, param, round_n, fold_n, strat, early_n, print_n){
  # Trying xgb.cv to find optimal stopping round
  xgb_cv <- xgb.cv( params = param, data = train_matrix, nrounds = round_n, nfold = fold_n, label = lab, showsd = T, stratified = strat, early_stopping_rounds = early_n, maximize = F, print_n = 5)
  return(xgb_cv)
  # Example #
  #param <- list(booster = "gbtree", objective = "multi:softprob", eval_metric = "mlogloss",  eta = 0.05, gamma = 0, max_depth = 5, min_child_weight = 1, subsample=0.8, colsample_bytree=0.8, num_class = 9)
  #xgb_cv <- xgb.cv( params = param, data = train_matrix, nrounds = 10000, nfold = 3, label = num_target, showsd = T, stratified = T, early_stopping_rounds = 20, maximize = F, print_every_n = 5)
  
}

myxgb_tuning <- function(nrounds, eta, max_depth, min_child_weight, gamma, subsample, colsample_bytree){
  # Try tuning parameters
  # Set up cross-validated hyper-parameter search
  xgb_grid_1 = expand.grid(nrounds = nrounds, eta = eta, max_depth = max_depth, min_child_weight = min_child_weight, gamma = gamma, subsample = subsample, colsample_bytree = colsample_bytree)
  
  # Pack training control parameters
  xgb_trcontrol_1 = trainControl(method = "cv", number = 3, verboseIter = TRUE, returnData = TRUE, returnResamp = "all", classProbs = TRUE, summaryFunction = mnLogLoss,
                                 allowParallel = TRUE)
  
  # Best performance with max_depth = 12, min_child_weight = 3
  xgb_train_1 = train(x = train_matrix, y = target, trControl = xgb_trcontrol_1, tuneGrid = xgb_grid_1, method = "xgbTree")
  return(xgb_train_1)
  
  # Example #
  #xgb_grid_1 = expand.grid(nrounds = 1073, eta = 0.05, max_depth = c(3,5,7,9,12), min_child_weight = c(1,3,5,7), gamma = 1, subsample = 0.8, colsample_bytree = 0.8)
  # Pass vectors as parameter !
}

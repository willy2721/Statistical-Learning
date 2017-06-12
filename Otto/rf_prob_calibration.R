# Input : train set, ratio and number of trees to train
# Output : List containing subtraining set, calibration set, calibration answer and random forest model trained on subtrain

rf_on_calibrate <- function(train, ratio, ntree){
  # Get answer from train
  train_answer <- myanswer(train)
  
  # Train = subtrain + calibrate
  train_holdout <- holdout(train$target, ratio = ratio, internalsplit = FALSE, mode = "stratified")
  subtrain <- train[train_holdout$tr,]
  caliset <- train[train_holdout$ts,]
  cali_answer <- train_answer[train_holdout$ts,]
  subtrain_rf <- randomForest(subtrain[,c(-1,-95)], as.factor(subtrain$target), ntree = ntree, mtry = 20, nodesize = 1)
  
  return(list(subtrain = subtrain, caliset = caliset, cali_answer = cali_answer, subtrain_rf = subtrain_rf))
  
}



# Input : training set, ratio for subtrain and calibration, optional random forest model
# Output : Predicted probabilities, isoReg calibrated probabilities and Platt calibrated probabilities for each class

rf_prob_calibration <- function(subtrain, caliset, cali_answer, subtrain_rf, ratio, ntree){
  
  if(missing(subtrain_rf)){
    # Train model on subtrain
    subtrain_rf <- randomForest(subtrain[,c(-1,-95)], as.factor(subtrain$target), ntree = ntree, mtry = 20, nodesize = 1)
  }
  
  # Predict for calibrate set 
  cali_predict <- predict(subtrain_rf, caliset[,c(-1,-95)], type="prob")
  
  # Initialize empty list to store probabilities for each class
  class_probs <- list()
  cali_model <- list()
  platt_model <- list()
  
  # Loop through each class
  for(i in 1:9){
    # Store predicted class probability and correct class
    pred_is_class <- cali_predict[,i]
    ans_is_class <- factor(cali_answer[,i + 1])
    ans_is_class <- factor(ans_is_class,levels(ans_is_class)[c(2,1)])
    
    # Calibrate the probabilities with CORElearn's "calibrate" function
    calibration <- calibrate(ans_is_class, pred_is_class, class1 = 1, method = "isoReg", weight=NULL, noBins=10, assumeProbabilities=TRUE)
    isoReg_class <- applyCalibration(pred_is_class, calibration)
    
    # Create dataframe by combining prediction and answer
    ans_prob <- data.frame(Class = ans_is_class, Probability = pred_is_class, IsoReg_Calibrated = isoReg_class)
    
    # IsoReg doesn't work well for small data, try Platt's scaling (fitting logistic regression model on calibrate set)
    model_log <- glm(Class ~ Probability, data = ans_prob, family = binomial)
    
    # Predicting on the calibrate set after platt scaling
    new_prob <- data.frame(Probability = ans_prob$Probability)
    platt_class <- predict(model_log, new_prob, type = "response")
    platt_class <- 1 - platt_class
    
    # Add platt_class
    ans_prob <- data.frame(Class = ans_is_class, Probability = pred_is_class, IsoReg_Calibrated = isoReg_class, Platt_Calibrated = platt_class)
    
    # Add entire dataframe of probabilities and platt, iso models to list
    class_probs[[i]] <- ans_prob
    cali_model[[i]] <- calibration
    platt_model[[i]] <- model_log
    
  }
  return(list(class_probs = class_probs, cali_predict = cali_predict, cali_model = cali_model, platt_model = platt_model))
  
}

# Input : Training set, testing set, number of bagging(1 if no bag)
# Output : Predicted probabilities, isoReg calibrated probabilities and Platt calibrated probabilities for each class
rf_cali_cv_bagged <- function(train, test, nbag, ncv){
  # Bag the model 
  for(n in 1 : nbag){
    # Cross-validate calibration
    folds <- createFolds(train$target, k = ncv, list = FALSE)
    train$fold <- folds
    
    # Store meta feature for train and predict
    train_meta <- data.frame()
    test_meta <- matrix(0, nrow = nrow(test), ncol = 9)
    
    # K-fold cross-validation
    for(k in 1 : ncv){
      
      # Seperate the training, calibration folds    
      calibrate_i <- which(train$fold == k)
      subtrain <- train[-calibrate_i, ]
      caliset <- train[calibrate_i, ]
      cali_answer <- train_answer[calibrate_i,]
      
      # Remember to remove folds when training !
      # Train random forest on subtrain
      subtrain_rf <- randomForest(subtrain[,c(-1,-95,-96)], as.factor(subtrain$target), ntree = 800, mtry = 20, nodesize = 1)
      # Predict for calibrate set 
      cali_predict <- predict(subtrain_rf, caliset[,c(-1,-95,-96)], type="prob")
      # Predict for test set 
      test_predict <- predict(subtrain_rf, test[,-1], type="prob")

      # Loop through each class
      for(i in 1:9){
        # Store predicted class probability and correct class
        pred_is_class <- cali_predict[,i]
        ans_is_class <- factor(cali_answer[,i + 1])
        ans_is_class <- factor(ans_is_class,levels(ans_is_class)[c(2,1)])
        
        # Calibrate the probabilities for test set
        calibration <- calibrate(ans_is_class, pred_is_class, class1 = 1, method = "isoReg", weight=NULL, noBins=10, assumeProbabilities=TRUE)
        test_predict[,i] <- applyCalibration(test_predict[,i], calibration)
        cali_predict[,i] <- applyCalibration(cali_predict[,i], calibration)
      } 
      
      # Add prediction to test_meta and train_meta
      train_meta <- rbind(train_meta, cali_predict)
      test_meta <- test_meta + test_predict
      
    }
    
    # Order meta feature dataframe by row name
    train_meta <- train_meta[order(as.numeric(row.names(train_meta))),]
    # Get the average of three test_predictions (also meta feature for test)
    test_meta <- test_meta / ncv
    if(nbag == 1){
      return(list(train_meta = train_meta, test_meta = test_meta))
    }
    
    # Add to bagging
    train_meta_bagged <- ifelse(n == 1, train_meta, train_meta_bagged + train_meta)
    test_meta_bagged <- ifelse(n == 1, test_meta, test_meta_bagged + test_meta)
    
  }
  # Get average
  train_meta_bagged <- train_meta_bagged / nbag
  test_meta_bagged <- test_meta_bagged / nbag
  
  
  return(list(train_meta_bagged = train_meta_bagged, test_meta_bagged = test_meta_bagged))
  
}



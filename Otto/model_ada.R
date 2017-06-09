# Simple Adabag boosting implementation 
myadaboost <- function(train,test){
  adaboost_submission <- data.frame(id=test$id, Class_1=NA, Class_2=NA, Class_3=NA, Class_4=NA, Class_5=NA, Class_6=NA, Class_7=NA, Class_8=NA, Class_9=NA)
  # SAMME method
  adaboost <- boosting(target ~ .-id, data = train, boos = TRUE, mfinal = 600, coeflearn = 'Zhu')
  adaboost.pred <- predict.boosting(adaboost, newdata = test)
  adaboost_submission[,2:10] <- adaboost.pred$prob
  write.csv(adaboost_submission, file="adaboost_submission.csv", row.names = FALSE)  
}

# Simple Adabag bagging implementation 
myadabag <- function(train,test){
  adabag_200_submission <- data.frame(id=test$id, Class_1=NA, Class_2=NA, Class_3=NA, Class_4=NA, Class_5=NA, Class_6=NA, Class_7=NA, Class_8=NA, Class_9=NA)
  # Bagging implementation
  adabag_200 <- bagging(target ~ .-id, data = train, mfinal = 600)
  adabag_200.pred <- predict.bagging(adabag_200, newdata = test)
  adabag_200_submission[,2:10] <- adabag_200.pred$prob
  write.csv(adabag_200_submission, file="adabag_200_submission.csv", row.names = FALSE)
}
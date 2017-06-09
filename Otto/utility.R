# Multiclass logloss function
logloss <- function(pred, tar){
  max <- 1 - 10 ^ (-15)
  min <- 10 ^ (-15)
  pred[pred > max] = max
  pred[pred < min] = min
  rownum <- nrow(tar)
  # Normalize
  pred <- ifelse(is.null(rownum), pred / sum(pred), apply(pred, 1, function(x)  x / sum(x)))
  return(-sum(tar * log(pred)) / ifelse(is.null(rownum), length(tar), rownum))
}


# Given train data create dataframe for correct answer
myanswer <- function(train){
  answer <- data.frame(id=train$id, Class_1=0, Class_2=0, Class_3=0, Class_4=0, Class_5=0, Class_6=0, Class_7=0, Class_8=0, Class_9=0)
  for(n in 1:nrow(answer)){
    # Extract the target, transform to character then split by "_" and extract the number, then add 1 to get the correct index
    answer[n,as.numeric(strsplit(as.character(train$target[n]),"_")[[1]][2]) + 1] <- 1
  }
  return(answer)
}
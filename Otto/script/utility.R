# Multiclass logloss function
logloss <- function(pred, tar){
  max <- 1 - 10 ^ (-15)
  min <- 10 ^ (-15)
  pred[pred > max] = max
  pred[pred < min] = min
  rownum <- nrow(tar)
  if(is.null(rownum)){
    return(-1 / length(tar) * (sum((tar * log(pred)+(1 - tar) * log(1 - pred)))))
  }
  else{
    return(-sum(tar * log(pred)) / rownum)
  }
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
# Multiclass logloss function
logloss <- function(pred, tar){
  max <- 1 - 10 ^ (-15)
  min <- 10 ^ (-15)
  pred[pred > max] = max
  pred[pred < min] = min
  rownum <- nrow(tar)
  return(-sum(tar * log(pred)) / rownum)
}
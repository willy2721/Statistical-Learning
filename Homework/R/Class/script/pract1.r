mytmean <- function(v){
  v <- sort(v)
  top <- round(0.1*length(v))
  bottom <- length(v) - top
  return (mean(v[top:bottom]))
}

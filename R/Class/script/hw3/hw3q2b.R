t_to_y <- function(row,w){
  return(1 / (1 + exp(-(t(w) %*% row)))) 
}

logicreg_l2_predict <- function(model1, xmattest1){
  prob <- as.matrix(apply(xmattest1,1,t_to_y,w=model1$w))
  class <- c()
  for(i in 1:nrow(xmattest1)){
    if(prob[i] > 0.5){
      class[i] = 1
    }
    else
      class[i] = 0
  }
  ans <- list(prob = prob, class=class)
  return(ans)
}

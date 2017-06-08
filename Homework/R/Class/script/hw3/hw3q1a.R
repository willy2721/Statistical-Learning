pgm_train<-function(outclass, alldata){
  ans <- list()
  for(iter in 1:length(outclass)){
    mu1 <- apply(train2[[iter]],2,mean)
    sigma1 <- cov(train2[[iter]])
    prec1 <- solve(sigma1)
    detsig_log <- log(det(sigma1))
    N1 <- nrow(train2[[iter]])
    ans[[iter]] <- list(mu1=mu1, sigma1=sigma1, prec1=prec1, detsig_log=detsig_log, N1=N1)
  }
  names(ans) <- outclass
  return(ans)
}

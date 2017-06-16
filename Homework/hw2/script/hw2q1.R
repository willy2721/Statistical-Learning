lm_evmax <- function(y,xmat){
  N <- nrow(xmat)
  lambda <- 0.001 * N
  xmatsqr <- t(xmat) %*% xmat
  I <- diag(nrow(xmatsqr))
  w <- (lambda * I + xmatsqr) %*% t(xmat) %*% y
  e_zero <- y - xmat %*% w
  beta_init <- as.numeric(N / (t(e_zero) %*% e_zero))
  alpha_init <- as.numeric(lambda * beta_init)
  alpha_old <- alpha_init
  beta_old <- beta_init
  thresh <- 1
  
  while(thresh >= 10^(-5)){
    A <- alpha_old * I + beta_old * xmatsqr
    mN <- beta_old * solve(A) %*% t(xmat) %*% y
    alpha <- alpha_old
    beta <- beta_old
    eigenvalues <- eigen(beta_old * xmatsqr, only.values = TRUE)
    gamma <- sum(unlist(lapply(eigenvalues,function(x) x / (alpha_old + x))[1]))
    alpha_new <- as.numeric(gamma / (t(mN) %*% mN))
    e_one <- y - xmat %*% mN
    beta_new <- as.numeric(1 / ((t(e_one) %*% e_one) / (N - gamma)))
    thresh <- abs(alpha_old - alpha_new) + abs(beta_old - beta_new)  
    alpha_old <- alpha_new
    beta_old <- beta_new
  }
  
  mNsd <- sqrt(diag(solve(A)))
  ret <- list(mN=mN,mNsd=mNsd,alpha=alpha,beta=beta)
  return(ret)
}

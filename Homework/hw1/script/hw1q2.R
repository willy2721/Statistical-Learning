### This function takes mu(the original MLE estimator of mu), s(the original MLE estimator of sigma), n(the number of observations in the original dataset), and x(the new observation), and returns the new mu, sigma and n ###
mle_update <- function(mu_old, s_old, n_old, x_new){
  n_new <- n_old + 1
  mu_new <- mu_old + (1 / n_new) * (x_new-mu_old)
  s_new <- (n_old / n_new) * s_old + (n_old * (n_new ^ (-3))) * ((x_new - mu_old) %*% t(x_new - mu_old)) + 1 / n_new * ((x_new - mu_new) %*% t(x_new - mu_new))
  
  # Construct the required list
  ret <- list(mu=mu_new, s=s_new, n=n_new)
  return(ret)
}

logicreg_l2_train <- function(y, xmat, lambda_rate = 0.0005, param_tol = 10^(-5), granditertol = 2, outitermax = 50, inneritermax = 20, debuglevel = 0){
  N <- length(dm_train_t)
  M <- ncol(xmat)
  t <- y
  
  # Initial lambda
  init_lambda <- lambda_rate * N
  old_lambda <- init_lambda
  
  # Initial w
  init_w <- solve(init_lambda * diag(M) + t(xmat) %*% xmat) %*% t(xmat) %*% t
  old_w <- init_w
  
  # Function to calculate y
  t_to_y <- function(row,w){
    return(1 / (1 + exp(-(t(w) %*% row)))) 
  }
  
  # BEGINNING OF FOR LOOP
  
  for(i in 1:outitermax){
    w_iter <- 0
    for( j in 1:inneritermax){
      # Increment number of inner loop iterations
      w_iter <- w_iter + 1
      # Calculate new y with w
      y <- as.matrix(apply(xmat,1,t_to_y,w=old_w))
      # Calculate R - REMEMBER TO UPDATE
      x = c()
      for(n in 1:N){
        x[n] <- y[n]*(1-y[n])
      }
      R <- diag(x)
      # Calculate Gradient
      grad <- old_lambda * old_w + t(xmat)%*%(y-t)
      # Calculate Hessian
      hess <- t(xmat) %*% R %*% xmat + old_lambda * diag(M)
      # Calculate new w
      new_w <- as.vector(old_w - (solve(hess) %*% (old_lambda * old_w + t(xmat) %*% (y-t))))
      # Calculate MAD
      MAD <- 0
      for(n in 1:length(old_w)){
        MAD <- MAD + abs(old_w[n]-new_w[n])
      }
      MAD <- MAD / length(old_w)
      # Update w
      old_w <- new_w
      # Break if MAD is less than threshhold
      if(MAD < param_tol) break
    }
    for( k in 1:inneritermax){
      # Calculate the latest R
      y <- apply(xmat,1,t_to_y,w=new_w)
      x = c()
      for(n in 1:N){
        x[n] <- y[n]*(1-y[n])
      }
      R <- diag(x)
      # Calculate gamma
      eigenvalue <- eigen((t(xmat)%*%R%*%xmat), FALSE, only.values = TRUE, EISPACK = FALSE)
      gamma <- 0
      for(i in 1:length(eigenvalue$values)){
        gamma <- gamma + eigenvalue$values[i]/(old_lambda + eigenvalue$values[i])
      }
      
      # Calculate new lambda
      new_lambda <- as.numeric(gamma / (t(new_w) %*% new_w))
      
      # Break if MAD is less than threshhold
      if(abs(old_lambda - new_lambda) < param_tol){
        old_lambda <- new_lambda
        break
      }
      old_lambda <- new_lambda
    }
    # Break condition
    if(w_iter <= 2){
      # Calculate w_sd
      S_n <- solve(new_lambda * diag(M) + t(xmat) %*% R %*% xmat)
      w_sd <- sqrt(diag(S_n))
      # Set up answer
      ans <- list(w = new_w, w_sd = w_sd, lambda = new_lambda, M = M, N = N)
      return(ans)
    } 
  }
}

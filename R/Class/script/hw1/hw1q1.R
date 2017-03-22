gpredict <- function(df_train, df_test){
  ### Check for unique cases ###
  # Assume that df_test is not missing
  miss <- FALSE
  # Specify the length of a and b
  a_len <- 1
  b_len <- ncol(df_train) - a_len
  # Check whether df_test is missing or null
  if(missing(df_test) || is.null(df_test)){
    pred <- NULL
    miss <- TRUE
  }
  # Return NULL if df_test has different number of columns(features)
  else if((ncol(df_test) != b_len)){
    return(NULL)
  }
  ### Calculate stats for df_train ###
  # Keep only the first column of df_train for xa
  xa <- df_train[,1]
  # Compute the mean for xa
  mean_a <- mean(xa)
  # Keep all columns EXCEPT the first one WITHOUT dropping dimensions
  xb <- df_train[,-1,drop = FALSE]
  # Calculate column means for xb
  mean_b <- colMeans(xb, na.rm = FALSE)
  # Construct the covarianc matrix
  cov_max <- cov(df_train)
  # Construct each part of the covariance matrix for cov_ab and cov_bb
  total_len <- ncol(df_train)
  cov_ab <- cov_max[1:a_len, (a_len+1):total_len]
  cov_bb <- cov_max[(a_len+1):total_len,(a_len+1):total_len]
  # Calculate "predict" if df_test exists
  if(!miss){
    pred <- apply(df_test, 1, function(df) mean_a + cov_ab %*% solve(cov_bb) 
                  %*% as.vector(unlist((df-mean_b), use.names = FALSE)))
  }
  # Construct the required list
  ret <- list(mua=mean_a, mub=mean_b, s_ab=cov_ab, s_bb=cov_bb, predict=pred)
  return(ret)
}

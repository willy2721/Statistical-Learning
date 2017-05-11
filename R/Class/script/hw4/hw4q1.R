### Q1 ###
filter_chisq <- function(dstrain, ypos = "pos", min_count = 5, chi_threshold = 10^(-5)){
  
  # Store the first column and convert to binary
  s.product <- dstrain[,1,drop = FALSE]
  #s.product_binary <- as.numeric(s.product == ypos)
  
  # Set all values larger than 0 to 1 for every column after the second one
  dstrain[,-1][dstrain[,-1] > 0] <- 1
  
  # Store the number of occurences for each feature
  feat_freq <- colSums(dstrain[,-1])
  
  # Create empty vectors to store column positions
  colpos <- c()
  colname <- c()
  chistat <- c()
  
  for(i in 2:ncol(dstrain)){
    if(feat_freq[i - 1] > min_count){
      contingency <- table(unlist(dstrain[,i,drop = FALSE]),unlist(s.product))
      chi <- chisq.test(contingency)$statistic
      # Save results if information gain is larger than threshold
      if(chi != "NaN"){
        if(chi > chi_threshold){
          colpos <- c(colpos,i)
          colname <- c(colname, names(dstrain)[i])
          chistat <- c(chistat, chi)
        }   
      }
      
    }
  }
  
  if(length(colpos) == 0){
    return(list(colpos = NULL, colname = NULL, chistat = NULL))
  }
  return_order <- order(chistat, decreasing = TRUE)
  return(list(colpos = colpos[return_order], colname = colname[return_order], chistat = unname(chistat[return_order])))
}

### Q2 ###
entropy <- function(prob){
  # Set entropy to 0 for probabilities that are too small
  return(ifelse(prob < 10 ^ (-6) || 1 - prob < 10 ^ (-6),0,-(prob * log2(prob) + (1-prob) * log2(1-prob))))
}

filter_ig = function(dstrain, ypos = "pos", min_count = 5, ig_threshold = 10^(-5)){
  
  # Store the first column
  s.product <- dstrain[,1]
  
  # Set all values larger than 0 to 1 for every column after the second one
  dstrain[,-1][dstrain[,-1] > 0] <- 1
  
  # Store the number of occurences for each feature
  feat_freq <- colSums(dstrain[,-1])
  
  # Calculate the probabilities of pos and neg in s.product
  s_pos <- sum(s.product == ypos) / length(s.product)
  
  # Calculate the entropy of Y(s.product)
  entropy_Y <- entropy(s_pos)
  
  # Create empty vectors to store column positions
  colpos <- c()
  colname <- c()
  igvalue <- c()
  
  for(i in 2:ncol(dstrain)){
    if(feat_freq[i - 1] > min_count){
      # Percentage of 1s for each feature
      per_1 <- sum(dstrain[,i] == 1) / nrow(dstrain)
      
      # Percentage of pos values given x = 1 and x = 0 for each features
      s_pos_1 <- sum(s.product[dstrain[,i] == 1] == ypos) / sum(dstrain[,i] == 1)
      s_pos_0 <- sum(s.product[dstrain[,i] == 0] == ypos) / sum(dstrain[,i] == 0)
      
      # Calculate conditional entropy
      H_Y_X <- per_1 * entropy(s_pos_1) + (1 - per_1) * entropy(s_pos_0)   
      IG <- entropy_Y - H_Y_X
      
      # Save results if information gain is less than threshold
      if(IG > ig_threshold){
        colpos <- c(colpos,i)
        colname <- c(colname, names(dstrain)[i])
        igvalue <- c(igvalue, IG)
      }   
    }
  }
  
  if(length(colpos) == 0){
    return(list(colpos = NULL, colname = NULL, igvalue = NULL))
  }
  # Specify the order to return
  return_order <- order(igvalue, decreasing = TRUE)
  return(list(colpos = colpos[return_order], colname = colname[return_order], igvalue = unname(igvalue[return_order])))
}

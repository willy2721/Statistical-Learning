gen_utagmat <- function(utagvec,y){
  
  # Create a tag vector containing the user_tags column split by commas
  utagvec <- strsplit(utagvec, ",")
  
  # Unlist the tag vector 
  vec<-unlist(utagvec)
  
  # Sort the tag vector by order 
  sortags <- sort(table(vec), decreasing=TRUE)
  
  # Filter out the tags that occur less than 5 times
  filtags <- sortags[sortags >= 5]
  
  # Return a column containing 1's if no tags occus less than 5 times
  if(length(filtags) == 0){
    return(as.matrix(rep(1,length(utagvec))))
  }
  ### Computing t-value for each tag ###
  
  # Create a named vector with the same length as the tag vector and set all values to NA
  fillen <- length(filtags)
  filname <- names(filtags)
  alltags <- structure(rep(NA, fillen), names=filname)
  
  # Compute and store the t-stats for each tag
  for(iter in 1:fillen){
    # Get feature name
    feat <- filname[iter]
    # Store the numerical value of whether the feature exists in the tag vector
    pred <- sapply(utagvec, function(taglist) as.numeric(is.element(feat, unlist(taglist))))
    # Store the t-statistic for each tag in "alltags", NOTE "~" for binary predictor!
    # Check names(summary(lm(tar~pred))) to find the corresponding entry
    alltags[iter] <- summary(lm(y~pred))$coefficient[length(summary(lm(y~pred))$coefficient[, "t value"]), "t value"]
    #alltags[iter] <- reg_tvalue(y,pred)
    
  }
  
  # Filter out the tags with absolute value of t-stat less than 1, and order the vector
  seltags <- alltags[abs(alltags) >= 1]
  seltags <- sort(abs(seltags), decreasing = TRUE)
  sellen <- length(seltags)
  selname <- names(seltags)
  
  # Create output matrix
  for(seliter in 1:sellen){
    # Get feature name
    selfeat <- selname[seliter]
    # Store the numerical value of whether the feature exists in the tag vector
    selpred <- sapply(utagvec, function(tag) as.numeric(is.element(selfeat, unlist(tag))))
    # Combine the columns in to one dataframe 
    if(seliter == 1){
      opmatrix <- data.frame(selpred)
    }
    else{
      opmatrix <- cbind(opmatrix,data.frame(selpred))  
    }
    
  }
  
  # Name the output matrix
  colnames(opmatrix) <- paste("user",selname, sep = "_")
  opmatrix <- as.matrix(cbind(constant=1,opmatrix))
  
  # Return the required the matrix
  return(opmatrix)
}

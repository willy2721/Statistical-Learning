gen_uagentmat <- function(uagentvec, y){
  
  # Define a function to calculate t-stats
  reg_tvalue = function(y, x) {
    y=matrix(y, ncol=1)
    xmat=matrix(1, ncol=2, nrow=length(y))
    xmat[,2] = x
    bhead = solve(t(xmat)%*%xmat, t(xmat)%*%y)
    yhead = xmat %*% bhead
    e1 = y - yhead
    var1 = sum(e1 * e1) / (length(e1)-2)
    sigma2 = solve(t(xmat)%*%xmat) * var1
    t1=bhead[2]/sqrt(sigma2[2,2])
    return(t1)
  }  
  
  # Define regular expression pattern
  pattern <- "([A-Za-z][A-Za-z0-9]{1,})"
  
  uagentvec <- sapply(uagentvec, function(x) regmatches(x, gregexpr(pattern, x)))
  uagentvec <- lapply(uagentvec, unique)
  
  # Unlist the vector
  agentvec <- unlist(uagentvec)
  
  # Sort the agent vector by order 
  soragent <- sort(table(agentvec), decreasing=TRUE)
  
  # Filter out the agents that occur less than 5 times
  filagent <- soragent[soragent >= 5]
  if(length(filagent) == 0){
    return(NULL)
  }
  
  ### Computing t-value for each agent ###
  
  # Create a named vector with the same length as the agent vector and set all values to NA
  agentlen <- length(filagent)
  agentname <- names(filagent)
  allagents <- structure(rep(NA, agentlen), names=agentname)
  
  # Compute and store the t-stats for each tag
  for(iter in 1:agentlen){
    # Get feature name
    agentfeat <- agentname[iter]
    # Store the numerical value of whether the feature exists in the tag vector
    agentpred <- sapply(uagentvec, function(agentlist) as.numeric(is.element(agentfeat, unlist(agentlist))))
    #if(iter == 1){
    #  print(str(agentpred))
    #}
    # Store the t-statistic for each tag in "alltags", NOTE "~" for binary predictor!
    # Check names(summary(lm(tar~pred))) to find the corresponding entry
    allagents[iter] <- summary(lm(y~agentpred))$coefficient[length(summary(lm(y~agentpred))$coefficient[, "t value"]), "t value"]
  }
  #print(allagents)
  
  # Filter out the tags with absolute value of t-stat less than 1, and order the vector
  selagents <- allagents[abs(allagents) >= 1]
  selagents <- sort(abs(selagents), decreasing = TRUE)
  #print(selagents)
  selagentlen <- length(selagents)
  selagentname <- names(selagents)
  #print(selagentname)
  
  # Create output matrix
  for(seliter in 1:selagentlen){
    # Get feature name
    selagentfeat <- selagentname[seliter]
    # Store the numerical value of whether the feature exists in the tag vector
    selpred <- sapply(uagentvec, function(tag) as.numeric(is.element(selagentfeat, unlist(tag))))
    # Combine the columns in to one dataframe 
    if(seliter == 1){
      opmatrix <- data.frame(selpred)
    }
    else{
      opmatrix <- cbind(opmatrix,data.frame(selpred))  
    }
  }
  
  # Name the output matrix
  colnames(opmatrix) <- paste("agent",selagentname, sep = "_")
  opmatrix <- as.matrix(cbind(constant=1,opmatrix))
  
  return(opmatrix)
}

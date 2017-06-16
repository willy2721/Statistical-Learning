gen_uagentmat <- function(uagentvec, y){
  
  # Define regular expression pattern
  pattern <- "([A-Za-z][A-Za-z0-9]{1,})"
  
  uagentvec <- sapply(uagentvec, function(x) regmatches(x, gregexpr(pattern, x)))
  uagentvec <- lapply(uagentvec, unique)
  ###print(head(uagentvec))
  
  # Unlist the vector
  agentvec <- unlist(uagentvec)
  ###print(head(agentvec))
  
  # Sort the agent vector by order 
  soragent <- sort(table(agentvec), decreasing=TRUE)
  ###print(soragent)
  
  # Select the agents that occur more than 10 times and less than or equal to floor(0.5N) times
  filagent <- soragent[soragent >= 10]
  filagent <- filagent[filagent <= floor(0.5 * length(uagentvec))] 
  ###print(filagent)
  
  # Return a column containing 1's if no tags occus less than 5 times
  if(length(filagent) == 0){
    return(as.matrix(rep(1,length(uagentvec))))
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
    
    # Store the t-statistic for each tag in "alltags", NOTE "~" for binary predictor!
    # Check names(summary(lm(tar~pred))) to find the corresponding entry
    allagents[iter] <- summary(lm(y~agentpred))$coefficient[length(summary(lm(y~agentpred))$coefficient[, "t value"]), "t value"]
  }
  
  # Try sorting by reverse alphabetical order before filtering
  order <- sort(names(allagents), decreasing = TRUE)
  allagents <- allagents[order]
  ###print(allagents)
  
  # Filter out the tags with absolute value of t-stat less than 1, and order the vector
  selagents <- allagents[abs(allagents) >= 1]
  selagents <- sort(abs(selagents), decreasing = TRUE)
  selagentlen <- length(selagents)
  ###print(selagents)
  
  # Return a column containing 1's if no agents have an absolute t value larger or equal to 1
  if(selagentlen == 0){
    return(as.matrix(rep(1,length(uagentvec))))
  }
  
  # Try reordering members with the same t-value
  #for(iter in 1:(selagentlen-1)){
  #  if(selagents[iter] == selagents[iter + 1]){
  #    tmp <- names(selagents)[iter]
  #    names(selagents)[iter] <- names(selagents)[iter + 1]
  #    names(selagents)[iter + 1] <- tmp
  #    iter <- iter-1
  #  }  
  #}
  ###print(selagents)
  
  selagentname <- names(selagents)
  ###print(selagentname)
  
  # Create output matrix
  for(seliter in 1:selagentlen){
    # Get feature name
    selagentfeat <- selagentname[seliter]
    # Store the numerical value of whether the feature exists in the tag vector
    selagentpred <- sapply(uagentvec, function(tag) as.numeric(is.element(selagentfeat, unlist(tag))))
    # Combine the columns in to one dataframe 
    if(seliter == 1){
      agentmatrix <- data.frame(selagentpred)
    }
    else{
      agentmatrix <- cbind(agentmatrix,data.frame(selagentpred))  
    }
  }
  
  # Name the output matrix
  colnames(agentmatrix) <- paste("agent",selagentname, sep = "_")
  agentmatrix <- as.matrix(cbind(constant=1,agentmatrix))
  
  return(agentmatrix)
}

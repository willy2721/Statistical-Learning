hmm_train <- function(sentvec, tagvec){
  # Split the tags
  bmes_tag_split <- strsplit(tagvec, "")
  text_int <- unname(sapply(sentvec,utf8ToInt))
  
  #  Method 1 : Map by the switch function
  bmes_tag_int <- lapply(bmes_tag_split, function(x) unname(sapply(x, switch, "S" = 1, "B" = 2, "M" = 3, "E" = 4)))
  
  # Method 2 : Map by name
  # Define mapping between S,B,M,E and 1,2,3,4
  #orders=c(1,2,3,4)
  #names(orders)=c("S", "B", "M", "E")
  #bmes_tag_int <- lapply(bmes_tag_split,function(x) unname(orders[x]))
  
  # Get sequence count
  tseq_count <- matrix(0,4,4)
  vec_seq_count <- function(int_vec){
    for(i in 1:length(int_vec - 1)){tseq_count[int_vec[i],int_vec[i + 1]] <<- tseq_count[int_vec[i],int_vec[i + 1]] + 1}  
  }
  lapply(bmes_tag_int, vec_seq_count)
  
  # Get count of each word label
  ct_count <- matrix(0,70000,4)
  for(i in 1 : length(bmes_tag_int)){
    for(j in 1 : length(bmes_tag_int[[i]])){
      row <- text_int[[i]][j]
      col <- bmes_tag_int[[i]][j]
      ct_count[row,col] <- ct_count[row,col] + 1
    }
  }
  # Get count of each variable
  tprior_count <- as.matrix(colSums(ct_count))
  
  return(list(ct_count=ct_count,tseq_count=tseq_count,tprior_count=tprior_count))
}

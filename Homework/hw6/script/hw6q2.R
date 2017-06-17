hmm_predict = function(model, allsent, sepchar = " ", addsmooth = 1){
  # Represent each state
  states <- c(1,2,3,4)
  
  # Define transition matrix and initial matrix and emission matrix
  tran <- log((model$tseq_count + addsmooth) / sum(model$tseq_count + addsmooth))
  init <- t(log((model$tprior_count + addsmooth) / sum((model$tprior_count + addsmooth))))
  emis <- log((model$ct_count + addsmooth) / sum((model$ct_count + addsmooth)))
  
  # Create vector to store outsent and outtags
  outsent_vec <- c()
  outtag_vec <- c()
  
  for( n in 1 : length(allsent)){
    seq <- unlist(lapply(allsent[n],utf8ToInt))
    
    if(nchar(allsent[n]) > 1){
      # Define dataframes to store history of states and probabilities
      prob_hist <- matrix()
      state_hist <- matrix()
      
      # Calculate the probability of being in each state given the first word and initial matrix
      prob_hist  <- t(matrix(unlist(lapply(states, function(x) {
        init[1,x] + emis[seq[1], x]
      }))))
      state_hist <- t(matrix(states))
      
      # Viterbi algorithm 
      for(i in 2:length(seq)){
        tmp_list <- lapply(states, function(x) {
          max_y <- unlist(lapply(states, function(y) {
            prob_hist[i - 1, y] + tran[y, x]
          }))
          return(c(states[which.max(max_y)], max(max_y) + emis[seq[i], x]))
        })
        prob_hist <- rbind(prob_hist, c(as.numeric(tmp_list[[1]][2]), as.numeric(tmp_list[[2]][2]), as.numeric(tmp_list[[3]][2]),as.numeric(tmp_list[[4]][2])))
        state_hist <- rbind(state_hist, c(tmp_list[[1]][1],tmp_list[[2]][1],tmp_list[[3]][1],tmp_list[[4]][1]))
      }
      
      # Find Viterbi path
      vit_path <- c(which.max(prob_hist[length(seq),]))
      for(i in length(seq):2){
        vit_path <- c(vit_path,state_hist[i,vit_path[length(seq) - i + 1]])
      }
      vit_path <- rev(vit_path)
      
      # Add spaces at certain locations
      sent <- allsent[n]
      cnt <- 0
      for(i in 1:(length(vit_path)-1)){
        if(vit_path[i] == 1 || vit_path[i] == 4){
          sent <- (paste(substr(sent, 1, i + (cnt * nchar(sepchar))), substr(sent, i + (cnt * nchar(sepchar)) + 1, nchar(sent)), sep = sepchar))
          cnt <- cnt + 1
        }
      }
      
      tag <- paste(sapply(vit_path, switch, "1" = "S", "2" = "B", "3" = "M", "4" = "E"), collapse = "")
      outtag_vec <- c(outtag_vec,tag)
      outsent_vec <- c(outsent_vec,sent)
      
    }
    else if(nchar(allsent[n]) == 1){
      outsent_vec <- c(outsent_vec,allsent[n])
      outtag_vec <- c(outtag_vec,switch(which.max(emis[seq,]+init), "1" = "S", "2" = "B", "3" = "M", "4" = "E"))  
    }
    else{
      outsent_vec <- c(outsent_vec,"")
      outtag_vec <- c(outtag_vec,"")
    }
  }
  return(list(outsent = outsent_vec, outtag = outtag_vec))
}

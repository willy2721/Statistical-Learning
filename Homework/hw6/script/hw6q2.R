hmm_predict = function(model, allsent, sepchar = " ", addsmooth = 1){
  # Represent each state
  states <- c(1,2,3,4)
  
  # Define transition matrix and initial matrix and emission matrix
  tran <- log((model$tseq_count + addsmooth) / sum(model$tseq_count + addsmooth))
  init <- log((model$tprior_count + addsmooth) / sum((model$tprior_count + addsmooth)))
  
  # Create vector to store outsent and outtags
  outsent_vec <- c()
  outtag_vec <- c()
  
  for( n in 1 : length(allsent)){
    emis <- model$ct_count[utf8ToInt(allsent[n]),]
    emis <- log((emis + addsmooth) / sum((emis + addsmooth)))
    seq <- unlist(lapply(allsent[n],utf8ToInt))
    
    if(length(emis) > 4){
      # Define dataframes to store history of states and probabilities
      prob_hist <- matrix()
      state_hist <- matrix()
      
      # Calculate the probability of being in each state given the first word and initial matrix
      prob_hist  <- t(matrix(unlist(lapply(states, function(x) {
        init[x, 1] + emis[1, x]
      }))))
      state_hist <- t(matrix(states))
      
      # Viterbi algorithm 
      for(i in 2:length(seq)){
        tmp_list <- lapply(states, function(x) {
          max_y <- unlist(lapply(states, function(y) {
            prob_hist[i - 1, y] + tran[y, x]
          }))
          return(c(states[which(max_y == max(max_y))], max(max_y) + emis[i, x]))
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
      
      # Cutting and pasting
      str = c()
      end = c()
      for(j in 1:length(vit_path)){
        if(vit_path[j] == 1){
          str = c(str, j)
          end = c(end, j)
        }
        else if(vit_path[j] == 2){
          str = c(str, j)
          if(j == length(vit_path)){
            end = c(end, j)
          }
        }
        else if(vit_path[j] == 4){
          end = c(end, j)
          if(j == 1){
            str = c(str, j)
          }
        }
        else{
          if(j == length(vit_path)){
            end = c(end, j)
          }
          if(j == 1){
            str = c(str, j)
          }
        }
      }
      tag <- paste(sapply(vit_path, switch, "1" = "S", "2" = "B", "3" = "M", "4" = "E"), collapse = "")
      sent <- paste(substring(allsent[n], str, end), collapse = sepchar)
      outtag_vec <- c(outtag_vec,tag)
      outsent_vec <- c(outsent_vec,sent)
      
    }
    else if(length(emis) > 0){
      outsent_vec <- allsent
      outtag_vec <- c(outtag_vec,switch(which.max(emis+init), "1" = "S", "2" = "B", "3" = "M", "4" = "E"))  
    }
    else{
      outsent_vec <- c(outsent_vec,"")
      outtag_vec <- c(outtag_vec,"")
    }
  }
  
  return(list(outsent = outsent_vec, outtag = outtag_vec))
}

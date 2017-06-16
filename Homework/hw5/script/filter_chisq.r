

filter_chisq = function(dstrain, ypos="pos", min_count=5, chi_threshold = 1e-5) {
    nugrams = ncol(dstrain) #number of unigram+1
    chiall = rep(-1, nugrams) #the first column is always -1, and will not be selected.
    yvec = as.numeric(dstrain[,1]==ypos)
    options(warn = -1) #silence the warning
    for(ii in 2:nugrams) {
        tmp1=cbind(yvec, as.numeric(dstrain[,ii]>0))
        tmp1a=table(tmp1[,1], tmp1[,2])
        
        if(nrow(tmp1a)<2 | ncol(tmp1a)<2) {
            #stop("tmp1a table dimension too small!")
            chiall[ii] = 0
        } else if(sum(tmp1[,2])<=min_count) {
            chiall[ii] = 0
            #cat("feature", ii, "count too low, skip\n")
        } else {
            tmp2=chisq.test(tmp1a, correct=FALSE)
            chiall[ii] = tmp2$statistic
        }    
    }
    options(warn = 0) #turn the warnings back on
    o1 = order(chiall, decreasing=TRUE)
    
    tmpind1 = chiall[o1] > chi_threshold
    if(sum(tmpind1) ==0) {
        #cat("We have not features selected. The maximum value of chisq test is ", max(chiall), "\n")
        return(list(colpos = NULL, colname=NULL, chistat=NULL))
    } else {
        o2=o1[tmpind1]
        retname = names(dstrain)[o2]
        return(list(colpos = o2, colname=retname, chistat=chiall[o2]))
    }
}



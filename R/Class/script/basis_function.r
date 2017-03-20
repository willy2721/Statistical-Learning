#basis_function.r
#generate Gaussian basis functions


adv=read.csv('Advertising.csv')
#gaussian basis function
nnode=4
feature = adv[,1:3] #taking TV, Radio, and Newspaper
nfeature = ncol(feature)
fname = names(feature)

nextcol = nfeature+1
for(afeature in 1:nfeature) {
    node1=quantile(feature[[afeature]], probs = seq(0.01, .99, length.out=nnode))
    s1 = (max(node1) - min(node1)) /length(node1)
    
    for(anode in 1:length(node1)) {
        newf = exp(-(feature[[afeature]] - node1[anode])^2/(2*s1^2))
        feature[[nextcol]] = newf
        aname = paste(fname[afeature], anode, sep="_")
        names(feature)[nextcol]=aname
        nextcol= nextcol+1
    }    
}

feature$outcome = adv$Sales

nrun=100 #number of run
diffall = rep(NA, nrun) #store results 
for(ii in 1:nrun) {
    #train-test split
    train_obs = sample(1:nrow(feature), floor(nrow(feature)*0.8))
    test_obs= setdiff(1:nrow(feature), train_obs)
    train = feature[train_obs,] 
    test = feature[test_obs,]
    #model training    
    lm2=lm(outcome~., data=train)
    lm1=lm(outcome~TV+Newspaper+Radio, data=train)
    #prediction
    pred2 = predict(lm2, test)
    pred1 = predict(lm1, test)
    mae2= mean(abs(test$outcome- pred2))
    mae1= mean(abs(test$outcome- pred1))    
    diffall[ii] = mae1 - mae2
}
#conduct t-test
print(t.test(diffall))







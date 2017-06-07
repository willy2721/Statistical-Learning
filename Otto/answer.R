# Create dataframe for correct answer
answer <- data.frame(id=train$id, Class_1=0, Class_2=0, Class_3=0, Class_4=0, Class_5=0, Class_6=0, Class_7=0, Class_8=0, Class_9=0)
for(n in 1:nrow(answer)){
  # Extract the target, transform to character then split by "_" and extract the number, then add 1 to get the correct index
  answer[n,as.numeric(strsplit(as.character(train$target[n]),"_")[[1]][2]) + 1] <- 1
}
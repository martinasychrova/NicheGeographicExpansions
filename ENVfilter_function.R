
#### ENVfilter by Jan Divisek ####

ENVfilter <- function(Data, 
                      Vars, 
                      Resolution, 
                      Seed= 123) {
  
  Data <- as.data.frame(Data)
  env  <- as.data.frame(scale(Data[, Vars]))
  
  min.score <- min(apply(env, 2, min))
  max.score <- max(apply(env, 2, max)) 
  
  Sekvence <- c(rev(seq(0, min.score-Resolution, by=Resolution*-1)), seq(Resolution, max.score+Resolution, by=Resolution))
  
  ID <- env
  for (i in 1:ncol(env)) {
    ID[,i] <- as.numeric(cut(env[,i], breaks = Sekvence, 
                             include.lowest = TRUE))
  }
  
  Data$Group <- apply(ID, 1, FUN = paste, collapse="")
  
  set.seed(Seed)
  Data <- Data[sample(nrow(Data)),]
  
  Result <- Data[!duplicated(Data$Group), ]
  Result <- Result[, colnames(Result) != "Group"]
  
  return(Result)
}

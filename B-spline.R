date <- read.csv('hwdata.csv',header=T)
date$x <- c(1:nrow(date))

B_spline <- function(data,x,y,interval){
 intercept <- rep(1,nrow(data))
 
 kernel <- function(val){
  return(sqrt((interval+val)*(interval-val)))
  }

 B <- NULL
 for(i in 0:round(nrow(data)/interval)){
  A <- vector(length=nrow(data))
  for(j in 1:nrow(data)){
   if((data[,grep(x,colnames(data))][j] >= interval*(i-1)) & (data[,grep(x,colnames(data))][j] <= interval*(i+1))) {A[j] <- kernel(j-interval*i)}
   else {A[j] <- 0}
  }
  B <- cbind(B,A)
 }

 B <- as.data.frame(B)

 for(i in 0:round(nrow(data)/interval)){
 colnames(B)[i] <- paste0('A',i)
 }

 lm <- lm(data[,grep(y,colnames(data))]~.,data=B)

 plot(date[,grep(x,colnames(data))],date[,grep(y,colnames(data))])
 lines(date[,grep(x,colnames(data))],predict(lm,B),col='red')
}

B_spline(date,'x','Æò±Õ±â¿Â..C.',90)



 

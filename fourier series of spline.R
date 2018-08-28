data <- read.csv('hwdata.csv')
data$x <- 1:nrow(data)

fourier_spline <- function(data,x,y,period,k){
 A <- NULL
 for(i in 1:k){
  Sin <- sin(i*(2*pi/period)*data[,grep(x,colnames(data))])
  Cos <- cos(i*(2*pi/period)*data[,grep(x,colnames(data))])
  A <- cbind(A,Sin,Cos)
 }
 
 df <- as.data.frame(A)
 
 for(i in 1:k){
 colnames(df)[2*i-1] <- paste0('Sin',i)
 colnames(df)[2*i] <- paste0('Cos',i) 
 }

 lm <- lm(data[,grep(y,colnames(data))]~.,data=df)
 
 plot(data[,grep(x,colnames(data))],data[,grep(y,colnames(data))])
 lines(data[,grep(x,colnames(data))],predict(lm,df),col='red')
}

fourier_spline(date,'x','Æò±Õ±â¿Â..C.',365,3)
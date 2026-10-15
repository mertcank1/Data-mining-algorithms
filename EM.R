###EM Algorithm####

Veri=c(26,33,40,11,76,14,20,54,55,40,41,51, NA, NA, NA)
Veri=as.data.frame(Veri)
EM=function(x,xjp,epsilon){
  y=sum(x, NA, na.rm = TRUE)/(nrow(x))+(sum(is.na(x))*xjp)/nrow(x)
  f=y-xjp
  yeni_xjp=y
  i=1
  print(y)
  while (i < 10) {
    xjp[i]=y[i]
    y[i]=sum(x, NA, na.rm = TRUE)/(nrow(x))+(sum(is.na(x))*xjp[i])/nrow(x)
    f[i]=y[i]-xjp[i]
    if(f[i]<epsilon){
      return(y[i])}
    else {
      print(y[i])
      
    }
  }

}
EM(Veri,30,0.1)

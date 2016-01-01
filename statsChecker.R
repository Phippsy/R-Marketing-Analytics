# A function to return some basic stats about a vector passed in

firstAnalysis<-function(x){
  stats<-matrix(nrow=7, ncol=1, dimnames=list(c("max", "mean", "median", "var", "sd", "IQR", "mad" ), "Numbers"))
  stats["max", ]<-max(x, na.rm=TRUE)
  stats["mean", ]<-mean(x, na.rm=TRUE)
  stats["median", ]<-median(x, na.rm=TRUE)
  stats["var", ]<-var(x, na.rm=TRUE)
  stats["sd", ]<-sd(x, na.rm=TRUE)
  stats["IQR", ]<-IQR(x, na.rm=TRUE)
  stats["mad", ]<-mad(x, na.rm=TRUE)
  stats
}
"rmaref.predict" <-
function(Future,p.e){
## Derive RMA+ expression.
PMindex<-pmindex(Future)
PM<-log2(pm(Future))
PM<-sweep(PM,1,unlist(p.e))
pm(Future)<-PM
PMlist<-lapply(PMindex,function(x,y) intensity(y)[x,],Future)
future<-t(sapply(PMlist,colMedians))
if(nrow(future)==1) {future <- t(future)}
colnames(future)<-sampleNames(Future)
return(future)
}


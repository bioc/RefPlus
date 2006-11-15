"rmaplus" <-
function(Future, rmapara, r.q, p.e, bg=TRUE){
## Derive RMA+ expression.


if (missing(r.q)&(missing(rmapara))) {
  stop("Missing Reference Quantiles")
}
if (missing(p.e)&(missing(rmapara))) {
  stop("missing Probe Effects")
}

if (!missing(rmapara)){
r.q=rmapara[[1]]
p.e=rmapara[[2]]
cat("Use rmapara.\n")
} else
{
  cat("Use Reference.Quantiles and Probe.Effects.\n")
}

if (bg == TRUE) Future<-bg.correct.rma(Future)   ##if yes, background correction
PM=pm(Future)
pm(Future)<-normalize.quantiles2(PM,r.q)
rm(PM)
future<-rmaref.predict(Future,p.e)
return(future)
}


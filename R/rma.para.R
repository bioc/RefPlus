"rma.para" <-
function(Train, bg=TRUE, exp=FALSE){
##Obtain reference quantiles, reference probe effects based on
##reference set Train

expression<-numeric(0)
if (bg == TRUE) Train=bg.correct.rma(Train)      ##if yes, background correction
PM<-pm(Train)
core<-rowMeans(apply(PM,2,sort))
PM<-normalize.quantiles(PM);
pm(Train)<-PM
rm(PM)

Pset<- rmaPLM(Train,normalize=FALSE,background=FALSE,output.param=list(
weights=FALSE,varcov="none",residuals=FALSE,resid.SE=FALSE))
##Here we use R2.1.0, where probe.coefs is a list.
##In R.2.0.1, probe.coefs is a vector
R<-coefs.probe(Pset)
if (exp == TRUE) expression<-coefs(Pset)        ##if yes, return gene expression
return(list(Reference.Quantiles=core,probe.effects=R,expression=expression))
}


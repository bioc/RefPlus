"normalize.quantiles2" <-
function(X,Reference.Quantiles){
##Quantile nomralization to a reference set
apply(X,2,function(x,y) y[rank(x)],Reference.Quantiles)
}


toy_model<-function(x){
     c( x[1] + x[2] + rnorm(1,0,0.1) , x[1] * x[2] + rnorm(1,0,0.1) )
}

toy_prior=list(c("unif",0,1),c("normal",1,2))

toy_targets = c(1.5, 0.5)



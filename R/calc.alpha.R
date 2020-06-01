calc.alpha <- function(sim.p,sim.p.names,target.data,specs=NULL){

  bias.info=grepl(".bias",specs$data)
  bias.names=specs$data[bias.info]
  if(length(bias.names)>0){
    bias.names = gsub(".bias", "", bias.names)
  }
  t.names=specs$data[!bias.info]

  min.z = qnorm(5e-30)
  p.obs=n.obs=NULL
  names.p.obs=NULL
  for(i in 1:length(t.names)){
    use.targets = grepl(t.names[i],sim.p.names)
    if(any(use.targets)){
      
      p.obs=c(p.obs,target.data[[t.names[i]]][['p']])
      n.obs=c(n.obs,target.data[[t.names[i]]][['n']])
      names.p.obs = c(names.p.obs,sim.p.names[use.targets][1:length(target.data[[t.names[i]]][['p']])])

    }
  }
  se.obs = sqrt(p.obs*(1-p.obs)/n.obs)
  names(p.obs)=names(se.obs)=names.p.obs
  sim.p=sim.p[,(names.p.obs),with=FALSE]

  z.value <- sweep(sim.p,2,p.obs,"-")
  z.value <- -1*abs(z.value)
  z.value <- sweep(z.value,2,se.obs,"/")
  z.value[z.value<min.z] = min.z
  z.value[is.na(z.value)] = -100

  if(any(bias.info)){ 
    
    p.obs.bias=n.obs.bias=NULL
    names.p.obs.bias=NULL
    for(i in 1:length(bias.names)){
      use.targets = grepl(bias.names[i],sim.p.names)
      if(any(use.targets)){
        
        p.obs.bias=c(p.obs.bias,target.data[[paste0(bias.names[i],".bias")]][['p']])
        n.obs.bias=c(n.obs.bias,target.data[[paste0(bias.names[i],".bias")]][['n']])
        names.p.obs.bias = c(names.p.obs.bias,
                             sim.p.names[use.targets][1:length(target.data[[bias.names[i]]][['p']])])
      }
    }
    se.obs.bias = sqrt(p.obs.bias*(1-p.obs.bias)/n.obs.bias)
    names(p.obs.bias)=names(se.obs.bias)=names.p.obs.bias
 
    z.value2 <- sweep(sim.p[,(names.p.obs.bias),with=FALSE],2,p.obs.bias,"-")
    sim.p.gt.p.bias = z.value2 >= 0
    
    z.value2 <- -1*abs(z.value2)
    z.value2 <- sweep(z.value2,2,se.obs.bias,"/")
    z.value2[z.value2<min.z] = min.z
    z.value2[is.na(z.value2)] = -100
    
    sim.p.gt.p.bias[is.na(sim.p.gt.p.bias)]=FALSE
    sim.p.lt.p.obs = (sweep(sim.p[,(names.p.obs.bias),with=FALSE],2,
                            p.obs[(names.p.obs.bias)],"-") <= 0)
    sim.p.between=(!sim.p.lt.p.obs) & (!sim.p.gt.p.bias) &  (!is.na(sim.p[,(names.p.obs.bias),with=FALSE]))
    
    z.value[,(names.p.obs.bias)] =  
      (z.value[,(names.p.obs.bias)] * sim.p.lt.p.obs) +   # if sim.p<p.obs or no bias>1 then use z.value
      (z.value2[,(names.p.obs.bias)] * sim.p.gt.p.bias) + # if sim.p>p.obs.bias & bias>1 then use z.value2
      0 * sim.p.between                                   # if p.obs<sim.p<bias*p.obs then z.value=0 / p-value = 1
    
  }

  p.value = as.data.table(2*pnorm(as.matrix(z.value)))

  return(p.value)
}

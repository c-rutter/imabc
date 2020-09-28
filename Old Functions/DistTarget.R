DistTarget <- function(parms,parm.names,calib.targets,target.specs,parm.priors,flag.new){
# note: before changing how user-defined files were sourced, this function was able to 
# uses the parm.names object, but it was not clear why - this is no longer the case,
# so parm.names is passed.

  source('IMABC/Library.R')
  
  bias.info=grepl(".bias",target.specs$data)
  bias.names=target.specs$data[bias.info]
  if(length(bias.names)>0){
    bias.names = gsub(".bias", "", bias.names)
  }
  use.targets = target.specs$data[!bias.info]
  
  get.dist.names <<- make.get.dist.names(target.specs$data[!bias.info])
  dist.names=get.dist.names()

  target.dist = get.empty.indiv.dist() 
  i.out <- get_i.out(target.specs[!bias.info,])

  sim.parm = as.numeric(rep(NA,max(i.out)))
  error_value = NA

  calib.ind = parm.names %in% parm.priors$parm 
  in.range = (parms[calib.ind]>=parm.priors$c & parms[calib.ind]<=parm.priors$d) 
  if(sum(in.range)==0){
    return(list(ll = target.dist, sp = sim.parm))
  }
  
  calib.parms=parm.names[calib.ind]
  
  # avoid too-long runs: is this a reasonable adenoma risk level?
  # base this on maximum adenoma risk at ages 50, 60, 70, and 80
  # revise up? drop?
  max.prev=c(0.3,0.4,0.5,0.5)
  sim.prev=(-cumsum(c((parms[parm.names=="ar.mean"]/30-parms[parm.names=="ar.20to49"]),
                          -parms[parm.names=="ar.50to59"],
                          -parms[parm.names=="ar.50to59"],
                          -parms[parm.names=="ar.70plus"])))
  in.range[1] = in.range[1] & all(sim.prev <= max.prev)

  in.range[calib.parms=="growth.colon.beta2"] = 
    (parms[calib.parms=="growth.colon.beta2"] <= growth.beta(p=unlist(calib.targets[["Range.p10mmIn10yrs"]])[1],
                                                           alpha=parms[parm.names=="growth.colon.beta1"])) &
    (parms[calib.parms=="growth.colon.beta2"] >= growth.beta(p=unlist(calib.targets[["Range.p10mmIn10yrs"]])[2],
                                                           alpha=parms[parm.names=="growth.colon.beta1"]))
  in.range[calib.parms=="growth.rectum.beta2"] = 
    (parms[calib.parms=="growth.rectum.beta2"] <= growth.beta(p=unlist(calib.targets[["Range.p10mmIn10yrs"]])[1],
                                                           alpha=parms[parm.names=="growth.rectum.beta1"])) &
    (parms[calib.parms=="growth.rectum.beta2"] >= growth.beta(p=unlist(calib.targets[["Range.p10mmIn10yrs"]])[2],
                                                           alpha=parms[parm.names=="growth.rectum.beta1"]))

  if(!all(in.range)){
    return(list(ll = target.dist, sp = sim.parm))
  }

  target.info = get.target.info()
 
  # need to figure out how to add back in Corley bias
  stop=FALSE # flag used to break loop specifically for SEER data, due multiple distances

  n.targets=nrow(target.specs[!bias.info,])
  sim.target=rep(TRUE,n.targets)
  if(any(flag.new)) sim.target=flag.new

  for(i in 1:n.targets){
    # process targets as specified in target.sims
    if(sim.target[i]){
      target.name = use.targets[i]

      sim.res =  tryCatch(SimTarget(parms=parms,
                                    parm.names=parm.names,
                                    specs=subset(target.specs,data==target.name),
                                    target.data=calib.targets[[target.name]],
                                    mix.n=unlist(target.info[target==target.name]$mix.n),
                                    target.age.distn.name = unlist(target.info[target==target.name]$age.distn.name),
                                    mix.distn=unlist(target.info[target==target.name]$mix.distn),
                                    target.age.distn.parms.fem = unlist(target.info[target==target.name]$age.parms.fem),
                                    target.age.distn.parms.male = unlist(target.info[target==target.name]$age.parms.male)), 
                          error = function(e){
                            print_error_info(e,parms)
                            as.numeric(rep(error_value, 2 + i.out[[target.name]][2] - i.out[[target.name]][1]))})
  
      sim.parm[i.out[[target.name]][1]:i.out[[target.name]][2]] = sim.res
  
      
      if(any(grepl("SEER",target.name))){
     
        #  4 distances for SEER: this order is fixed and aligns with SimTarget, 
        #   code could be improved so that it doesn't ref column nums            
        SEER.names= paste0(target.name,c(".colon.fem",".colon.male",".rectal.fem",".rectal.male"))
        target.dist[names(target.dist) %in% SEER.names ] = 
            -1*c(get.p.dist(obs=calib.targets[[target.name]]$p[ 1: 5],sim=sim.res[ 1: 5]),
                 get.p.dist(obs=calib.targets[[target.name]]$p[ 6:10],sim=sim.res[ 6:10]),
                 get.p.dist(obs=calib.targets[[target.name]]$p[10:15],sim=sim.res[11:15]),
                 get.p.dist(obs=calib.targets[[target.name]]$p[16:20],sim=sim.res[16:20])  )
   
        for(i in 1:4){
          c.range=(5*(i-1)+1):(5*i)
          if(in.range(sim.res[(c.range)],
                      calib.targets[[target.name]]$lo.lim[c.range],
                      calib.targets[[target.name]]$up.lim[c.range])){
            target.dist[SEER.names[i]] = -1*target.dist[SEER.names[i]]
          }
        }
      }else{
    
        n.calib = nrow(calib.targets[[target.name]])
        
        target.dist[target.name] = -1*get.p.dist(obs=calib.targets[[target.name]]$p,
                                                 sim=sim.res[1:n.calib])
    
        if(target.name %in% bias.names){ 
  
          if(!(in.range(sim.res[1:n.calib],
                        calib.targets[[target.name]]$lo.lim,
                        calib.targets[[paste0(target.name,".bias")]]$up.lim))){stop=TRUE}
          
        }else{
   
          if(!in.range(sim.res[1:n.calib],
                    calib.targets[[target.name]]$lo.lim,
                    calib.targets[[target.name]]$up.lim)){ stop=TRUE}
          
        }
  
        if(!stop){
          target.dist[target.name]=-1*target.dist[target.name]
        }else{
          break
        }
      }
  
      if(stop){break}
    }
  }
  return(list(ll = target.dist, sp = sim.parm))
}

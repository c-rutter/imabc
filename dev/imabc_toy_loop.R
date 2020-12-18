source('R/Library.R')
# sources all requisite functions, including the toy calibration model

imabcloop <- function(N0=1000,
                    N.centers=1,
                    B=10,
                    N.post=1,
                    max.iter=1,
                    seed=1234,
                    toy=TRUE,
                    N.cov.points=0,     
                    sample.inflate=1,
                    inputfile = FALSE,
                    inputs.dir="calib_inputs", 
                    prior.file="parmpriors.csv",
                    default.file="default_parm_values.csv",
                    target.sims="targetspecs.csv",
                    target.file = "calibrationtargets.xlsx",
                    model.parm.order = "model_parm_order.csv",
                    userfn.dir="calib_userfn", 
                    outfile.modelparms="modelparms.csv",
                    outfile.dist="distance.csv",
                    outfile.simparms="simparms.csv",
                    outfile.sampling="meancov.csv",
                    output.directory=".",
                    LHS=TRUE,
                    continue.runs=FALSE,
                    reeval=FALSE,
                    prevruns.dir=".",
                    print.status=FALSE,
                    recalc.centers=TRUE){

       #  N0: initial draw size
       #  seed: overall seed value, used to simulate multiple streams
       #  inputs.dir: directory containing model inputs
       #  prior.file: contains prior distribution information for model parameters
       #  target.sims: specification for simulations for each target
       #  userfn.dir: dirctory containing user-specified functions
       #  reeval: reevaluate currently accepted runs (logical)

       #  ABCLoopFunction creates 7 output files:
       #  results.dir is the results directory.
       #  1. outfile.modelparms: all drawn model parameters
       #  2. outfile.dist: distance from targets 
       #  3. outfile.simparms: simulated targets at model parameters
       #  4. 'good.'outfile.modelparms: accepted model parameters
       #  5. 'good.'outfile.dist: distance from targets for accepted model parameters
       #  6. 'good.'outfile.simparms: simulated targets at acccepted model parameters
       #  7. outfile.sampling: mean and covariance of sampling distributions
       #
       #  Notes:
       #
       #  The current implementation assumes that estimtaes are downwardly biased. Code would need ot be generalized to allow upward bias.
       #
       #  We incorporate one type of user-specified prior distribution: a resticted uniform, which is used to 
       #  accomodate the restricted range of beta2 growth parameters, which depend on beta1
       #
       #  There is currently some specialized coding to allow continuation of prior runs. Continuation requres
       #  the use of the same targets. Some code that can be removed correcs an issue with naming of SEER targets



# User-provided parameter information
# User needs to provide:
     # - priors
     # - threshold / tolerance interval
     # - targets
     # - stopping criteria




# max.iter=1
# start.iter=1
# end.iter=max.iter
# total.draws=0
# prevruns.dir=NULL
# N0=1000
# n.draw=N0   # first iteration only, then set to n.center*B
# N.in=0
# N.use=0
# f.append=FALSE
# n.targets=length(dist.names)
# ESS=0
# N.post=1
# N.centers=1
# B=10
# N.store = N.post + N.centers*(B+1)




# parameter information (priors).............................................
parm.priors.df <- read.csv(prior.file, header=FALSE, stringsAsFactors=FALSE)


names(parm.priors.df) = c("parm","prior.dist","a","b","c","d")

## TE NOTE: explain this
parm.priors.df$sd=sqrt((parm.priors.df$b - parm.priors.df$a)^2/12)
parm.priors.df$sd[parm.priors.df$prior.dist==1] = parm.priors.df$b[parm.priors.df$prior.dist==1]


# easiest thing to do is allow truncated normal, and uniform

# a -> mean
# b -> sd
# c -> left
# d -> right

toy_prior=list(c("unif",0,1),c("normal",1,2))
toy.prior.df <- data.frame(parm=c("parm1", "parm2") , type=c("unif", "normal"), "a" = c(0,1), "b"=c(1, 2)) 

if(toy){
     parm.names = c("parm1", "parm2")
     n.parm=length(parm.names)
}


# removing alpha as the stopping condition


# # Get parameter names, ##user-provided parameter

# if(inputfile){
#      parm.names = get.model.parm.names()
#      n.parms=length(parm.names)
# }

  

#   # Name-fetch error call
#   if(!all(unlist(parm.priors.df$parm) %in% parm.names)){
#     print(paste0("Error: prior given for unspecified model parameter in ",prior.file))
#     break
#   }

#   # Calibration parameters
#   calib.parm.names <- unlist(parm.priors.df$parm[parm.priors.df$sd>0])
#   n.calib.parms <- length(calib.parm.names)  


#   if(N.cov.points==0) N.cov.points=25*n.calib.parms
  
#   # fixed values specified in the priors file: these can differ from usual model
#   # Fixed values are values we need that we do not calibrate, the complement of calib.parms
#   fixed.parm.names <- parm.names[parm.priors.df$sd==0]
#   fixed.parm.values <- parm.priors.df$a[parm.priors.df$parm %in% 
#                                           fixed.parm.names]
#   # default values for other parameters
#   if(!all(parm.names %in% c(calib.parm.names,fixed.parm.names))){
#     default.parms <- read.csv(default.file, header=FALSE, stringsAsFactors=FALSE)
#     use.defaults <- !unlist(default.parms[,1]) %in% c(calib.parm.names,fixed.parm.names)
#     default.parm.names  <- unlist(default.parms[,1][use.defaults])
#     default.parm.values <- unlist(default.parms[,2][use.defaults])
#   }

#   parm.priors.df = parm.priors.df[parm.priors.df$parm %in% calib.parm.names,]


#    if(!all(parm.names %in% c(calib.parm.names,fixed.parm.names,default.parm.names))){
#     print("Error: all parameters need to have a prior distribution, fixed value, or default value")
#     break
#   } 
  
#   if(!all(c(calib.parm.names,fixed.parm.names,default.parm.names) %in% parm.names)){
#     print("Error: extra model parameters specified")
#     break
#   } 



#   # combine fixed and default parameter names/values
#   fixed.parm.names = c(fixed.parm.names,default.parm.names)
#   fixed.parm.values = c(fixed.parm.values,default.parm.values)
  
#   # put parameters as specified in model.parm.order
#   x=data.table(read.csv(paste0("./",model.parm.order), header=FALSE, stringsAsFactors=FALSE))

#   names(x)="parm"
#   parm.priors.df=as.data.table(parm.priors.df)
#   parm.priors.df=as.data.frame(parm.priors.df[x, on="parm"])
#   parm.priors.df=parm.priors.df[!is.na(parm.priors.df$prior.dist),]
  
#   # put parm names vectors in the expected order
#   calib.parm.names = parm.names[parm.names %in% calib.parm.names]
#   fixed.parm.names = parm.names[parm.names %in% fixed.parm.names]

#   # target data ## user-provided parameters...........................................
#   target.specs <- read.csv(target.sims, header=FALSE, stringsAsFactors=FALSE)
#   names(target.specs)=c("data","m","date","pct.male","n.ret", "alpha","alpha.target")

#   target.specs$date <- as.Date(target.specs$date,format="%Y-%m-%d")
#   bias.info=grepl(".bias",target.specs$data)

#   get.dist.names <<- make.get.dist.names(target.specs$data[!bias.info])
#   dist.names <- get.dist.names()
#   target.specs$update.alpha = (target.specs$alpha<target.specs$alpha.target) 
#   target.specs$new.alpha = target.specs$alpha
#   update.names = target.specs[target.specs$update.alpha,]$data
 
#   if(print.status){
#     print(paste0("target alpha levels are: ",     paste(target.specs$data[!bias.info]," ",
#                        format(target.specs$alpha.target[!bias.info],digits=2),
#                        collapse="; ")))
#   }

#   calib.targets=get.targets(target.specs,target.file = target.file)

#   sim.parm.names = get.simparm.names(target.specs[!bias.info,])
#   calib.target.names = get.calib.target.names(sim.parm.names,target.specs[!bias.info,],calib.targets)  

#   update.target.names = get.update.targets(calib.target.names, target.specs$data[target.specs$update.alpha])






#   # set seed values ...........................................................
#   seed=1234
#   set.seed(seed)
#   rngKind = "L'Ecuyer-CMRG"
#   RNGkind(kind = rngKind, normal.kind = NULL)

#   # set columns associated with simulated parameter values for each target 
#   # dataset. The first three columns are iter, draw, and step
#   #............................................................................






# # Initiate data frames for draws, target distributions and simulation parameters

# good.parm.draws <- init.draws(N.store, c(parm.names,"scaled.dist","sample.wt"))
# good.target.dist=init.sims(N.store,c(dist.names,"tot.dist"))

# # alpha no longer useful, figure out how to add bounds
# good.target.dist$alpha=as.numeric(0)
# good.target.dist$n.good=as.integer(0)

# setcolorder(good.target.dist,c("iter","draw","step",dist.names, "tot.dist","alpha","n.good"))

# # good.sim.parm=init.sims(N.store,sim.parm.names)

# # take non-missing names

# good.sim.parm=init.sims(N.store,sim.parm.names[1:44])



# ###############################################################################
# # initialize data.tables and matrices used for calculations
# ###############################################################################

# recalc.centers=TRUE

# num.rows=max(n.draw,N.centers*B) + (recalc.centers)*N.centers
# parm.draws <- init.draws(num.rows,c(parm.names,"scaled.dist","sample.wt"))
# parm.draws$step=0

# target.dist <- init.sims(num.rows,c(dist.names,"tot.dist"))
# target.dist$alpha = as.numeric(0)
# target.dist$n.good=as.integer(0)
# target.dist$step=0

# sim.parm <- init.sims(num.rows,sim.parm.names[1:44])
# sim.parm$step=0

# setcolorder(target.dist, c("iter","draw","step",dist.names,"tot.dist","alpha","n.good"))



# ###############################################################################
# # if starting a new run, draw parameters from prior distributions
# ###############################################################################

# parm.draws$iter = target.dist$iter = sim.parm$iter = 1

# draws=1:N0
# parm.draws$draw[draws] = target.dist$draw[draws] = 
#  sim.parm$draw[draws] = draws


# s <- .Random.seed
# parm.draws[draws, seed := get.random.seed.strings(s,n.draw)]

# LHS=TRUE

# # Latin Hypercube
# if(LHS){
#    u.draws=randomLHS(N0,n.calib.parms)
# }else{
#    u.draws=matrix(runif(N0*n.calib.parms),nrow=n.parms,ncol=N0)
# }


# # set any fixed parameter values (specified in parm.priors)
# if(length(fixed.parm.names)>0){
#  names(fixed.parm.values)=fixed.parm.names
#  set(parm.draws,j=fixed.parm.names,value=as.data.table(t(fixed.parm.values)))
# }

# # draw calibrated parameters from priors: consider making draw.parms more general....

# # TE notes: make draw parms more general
# for(i in 1:n.calib.parms){
#  if(parm.priors.df[i,'prior.dist']==1){
  
#         parm.draws[draws,calib.parm.names[i] := qtruncnorm(u.draws[,i],
#                                                 a=parm.priors.df[i,'c'],
#                                                 b=parm.priors.df[i,'d'],
#                                                 mean=parm.priors.df[i,'a'],
#                                                 sd=parm.priors.df[i,'b'])]
   
#     }else if((parm.priors.df[i,'prior.dist']==0)){  

         parm.draws[draws,calib.parm.names[i] := u.draws[,i]*(parm.priors.df[i,'b'] - parm.priors.df[i,'a']) + parm.priors.df[i,'a']]
      
      
    }else if((parm.priors.df[i,'prior.dist']==3)){  
    
      parm.draws[draws,calib.parm.names[i] := 
                   user.dist(u=u.draws[,i],
                             parm.name=calib.parm.names[i],
                             parm.draws=parm.draws[draws,],
                             parm.priors=parm.priors.df,
                             calib.targets=calib.targets)]

    }
}





############################################################################################
############################################################################################
# MAIN LOOP

#============================ mainloop ========================================
  for(iter in start.iter:end.iter){
    N.in.i=0

    if(!(continue.runs==TRUE & iter==start.iter)){ 

      # in this case, skip to simulation of new draws
      if(print.status){
        iter.status=paste0("Starting iteration ",iter,
                           " at ",Sys.time()," N.in=",N.in)
        if(any(target.specs$update.alpha)){
          print(paste0(iter.status,
                       " current alpha-levels are ",
                       paste(target.specs[target.specs$update.alpha & !bias.info,]$data," ",       format(
                               target.specs[target.specs$update.alpha & !bias.info,]$alpha, digits=2),
                             collapse="; ")))
        }else{
          print(paste0(iter.status," all tolerance intervals at target width"))
        }
      }
      write.table(parm.draws[1:n.draw,
                             c("iter","draw","step","seed",parm.names),
                             with=FALSE],
                  file=paste0(output.directory,"/",outfile.modelparms),
                  sep=",",
                  append=f.append,col.names=!f.append,row.names=FALSE)

#     # Loop over parameter draws: this process parallelized
#     # each of these calls provides the target data and so returns both the
#     # simulated parameters & the likelihood.
#     #--------------------------------------------------------------------------

#     parms.to.run <- parm.draws[1:n.draw,c("seed",parm.names),with=FALSE]

    ## Getting alphas
    alphas = unlist(target.specs$alpha) 
    ## Create a data.table where each row is an alphas vector
    alphasdt = data.table(matrix(rep(alphas,n.draw),byrow=T,ncol=length(alphas)))

    ## cbind to parms.to.run
    parms.to.run <- cbind(parms.to.run, alphasdt)
     
    ## Interface with OUT_put and IN_get EMEWS functions ###

    # Push string representation of parms.to.run to queue
    OUT_put(parms.dt.to.string(parms.to.run))

    # Get string representation of results from queue
    q.res <- IN_get()

#     # Check to see if there were any results returned
#     if (q.res != ""){
#     # the call to results.from.q.results causes NAs to be introduced by coercion
#       res <- results.from.q.result(q.res)
#       target.dist[1:n.draw, (dist.names) := res$ll]
#       sim.parm[1:n.draw, (sim.parm.names) := res$sp]
#     }

#     total.draws=total.draws+n.draw

#     # write out results to csv files
#     write.table(target.dist[1:n.draw,
#                             c("iter","draw","step",dist.names),with=FALSE],
#                 file=paste0(output.directory,"/",outfile.dist),sep=",",
#                 append=f.append,
#                 col.names=!f.append,
#                 row.names=FALSE)

#     write.table(sim.parm[1:n.draw,
#                          c("iter","draw","step",sim.parm.names),with=FALSE],
#                 file=paste0(output.directory,"/",outfile.simparms),sep=",",
#                 append=f.append,
#                 col.names=!f.append,
#                 row.names=FALSE)

#     # have already opened and written to mixture.sampling files
#     if(continue.runs) f.append=TRUE  

#     # Count the good points: points associated with positive distances
#     target.dist$n.good=as.integer(0)
#     target.dist$n.good[1:n.draw] = rowSums(target.dist[1:n.draw,(dist.names),
#                                                        with=FALSE]>=0,na.rm=TRUE)

#     N.in.i = sum(target.dist[step<=N.centers,]$n.good==n.targets,na.rm=TRUE)

#     # Stop if there are no close points (and so cannot continue)
#     if(N.in+N.in.i==0){
#       if(print.status) print("No parms to work from")
#       break
#     }

#     # replace re-simulated targets for center draws in good.* matrices
#     # and recalculate p-value and distance

#     if(recalc.centers & iter>1){

#       center.draw = parm.draws[step==(N.centers+1),]$draw
                    
#       # sort to ensure alignment across data tables
#       center.draw=sort(center.draw,na.last=TRUE)
#       setorder(good.parm.draws,iter,draw,step,na.last=TRUE)
#       setorder(parm.draws,iter,draw,step,na.last=TRUE)
#       setorder(good.sim.parm,iter,draw,step,na.last=TRUE)
#       setorder(sim.parm,iter,draw,step,na.last=TRUE)
#       setorder(good.target.dist,iter,draw,step,na.last=TRUE)
#       setorder(target.dist,iter,draw,step,na.last=TRUE)

#       good.sim.parm[draw %in% center.draw,c("draw",sim.parm.names)] <-
#         sim.parm[draw %in% center.draw,c("draw",sim.parm.names),with=FALSE]

#       good.target.dist[draw %in% center.draw,c("draw",dist.names)] <-
#         target.dist[draw %in% center.draw,c("draw",dist.names),with=FALSE]
      
#       good.target.dist[draw %in% center.draw,]$n.good = 
#         rowSums(good.target.dist[draw %in% center.draw,
#                                  (dist.names),with=FALSE]>=0,na.rm=TRUE)
#       good.target.dist[is.na(n.good),]$n.good=as.integer(0)
 
#       # some recalculated centers may no longer be in range
#       remove.draws = good.target.dist[((draw %in% center.draw) & (n.good < n.targets)),]$draw

#       keep.draws = good.target.dist[((draw %in% center.draw) & 
#                                        (n.good==n.targets)),]$draw
      
#       if(length(keep.draws)>0){
#         # if there are any centers that are kept, recalculate
#         # total distance and alpha-levels
        
#         good.target.dist[draw %in% keep.draws,]$tot.dist = 
#           get.updating.dist(x=good.target.dist[draw %in% keep.draws,],
#                             update.names)
   
#         if(any(target.specs$update.alpha)){
#           good.target.dist[draw %in% keep.draws,]$alpha =
#             get.alpha(sim.p=good.sim.parm[draw %in% keep.draws,],
#                       sim.p.names=update.target.names,
#                       target.data=calib.targets,
#                       specs=target.specs[target.specs$update.alpha,])
#         }else{
#           good.target.dist$alpha = get.alpha(sim.p=good.sim.parm,
#                                              sim.p.names=calib.target.names,
#                                              target.data=calib.targets,
#                                              specs=target.specs)
#         }
        
#         } # if (length(keep.draws)>0)
      
#       if(length(remove.draws)>0){
#         if(print.status){
#           print(paste0("removing centers as good draws:",
#                        paste0(remove.draws,collapse=", ")))
#         }
#         good.parm.draws  = good.parm.draws[draw %in% remove.draws, 
#                                            names(good.parm.draws) := NA]
#         good.sim.parm    = good.sim.parm[draw %in% remove.draws, 
#                                          names(good.sim.parm) := NA]
#         good.target.dist = good.target.dist[draw %in% remove.draws, 
#                                             names(good.target.dist) := NA]
#         # place removed draws at the bottom
#         setorder(good.parm.draws,draw,iter,step,na.last=TRUE)
#         setorder(good.sim.parm,draw,iter,step,na.last=TRUE)
#         setorder(good.target.dist,draw,iter,step,na.last=TRUE)

#       }

#       # after replacing recalculated centers in good.* data tables,
#       # remove them from the iteration-specific data tables
#       parm.draws[step==(N.centers+1),(parm.names) := NA]
#       sim.parm[step==(N.centers+1),(sim.parm.names) := NA]
#       target.dist[step==(N.centers+1),c(dist.names,"tot.dist") := NA]
#       target.dist[step==(N.centers+1), n.good := as.integer(0)]
#       target.dist[step==(N.centers+1), alpha  := 0]

#       N.in = sum(good.target.dist$n.good==n.targets,na.rm=TRUE)

#     } # if(recalc.centers & iter>start.iter)

#     # save good draws, distances, and simulated parms
#     if(N.in.i>0){

#       if((iter==1) & (N.in.i>N.store)){
#         # keep the N.store best draws (largest alpha level & smallest distance)

#         setorder(target.dist,-alpha,tot.dist,na.last=TRUE)
#         add.draws = target.dist$draw[1:N.store]
#         setorder(target.dist,draw,na.last=TRUE)  # likely an unnecessary sort

#         good.target.dist <- target.dist[draw %in% add.draws,]
#         good.parm.draws  <- parm.draws[draw %in% add.draws,]
#         good.sim.parm    <- sim.parm[draw %in% add.draws,]

#         add.row.range = 1:N.store
        
#       }else{
        
#         if((N.in+N.in.i)>N.store){
#             # keep the best (N.store - N.in.i) draws (largest alpha level & smallest distance)
#             # and add the current N.in.i runs to the bottom
          
#             N.keep=N.store-N.in.i
#             setorder(good.target.dist,-alpha,tot.dist,na.last=TRUE)
#             keep.draws = good.target.dist$draw[1:N.keep]
#             setorder(good.target.dist,draw,na.last=TRUE)  # likely an unnecessary sort
#             good.parm.draws[1:N.keep,]  <- good.parm.draws[draw %in% keep.draws,]
#             good.sim.parm [1:N.keep,]   <- good.sim.parm[draw %in% keep.draws,]
        
#             add.row.range = (N.keep+1):(N.store)
            
#           }else{
  
#             add.row.range = (N.in+1):(N.in+N.in.i)
            
#           }

#           add.draws = target.dist[n.good==n.targets & step<(N.centers+1),]$draw
        
#           good.parm.draws[add.row.range,names(parm.draws)] <-
#             parm.draws[draw %in% add.draws,]
  
#           good.target.dist[add.row.range,] <-
#             target.dist[draw %in% add.draws,names(good.target.dist),with=FALSE]
  
#           good.sim.parm[add.row.range,] <-
#             sim.parm[draw %in% add.draws,names(good.sim.parm),with=FALSE]
          
#       }

#       if(any(target.specs$update.alpha)){ 
#         good.target.dist[add.row.range,]$alpha =
#           get.alpha(sim.p=good.sim.parm[add.row.range,],
#                     sim.p.names=update.target.names,
#                     target.data=calib.targets,
#                     specs=target.specs[target.specs$update.alpha,])
#       }else{
#         good.target.dist[add.row.range,]$alpha =
#           get.alpha(sim.p=good.sim.parm[add.row.range,],
#                     sim.p.names=calib.target.names,
#                     target.data=calib.targets,
#                     specs=target.specs)
#       }

#       good.target.dist[add.row.range,]$tot.dist =
#           get.updating.dist(x=good.target.dist[add.row.range,],
#                             target.names=update.names)
#     } # if(N.in.i>0)

#     N.in = sum(good.target.dist$n.good==n.targets,na.rm=TRUE)
#     } # !(continue.runs & iter==start.iter)

#     good.row.range = 1:N.in

#     if((N.in>=2*N.cov.points) & any(target.specs$update.alpha)){

#       ##########################################################################
#       # until target alpha-levels are reached, update alpha-levels every 
#       # 2*N.cov.points, reducing good points by half
#       ###########################################################################
      
#       # 1. Identify the point that yields best n.keep draws, 
#       #    based on \rho_{i\cdot} and  distance from targets still being updated.
 
#       good.target.dist$tot.dist = get.updating.dist(x=good.target.dist,
#                                                     target.names=update.names)
    
#       # ensure that we end up with at least N.cov.points. 
#       # This is needed if alpha.target differs across targets & there is tension in targets
      
#       keep.points = c(trunc( seq(1.0,2.0,0.1)*N.cov.points),N.in)

#       for(N.get in keep.points){
 
#         if(N.get<N.in){
          
#           alpha.draw = good.target.dist[good.row.range,][
#                           order(-alpha,tot.dist,na.last=TRUE)][N.get]$draw
  
#           # get new.alpha for all targets currently being updated, 
#           # based on simulated values at alpha.draw
#           target.specs[target.specs$update.alpha,]$new.alpha =
#                       update.alpha(sim.p=good.sim.parm[draw==alpha.draw,],
#                                   sim.p.names=update.target.names,
#                                   target.data=calib.targets,
#                                   specs=target.specs[target.specs$update.alpha,])
#         }else{
#           # return target.specs to original values
#           target.specs[target.specs$update.alpha,]$new.alpha =
#             target.specs[target.specs$update.alpha,]$alpha
          
#         }
        
#         if(any(target.specs[target.specs$update.alpha,]$new.alpha>
#                 target.specs[target.specs$update.alpha,]$alpha) | N.get==N.in){
#            # if any new.alpha>alpha

#           # don't go above specified target
#           target.specs[target.specs$update.alpha,]$new.alpha =
#             apply(target.specs[target.specs$update.alpha,
#                               c("new.alpha","alpha.target")], 1, FUN=min)

#          for(i in 1:length(update.names)){

#            # 2. update tolerance bounds
#             calib.targets[[update.names[i]]] =
#               get.bounds(x=calib.targets[[update.names[i]]],
#                          alpha.level=target.specs[
#                                   target.specs$data==update.names[i],]$new.alpha)
#           }
#             # 3. update distances (values<0 indicate out of tolerance bounds)
#             good.target.dist[good.row.range,] =
#               update.in.range(target.dist=good.target.dist[good.row.range,],
#                               sim.parm=good.sim.parm[good.row.range,],
#                               target.specs=target.specs[target.specs$update.alpha,],
#                               calib.targets=calib.targets)

#           # update n.good
#           good.target.dist$n.good = as.integer(0)
#           good.target.dist[good.row.range]$n.good = 
#             rowSums(good.target.dist[good.row.range,
#                                     (dist.names),with=FALSE]>=0,na.rm=TRUE)
          
#           N.in.new   = sum(good.target.dist$n.good==n.targets,na.rm=TRUE)
#           if(N.in.new>=N.cov.points) break
#         } # if any new.alpha>alpha
#         } # for(i.get in keep.points)
   
#         if(N.in.new>=N.cov.points & N.in.new<N.in){
#           # some points are dropped
          
#           target.specs[target.specs$update.alpha,]$alpha = 
#             target.specs[target.specs$update.alpha,]$new.alpha

#           # update N.in 
#           keep.draws = good.target.dist[n.good==n.targets,]$draw
#           N.in   = sum(good.target.dist$n.good==n.targets,na.rm=TRUE)
         
  
#           # update the names of targets with alpha-levels less than target alpha
#           update.names = target.specs$data[target.specs$alpha<
#                                              target.specs$alpha.target]

#           # if(N.in>N.cov.points & 
#           #    (sum(target.specs$alpha<target.specs$alpha.target)>=1)){
#           #   # if more than N.cov.points that meet the updated alpha-criteria &
#           #   # we are still updating tolerance intervals for some targets, then
#           #   # keep only the closest N.cov.points among them, based on nearness 
#           #   # to targets still being updated. needed b/c SEER data starts @ alpha=0
#           # 
#           #   good.target.dist[,tot.dist := NA]
#           #   good.target.dist[draw %in% keep.draws, 
#           #                    tot.dist := 
#           #                      get.updating.dist(x=good.target.dist[draw %in% 
#           #                                                           keep.draws,],
#           #                                        update.names)]
#           #   
#           #   setorder(good.target.dist,-n.good,-alpha,tot.dist,na.last=TRUE)
#           #   keep.draws = good.target.dist[1:N.cov.points,]$draw
#           #   setorder(good.target.dist,draw)
#           #   N.in=N.cov.points
#           #   
#           # } 
  
#           good.row.range = 1:N.in
#           good.target.dist[good.row.range,]<-good.target.dist[draw %in% 
#                                                                 keep.draws,]
#           good.parm.draws[good.row.range]  <-good.parm.draws[draw %in% keep.draws]
#           good.sim.parm[good.row.range]    <-good.sim.parm[draw %in% keep.draws]
  
#           if(N.in<N.store){ 
#             # clear unused rows in good.* data tables
#             blank.rows = (N.in+1):N.store
  
#             good.parm.draws[blank.rows,c("draw","step","iter") := as.integer(NA)]
#             good.parm.draws[blank.rows,c(parm.names,"scaled.dist","sample.wt") := 
#                               as.numeric(NA)]
#             good.parm.draws$seed[blank.rows]=""
  
#             good.sim.parm[blank.rows,c("draw","step","iter") := as.integer(NA)]
#             good.sim.parm[blank.rows,(sim.parm.names) := as.numeric(NA)]
  
#             good.target.dist[blank.rows,c("draw","step","iter") := as.integer(NA)]
#             good.target.dist[blank.rows,c(dist.names,"tot.dist","alpha") := 
#                                as.numeric(NA)]
#             good.target.dist$n.good[blank.rows]=as.integer(0)
            
#           } #  if(N.in<N.store)

#           if(print.status){
#             print(paste0("Updated alpha, new N.in=",N.in,
#                          " new alpha-levels: ",
#                          paste(target.specs[target.specs$update.alpha & !bias.info,]$data," ",
#                                format(
#                                      target.specs[
#                                        target.specs$update.alpha & !bias.info,]$alpha,
#                                      digits=2),
#                                collapse="; ")
#                   ))
#             }
          
#           # update.alpha: indicates if tolerance intervals are still being adjusted
#           target.specs$update.alpha = target.specs$alpha<target.specs$alpha.target 
#           update.target.names = get.update.targets(calib.target.names,
#                                                    target.specs$data[
#                                                      target.specs$update.alpha])
          
#         # if N.in.new<N.in - alpha levels are updated
#         }else{
#           if(print.status){
#             print(paste0("Unable to update alpha-levels, N.in=",N.in,
#                          " alpha-levels below targets: ",
#                          paste(target.specs[target.specs$update.alpha & !bias.info,]$data," ",
#                                format(
#                                  target.specs[
#                                    target.specs$update.alpha & !bias.info,]$alpha,
#                                  digits=2),
#                                collapse="; ")
#             ))
#           }
          
#         }

#     } 
#     # if((N.in>=2*N.cov.points) & any(target.specs$update.alpha))

#     # calculate weights if there are potentially enough points OR 
#     #                   we're at the end of iterations
#     # --------------------------------------------------------------------------
#     if((N.in>=N.post | !any(target.specs$update.alpha)) | 
#        (iter>=end.iter & N.in>0)){

#       good.parm.draws$sample.wt = 0
#       in.draws = good.target.dist[!is.na(draw),]$draw
#       if(continue.runs==TRUE & iter==start.iter & start.iter>1){
#       # if continuing runs, at the first iter use only previous mixture distns
#         m.file=paste0(prevruns.dir,"/",outfile.sampling)
#       }
#       else if(continue.runs==TRUE & start.iter==1){
#         m.file=paste0(output.directory,"/",outfile.sampling)
#       }
#       else {
#         m.file=paste0(c(prevruns.dir,output.directory),"/",outfile.sampling)
#       }

#       good.parm.draws[draw %in% in.draws,]$sample.wt =
#             get.weight(parms=good.parm.draws[draw %in% in.draws,],
#                        p.names=calib.parm.names,
#                        priors=parm.priors.df,
#                        mixture.file=m.file,
#                        N0)

#       # calculate effective sample size using Kish formula. Here sum(sample.wt)=1
#       ESS = 1/sum(good.parm.draws[draw %in% in.draws,]$sample.wt^2)    
#       if(print.status){
#         print(paste("    Effective Sample Size is",round(ESS,2)))
#       }
#     }

#     # Determine if there are enough points to quit
#     # ---------------------------------------------------------------------------
#     if(ESS>=N.post & !any(target.specs$update.alpha)){ # stop if ESS > N.post
#       if(print.status){
#         print(paste("Generated ",N.in,
#                     " in-range points. Effective Sample Size was ",
#                     round(ESS,2)," (Target was ",N.post,")"))
#       }
#       break
#     }

#     if(iter<end.iter){ # simulate new draws
#     ############################################################################
#     # Find the n.center in range draws with model predictions closest to targets
#     ############################################################################
 
#       # check for high-weight points, defined as $\theta_i$ with 
#       # $w_i> 10/\sum_{i=1}^{N_{(t+1)}}$, meaning that the maximum weight is 10 times 
#       # greater than expected for a simple random sample from in-range points
#       N.hiwt=0
#       center.draw.hiwt=NULL
#       if(!any(target.specs$update.alpha)){ # & N.in>500){
#         max.wt = max(good.parm.draws[draw %in% in.draws,]$sample.wt)
#         if(max.wt >= 10/N.in){
#           draw.order = setorder(good.parm.draws[good.row.range,],
#                                 -sample.wt,na.last=TRUE)$draw
#           N.hiwt = min(N.centers,length(draw.order))
#           center.draw.hiwt= draw.order[1:N.hiwt]
          
#           if(print.status){
#               print(paste0("adding samples around high weight points: ",
#                            paste0(center.draw.hiwt,collapse=", ")))
#             }
            
#         }
#       }

#       N.best.draw=0
#       center.draw.best=NULL
#       if(N.hiwt<N.centers){
#         draw.order = setorder(good.target.dist[good.row.range,],
#                               -alpha,tot.dist,na.last=TRUE)$draw
#         setorder(good.target.dist,draw,na.last=TRUE)
#         N.best.draw=min(N.in,(N.centers-N.hiwt))
#         center.draw.best=draw.order[1:N.best.draw]
#       }
#       num.centers=N.best.draw+N.hiwt
#       center.draw=c(center.draw.best,center.draw.hiwt)
      
#       center.next=  as.matrix(good.parm.draws[draw %in% 
#                                                 center.draw,parm.names,
#                                               with=FALSE])

#       ###########################################################################
#       # Sample B draws around these centers. Some draws may be out of range
#       ###########################################################################

#       # re-initialize parm.draws, sim.parms, & target dist before simulating
#       # draws for next iteration
#       #--------------------------------------------------------------------------
#       n.draw=num.centers*B + recalc.centers*num.centers
#       new.steps=rep(1:num.centers,each=B)
#       if(recalc.centers) new.steps = c(new.steps,rep((N.centers+1),num.centers))
#       new.draws = (total.draws+1):(total.draws+n.draw)

#       parm.draws$draw = target.dist$draw = sim.parm$draw=as.integer(NA)
#       parm.draws$step = target.dist$step = sim.parm$step=as.integer(NA)

#       parm.draws$iter = target.dist$iter = sim.parm$iter = iter+1

#       parm.draws$draw[1:n.draw] = target.dist$draw[1:n.draw] =
#         sim.parm$draw[1:n.draw] = new.draws
#       parm.draws$step[1:n.draw] = target.dist$step[1:n.draw] = 
#         sim.parm$step[1:n.draw] = as.integer(new.steps)

#       parm.draws[,c(parm.names,"scaled.dist","sample.wt") := as.numeric(NA)]
#       parm.draws$seed=""
#       s <- .Random.seed
#       parm.draws[1:n.draw, seed := get.random.seed.strings(s,n.draw)]

#       sim.parm[,(sim.parm.names) := as.numeric(NA)]

#       target.dist[,c(dist.names,"tot.dist","alpha") := as.numeric(NA)]
#       target.dist$n.good=as.integer(0)

#       # simulate new draws
#       #--------------------------------------------------------------------------
      
#       # set any fixed parameter values (specified in parm.priors)
#       if(length(fixed.parm.names)>0){
#         names(fixed.parm.values)=fixed.parm.names
#         set(parm.draws,j=fixed.parm.names,value=as.data.table(t(fixed.parm.values)))
#       }
      
#       if(N.in<N.cov.points){ 
#         # sample from an independent normal if there are too few good points
#         sd.next=matrix(0.5*parm.priors.df$sd,
#                        ncol=n.calib.parms,nrow=num.centers,byrow=TRUE)
#         parm.draws[1:(num.centers*B),calib.parm.names] = draw.parms(n.add=B,
#                                                      mu=center.next[,calib.parm.names],
#                                                      sigma=sd.next,
#                                                      parm.priors=parm.priors.df,
#                                                      parm.names=calib.parm.names,
#                                                      calib.targets=calib.targets)

#         if(recalc.centers){ 
#           parm.draws[step==(N.centers+1),(parm.names)] = 
#             as.data.table(center.next)
#           parm.draws[step==(N.centers+1),]$draw = center.draw
#           sim.parm[step==(N.centers+1),]$draw = center.draw
#           target.dist[step==(N.centers+1),]$draw = center.draw
#         }
#         x = parm.draws[1:n.draw,c("iter","step"),with=FALSE]
#         x$in.range = get.in.range(parm.draws[1:n.draw,calib.parm.names,with=FALSE],
#                                   parm.priors.df,
#                                   calib.parm.names)

#         setkey(x,iter,step)
#         B.in=x[step<=N.centers,list(B.in = sum(in.range,na.rm=TRUE)),
#                               by = .(iter,step)]

#         sampling.output = get.sampling.output(iter=iter+1,
#                                               mu=center.next[,calib.parm.names],
#                                               sd=sd.next,
#                                               center=center.draw,
#                                               B=B.in,
#                                               parm.names=calib.parm.names)

#         write.table(sampling.output[,c("iter","step","center","B.in",
#                                        "parm",calib.parm.names),
#                                     with=FALSE],
#                     file=paste0(output.directory,"/",outfile.sampling),sep=",",
#                     append=f.append,
#                     col.names=!f.append,
#                     row.names=FALSE)
#         f.append=TRUE
#     }else{
#       # sample MVN points around centers if there are enough points to 
#       # estimate the cov matrix
#       #--------------------------------------------------------------------------
#       n.use = min(N.in,N.cov.points) 
#       sample.mean = as.data.frame(center.next)

#       if(N.in<=N.cov.points){ 
#         var.data=good.parm.draws[1:n.use,calib.parm.names,with=FALSE]
#         sample.cov = get.parm.cov(var.data)
#         if(sample.cov==-1) return()
#         if(any(diag(sample.cov)==0)){
#           # this occurs when adding a new parameter: it is set to default for all prior draws
#           is.zero=(diag(sample.cov)==0)
#           sd.next=0.5*parm.priors.df$sd
#           sd.next[!is.zero] = 0
#           diag(sample.cov) <- diag(sample.cov)+(sd.next^2)
#         }
#       }
      
#       for(i.center in 1:num.centers){
#         sample.mean.i = as.vector(
#                           unlist(sample.mean[i.center,
#                                              which(names(sample.mean) %in% 
#                                                      calib.parm.names)]))
#         draw.rows = ((i.center-1)*B + 1):(i.center*B)

#         if(N.in>=N.cov.points){ # use different var-cov matrices for each center
#           # Find the n.use closest draws to each center point,
#           #------------------------------------------------------------------------
#           good.parm.draws$scaled.dist = Inf
#           good.parm.draws$scaled.dist[1:N.in] = 
#               get.dist(p.draws=good.parm.draws[1:N.in,],
#                        p.names=parm.names,
#                        mu=as.vector(center.next[i.center,calib.parm.names]),
#                        sd=parm.priors.df$sd)
#           setorder(good.parm.draws,scaled.dist,na.last=TRUE)
#           var.data=good.parm.draws[1:n.use,calib.parm.names,with=FALSE]
#           sample.cov = get.parm.cov(var.data)
#           if(sample.cov==-1) return()
#           if(any(diag(sample.cov)==0)){
#             # this occurs when adding a new parameter: it is set to default for all prior draws
#             is.zero=(diag(sample.cov)==0)
#             sd.next=0.5*parm.priors.df$sd
#             sd.next[!is.zero] = 0
#             diag(sample.cov) <- diag(sample.cov)+(sd.next^2)
#           }
#         }
        
#         # Draw B new parm values usign an MVN draw...............................

#         # assign fixed parameters
#         parm.draws[draw.rows,(fixed.parm.names) := as.list(fixed.parm.values)]
        
#         # simulate B random draws of calibrated parameters
#         if(abs(sum(sample.cov) - sum(diag(sample.cov)))<1e-10){
#           parm.draws[draw.rows,(calib.parm.names) := draw.parms(n.add=B,
#                                                               mu=as.matrix(t(sample.mean.i)),
#                                                               sigma=as.matrix(t(diag(sample.cov))),
#                                                               parm.priors=parm.priors.df,
#                                                               parm.names=calib.parm.names,
#                                                               calib.targets)]
          
#         }else{
#           x=get.B.draws(B, 
#                         inflate=sample.inflate,
#                         center=sample.mean.i, 
#                         cov=sample.cov,
#                         priors=parm.priors.df,
#                         p.names=calib.parm.names)
#           if(is.null(x)){
#             print(paste("iteration=",iter,"center=",i.center))
#             return()
#           }
#           parm.draws[draw.rows,(calib.parm.names) := x]
#         }
#         sample.mean.i =setnames(as.data.frame(t(sample.mean.i)),
#                                 calib.parm.names)
#         sample.cov = setnames(as.data.frame(sample.cov),
#                               calib.parm.names)

#         sampling.output= data.table(rbind(sample.mean.i,sample.cov))
#         sampling.output$iter = iter+1
#         sampling.output$step = i.center
#         sampling.output$center=center.draw[i.center]
#         sampling.output$parm = 0:(length(calib.parm.names))
#         B.in = sum(get.in.range(parm.draws[draw.rows,],
#                                 parm.priors.df,calib.parm.names))
#         sampling.output$B.in = B.in
#         if(B.in==0) print(paste("*** warning: B.in=",B.in,
#                                 "for iter=",iter+1,"and center=",i.center))
        
#         setcolorder(sampling.output,
#                     c("iter","step","center","B.in","parm",calib.parm.names))

#         write.table(sampling.output[,c("iter","step","center","B.in",
#                                        "parm",calib.parm.names),
#                                     with=FALSE],
#                     file=paste0(output.directory,"/",outfile.sampling),sep=",",
#                     append=f.append,
#                     col.names=!f.append,
#                     row.names=FALSE)
#         f.append=TRUE

#       } # loop over centers

#       if(recalc.centers){
#         parm.draws[step==(N.centers+1),(parm.names)] = as.data.table(center.next)
#         parm.draws[step==(N.centers+1),]$draw = center.draw
#         sim.parm[step==(N.centers+1),]$draw = center.draw
#         target.dist[step==(N.centers+1),]$draw = center.draw
#       }

#       # put good.* data tables in draw order 
#       # (though all references are by draw %in% avoid potential problems)
#       setorder(good.parm.draws,draw,na.last = TRUE)
#       setorder(good.sim.parm,draw,na.last = TRUE)
#       setorder(good.target.dist,draw,na.last = TRUE)
#     } # !(N.in<N.cov.points)

#     if(continue.runs==TRUE & iter==start.iter) f.append=FALSE

#     } # if(iter<end.iter)
#   } # for(iter in start.iter:end.iter)


#   
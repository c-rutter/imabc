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
                    output.directory=".",
                    LHS=TRUE,
                    continue.runs=FALSE,
                    reeval=FALSE,
                    prevruns.dir=".",
                    print.status=FALSE,
                    recalc.centers=TRUE){


## Handle priors

# TO DO: ### add the possibility of quantile function

fn1 <- x[1] + x[2] + rnorm(1,0,0.1)
fn2 <- x[1] * x[2] + rnorm(1,0,0.1)


prior1 = c("unif",0,1)
prior2 = c("qnormal",1,2)


parm.priors <- add_prior(fn1)
parm.priors <- add_prior(fn2)


parm.names <- c("parm1", "parm2")

#   calib.parm.names <- unlist(parm.priors.df$parm[parm.priors.df$sd>0])
#   n.calib.parms <- length(calib.parm.names)  


# TASK-SPECIFIC NOTES

# input file priors -> chris helper function
# what functional form should we accept,
# hard-coding of parm functions



## Handle targets
t1=1.5
t2=0.5

parm.targets <- add_targets(t1)
parm.targets <- add_targets(t2)










## Handle tolerance intervals/stopping criteria

# TO DO: ### add tolerance intervals to replace alpha levels, remove alpha as the stopping condition

# TO DO: ### have sampling distributions and density distributions separately






####
# Init draws
####

start.iter=1
end.iter=max.iter
total.draws=0
prevruns.dir=NULL
n.draw=N0   # first iteration only, then set to n.center*B
N.in=0
N.use=0

# Initiate data frames for draws, target distributions and simulation parameters

good.parm.draws <- init.draws(N.store, c(parm.names,"scaled.dist","sample.wt"))

good.target.dist=init.sims(N.store,c(dist.names,"tot.dist"))

# alpha no longer useful, figure out how to add bounds
good.target.dist$alpha=as.numeric(0)
good.target.dist$n.good=as.integer(0)

setcolorder(good.target.dist,c("iter","draw","step",dist.names, "tot.dist","alpha","n.good"))

# good.sim.parm=init.sims(N.store,sim.parm.names)

# take non-missing names
good.sim.parm=init.sims(N.store,sim.parm.names[1:44])



###############################################################################
# initialize data.tables and matrices used for calculations
###############################################################################


num.rows=max(n.draw,N.centers*B) + (recalc.centers)*N.centers
parm.draws <- init.draws(num.rows,c(parm.names,"scaled.dist","sample.wt"))
parm.draws$step=0

target.dist <- init.sims(num.rows,c(dist.names,"tot.dist"))
target.dist$alpha = as.numeric(0)
target.dist$n.good=as.integer(0)
target.dist$step=0

sim.parm <- init.sims(num.rows,sim.parm.names[1:44])
sim.parm$step=0

setcolorder(target.dist, c("iter","draw","step",dist.names,"tot.dist","alpha","n.good"))




# set seed values ...........................................................
seed=1234
set.seed(seed)
rngKind = "L'Ecuyer-CMRG"
RNGkind(kind = rngKind, normal.kind = NULL)


#............................................................................


###############################################################################
# if starting a new run, draw parameters from prior distributions
###############################################################################

parm.draws$iter = target.dist$iter = sim.parm$iter = 1

draws=1:N0
parm.draws$draw[draws] = target.dist$draw[draws] =  sim.parm$draw[draws] = draws


s <- .Random.seed
parm.draws[draws, seed := get.random.seed.strings(s,n.draw)]

LHS=TRUE

# Latin Hypercube
if(LHS){
   u.draws=randomLHS(N0,n.calib.parms)
}else{
   u.draws=matrix(runif(N0*n.calib.parms),nrow=n.parms,ncol=N0)
}


# set any fixed parameter values (specified in parm.priors)
if(length(fixed.parm.names)>0){
 names(fixed.parm.values)=fixed.parm.names
 set(parm.draws,j=fixed.parm.names,value=as.data.table(t(fixed.parm.values)))
}

# draw calibrated parameters from priors: consider making draw.parms more general....

# TE notes: make draw parms more general


#### MAIN LOOP ####


}





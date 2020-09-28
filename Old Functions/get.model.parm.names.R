get.model.parm.names <- function(){
  parm.names =  c("ar.mean","ar.fem","ar.sd",
                  "ar.20to49","ar.50to59","ar.60to69","ar.70plus",
                  "growth.colon.beta1","growth.colon.beta2",
                  "growth.rectum.beta1","growth.rectum.beta2",
                  "richards.power",
                  "trans.prob.intercept","trans.prob.fem",
                  "trans.prob.rectum","trans.prob.fem.rectum",
                  "trans.prob.age","trans.prob.agesq",
                  "trans.prob.sd",
                  "mst.shape","mst.scale","mst.rectum",
                  "rf.proximal.shift","rf.adeno.incidence","rf.adeno.growth","rf.preclin.trans",
                  "rf.sojourn.time","rf.oc.survival")
  return(parm.names)
}
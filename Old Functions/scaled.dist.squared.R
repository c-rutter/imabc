# calculate distance from model sim to target
scaled.dist.squared = function(obs,sim){
  (obs-sim)^2/obs^2
}

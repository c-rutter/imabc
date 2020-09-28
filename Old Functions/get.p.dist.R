get.p.dist = function(obs,sim){
  sqrt(
    sum(
      scaled.dist.squared(obs,sim)
    )
  )
}

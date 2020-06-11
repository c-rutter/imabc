get.simparm.names <- function(target.specs){
  
  # calibrated values must be listed first, and the order needs to correspond to 
  # the order provided in calib.targets (and also in SimTargets.R)
  
  simparm.names=rep("",sum(target.specs$n.ret))
  j=1
  for(i in 1:(nrow(target.specs))){
    name.cols=j:(j+target.specs$n.ret[i]-1)
    j=j+target.specs$n.ret[i]
    if(target.specs$data[i]=="Pickhardt"){
      simparm.names[name.cols]= 
        paste0('Pickhardt.',c('lt6mm','6to10mm','gt10mm','mu','pca'))}
    if(target.specs$data[i]=="Imperiale"){
      simparm.names[name.cols]='Imperiale'}
    if(grepl("UKFSS",target.specs$data[i])){
      temp.name=NULL
      if(grepl(".overall",target.specs$data[i])) temp.name="UKFSS.overall.p.dist.ca"
      simparm.names[name.cols]=c(temp.name,
        paste0(paste0(target.specs$data[i],".p.dist"),paste0(rep(c('.ca','.adeno','.3plus','.hirisk'),each=2),
                                    rep(c('.m','.f'),times=3))))
    }
    if(target.specs$data[i]=="Lieberman"){
      simparm.names[name.cols]=
        paste0('Lieberman.',c('pca.6to9','pca.gt9','pca.lt6','adeno.prev','10to14','15to20','20to24','25plus'))}
    if(target.specs$data[i]=="Church.Odom"|target.specs$data[i]=="Church"|target.specs$data[i]=="Odom"){
      simparm.names[name.cols]=
        paste0(paste0(target.specs$data[i],'.pCA.'),c('6to10','gt10','1to6'))}
    if(target.specs$data[i]=="Corley"){
      ages=c('50','55','60','65','70','75')
      locs=c('distal','proximal','distandprox')
      simparm.names[name.cols]=
        paste0('Corley.',c(paste0(ages,'m'),
                           paste0(ages,'f'),
                           paste0(locs,'.m'),
                           paste0(locs,'.f'),
                           paste0(rep(locs,length(ages)),'.',rep(ages,each=length(locs)))))
    }
    if(target.specs$data[i]=="Corley"){
      ages=c('50','55','60','65','70','75')
      locs=c('distal','proximal','distandprox')
      simparm.names[name.cols]=
        paste0('Corley.',c(paste0(ages,'m'),
                           paste0(ages,'f'),
                           paste0(locs,'.m'),
                           paste0(locs,'.f'),
                           paste0(rep(locs,length(ages)),'.',rep(ages,each=length(locs)))))
    }
    
    if(grepl("SEER",target.specs$data[i])){
      ages=c('20','50','60','70','85')
      simparm.names[name.cols]=
        paste0(target.specs$data[i],'.',
                       c(paste0('colon.fem.',ages),
                         paste0('colon.male.',ages),
                         paste0('rectal.fem.',ages),
                         paste0('rectal.male.',ages)))
    }
  }
  return(simparm.names)
}
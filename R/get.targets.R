get.targets <- function(target.specs,
                        target.file = "calibrationtargets.xlsx"
                        ){

  minsize=6 # minimum size category, applied to lieberman, odom, and church
  require(readxl,quietly=T,warn.conflicts=F)

  calib.targets <- vector("list", nrow(target.specs)+1)
  
  Range.p10mmIn10yrs=as.data.frame(read_excel(target.file, sheet = "Range.p10mmIn10yrs"))
  names(Range.p10mmIn10yrs)=c('min','max')
  
  calib.targets[[1]]=list(Range.p10mmIn10yrs)

  for(i in 1:nrow(target.specs)){
    X = as.data.frame(read_excel(target.file, sheet = target.specs$data[i]))

    if(grepl('SEER',target.specs$data[i])){
      # SEER data of any type: a particular form is expected

      Y = as.data.frame(
                matrix(data=c(rep(X$male,2),
                              rep(X$min.age,2),
                              rep(X$max.age,2),
                              rep(X$n,2),
                              rep(c(0,1),each=10),
                                c(X$colon.ca,X$rectal.ca)),
                          byrow=FALSE,ncol=6))
      names(Y)=c('male','min.age','max.age','n','rectal','n.ca')
      Y = mutate(Y, p= n.ca /n)
      
    }
    
    if(grepl("Corley",target.specs$data[i])){
     # adenoma prevalence data: Corley and Corley.bias
     #-----------------------------------------------------------------------------------------
      
      Y = mutate(X, n.adeno=round(n*p))
      
    }
    
    if(target.specs$data[i]=="Pickhardt"){
    # ------------------------------------------------------------------------------------------
    # Pickhardt CTC+Colonoscopy
      
      Y = X[X$min.size>0,] # for the moment, just use size distribution
      Y = mutate(Y, p = n.adeno / n)
  
      Y$min.size=NULL
      Y$n.adeno=NULL
      setcolorder(Y,c('p','n'))
  
    }
    
    if(target.specs$data[i]=="Imperiale"){
    #------------------------------------------------------------------------------------------
    # Imperiale: screen detected cancers
      
      Y = mutate(X, p = n.cancer / n)
      Y$n.cancer=NULL
      setcolorder(Y,c('p','n'))
  
    }
    
    if(grepl("UKFSS",target.specs$data[i])){
      #------------------------------------------------------------------------------------------
      # UKFSS: screen detected cancers

      if(grepl(".bysex",target.specs$data[i])){
        # calibrate to screen detction in men and women
        Y = mutate(X, p = n.distal.ca / n)
        Y$n.distal.ca=NULL
        setcolorder(Y,c('male','p','n'))
      }
      if(grepl(".overall",target.specs$data[i])){
        # calibrate to just overall screen detection
        n = sum(X$n)
        p = sum(X$n.distal.ca)/n
        Y=data.frame(p= p, n=n)
      }
    }
    
    if( target.specs$data[i] %in% c("Lieberman","Church","Odom") ){
    #---------------------------------------------------------------------------------------------------
    # cancer by lesion size
    # Lieberman: to get total number of adenomas, from Table 3, add total advanced + tubular and subtract serrated

      Y=mutate(X,p=n.cancer/n)
      Y=Y[Y$min.size>=minsize,]

    }

    Y = get.bounds(x=Y,alpha.level=target.specs[i,'alpha'])
 
    calib.targets[[i+1]] = Y

  }

  names(calib.targets) = c("Range.p10mmIn10yrs",target.specs$data)
 
  return(calib.targets)
}
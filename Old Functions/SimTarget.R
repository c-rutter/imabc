SimTarget <- function(parms=parms.now,
                      parm.names,
                      specs=subset(target,data="Pickhardt"),
                      target.data=NULL,
                      mix.n=1,
                      mix.distn=1,
                      target.age.distn.name="uniform",
                      target.age.distn.parms.fem = c(40,40.99),
                      target.age.distn.parms.male = c(40,40.99)){

  # parms: CRCSPIN parameter vector.
  #         parameter names are not passed: vector is expected in a particular order
  #         as provided in the calling routine ABCloop or CRCSPIN.TargetSims.R
  # specs:  vector containg run information:
  #         dataset (character), number of embedded simulations (m), index date, %male,
  #         number of returns, tolerance/percent error allowed in return
  # target.data: if null, then the data-parameter vector is returned
  #         if provided, then the likelihood is calculated and
  #                      both the likelihood and the parameter vector are returned
  # mix.n:  refers to the number of  mixture distributions used to simulate
  #         the age of the population at index date. mix.n is in (1,2,3)
  #         the Lin target is the only one with mix.n=3 and this is used for a
  #         hard-coded exception, with %male changed for the 3rd mixture population.
  # mix.distn: proportion of population simulated from each mixture - should sum to 1
  # target.age.distn.name: length(mix.n), specifies a uniform or
  #                                                   truncated normal distribution
  # target.age.distn.parms.fem:  2*number of uniform distns (lower & upper limits) +
  #                              4*number of TN distns (mean, sd, lower & upper  limits)
  # target.age.distn.parms.male:  2*number of uniform distns (lower & upper limits) +
  #                               4*number of TN distns (mean, sd, lower & upper limits)
  #                return
  #    Target      length  return values
  #    Pickhardt      5    size.pct, mu.hat, p.ca
  #    Imperiale      1    p.ca
  #    UKFSS.bysex    9    1 target: p.distal.ca
  #    UKFSS.bysex    8    2 targets: p.distal.ca for men then women
  #                        6 auxillary outcomes distal adenoma prev (m/f), distal prev of 3+ adenoms (m/f),
  #                         distal prev of high risk findings (3+ adeno or any adeno >=10mm, m/f).
  #    Lin           12    v.p.hat (vector, arranged by size then age)
  #    Lieberman      8    size.pct: P(CA) by size largest [6,10), [10,50.1),  [1,6), adenoma prev, A
  #                        Adenoma size distribution | >10mm, [10,15),[15,20),[20,25),[25,50.1)
  #    Church.Odom    3    size.pct: P(CA) by size [6,10), [10,50.1), [1,6)
  #    Corley        36    p.long, for men then women, adenoma prevalence by age group:
  #                                     [50,55), [55,60), [60,65), [65,70), [70,75), [75,90)
  #                        24 other auxiliary outputs
  #    SEER          16    p.hat, annual incidence for ages: [20,50), [50,60), [60,70), [70,84)
  #                        grouped by male colon cancer, female colon cancer, male rectal cancer, female rectal cancer
  #

  ret.len = specs$n.ret
  if(is.null(target.data)){return(rep(Inf,specs$n.ret))}

  for(j in 1:mix.n){
    distn.name=target.age.distn.name[j]
    if(distn.name=="uniform") n.d = 2
    if(distn.name=="truncated normal") n.d=4
    if(j==1){j.row=0
             start.id=1}
    t.age.distn.parms.fem =  target.age.distn.parms.fem[(j.row+1):(j.row+n.d)]
    t.age.distn.parms.male =  target.age.distn.parms.male[(j.row+1):(j.row+n.d)]
    j.row=j.row+n.d
    m = ceiling(specs$m*mix.distn[j])
    p.male=specs$pct.male

    x <- CRCSPIN( crc.free = FALSE, # crc.free=TRUE option needs to be updated
                  start.id = start.id,
                  index.date = specs$date,
                  block.size = m,
                  pct.male = p.male,
                  parms = parms,
                  names.parms = parm.names,
                  age.distn.name = distn.name,
                  age.distn.parms.fem =  t.age.distn.parms.fem,
                  age.distn.parms.male =  t.age.distn.parms.male,
                  crc.surv.parms.file = "model_inputs/crc.surv.parms.allrace.csv",
                  male.allmort.file = "model_inputs/maleallcausemortalityrates.csv",
                  female.allmort.file = "model_inputs/femaleallcausemortalityrates.csv",
                  size.distn.file = "model_inputs/sizegivenstage.csv",
                  max.years = 1)
    if(j==1){  person <- x[[1]]
               adenoma <- x[[2]]
    }else{     person <- rbind(person,x[[1]])
               adenoma <- rbind(adenoma,x[[2]])
    }
    start.id=start.id+m
  } # for(j in 1:mix.n)

  # throw out people with clinical crc at index...
  pid.with.crc =get.pid.with.crc(adenoma,index.date=specs$date)
  person = subset(person, !(p.id %in% pid.with.crc))
  adenoma = subset(adenoma, p.id %in% person$p.id)
  n.pop=nrow(person)

  if(!(grepl("SEER",specs$data)) & nrow(adenoma)>0){
    
    # keep only adenomas initiated on or before the index date
    adenoma <- subset(adenoma,date.init<=specs$date)
    
    # keep only detected adenomas
    # use size-based sensitivity function for colonoscopy
    # cspy.sens.fn takes arguments: size, cancer, location, reach
    cspy.sens.fn = SensFnSetup(sens.parms=list(cutpoints=NULL,
                                               sensitivites=NULL,
                                               coeff=c(0.40,0.055,-0.001)),
                               person.level=FALSE)
    adenoma$size=get.adenoma.size(adeno=adenoma,screening.date=specs$date,p=parms[parm.names=="richards.power"] )
    
    adenoma$cancer = 0
    adenoma$cancer[adenoma$date.trans<=specs$date]=1
    adenoma$sens = cspy.sens.fn(size=adenoma$size,cancer=adenoma$cancer,location=adenoma$location,reach=6)

    # this is incorporated in SensFnSetup
    # adenoma$sens[adenoma$size>15]=0.99    #  1% miss rate, adenomas 15-30mm
    # adenoma$sens[adenoma$size>30]=0.995   #  0.5% miss rate, adenomas 30-40mm
    # adenoma$sens[adenoma$size>40]=0.999   #  0.1% miss rate, adenomas>40mm
 
    if(specs$data=="Pickhardt"){
      # assume 2 correlated tests
      adenoma$sens = 1- ( (1-adenoma$sens)^1.25 )
     }
    
    adenoma$det=rbinom(nrow(adenoma),1,adenoma$sens)

    # keep only detected adenomas
    adenoma <- subset(adenoma,det==1)
  }

  if(specs$data=="Pickhardt"){

    size.cat <- cut(adenoma$size,
                    breaks = c(1,5.5,9.5,50.1), # <=5, 6-9, 10+
                    right=FALSE)
    size.pct = table(size.cat)/sum(table(size.cat))
    # names(size.pct) =  c("[1,5.5)","[5.5,9.5)","[9.5,50.1)"

    # additional (non-calibrated) info
    # average number of adenomas per person at specs$date
    mu.hat=nrow(adenoma)/n.pop
    
    # screen detected cancers from the Pickhardt study
    p.ca=sum(adenoma$cancer)/n.pop
    return(c(size.pct,mu.hat,p.ca))
  
  } # if(specs$data=="Pickhardt")

  if(specs$data=="Imperiale"){
    
    # keep only preclinical cancers that transition on or before the index date
    adenoma <- subset(adenoma,cancer==1)

    # number with at least one detected preclinical cancer present at index
    p.hat=length(unique(adenoma$p.id))/n.pop

    return(p.hat)
    
  } # if(specs$data=="Imperiale")

  if(grepl("UKFSS",specs$data)){

    # keep only distal lesions / located in the sigmoid or rectum
    adenoma <- subset(adenoma,location<3) # 1 = rectum, 2=sigmoid 3=descending
   
    if(nrow(adenoma)>0){
      
    adenoma = as.data.table(adenoma)
    sum.adeno = adenoma[, .(n.adeno = .N, biggest=max(size), ca=max(cancer)), by = c("p.id","fem")]
    sum.adeno[,any.adeno := 1*(n.adeno>0)]
    sum.adeno[,gt2 := 1*(n.adeno>2)]
    sum.adeno[,hi.risk := 1*(biggest>10 | gt2)]
    
    mf.pop=c( nrow(person[person$fem==0,]), nrow(person[person$fem==1,]))
    
    res=rep(0,ret.len)
    # order of all results are male, female
    
    denom=rep(mf.pop,4)
  
    scr.det.overall=NULL
    if(grepl(".overall",specs$data)){
      scr.det.overall=nrow(sum.adeno[ca==1,])
      denom=c(sum(mf.pop),denom)
    }
    res=c(scr.det.overall,                       # distal screen detected cancer: overall
          nrow(sum.adeno[fem==0 & ca==1,]),      #   men
          nrow(sum.adeno[fem==1 & ca==1,]),      #   women
          nrow(sum.adeno[fem==0,]),              # distal adenoma prevalence
          nrow(sum.adeno[fem==1,]),      
          nrow(sum.adeno[fem==0 & n.adeno>2,]),  # prevalence 3 or more distal adenomas
          nrow(sum.adeno[fem==1 & n.adeno>2,]),            
          nrow(sum.adeno[fem==0 & (n.adeno>2 | biggest>=10),]), # prevalence hi risk (large or 3 or more distal) adenomas
          nrow(sum.adeno[fem==0 & (n.adeno>2 | biggest>=10),])) / denom
    
    return(res)
       
    }else{
      return(rep(NaN,ret.len))
    }
    
  } # if(grepl("UKFSS",specs$data)
  
    
  if(specs$data=="Lieberman"){

    if(nrow(adenoma)==0){
      return(rep(NaN,ret.len))
    }

    # identify any detected cancers
    cancer.pid=unique(subset(adenoma,cancer==1)$p.id)

    # identify the largest lesion within person
    adenoma.largest=get.biggest(adenoma)

    # cancer and size are person-level, cancer is not necessarily in the largest adenoma
    adenoma.largest$cancer=0
    adenoma.largest$cancer[adenoma.largest$p.id %in% cancer.pid]=1

    adenoma.largest$size.cat <- cut(adenoma.largest$size,
                                    breaks = c(1,5.5,9.5,50.1), # 1-5, 6-9, 10+ 
                                    right=FALSE)                 

    n.size = table(adenoma.largest$size.cat)

    if(length(cancer.pid)>0){
      n.ca = table(adenoma.largest$size.cat[adenoma.largest$cancer==1])
    } else {
      n.ca = rep(0,3)
    }
    size.pct=n.ca/n.size
    size.pct=size.pct[c('[5.5,9.5)','[9.5,50.1)','[1,5.5)')] # re-order, non-calibrated last
    
    # additional (non-calibrated) info
    # adenoma prevalence
    p.adeno=length(unique(adenoma$p.id))/n.pop
    
    # size distribution among adenomas>10mm
    size.cat <- cut(adenoma$size[adenoma$size>=9.5],
                    breaks = c(9.5,14.5,19.5,24.5,50.1), # 10-14, 15-20, 20-24, >=24
                    right=FALSE)

    pct.10plus = table(size.cat)/sum(table(size.cat))

    return(c(size.pct,p.adeno,pct.10plus))
  } # if(specs$data=="Lieberman")

  if(specs$data %in% c("Church.Odom","Church","Odom")){

    size.cat <- cut(adenoma$size,
                    breaks = c(1,6,10,50.1), #<6,6-10, >10
                    right=FALSE)
    n.size = table(size.cat)
    n.ca = table(size.cat[adenoma$cancer==1])

    size.pct = n.ca/n.size
    size.pct=size.pct[c('[6,10)','[10,50.1)','[1,6)')] # re-order, non-calibrated last
    
    return(c(size.pct))
    
  } #if(specs$data=="Church.Odom")

  if(specs$data=="Corley"){
    # prevalence: select one obs per person
    p.id.adenoma <- unique(adenoma$p.id)

    person$adeno.index=0
    person$adeno.index[person$p.id %in% p.id.adenoma]=1

    age.cat.n <- cut(person$age.index,
                     breaks = c(50,55,60,65,70,75,90),
                     right=FALSE)

    age.cat.adeno <- cut(person$age.index[person$adeno.index==1],
                         breaks = c(50,55,60,65,70,75,90),
                         right=FALSE)
    p.adeno = table(age.cat.adeno,person$fem[person$adeno.index==1])/
      table(age.cat.n,person$fem)
    
    # list of adenoma prevalences by age, grouped by sex (male first, then female)
      p.long = c(p.adeno[,1],p.adeno[,2])
   
    # auxilliary output: prevalence by location
      

      sex.n = table(person$fem)
      x = adenoma  %>% group_by(p.id,fem) %>% summarise(p.by.loc = proxdist(location))
      p.loc.sex = matrix(table(x$fem,x$p.by.loc)/matrix(sex.n, nrow=2, ncol=3, byrow=FALSE),
                          nrow=1,byrow=FALSE)

    x <- merge(adenoma,person[person$p.id %in% p.id.adenoma,c('p.id','age.index')],by="p.id")
    x$age.cat = cut(x$age.index,
                    breaks = c(50,55,60,65,70,75,90),
                    right=FALSE)
    x = x  %>% group_by(p.id,age.cat) %>% summarise(p.by.loc = proxdist(location))
    p.loc.age = matrix((table(x$age.cat,x$p.by.loc) /
                          matrix(table(age.cat.n),nrow=6,ncol=3,byrow=FALSE)),
                       nrow=1,byrow=FALSE)

      return(c(p.long,p.loc.sex,p.loc.age))
  } #if(specs$data=="Corley")

  
  if(grepl("SEER",specs$data)){

    # count clinical cancers that transition between index date and next year
    end.date = specs$date
    year(end.date) <- year(end.date) + 1
    adenoma <-subset(adenoma,date.clindet<=end.date) 
    age.cat <- cut(adenoma$age.clindet,
                   breaks = c(20,50,60,70,85,100),
                   right=FALSE)
    age.denom <- cut(person$age.index,
                     breaks = c(20,50,60,70,85,100),
                     right=FALSE)
    pct.cf = table(age.cat[adenoma$fem==1 & adenoma$location>1]) / 
             table(age.denom[person$fem==1])
    
    pct.cm = table(age.cat[adenoma$fem==0 & adenoma$location>1]) / 
             table(age.denom[person$fem==0])
    
    pct.rf = table(age.cat[adenoma$fem==1 & adenoma$location==1]) / 
             table(age.denom[person$fem==1])
    
    pct.rm = table(age.cat[adenoma$fem==0 & adenoma$location==1]) / 
             table(age.denom[person$fem==0])
    
    p.hat = c(pct.cf,pct.cm,pct.rf,pct.rm)
    
    return(c(p.hat))
    
  } # if(grepl("SEER",specs$data))
  
  return(NULL)
}


readme.txt

user-defined functions: try to whittle this down:
  get.model.parm.names.R:   define the function to create all of the simulated outcomes 
  get.simparm.names.R:      create all of the simulated outcomes 
  get.calib.target.names.R  identify simulated outcomes that are calibration targets
  get.targets.R             read in calibration targets and calculate initial tolerance intervals
  get.updating.dist.R       calculate the distance to targets still being updatedm refers to  targets by name
  update.in.range.R         refers to specific calibation targets
  draw.parms.R              initial draw of parameters, the condl draw of beta2 given beta1 makes this a userfn
  growth.beta.R             called by beta2.min and beta2.max
  userfn.dir                used to draw of beta2 parameters given beta1 specific to CRC-SPIN
  beta2.max.R               called by DistTarget: max range of beta2 given beta1
  beta2.min.R               called by DistTarget: min range of beta2 given beta1
  DistTarget.R
  SimTarget.R

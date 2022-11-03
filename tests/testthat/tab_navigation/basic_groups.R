#'Create two groups using the example data.
#'
groups_basic <- function(app) {
  app$click("goto_groups")
  app$set_inputs(group_name = "G1")
  app$set_inputs(group_samples = "EM0011_sample")
  app$set_inputs(group_samples = c("EM0011_sample", "EM0013_sample"))
  app$set_inputs(group_samples = c("EM0011_sample", "EM0013_sample", "EM0015_sample"))
  app$set_inputs(group_samples = c("EM0011_sample", "EM0013_sample", "EM0015_sample", 
                                   "EM0017_sample"))
  app$set_inputs(group_samples = c("EM0011_sample", "EM0013_sample", "EM0015_sample", 
                                   "EM0017_sample", "EM0019_sample"))
  app$set_inputs(group_samples = c("EM0011_sample", "EM0013_sample", "EM0015_sample", 
                                   "EM0017_sample", "EM0019_sample", "EM0061_sample"))
  app$set_inputs(group_samples = c("EM0011_sample", "EM0013_sample", "EM0015_sample", 
                                   "EM0017_sample", "EM0019_sample", "EM0061_sample", "EM0063_sample"))
  app$set_inputs(group_samples = c("EM0011_sample", "EM0013_sample", "EM0015_sample", 
                                   "EM0017_sample", "EM0019_sample", "EM0061_sample", "EM0063_sample", "EM0065_sample"))
  app$set_inputs(group_samples = c("EM0011_sample", "EM0013_sample", "EM0015_sample", 
                                   "EM0017_sample", "EM0019_sample", "EM0061_sample", "EM0063_sample", "EM0065_sample", 
                                   "EM0067_sample"))
  app$set_inputs(group_samples = c("EM0011_sample", "EM0013_sample", "EM0015_sample", 
                                   "EM0017_sample", "EM0019_sample", "EM0061_sample", "EM0063_sample", "EM0065_sample", 
                                   "EM0067_sample", "EM0069_sample"))
  app$click("add_group")
  
  app$set_inputs(group_name = "G2")
  app$click("add_group")
  
  return(app)
}
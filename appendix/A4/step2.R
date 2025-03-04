#_________________________________________________________________________________#
# The Impacts of Health Shocks on Household Labor Supply and Domestic Production
# Di Meo & Eryilmaz, 2025	 	 	 	 	 	 	 	 	 	   
#_________________________________________________________________________________#

# SET DIRECTORY WHERE DATA ARE STORED
setwd(datasets)

#_________________________________________________________________________________#
# LOAD ESTIMATION RESULTS RUN IN STATA AND RE-ESTIMATE CS
#---------------------------------------------------------------------------------#

Variant <- ""
varnames <- c("quitjob_pequiv", "i11110")

for (var in varnames) {

  # dC&dH
  #---------------------------------------------------------
  data_multiplegt <- read_xlsx(paste0("did_multiplegt_", var, ".xlsx"), col_names = FALSE)
  
  columns = c("effect","variances") 
  multiplegt_plot = data.frame(matrix(nrow = 11, ncol = length(columns))) 
  colnames(multiplegt_plot) = columns
  
  # Placebos
  for (i in 1:5) {
    multiplegt_plot$effect[i] <- data_multiplegt$...1[i+7]
    multiplegt_plot$variances[i] <- data_multiplegt$...2[i+7]
  }
  
  # Effects
  for (i in 6:11) {
    multiplegt_plot$effect[i] <- data_multiplegt$...1[i-5]
    multiplegt_plot$variances[i] <- data_multiplegt$...2[i-5]
  }
  
  # SE
  multiplegt_plot$se <- sqrt(multiplegt_plot$variances)
  
  # CI
  multiplegt_plot$lower <- multiplegt_plot$effect - multiplegt_plot$se * 1.96
  multiplegt_plot$upper <- multiplegt_plot$effect + multiplegt_plot$se * 1.96
  
  # Time Event
  for (i in 1:11) {
  multiplegt_plot$group[i] <- i - 6 
  }  
  
  # Reduce df
  multiplegt_plot <- multiplegt_plot[c("group", "effect", "lower", "upper")]
  
  
  # TWFE
  #--------------------------------------------------------- 
  data_reghdfe <- read_xlsx(paste0("ols_", var, ".xlsx"), col_names = FALSE)
  
  columns = c("effect","se") 
  reghdfe_plot = data.frame(matrix(nrow = 11, ncol = length(columns))) 
  colnames(reghdfe_plot) = columns
  
  # Placebos
  for (i in 1:5) {
    reghdfe_plot$effect[i] <- data_reghdfe$...1[6-i]
    reghdfe_plot$se[i] <- data_reghdfe$...2[6-i]
  }
  
  # Effects
  for (i in 6:11) {
    reghdfe_plot$effect[i] <- data_reghdfe$...1[i+1]
    reghdfe_plot$se[i] <- data_reghdfe$...2[i+1]
  }
  
  
  # CI
  reghdfe_plot$lower <- reghdfe_plot$effect - reghdfe_plot$se * 1.96
  reghdfe_plot$upper <- reghdfe_plot$effect + reghdfe_plot$se * 1.96
  
  # Time Event
  for (i in 1:11) {
    reghdfe_plot$group[i] <- i - 6 
  }  
  
  # Reduce df
  reghdfe_plot <- reghdfe_plot[c("group", "effect", "lower", "upper")]
  
  
    
  # CS
  #---------------------------------------------------------
  # Define data types and corresponding data sets
  
  xformla_alt <- ~ age + age_sq + gender
  set.seed(1215) 
  
  data_list <- list(data)
  
  # Iterate over each variable
    # Process each data type

      cs <- cs_fun(var, data, xformla_alt, 0.05)
      
      cs_plot <- data.frame(
        group = cs$es$egt,
        effect = cs$es$att.egt,
        lower = cs$es$att.egt - cs$es$crit.val.egt * cs$es$se.egt,
        upper = cs$es$att.egt + cs$es$crit.val.egt * cs$es$se.egt
      )
      
    

  
  # Define data types and corresponding datasets
  #---------------------------------------------------------
  
  data_types <- c("CS", "DCDH", "TWFE")
  data_list <- list(cs_plot, multiplegt_plot, reghdfe_plot)
  
  assign("CS", cs_plot)
  assign("DCDH", multiplegt_plot)
  assign("TWFE", reghdfe_plot)
  
  data_list_quitjob_pequiv <- list(
    `Callaway and Sant'Anna` = get("CS"),
    `De Chaisemartin and D'Haultfoeuille` =  get("DCDH"),
    `TWFE` =  get("TWFE")
  )
  
  plot_data_name <- paste0("plot_", var, "_data")
  plot_data <- do.call(rbind, lapply(names(data_list_quitjob_pequiv), function(name) {
    transform(data_list_quitjob_pequiv[[name]], name = name)
  }))
  
  # Assign the plot data to a dynamically named variable
  assign(plot_data_name, plot_data)
}



#_________________________________________________________________________________#
# SAVE ALL OBJECTS AS AN IMAGE
#---------------------------------------------------------------------------------#
# SET DIRECTORY WHERE RESULTS ARE STORED
setwd(results)

# Select only those beginning with 'tot_' or 'partner_'
objects_to_save <- ls(pattern = "^(plot_quitjob_pequiv_data|plot_i11110_data)")
save(list = objects_to_save, file = paste0("ComparisonEstimators", Variant, ".RData"))



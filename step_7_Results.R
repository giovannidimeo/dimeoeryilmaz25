#_________________________________________________________________________________#
# The Impacts of Health Shocks on Household Labor Supply and Domestic Production
# Di Meo & Eryilmaz, 2025 	 	 	 	 	 	 	 	 	   
#_________________________________________________________________________________#

# SET DIRECTORY WHERE RESULTS ARE STORED
setwd(results)

# Assuming 'levels' is predefined
levels <- c(0.01, 0.05, 0.1)


# Set # of cores to be used for parallelization
plan(multisession, workers = future::availableCores() - 5) # leave one for other system tasks

tic()


# _________________________________________________________________________________#
# CHOOSE VARIABLES
#---------------------------------------------------------------------------------#

varnames <- c(
  # Income
  "i11101",
  "i11102",
  "i11101_adj",
  "i11102_adj",
  "i11103",
  "i11107",
  "i11110",
  "hhialg_ss",
  
  # Labor Force Participation
  "e11101",
  "e11101_weekly",
  "unemployed_pequiv",
  "retired_pequiv",
  "quitjob_pequiv",
  "handicap_EWB_pequiv",
  "er_rente_pequiv",
  "nonemployed_pequiv",
  "parttime_pequiv",
  "fulltime_pequiv",
  
  # Time Use
  "time_leisure_weekdays",
  "time_caring_weekdays",
  "time_chores_weekdays",
  "time_childcare_weekdays",
  "housework_fulltime",
  
  # Health and Other
  "satisfaction_health",
  "hospital_staysnights",
  "health_now",
  "m11124",
  "child_born"
)

varnames_partner <- c(
  # Income
  "i11110",

  # Labor Force Participation
  "e11101",
  "e11101_weekly",
  "unemployed_pequiv",
  "retired_pequiv",
  "quitjob_pequiv",
  "handicap_EWB_pequiv",
  "er_rente_pequiv",
  "nonemployed_pequiv",
  "parttime_pequiv",
  "fulltime_pequiv",
  
  # Time Use
  "time_leisure_weekdays",
  "time_caring_weekdays",
  "time_chores_weekdays",
  "time_childcare_weekdays",
  "housework_fulltime",
  
  # Health and Other
  "m11124"
)

varnames_gender <- c(
  "time_chores_weekdays",
  "time_childcare_weekdays",
  "i11110",
  "quitjob_pequiv",
  "parttime_pequiv",
  "fulltime_pequiv"
)

varnames_reform <- c(
  "time_leisure_weekdays",
  "time_chores_weekdays",
  "time_childcare_weekdays",
  "housework_fulltime",
  "i11110",
  "m11124",
  "unemployed_pequiv",
  "retired_pequiv",
  "quitjob_pequiv",
  "handicap_EWB_pequiv",
  "er_rente_pequiv",
  "nonemployed_pequiv",
  "parttime_pequiv",
  "fulltime_pequiv"
)

varnames_recent <- c("time_leisure_weekdays", 
              "time_caring_weekdays", 
              "time_chores_weekdays", 
              "time_childcare_weekdays", 
              "housework_fulltime", 
              "child_born")

# _________________________________________________________________________________#
# RUN THE FUNCTION FOR SOEP
#----------------------------------------------------------------------------------#

xformla_main <- ~ age + age_sq + gender
xformla_gender <- ~ age + age_sq

# Specify the datasets and variables for main and partner analyses
analysis_settings <- list(
  
  # MAIN RESULTS
  #---------------------------------------------------------
  
    self = list(
      data = data,  # DEFINE DATA SET
      varnames = varnames,  # DEFINE VARIABLE LIST
      xformla = xformla_main  # DEFINE CONTROL VARIABLE
    ),
    
    partner = list(
      data = data_partner,  # DEFINE DATA SET
      varnames = varnames_partner,  # DEFINE VARIABLE LIST
      xformla = xformla_main  # DEFINE CONTROL VARIABLE
    ),
    
  # GENDER SPLITS
  #---------------------------------------------------------
  
    male = list(
        data_list = data_male,  # DEFINE DATA SET
        varnames = varnames_gender,  # DEFINE VARIABLE LIST
        xformla = xformla_gender  # DEFINE CONTROL VARIABLE
      ),
    
    female = list(
        data_list = data_female,  # DEFINE DATA SET
        varnames = varnames_gender,  # DEFINE VARIABLE LIST
        xformla = xformla_gender  # DEFINE CONTROL VARIABLE
      ),
  
  # GENDER SPLITS - PARTNER
  #---------------------------------------------------------
  
    male_partner = list(
      data = data_partner_male,  # DEFINE DATA SET
      varnames = varnames_gender,  # DEFINE VARIABLE LIST
      xformla = xformla_gender  # DEFINE CONTROL VARIABLE
    ),
    
    female_partner = list(
      data = data_partner_female,  # DEFINE DATA SET
      varnames = varnames_gender,  # DEFINE VARIABLE LIST
      xformla = xformla_gender  # DEFINE CONTROL VARIABLE
    ),
    
  # REFORM SPLITS
  #---------------------------------------------------------
  
    control = list(
      data = data_control,  # DEFINE DATA SET
      varnames = varnames_reform,  # DEFINE VARIABLE LIST
      xformla = xformla_main  # DEFINE CONTROL VARIABLE
    ),
    
    reform = list(
      data = data_reform,  # DEFINE DATA SET
      varnames = varnames_reform,  # DEFINE VARIABLE LIST
      xformla = xformla_main  # DEFINE CONTROL VARIABLE
    ),
  
  # EXCLUDE RECENT MOTHERS
  #---------------------------------------------------------
  
    female_NoRecentBirths = list(
      data = data_female_NoRecentBirths,  # DEFINE DATA SET
      varnames = varnames_recent,  # DEFINE VARIABLE LIST
      xformla = xformla_gender  # DEFINE CONTROL VARIABLE
    )
)


# Initialize an empty list to store results for all variables and significance levels
MainResults_Megaloop <- list()

# Use future_map to iterate over analysis types in parallel
results_list <- future_map(names(analysis_settings), ~{
  type <- .x
  current_setting <- analysis_settings[[type]]
  current_data <- current_setting$data
  current_xformla <- current_setting$xformla
  current_varnames <- current_setting$varnames
  
  # Initialize a list to store results for the current analysis type
  analysis_results <- list()
  
  # Iterate over variables (this could also be parallelized with future_map if desired)
  for (var in current_varnames) {
    es_01 <- NULL
    es_05 <- NULL
    es_10 <- NULL
    
    # Run the analysis for each significance level
    for (l in levels) {
      result <- cs_fun(var, current_data, current_xformla, l)
      es <- result$es
      
      # Store results based on significance level
      if (l == 0.01) {
        es_01 <- es
      } else if (l == 0.05) {
        es_05 <- es
      } else if (l == 0.1) {
        es_10 <- es
      }
    }
    
    # Create the table for a given variable
    table_name <- paste0(type, "_", var)
    table_data <- data.frame(
      group = es_05$egt,
      effect = es_05$att.egt,
      se = es_05$se.egt,
      lower = es_05$att.egt - es_05$crit.val.egt * es_05$se.egt,
      upper = es_05$att.egt + es_05$crit.val.egt * es_05$se.egt,
      critval99 = es_01$crit.val.egt,
      critval95 = es_05$crit.val.egt,
      critval90 = es_10$crit.val.egt,
      att = es_05$overall.att,
      attse = es_05$overall.se,
      unique_pids = es_05$DIDparams$n
    )
    # Store the table in the analysis_results list
    analysis_results[[table_name]] <- table_data
  }
  analysis_results
}, .progress = TRUE)  # Optional: show progress

# Combine results from all analyses into a single list
MainResults_Megaloop <- do.call(c, results_list)

toc()

# _________________________________________________________________________________#
# SAVE ALL OBJECTS AS AN IMAGE
#---------------------------------------------------------------------------------#

# Select only those beginning with 'tot_' or 'partner_'
objects_to_save <- ls(pattern = "^(MainResults_Megaloop)")
save(list = objects_to_save, file = paste0("AllResults", ".RData"))


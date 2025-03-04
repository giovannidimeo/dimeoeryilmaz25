#_________________________________________________________________________________#
# The Impacts of Health Shocks on Household Labor Supply and Domestic Production
# Di Meo & Eryilmaz, 2025 	 	 	 	 	 	 	 	   
#_________________________________________________________________________________#
library(HonestDiD)

# SET DIRECTORY WHERE RESULTS ARE STORED
setwd(results)

#---------------------------------------------------------------------------------#


#' @title honest_did
#'
#' @description a function to compute a sensitivity analysis
#'  using the approach of Rambachan and Roth (2021)
honest_did <- function(...) UseMethod("honest_did")

#' @title honest_did.AGGTEobj
#'
#' @description a function to compute a sensitivity analysis
#'  using the approach of Rambachan and Roth (2021) when
#'  the event study is estimating using the `did` package
#'
#' @param e event time to compute the sensitivity analysis for.
#'  The default value is `e=0` corresponding to the "on impact"
#'  effect of participating in the treatment.
#' @param type Options are "smoothness" (which conducts a
#'  sensitivity analysis allowing for violations of linear trends
#'  in pre-treatment periods) or "relative_magnitude" (which
#'  conducts a sensitivity analysis based on the relative magnitudes
#'  of deviations from parallel trends in pre-treatment periods).
#' @inheritParams HonestDiD::createSensitivityResults
#' @inheritParams HonestDid::createSensitivityResults_relativeMagnitudes
honest_did.AGGTEobj <- function(es,
                                e          = 0,
                                type       = c("smoothness", "relative_magnitude"),
                                gridPoints = 50,
                                ...) {
  
  type <- match.arg(type)
  
  # Make sure that user is passing in an event study
  if (es$type != "dynamic") {
    stop("need to pass in an event study")
  }
  
  # Check if used universal base period and warn otherwise
  if (es$DIDparams$base_period != "universal") {
    stop("Use a universal base period for honest_did")
  }
  
  # Recover influence function for event study estimates
  es_inf_func <- es$inf.function$dynamic.inf.func.e
  
  # Recover variance-covariance matrix
  n <- nrow(es_inf_func)
  V <- t(es_inf_func) %*% es_inf_func / n / n
  
  # Remove the coefficient normalized to zero
  referencePeriodIndex <- which(es$egt == -1)
  V    <- V[-referencePeriodIndex,-referencePeriodIndex]
  beta <- es$att.egt[-referencePeriodIndex]
  
  nperiods <- nrow(V)
  npre     <- sum(1*(es$egt < -1))
  npost    <- nperiods - npre
  baseVec1 <- basisVector(index=(e+1),size=npost)
  orig_ci  <- constructOriginalCS(betahat        = beta,
                                  sigma          = V,
                                  numPrePeriods  = npre,
                                  numPostPeriods = npost,
                                  l_vec          = baseVec1)
  
  if (type=="relative_magnitude") {
    robust_ci <- createSensitivityResults_relativeMagnitudes(betahat        = beta,
                                                             sigma          = V,
                                                             numPrePeriods  = npre,
                                                             numPostPeriods = npost,
                                                             l_vec          = baseVec1,
                                                             gridPoints     = gridPoints,
                                                             ...)
    
  } else if (type == "smoothness") {
    robust_ci <- createSensitivityResults(betahat        = beta,
                                          sigma          = V,
                                          numPrePeriods  = npre,
                                          numPostPeriods = npost,
                                          l_vec          = baseVec1,
                                          ...)
  }
  
  return(list(robust_ci=robust_ci, orig_ci=orig_ci, type=type))
}


library(tidyverse)
library(ggstance)

# Function to clean and prepare each dataframe
prepare_data <- function(df, id) {
  # Rename columns for ease of use
  names(df)[names(df) == "lb[,1]"] <- "lb"
  names(df)[names(df) == "ub[,1]"] <- "ub"
  
  # Round Mbar values to the nearest first decimal
  df$Mbar_rounded <- round(df$Mbar, 1)
  
  # Create a label column
  df$label <- paste(df$method, format(df$Mbar_rounded, nsmall = 1))
  
  # Calculate the midpoint as the x value for each row
  df$midpoint <- (df$lb + df$ub) / 2
  
  # Add the id column
  df$id <- id
  
  return(df)
}


# Formula for covariates 
xformla <- ~ age + age_sq + gender


# Define your variable names
varnames <- c("i11110", "quitjob_pequiv")

# Initialize a list to store combined data for all variables
combined_data_list <- list()

# Loop through each variable name
for (var in varnames) {
  
  set.seed(1215) 
  
  # Estimate the ATT(g,t) for each variable
  assign(paste0("CS_", var), att_gt(yname = var,
                                    gname = "shock",
                                    idname = "pid",
                                    tname = "syear",
                                    est_method = "ipw",
                                    control_group = "notyettreated",
                                    xformla = xformla,
                                    panel = TRUE,
                                    allow_unbalanced_panel = TRUE,
                                    pl = FALSE,
                                    cores = 12,
                                    base_period = "universal",
                                    data = data))
  
  # Compute event study for each variable
  assign(paste0("CS_es_", var), aggte(get(paste0("CS_", var)), 
                                      type = "dynamic",
                                      min_e = -5, max_e = 5, na.rm = TRUE))
  
  # Generate the figure for each variable
  assign(paste0("fig_CS_", var), ggdid(get(paste0("CS_es_", var)),
                                       title = paste0("Event-study DiD based on conditional PTA and NYT", var)))
  
  # Initialize lists to store results for each variable
  hd_cs_rm_never_list_var <- list()
  cs_HDiD_relmag_list_var <- list()
  
  # Loop from 1 to 5 for each variable
  
  print(var)
  
  for (i in 0:5) {
    
    print(i)
    
    # Honest DiD for relative magnitude for each variable
    hd_cs_rm_never <- honest_did(es = get(paste0("CS_es_", var)), 
                                 e = i,
                                 type = "relative_magnitude",
                                 Mbarvec=seq(from = 0, to = 2, by = 0.50))
    
    # Drop 0 as it's not allowed
    hd_cs_rm_never$robust_ci <- hd_cs_rm_never$robust_ci[-1,]

    # Create and store plot for each variable
    cs_HDiD_relmag <- createSensitivityPlot_relativeMagnitudes(hd_cs_rm_never$robust_ci,
                                                               hd_cs_rm_never$orig_ci)
    cs_HDiD_relmag_list_var[[i+1]] <- cs_HDiD_relmag
  }
  
  # Assign the lists to the global environment with the variable name appended
  assign(paste0("cs_HDiD_relmag_list_", var), cs_HDiD_relmag_list_var)
  
  # Prepare each dataframe and combine them for each variable
  combined_data_var <- do.call(rbind, lapply(seq_along(get(paste0("cs_HDiD_relmag_list_", var))), function(i) {
    prepare_data(get(paste0("cs_HDiD_relmag_list_", var))[[i]][["data"]], i)
  }))
  
  # Split the label into method and Mbar_rounded components and then recreate the label for each variable
  combined_data_var <- combined_data_var %>%
    separate(label, into = c("method", "Mbar_label"), sep = " ") %>%
    mutate(Mbar_label = as.numeric(Mbar_label),
           label = paste(method, Mbar_label))
  
  # Store the combined data for each variable in the list
  combined_data_list[[var]] <- combined_data_var
  
}

#_________________________________________________________________________________#
# SAVE ALL OBJECTS AS AN IMAGE
#---------------------------------------------------------------------------------#

# Identify objects to save: those ending with '_male' or '_female'
objects_to_save <- ls(pattern = "^(hd_cs_rm_|fig_CS|CS_es_|cs_HDiD_|CS_i11110|CS_quitjob_pequiv|combined_data_list)")
save(list = objects_to_save, file = paste0("HonestDiD", ".RData"))



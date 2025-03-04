#_________________________________________________________________________________#
# The Impacts of Health Shocks on Household Labor Supply and Domestic Production
# Di Meo & Eryilmaz, 2025 	 	 	 	 	 	 	 	 	   
#_________________________________________________________________________________#

rm(list = ls(all.names = TRUE))

#_________________________________________________________________________________#
# LOAD PACKAGES, SET UP ENVIRONMENT
#---------------------------------------------------------------------------------#

# PACKAGES

library(haven)
library(did)
library(tidyverse)
library(xtable)
library(foreign)
library(extrafont)
library(Cairo)
library(cowplot)
library(purrr)
library(here)
library(plm)
library(lfe)
library(readxl)
library(future)
library(furrr)
library(tictoc)
library(data.table)
library(purrr)
options(datatable.rbindlist.check = "none") # To silence the rbindlist() related warning. The warning has no effect on the estimation, and occurs only because "did" uses data.table in the latest version.

# DEFINE DIRECTORY WHERE DATA ARE STORED
datasets <- ("***")

# DEFINE DIRECTORY WHERE RESULTS ARE STORED
results <- ("***")

# DEFINE DIRECTORY WHERE SCRIPTS ARE STORED
scripts <- ("***")

#_________________________________________________________________________________#
# LOAD FONTS
#---------------------------------------------------------------------------------#

# For Computer Modern Serif
font_import() # ONLY NEED TO REGISTER THIS ONCE; load them all ("y")
font_import(pattern = "CMU Serif", prompt = FALSE)

 loadfonts(device = "win")
 device = "Wiyn"

#_________________________________________________________________________________#
# WRITE CUSTOM CSDID FUNCTION
#---------------------------------------------------------------------------------#

# Define time window for the event study plot, confidence levels
span_top <- 5
span_bottom <- 5
levels <- c(0.01, 0.05, 0.1)

# cs_fun requires (1) outcome variable, (2) data set, (3) object name. It estimates the ATT_gt for a given outcome and produces (a) tex table of the event study output and (2) event study plot objects used in <<3_Plot.R>>.


cs_fun <- function(y, df_data, xformla, alpha) {
  
  set.seed(1215) # Setting seed in the function. This should prevent us generating results w/o explicitly setting the right seed. 

  # Estimate the ATT(g,t)
  result <- att_gt(yname = y,
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
                   alp = alpha,
                   base_period = "universal",
                   data = df_data)
  
  # Aggregate to ES coefficients
  es <- aggte(result, type = "dynamic", na.rm = TRUE, min_e = -span_bottom, max_e = span_top)
  
  return(list(result = result, es = es))
}

#_________________________________________________________________________________#
# LOAD DATA: TREATED PERSON 
#---------------------------------------------------------------------------------#
setwd(datasets)
data <- read_dta("R_soeponly.dta")

#_________________________________________________________________________________#
# GENERATE DATASETS FOR SUBGRUPS
#---------------------------------------------------------------------------------#


data_male <- data %>% 
  filter(gender == 0) 

data_female <- data %>% 
  filter(gender == 1) 

data <- data %>%
  mutate(reform = replace(reform, shock < 2001 & shock >= 1996 & year_birth >= 1961, NA))

data <- data %>%
  mutate(reform = replace(reform, shock >= 2001 & year_birth < 1961, NA))

data_control <- data %>% 
  filter(reform == 0)

data_reform <- data %>% 
  filter(reform == 1)

data_stable_couples <- data %>% 
  filter(stable_partner == 1)

data_female_NoRecentBirths <- data_female %>% 
  filter(is.na(recent_births)) 


#_________________________________________________________________________________#
# LOAD DATA: SOEP PARTNER
#---------------------------------------------------------------------------------#
setwd(datasets)
data_partner  <- read_dta("R_soeponly_partner.dta")

#_________________________________________________________________________________#
# GENERATE DATASETS FOR SUBGRUPS
#---------------------------------------------------------------------------------#

data_partner_male <- data_partner %>% 
  filter(gender == 0)

data_partner_female <- data_partner %>% 
  filter(gender == 1)

data_partner_control <- data_partner %>% 
  filter(reform == 0)

data_partner_reform <- data_partner %>% 
  filter(reform == 1)

#_________________________________________________________________________________#
# CHECK NUMBERS
#---------------------------------------------------------------------------------#

unique_pid_count <- data %>%
  filter(event_time == -1) %>%
  distinct(pid) %>%
  nrow()

unique_pid_count 


unique_pid_count_partner <- data_partner %>%
  filter(event_partner == -1) %>%
  distinct(pid) %>%
  nrow()

unique_pid_count_partner


#_________________________________________________________________________________#
# The Impacts of Health Shocks on Household Labor Supply and Domestic Production
# Di Meo & Eryilmaz, 2025 	 	 	 	 	 	 	 	 	   
#_________________________________________________________________________________#


#_________________________________________________________________________________#
# DEFINE STANDARD THEME
#---------------------------------------------------------------------------------#

# LOAD THE THEME
setwd(scripts)
source("_theme_for_plots.R")

# Set y-labels
#-----------------------------------------------------------------------
income <- "In 2020 Euros"
hours <- "Hours"
hoursweekdays <- "Hours on Weekdays"
share <- "Share"
zscore <- "z-score"

# SET DIRECTORY WHERE RESULTS ARE STORED
setwd(results)

#LOAD RESULTS IF NOT ALREADY LOADED
load("AllResults.RData")

#_________________________________________________________________________________#
# MAIN RESULT PLOTS
#---------------------------------------------------------------------------------#


# IndividualIncomeLFP
#---------------------------------------------------------

data_list_i11110 <- list(
  `Gross Labor Earnings` = MainResults_Megaloop$self_i11110
)

plot_i11110 <- create_multplot(
  data_list = data_list_i11110, 
  title = "A: Annual Gross Labor Income", 
  y_label = income, 
  my_theme = theme_single, 
  color_map = c(`Gross Labor Earnings` = "black"),
  y_axis_limits = c(-8000, 4000), 
  y_axis_breaks = seq(-8000, 4000, by = 2000),  
  info_text = info_texts$self_i11110
  
)

print(plot_i11110)

data_list_quitjob_pequiv <- list(
  `Labor Force Participation` = MainResults_Megaloop$self_quitjob_pequiv
)

plot_quitjob_pequiv <- create_multplot(
  data_list = data_list_quitjob_pequiv, 
  title = "B: Labor Force Participation", 
  y_label = share, 
  my_theme = theme_single, 
  color_map = c(`Labor Force Participation` = "black"),
  y_axis_limits = c(-0.4, 0.1),
  y_axis_breaks = seq(-0.4, 0.1, by = 0.1), 
  info_text = info_texts$self_quitjob_pequiv
  
)

# Combine the plots with the legend
final_plot_asp <- cowplot::plot_grid(
  plot_i11110, plot_quitjob_pequiv,
  ncol = 2,
  align = 'vh',
  labels = c("", ""),
  rel_heights = c(1)
)

print(final_plot_asp)

# Save the combined plot
ggsave(file.path(paste0("IndividualIncomeLFP", ".pdf")), 
       final_plot_asp, width = 10, height = 5, device=cairo_pdf)


# LFP
#---------------------------------------------------------

data_list_e11101_weekly <- list(
  `Working Hours` = MainResults_Megaloop$self_e11101_weekly
)

plot_e11101_weekly <- create_multplot(
  data_list = data_list_e11101_weekly, 
  title = "C: Weekly Working Hours", 
  y_label = hours, 
  my_theme = theme_single, 
  color_map = c(`Working Hours` = "black"),
  y_axis_limits = c(-15, 5),
  info_text = info_texts$self_e11101_weekly
  
)


data_list_unemployed_pequiv <- list(
  `Unemployed` = MainResults_Megaloop$self_unemployed_pequiv
)

plot_unemployed_pequiv <- create_multplot(
  data_list = data_list_unemployed_pequiv, 
  title = "D: Unemployed with Benefits", 
  y_label = share, 
  my_theme = theme_single, 
  color_map = c(`Unemployed` = "black"),
  y_axis_limits = c(-0.2, 0.2),
  info_text = info_texts$self_unemployed_pequiv
  
)

data_list_nonemployed_pequiv <- list(
  `Nonemployed` = MainResults_Megaloop$self_nonemployed_pequiv
)

plot_nonemployed_pequiv <- create_multplot(
  data_list = data_list_nonemployed_pequiv, 
  title = "E: Unemployed w/o Benefits", 
  y_label = share, 
  my_theme = theme_single, 
  color_map = c(`Nonemployed` = "black"),
  y_axis_limits = c(-0.2, 0.2),
  info_text = info_texts$self_nonemployed_pequiv
  
)

data_list_er_rente_pequiv <- list(
  `Disability Pension` = MainResults_Megaloop$self_er_rente_pequiv
)


plot_er_rente_pequiv <- create_multplot(
  data_list = data_list_er_rente_pequiv, 
  title = "F: Disability Pension", 
  y_label = share, 
  my_theme = theme_single, 
  color_map = c("Disability Pension" = "black"),
  y_axis_limits = c(-0.2, 0.2),
  info_text = info_texts$self_er_rente_pequiv
)




data_list_fulltime_pequiv <- list(
  `Full-Time` = MainResults_Megaloop$self_fulltime_pequiv
)

plot_fulltime_pequiv <- create_multplot(
  data_list = data_list_fulltime_pequiv,
  title = "A: Full-Time Employment",
  y_label = share,
  my_theme = theme_single,
  color_map = c(`Full-Time` = "black"),
  y_axis_limits = c(-0.4, 0.1),
  info_text = info_texts$self_fulltime_pequiv
  
)

data_list_parttime_pequiv <- list(
  `Part-Time` = MainResults_Megaloop$self_parttime_pequiv
)

plot_parttime_pequiv <- create_multplot(
  data_list = data_list_parttime_pequiv,
  title = "B: Part-Time Employment",
  y_label = share,
  my_theme = theme_single,
  color_map = c(`Part-Time` = "black"),
  y_axis_limits = c(-0.2, 0.2),
  info_text = info_texts$self_parttime_pequiv
  
)


# Combine the plots with the legend
final_plot_asp <- cowplot::plot_grid(
  plot_fulltime_pequiv, plot_parttime_pequiv, plot_e11101_weekly, plot_unemployed_pequiv, plot_nonemployed_pequiv, plot_er_rente_pequiv, 
  ncol = 2,
  align = 'vh',
  labels = c("", "", "", "", "", ""),
  rel_heights = c(1, 1, 1)
)

print(final_plot_asp)
# Save the combined plot
ggsave(file.path(paste0("LFP", ".pdf")), final_plot_asp, width = 10, height = 15, device=cairo_pdf)


# HHIncome
#---------------------------------------------------------

data_list_i11101 <- list(
  `Household Pre-Government Income` = MainResults_Megaloop$self_i11101
)


plot_i11101 <- create_multplot(
  data_list = data_list_i11101, 
  title = "A: Household Pre-Government Income", 
  y_label = income, 
  my_theme = theme_single, 
  color_map = c(`Household Pre-Government Income` = "black"),
  y_axis_limits = c(-8000, 4000),
  y_axis_breaks = seq(-8000, 4000, by = 2000),  
  info_text = info_texts$self_i11101
  
)


data_list_i11102 <- list(
  `Post-Government Income` = MainResults_Megaloop$self_i11102
)

plot_i11102 <- create_multplot(
  data_list = data_list_i11102, 
  title = "B: Household Post-Government Income", 
  y_label = income, 
  my_theme = theme_single, 
  color_map = c(`Post-Government Income` = "black"),
  y_axis_limits = c(-8000, 4000),
  y_axis_breaks = seq(-8000, 4000, by = 2000), 
  info_text = info_texts$self_i11102
)

data_list_i11101_adj <- list(
  `Equivalent Household Pre-Government Income` = MainResults_Megaloop$self_i11101_adj
)


plot_i11101_adj <- create_multplot(
  data_list = data_list_i11101_adj, 
  title = "C: Household Pre-Government Income \n Equivalent", 
  y_label = income, 
  my_theme = theme_single, 
  color_map = c(`Equivalent Household Pre-Government Income` = "black"),
  y_axis_limits = c(-8000, 4000),
  y_axis_breaks = seq(-8000, 4000, by = 2000), 
  info_text = info_texts$self_i11101_adj
  
  
)


data_list_i11102_adj <- list(
  `Equivalent Post-Government Income` = MainResults_Megaloop$self_i11102_adj
)

plot_i11102_adj <- create_multplot(
  data_list = data_list_i11102_adj, 
  title = "D: Household Post-Government Income \n Equivalent", 
  y_label = income, 
  my_theme = theme_single, 
  color_map = c(`Equivalent Post-Government Income` = "black"),
  y_axis_limits = c(-8000, 4000),
  y_axis_breaks = seq(-8000, 4000, by = 2000), 
  info_text = info_texts$self_i11102_adj
  
)

# Combine the plots with the legend
final_plot_asp <- cowplot::plot_grid(
  plot_i11101, plot_i11102, plot_i11101_adj, plot_i11102_adj,
  ncol = 2,
  align = 'vh',
  labels = c("", "", "", ""),
  rel_heights = c(1, 1)
)

print(final_plot_asp)


# Save the combined plot
ggsave(file.path(paste0("HHIncomeMain", ".pdf")), final_plot_asp, width = 10, height = 10, device=cairo_pdf)


# TU Treated & Spouse
#---------------------------------------------------------
data_list_time_childcare_weekdays <- list(
  Treated = MainResults_Megaloop$self_time_childcare_weekdays
)

data_list_time_chores_weekdays <- list(
  Treated = MainResults_Megaloop$self_time_chores_weekdays
)


data_list_time_childcare_weekdays_p <- list(
  Spouse = MainResults_Megaloop$partner_time_childcare_weekdays
)

data_list_time_chores_weekdays_p <- list(
  Spouse = MainResults_Megaloop$partner_time_chores_weekdays
)


plot_1 <- create_multplot(
  data_list = data_list_time_childcare_weekdays, 
  title = "A: Childcare - Treated", 
  y_label = hoursweekdays, 
  my_theme = theme_single, 
  color_map = c("Treated" = "black"),
  y_axis_limits = c(-1, 2),
  y_axis_breaks = seq(-1, 2, by = 0.5), 
  info_text = info_texts$self_time_childcare_weekdays 
  
)

plot_2 <- create_multplot(
  data_list = data_list_time_chores_weekdays, 
  title = "B: Household chores - Treated", 
  y_label = hoursweekdays, 
  my_theme = theme_single, 
  color_map = c("Treated" = "black"),
  y_axis_limits = c(-1, 2),
  y_axis_breaks = seq(-1, 2, by = 0.5), 
  info_text = info_texts$self_time_chores_weekdays  
)

plot_3 <- create_multplot(
  data_list = data_list_time_childcare_weekdays_p, 
  title = "C: Childcare - Spouses", 
  y_label = hoursweekdays, 
  my_theme = theme_single, 
  color_map = c("Spouse" = "black"),
  y_axis_limits = c(-1, 2),
  y_axis_breaks = seq(-1, 2, by = 0.5), 
  info_text = info_texts$partner_time_childcare_weekdays  
  
)

plot_4 <- create_multplot(
  data_list = data_list_time_chores_weekdays_p, 
  title = "D: Household chores - Spouses", 
  y_label = hoursweekdays, 
  my_theme = theme_single, 
  color_map = c("Spouse" = "black"),
  y_axis_limits = c(-1, 2),
  y_axis_breaks = seq(-1, 2, by = 0.5), 
  info_text = info_texts$partner_time_chores_weekdays  
)


# Combine the plots with the legend
final_plot_asp <- cowplot::plot_grid(
  plot_1, plot_2, plot_3, plot_4,
  ncol = 2,
  align = 'vh',
  labels = c("", "", "", ""),
  rel_heights = c(1, 1, 1)
  
)


print(final_plot_asp)

# Print combined plot
ggsave(file.path(paste0("TU_treatedspouses", ".pdf")),  width = 10, height = 10, device=cairo_pdf)



# treatment_A
#---------------------------------------------------------

# Define data lists for each variable
data_list_satisfaction_health <- list(
  All = MainResults_Megaloop$self_satisfaction_health
)

plot_satis <- create_multplot(
  data_list = data_list_satisfaction_health, 
  title = "B: Satisfaction with Own Health", 
  y_label = zscore, 
  my_theme = theme_single, 
  color_map = c(All = "black"),
  y_axis_limits = c(-0.6, 0.5),
  info_text = info_texts$self_satisfaction_health 
) 

data_list_hospital <- list(
  All = MainResults_Megaloop$self_hospital_staysnights
)

plot_hospital <- create_multplot(
  data_list = data_list_hospital, 
  title = "A: Overnight Stays at Hospital", 
  y_label = "Number of Nights", 
  my_theme = theme_single, 
  color_map = c(All = "black"),
  y_axis_limits = c(-2, 10),
  info_text = info_texts$self_hospital_staysnights  
) 



data_list_health <- list(
  All = MainResults_Megaloop$self_health_now
)

plot_health <- create_multplot(
  data_list = data_list_health, 
  title = "C: Subjective Health", 
  y_label = zscore, 
  my_theme = theme_single, 
  color_map = c(All = "black"),
  y_axis_limits = c(-0.6, 0.5),
  info_text = info_texts$self_health_now
) 



data_list_handicap_EWB_pequiv <- list(
  `Disabled` = MainResults_Megaloop$self_handicap_EWB_pequiv
)

plot_handicap_EWB_pequiv <- create_multplot(
  data_list = data_list_handicap_EWB_pequiv, 
  title = "D: Disability Status", 
  y_label = share, 
  my_theme = theme_single, 
  color_map = c(`Disabled` = "black"),
  y_axis_limits = c(-0.05, 0.2),
  info_text = info_texts$self_handicap_EWB_pequiv
) 


# Combine the plots with the legend
final_plot_asp <- cowplot::plot_grid(
  plot_hospital, plot_satis, plot_health, plot_handicap_EWB_pequiv,
  ncol = 2,
  align = 'vh',
  labels = c("", "", "", ""),
  rel_heights = c(1, 1)
  
)

plot(final_plot_asp)

# Save the combined plot
ggsave(file.path(paste0("treatment_A", ".pdf")), final_plot_asp, width = 10, height = 10, device=cairo_pdf)



# labor_supply_TU_gender (treated)
#---------------------------------------------------------

data_list_i11110 <- list(
  Female = MainResults_Megaloop$female_i11110,
  Male = MainResults_Megaloop$male_i11110
)

combined_info_text <- paste0("\n", "Female ", info_texts$female_i11110, "\n", "Male ", info_texts$male_i11110, sep = "   ")

plot_i11110 <- create_multplot(
  data_list = data_list_i11110, 
  title = "A: Annual Gross Labor Income", 
  y_label = income, 
  my_theme = theme_single, 
  color_map = c("Male" = "#00798c", "Female" = "black"),
  y_axis_limits = c(-10000, 4000), 
  y_axis_breaks = seq(-10000, 4000, by = 2000),  
  info_text = combined_info_text

)

print(plot_i11110)

                
combined_info_text <- paste0("\n", "Female ", info_texts$female_quitjob_pequiv, "\n", "Male ", info_texts$male_quitjob_pequiv, sep = "   ")

data_list_quitjob_pequiv <- list(
  Female = MainResults_Megaloop$female_quitjob_pequiv,
  Male = MainResults_Megaloop$male_quitjob_pequiv
)

plot_quitjob_pequiv <- create_multplot(
  data_list = data_list_quitjob_pequiv, 
  title = "B: Labor Force Participation", 
  y_label = share, 
  my_theme = theme_single, 
  color_map = c("Male" = "#00798c", "Female" = "black"),
  y_axis_limits = c(-0.4, 0.4),
  info_text = combined_info_text    
  
  
)



combined_info_text <- paste0("\n", "Female ", info_texts$female_fulltime_pequiv, "\n", "Male ", info_texts$male_fulltime_pequiv, sep = "   ")

                
data_list_fulltime_pequiv <- list(
  Female = MainResults_Megaloop$female_fulltime_pequiv,
  Male = MainResults_Megaloop$male_fulltime_pequiv
)

plot_fulltime_pequiv <- create_multplot(
  data_list = data_list_fulltime_pequiv, 
  title = "C: Full-Time Employment", 
  y_label = share, 
  my_theme = theme_single, 
  color_map = c("Male" = "#00798c", "Female" = "black"),
  y_axis_limits = c(-0.4, 0.4),
  info_text = combined_info_text  
  
)
                

combined_info_text <- paste0("\n", "Female ", info_texts$female_parttime_pequiv, "\n", "Male ", info_texts$male_parttime_pequiv, sep = "   ")


data_list_parttime_pequiv <- list(
  Female = MainResults_Megaloop$female_parttime_pequiv,
  Male = MainResults_Megaloop$male_parttime_pequiv
)

plot_parttime_pequiv <- create_multplot(
  data_list = data_list_parttime_pequiv, 
  title = "D: Part-Time Employment", 
  y_label = share, 
  my_theme = theme_single, 
  color_map = c("Male" = "#00798c", "Female" = "black"),
  y_axis_limits = c(-0.4, 0.4),
  info_text = combined_info_text   
  
)

combined_info_text <- paste0("\n", "Female ", info_texts$female_time_childcare_weekdays, "\n", "Male ", info_texts$male_time_childcare_weekdays, sep = "   ")

data_list_time_childcare_weekdays <- list(
    Female = MainResults_Megaloop$female_time_childcare_weekdays,
    Male = MainResults_Megaloop$male_time_childcare_weekdays
  )
  
  plot_time_childcare_weekdays <- create_multplot(
    data_list = data_list_time_childcare_weekdays, 
    title = "E: Childcare", 
    y_label = hoursweekdays, 
    my_theme = theme_single, 
    color_map = c("Male" = "#00798c", "Female" = "black"),
    y_axis_limits = c(-1, 2),
    y_axis_breaks = seq(-1, 2, by = 0.5), 
    info_text = combined_info_text 
    
  )
   
  
  combined_info_text <- paste0("\n", "Female ", info_texts$female_time_chores_weekdays, "\n", "Male ", info_texts$male_time_chores_weekdays, sep = "   ")
  
  
data_list_time_chores_weekdays <- list(
  Female = MainResults_Megaloop$female_time_chores_weekdays,
  Male = MainResults_Megaloop$male_time_chores_weekdays
)


plot_time_chores_weekdays <- create_multplot(
  data_list = data_list_time_chores_weekdays, 
  title = "F: Household Chores", 
  y_label = hoursweekdays, 
  my_theme = theme_single, 
  color_map = c("Male" = "#00798c", "Female" = "black"),
  y_axis_limits = c(-1, 2),
  y_axis_breaks = seq(-1, 2, by = 0.5), 
  info_text = combined_info_text 
)
                

# Usage for legend extraction
plot_for_legend <- create_multplot(
  data_list = data_list_time_chores_weekdays,
  title = "",
  y_label = "",
  my_theme = theme_single,
  color_map = c("Male" = "#00798c", "Female" = "black"),
  include_legend = TRUE
)

# Extract legend
#legend_plot <- get_legend(plot_for_legend)
legend_plot <- cowplot::get_plot_component(plot_for_legend, "guide-box", return_all = TRUE)[[3]]

# Combine the plots with the legend
final_plot_asp <- cowplot::plot_grid(
  plot_i11110, plot_quitjob_pequiv, 
  plot_fulltime_pequiv ,plot_parttime_pequiv,
  plot_time_childcare_weekdays, plot_time_chores_weekdays,
  ncol = 2,
  align = 'vh',
  rel_heights = c(1, 1),
  labels = c("", "", "", "")
)

# Combine the plots with the legend
final_plot_asp_with_legend <- cowplot::plot_grid(
  final_plot_asp,
  legend_plot,
  ncol = 1,
  rel_heights = c(1, 0.05)
)


print(final_plot_asp_with_legend)

# Print combined plot
ggsave(file.path(paste0("labor_supply_TU_gender", ".pdf")), width = 10, height = 18, device=cairo_pdf)




# labor_supply_TU_gender (spouses)
#---------------------------------------------------------

data_list_i11110 <- list(
  Female = MainResults_Megaloop$female_partner_i11110,
  Male = MainResults_Megaloop$male_partner_i11110
)

combined_info_text <- paste0("\n", "Female ", info_texts$female_partner_i11110, "\n", "Male ", info_texts$male_partner_i11110, sep = "   ")

plot_i11110 <- create_multplot(
  data_list = data_list_i11110, 
  title = "A: Annual Gross Labor Income", 
  y_label = income, 
  my_theme = theme_single, 
  color_map = c("Male" = "#00798c", "Female" = "black"),
  y_axis_limits = c(-8000, 14000), 
  y_axis_breaks = seq(-8000, 14000, by = 4000),  
  info_text = combined_info_text   
  
)

data_list_quitjob_pequiv <- list(
  Female = MainResults_Megaloop$female_partner_quitjob_pequiv,
  Male = MainResults_Megaloop$male_partner_quitjob_pequiv
)

combined_info_text <- paste0("\n", "Female ", info_texts$female_partner_quitjob_pequiv, "\n", "Male ", info_texts$male_partner_quitjob_pequiv, sep = "   ")


plot_quitjob_pequiv <- create_multplot(
  data_list = data_list_quitjob_pequiv, 
  title = "B: Labor Force Participation", 
  y_label = share, 
  my_theme = theme_single, 
  color_map = c("Male" = "#00798c", "Female" = "black"),
  y_axis_limits = c(-0.4, 0.4),
  info_text = combined_info_text    
  
)

data_list_fulltime_pequiv <- list(
  Female = MainResults_Megaloop$female_partner_fulltime_pequiv,
  Male = MainResults_Megaloop$male_partner_fulltime_pequiv
)

combined_info_text <- paste0("\n", "Female ", info_texts$female_partner_fulltime_pequiv, "\n", "Male ", info_texts$male_partner_fulltime_pequiv, sep = "   ")



plot_fulltime_pequiv <- create_multplot(
  data_list = data_list_fulltime_pequiv, 
  title = "C: Full-Time Employment", 
  y_label = share, 
  my_theme = theme_single, 
  color_map = c("Male" = "#00798c", "Female" = "black"),
  y_axis_limits = c(-0.4, 0.4),
  info_text = combined_info_text    
  
)

data_list_parttime_pequiv <- list(
  Female = MainResults_Megaloop$female_partner_parttime_pequiv,
  Male = MainResults_Megaloop$male_partner_parttime_pequiv
)

combined_info_text <- paste0("\n", "Female ", info_texts$female_partner_parttime_pequiv, "\n", "Male ", info_texts$male_partner_parttime_pequiv, sep = "   ")


plot_parttime_pequiv <- create_multplot(
  data_list = data_list_parttime_pequiv, 
  title = "D: Part-Time Employment", 
  y_label = share, 
  my_theme = theme_single, 
  color_map = c("Male" = "#00798c", "Female" = "black"),
  y_axis_limits = c(-0.4, 0.4),
  info_text = combined_info_text    
  
)

data_list_time_childcare_weekdays <- list(
  Female = MainResults_Megaloop$female_partner_time_childcare_weekdays,
  Male = MainResults_Megaloop$male_partner_time_childcare_weekdays
)

combined_info_text <- paste0("\n", "Female ", info_texts$female_partner_time_childcare_weekdays, "\n", "Male ", info_texts$male_partner_time_childcare_weekdays, sep = "   ")


plot_time_childcare_weekdays <- create_multplot(
  data_list = data_list_time_childcare_weekdays, 
  title = "E: Childcare", 
  y_label = hoursweekdays, 
  my_theme = theme_single, 
  color_map = c("Male" = "#00798c", "Female" = "black"),
  y_axis_limits = c(-2, 2),
  info_text = combined_info_text   
  
)

data_list_time_chores_weekdays <- list(
  Female = MainResults_Megaloop$female_partner_time_chores_weekdays,
  Male = MainResults_Megaloop$male_partner_time_chores_weekdays
)


combined_info_text <- paste0("\n", "Female ", info_texts$female_partner_time_chores_weekdays, "\n", "Male ", info_texts$male_partner_time_chores_weekdays, sep = "   ")


plot_time_chores_weekdays <- create_multplot(
  data_list = data_list_time_chores_weekdays, 
  title = "F: Household Chores", 
  y_label = hoursweekdays, 
  my_theme = theme_single, 
  color_map = c("Male" = "#00798c", "Female" = "black"),
  y_axis_limits = c(-2, 2),
  info_text = combined_info_text   
)


# Combine the plots with the legend
final_plot_asp <- cowplot::plot_grid(
  plot_i11110, plot_quitjob_pequiv, 
  plot_fulltime_pequiv ,plot_parttime_pequiv,
  plot_time_childcare_weekdays, plot_time_chores_weekdays,
  ncol = 2,
  align = 'vh',
  rel_heights = c(1, 1),
  labels = c("", "", "", "")
)


# Usage for legend extraction
plot_for_legend <- create_multplot(
  data_list = data_list_time_chores_weekdays, 
  title = "", 
  y_label = "", 
  my_theme = theme_single, 
  color_map = c("Male" = "#00798c", "Female" = "black"),
  include_legend = TRUE
)

# Extract legend
#legend_plot <- get_legend(plot_for_legend)
legend_plot <- cowplot::get_plot_component(plot_for_legend, "guide-box", return_all = TRUE)[[3]]


# Combine the plots with the legend
final_plot_asp_with_legend <- cowplot::plot_grid(
  final_plot_asp,
  legend_plot,
  ncol = 1,
  rel_heights = c(1, 0.05)
)

final_plot_asp_with_legend

# Print combined plot
ggsave(file.path(paste0("labor_supply_TU_gender_partner", ".pdf")), width = 10, height = 15, device=cairo_pdf)


# labor_supply_reform
#---------------------------------------------------------

data_list_quitjob <- list(
  #All = MainResults_Megaloop$self_quitjob,
  Pre = MainResults_Megaloop$control_quitjob_pequiv,
  Post = MainResults_Megaloop$reform_quitjob_pequiv
)

combined_info_text <- paste0("\n", "Pre ", info_texts$control_quitjob_pequiv, "\n", "Post ", info_texts$reform_quitjob_pequiv, sep = "   ")


plot_1 <- create_multplot(
  data_list = data_list_quitjob, 
  title = "B: Labor Force Participation", 
  y_label = share, 
  my_theme = theme_single, 
  color_map = c("Pre" = "black", "Post" = "#00798c"),
  y_axis_limits = c(-0.5, 0.2),
  y_axis_breaks = seq(-0.5, 0.2, by = 0.1),  
  info_text = combined_info_text  
  
)

data_list_nonemployed <- list(
  #All = MainResults_Megaloop$self_nonemployed,
  Pre = MainResults_Megaloop$control_nonemployed_pequiv,
  Post = MainResults_Megaloop$reform_nonemployed_pequiv
)

combined_info_text <- paste0("\n", "Pre ", info_texts$control_nonemployed_pequiv, "\n", "Post ", info_texts$reform_nonemployed_pequiv, sep = "   ")


plot_2 <- create_multplot(
  data_list = data_list_nonemployed, 
  title = "C: Unemployed w/o Benefits", 
  y_label = share, 
  my_theme = theme_single, 
  color_map = c("Pre" = "black", "Post" = "#00798c"),
  y_axis_limits = c(-0.1, 0.4),
  info_text = combined_info_text 
)


data_list_i11110 <- list(
  #All = MainResults_Megaloop$self_i11110,
  Pre = MainResults_Megaloop$control_i11110,
  Post = MainResults_Megaloop$reform_i11110
)

combined_info_text <- paste0("\n", "Pre ", info_texts$control_i11110, "\n", "Post ", info_texts$reform_i11110, sep = "   ")

plot_3 <- create_multplot(
  data_list = data_list_i11110, 
  title = "A: Annual Gross Labor Income", 
  y_label = income, 
  my_theme = theme_single, 
  color_map = c("Pre" = "black", "Post" = "#00798c"),
  y_axis_limits = c(-8000, 4000),
  y_axis_breaks = seq(-8000, 4000, by = 2000),  
  info_text = combined_info_text 
)

data_list_er_rente <- list(
  #All = MainResults_Megaloop$self_er_rente,
  Pre = MainResults_Megaloop$control_er_rente_pequiv,
  Post = MainResults_Megaloop$reform_er_rente_pequiv
)


combined_info_text <- paste0("\n", "Pre ", info_texts$control_er_rente_pequiv, "\n", "Post ", info_texts$reform_er_rente_pequiv, sep = "   ")

plot_4 <- create_multplot(
  data_list = data_list_er_rente, 
  title = "D: Disability Pension", 
  y_label = share, 
  my_theme = theme_single, 
  color_map = c("Pre" = "black", "Post" = "#00798c"),
  y_axis_limits = c(-0.05, 0.2),
  info_text = combined_info_text 
)


# Combine the plots with the legend
final_plot_asp <- cowplot::plot_grid(
  plot_3, plot_1, plot_2, plot_4,
  ncol = 2,
  align = 'vh',
  labels = c("", "", "", "")
)


# Usage for legend extraction
plot_for_legend <- create_multplot(
  data_list = data_list_i11110, 
  title = "", 
  y_label = "", 
  my_theme = theme_single, 
  color_map = c("Pre" = "black", "Post" = "#00798c"),
  include_legend = TRUE
)

# Extract legend
#legend_plot <- get_legend(plot_for_legend)
legend_plot <- cowplot::get_plot_component(plot_for_legend, "guide-box", return_all = TRUE)[[3]]


# Combine the plots with the legend
final_plot_asp_with_legend <- cowplot::plot_grid(
  final_plot_asp,
  legend_plot,
  ncol = 1,
  rel_heights = c(1, 0.05)
)

print(final_plot_asp_with_legend)

ggsave(file.path(paste0("labor_supply_reform", ".pdf")), width = 10, height = 10, device=cairo_pdf)



# SpouseIncomeLFP
#---------------------------------------------------------

data_list_i11110 <- list(
  `Gross Labor Earnings` = MainResults_Megaloop$partner_i11110
)


plot_i11110 <- create_multplot(
  data_list = data_list_i11110, 
  title = "A: Annual Gross Labor Income", 
  y_label = income, 
  my_theme = theme_single, 
  color_map = c(`Gross Labor Earnings` = "black"),
  y_axis_limits = c(-5000, 10000),
  info_text = info_texts$partner_i11110
  
)


data_list_quitjob_pequiv <- list(
  `Labor Force Participation` = MainResults_Megaloop$partner_quitjob_pequiv
)

plot_quitjob_pequiv <- create_multplot(
  data_list = data_list_quitjob_pequiv, 
  title = "B: Labor Force Participation", 
  y_label = share, 
  my_theme = theme_single, 
  color_map = c(`Labor Force Participation` = "black"),
  y_axis_limits = c(-0.2, 0.2),
  info_text = info_texts$partner_quitjob_pequiv
  
)


data_list_fulltime_pequiv <- list(
  `Full-Time` = MainResults_Megaloop$partner_fulltime_pequiv
)

plot_fulltime_pequiv <- create_multplot(
  data_list = data_list_fulltime_pequiv,
  title = "C: Full-Time Employment",
  y_label = share,
  my_theme = theme_single,
  color_map = c(`Full-Time` = "black"),
  y_axis_limits = c(-0.2, 0.2),
  info_text = info_texts$partner_fulltime_pequiv
  
)

data_list_parttime_pequiv <- list(
  `Part-Time` = MainResults_Megaloop$partner_parttime_pequiv
)

plot_parttime_pequiv <- create_multplot(
  data_list = data_list_parttime_pequiv,
  title = "D: Part-Time Employment",
  y_label = share,
  my_theme = theme_single,
  color_map = c(`Part-Time` = "black"),
  y_axis_limits = c(-0.2, 0.2),
  info_text = info_texts$partner_parttime_pequiv
  
)

# Combine the plots with the legend
final_plot_asp <- cowplot::plot_grid(
  plot_i11110, plot_quitjob_pequiv, plot_fulltime_pequiv, plot_parttime_pequiv, 
  ncol = 2,
  align = 'vh',
  labels = c("", "", "", "", "", ""),
  rel_heights = c(1, 1, 1)
)


print(final_plot_asp)

# Save the combined plot
ggsave(file.path(paste0("SpouseIncomeLFP", ".pdf")), final_plot_asp, width = 10, height = 10, device=cairo_pdf)



# TU Treated & Spouse - Additional Time Use
#---------------------------------------------------------

data_list_time_leisure_weekdays <- list(
  Treated = MainResults_Megaloop$self_time_leisure_weekdays
)

plot_1 <- create_multplot(
  data_list = data_list_time_leisure_weekdays, 
  title = "A: Leisure Activities - Treated", 
  y_label = hoursweekdays, 
  my_theme = theme_single, 
  color_map = c("Treated" = "black"),
  y_axis_limits = c(-1, 2),
  info_text = info_texts$self_time_leisure_weekdays  
  
)

data_list_time_leisure_weekdays_p <- list(
  Spouse = MainResults_Megaloop$partner_time_leisure_weekdays
)

plot_3 <- create_multplot(
  data_list = data_list_time_leisure_weekdays_p, 
  title = "B: Leisure Activities - Spouses", 
  y_label = hoursweekdays, 
  my_theme = theme_single, 
  color_map = c("Spouse" = "black"),
  y_axis_limits = c(-1, 2),
  info_text = info_texts$partner_time_leisure_weekdays 
  
)

data_list_time_caring_weekdays_p <- list(
  Spouse = MainResults_Megaloop$partner_time_caring_weekdays
)

plot_4 <- create_multplot(
  data_list = data_list_time_caring_weekdays_p, 
  title = "C: Informal Care - Spouses", 
  y_label = hoursweekdays, 
  my_theme = theme_single, 
  color_map = c("Spouse" = "black"),
  y_axis_limits = c(-1, 2),
  info_text = info_texts$partner_time_caring_weekdays  
)


final_plot_asp <- cowplot::plot_grid(
  plot_grid(plot_1, plot_3, nrow = 1, ncol = 2),
  plot_grid(NULL, plot_4, NULL, nrow = 1, rel_widths = c(0.5, 1, 0.5)),
  nrow = 2
)

# Display the final plot
print(final_plot_asp)


# Print combined plot
ggsave(file.path(paste0("TU_treatedspouses_add", ".pdf")),  width = 10, height = 10, device=cairo_pdf)



# treatment_A
#---------------------------------------------------------


filtered_data <- data %>% 
  filter(!is.na(sickdays), sickdays > 0, syear == shock, sickdays >= 30) 

average_data <- data %>%
  filter(event_time>=-5 & event_time<=5) %>%
  group_by(event_time) %>%
  summarize(average_hospital_staysnights = mean(hospital_staysnights, na.rm = TRUE),
            average_satisfaction_health = mean(satisfaction_health, na.rm = TRUE),
            average_health_now = mean(health_now, na.rm = TRUE),
            average_m11124 = mean(m11124, na.rm = TRUE),
            average_sickdays = mean(sickdays, na.rm = TRUE)
  )



intensities <- ggplot(filtered_data, aes(x = sickdays)) +
  geom_histogram(data = filtered_data, aes(x = sickdays), fill = "black", alpha = 0.9, binwidth = 10) +  
  labs(x = "Days", y = "Frequency", title = "A: Distribution of Days on Sick Leave") +
  theme_single + 
  scale_x_continuous(breaks = seq(min(filtered_data$sickdays), max(filtered_data$sickdays), by = 50)) + # Customize x-axis ticks
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, 100), expand = c(0, 0))  # no padding


host <- ggplot(average_data, aes(x = event_time, y = average_hospital_staysnights)) +
  geom_point() +  # Scatter plot
  geom_line(color = "black", size = 0.4) +  
  geom_vline(xintercept = 0, color = "red", linetype = "solid", size = 0.4, alpha = 0.7) +
  labs(x = "Event Time", y = "Average Per Event Time", 
       title = "C: Overnight Hospital Stays") +
  theme_single  + # Or any other theme you prefer
  scale_x_continuous(limits=c(-5, 5), breaks=seq(-5, 5, 1)) +
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, 2), expand = c(0, 0))  # no padding


satisfaction_health <- ggplot(average_data, aes(x = event_time, y = average_satisfaction_health)) +
  geom_point() +  # Scatter plot
  geom_line(color = "black", size = 0.4) +  
  geom_vline(xintercept = 0, color = "red", linetype = "solid", size = 0.4, alpha = 0.7) +
  labs(x = "Event Time", y = "Average Z-Score Per Event Time", 
       title = "D: Satisfaction with Own Health") +
  theme_single  + # Or any other theme you prefer
  scale_x_continuous(limits=c(-5, 5), breaks=seq(-5, 5, 1)) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5), expand = c(0, 0))  # no padding


health_now <- ggplot(average_data, aes(x = event_time, y = average_health_now)) +
  geom_point() +  # Scatter plot
  geom_line(color = "black", size = 0.4) +  
  geom_vline(xintercept = 0, color = "red", linetype = "solid", size = 0.4, alpha = 0.7) +
  labs(x = "Event Time", y = "Average Z-Score Per Event Time", 
       title = "D: Subjective Health") +
  theme_single  + # Or any other theme you prefer
  scale_x_continuous(limits=c(-5, 5), breaks=seq(-5, 5, 1)) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5), expand = c(0, 0))  # no padding


sickdays <- ggplot(average_data, aes(x = event_time, y = average_sickdays)) +
  geom_point() +  # Scatter plot
  geom_line(color = "black", size = 0.4) +  
  geom_vline(xintercept = 0, color = "red", linetype = "solid", size = 0.4, alpha = 0.7) +
  labs(x = "Event Time", y = "Average Per Event Time", 
       title = "B: Days on Sick Leave") +
  theme_single  + # Or any other theme you prefer
  scale_x_continuous(limits=c(-5, 5), breaks=seq(-5, 5, 1)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25), expand = c(0, 0))  # no padding





# Combine the plots with the legend
final_plot_asp <- cowplot::plot_grid(
 intensities, sickdays, host, satisfaction_health,
  ncol = 2,
  align = 'vh',
  labels = c("", "", "", "", "", ""),
  rel_heights = c(1, 1, 1)
)

print(final_plot_asp)

# Save the combined plot
ggsave(file.path(paste0("TreatmentDescriptive", ".pdf")), final_plot_asp, width = 10, height = 10, device=cairo_pdf)



# COMPARISON ESTIMATORS
#---------------------------------------------------------
load("ComparisonEstimators.RData")

plot_quitjob_pequiv_data$name <- ifelse(plot_quitjob_pequiv_data$name == "Callaway and Sant'Anna", 
                                        "CS (2021)", plot_quitjob_pequiv_data$name)

plot_quitjob_pequiv_data$name <- ifelse(plot_quitjob_pequiv_data$name == "De Chaisemartin and D'Haultfoeuille", 
                                        "DCDH (2024)", plot_quitjob_pequiv_data$name)


plot_i11110_data$name <- ifelse(plot_i11110_data$name == "Callaway and Sant'Anna", 
                                        "CS (2021)", plot_i11110_data$name)

plot_i11110_data$name <- ifelse(plot_i11110_data$name == "De Chaisemartin and D'Haultfoeuille", 
                                        "DCDH (2024)", plot_i11110_data$name)



data_cs_plot <- list(
  `CS (2021)` = plot_quitjob_pequiv_data
)


plot_comp1 <- create_multplot(
  data_list = data_cs_plot, 
  title = "A: Labor Force Participation", 
  y_label = share, 
  my_theme = theme_single, 
  color_map = c(`CS (2021)` = "black", `TWFE` = "darkgreen", `DCDH (2024)` = "darkorange"),
  y_axis_limits = c(-0.3, 0.2) 
  
)

plot(plot_comp1)


data_cs_plot2 <- list(
  `CS (2021)` = plot_i11110_data
)


plot_comp2 <- create_multplot(
  data_list = data_cs_plot2, 
  title = "B: Annual Gross Labor Income", 
  y_label = income, 
  my_theme = theme_single, 
  color_map = c(`CS (2021)` = "black", `TWFE` = "darkgreen", `DCDH (2024)` = "darkorange"),
  y_axis_limits = c(-10000, 4000), 
  y_axis_breaks = seq(-10000, 4000, by = 2000)  
)

plot(plot_comp2)


# Combine the plots with the legend
final_plot <- cowplot::plot_grid(
  plot_comp1, plot_comp2,
  ncol = 2,
  align = 'vh',
  labels = c("", "")
)


# Usage for legend extraction
plot_for_legend <- create_multplot(
  data_list = data_cs_plot2, 
  title = "", 
  y_label = "", 
  my_theme = theme_single, 
  color_map = c(`CS (2021)` = "black", `TWFE` = "darkgreen", `DCDH (2024)` = "darkorange"),
  include_legend = TRUE
)

# Extract legend
#legend_plot <- get_legend(plot_for_legend)
legend_plot <- cowplot::get_plot_component(plot_for_legend, "guide-box", return_all = TRUE)[[3]]


# Combine the plots with the legend
final_plot_with_legend <- cowplot::plot_grid(
  final_plot,
  legend_plot,
  ncol = 1,
  rel_heights = c(1, 0.1)
)

final_plot_with_legend


# Save the combined plot
ggsave(file.path(paste0("combined_estimators", ".pdf")), final_plot_with_legend, width = 10, height = 5, device=cairo_pdf)




# TIME USE - BY GENDER - NO RECENT BIRTHS
#---------------------------------------------------------

# Define data lists for each variable
data_list_time_childcare_weekdays <- list(
  Female = MainResults_Megaloop$female_NoRecentBirths_time_childcare_weekdays
)

data_list_time_chores_weekdays <- list(
  Female = MainResults_Megaloop$female_NoRecentBirths_time_chores_weekdays
)


plot_1 <- create_multplot(
  data_list = data_list_time_childcare_weekdays, 
  title = "A: Childcare", 
  y_label = hoursweekdays, 
  my_theme = theme_single, 
  color_map = c("Female" = "black"),
  y_axis_limits = c(-0.7, 2),
  info_text = info_texts$female_NoRecentBirths_time_childcare_weekdays   
  
)

plot_2 <- create_multplot(
  data_list = data_list_time_chores_weekdays, 
  title = "B: Household chores", 
  y_label = hoursweekdays, 
  my_theme = theme_single, 
  color_map = c("Female" = "black"),
  y_axis_limits = c(-0.5, 1),
  y_axis_breaks = seq(-0.5, 1, by = 0.25), 
  info_text = info_texts$female_NoRecentBirths_time_chores_weekdays
)

# Combine the plots with the legend
final_plot_asp <- cowplot::plot_grid(
  plot_1, plot_2,
  #, plot_3, plot_4,
  ncol = 2,
  align = 'vh',
  labels = c("", "", "", "")
)


# Print combined plot
ggsave(file.path(paste0("TU_GENDER_NoRecentBirths", ".pdf")),  final_plot_asp, width = 10, height = 5,device=cairo_pdf)


# HONESTDID
#---------------------------------------------------------
load("HonestDiD.RData")

my_palette <- c(
  "Original 0" = "#00798c",  # Blue
  "C-LF 0.5" = "#d1495b",    # Red
  "C-LF 1" = "#edae49",      # Yellow
  "C-LF 1.5" = "#66a182",    # Green
  "C-LF 2" = "#2e4057"       # Navy
)


theme_single_honestdid <- theme_bw() +  # Start with theme_bw() for the white background
  theme(
    plot.title = element_text(size = 10, hjust = 0.5, family = "CMU Serif"),
    axis.title = element_text(size = 8, family = "CMU Serif"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "lightgray", size = 0.3),
    panel.grid.minor.y = element_blank(), 
    axis.text.x = element_text(size = 6, color = "black", family = "CMU Serif"),  
    axis.text.y = element_text(size = 6, color = "black", family = "CMU Serif"),  
    axis.title.x = element_text(size = 8, color = "black", family = "CMU Serif"),
    axis.title.y = element_text(size = 8, color = "black", family = "CMU Serif"),
    legend.text = element_text(size = 6, family = "CMU Serif"),
    panel.border = element_blank(),
    axis.ticks = element_line(size = 0.4),  
    axis.line = element_line(color = "black", size = 0.1)
  )


# Extract the data frame for the variable "quitjob" from th e list
combined_data_quitjob <- combined_data_list[["quitjob_pequiv"]]
# Adjustments for error bar thickness and dodge distance
error_bar_thickness <- 0.25  # Adjust this value to make the error bars thinner
dodge_width <- 0.8          # Increase this value to increase the spacing within groups
error_bar_width <- 0.2 

plot_1 <- ggplot(combined_data_quitjob, aes(x = as.factor(id), y = midpoint, ymin = lb, ymax = ub, colour = label, group = interaction(id, Mbar_rounded))) +
  geom_errorbar(width = error_bar_width, size = error_bar_thickness, position = position_dodge(width = dodge_width)) +
  #geom_point(position = position_dodge(width = dodge_width), size = 0) +
  geom_hline(yintercept = 0, color = "red", linetype = "solid", size = 0.3, alpha = 0.7) +
  labs(x = "Event Time", y = "Share", colour = expression(bar(M)), title = "B: Labor Force Participation") +
  theme_single_honestdid +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_color_manual(values = my_palette) +
  scale_x_discrete(labels = c("1" = "0", "2" = "1", "3" = "2", "4" = "3", "5" = "4", "6" = "5")) +
  scale_y_continuous(limits = c(-0.3, 0.1), breaks = seq(-0.3, 0.1, 0.1), expand = c(0, 0), 
                     labels = scales::number_format(accuracy = 0.01))  # Force decimal format

plot_1


combined_data_i11110 <- combined_data_list[["i11110"]]

plot_2 <- ggplot(combined_data_i11110, aes(x = as.factor(id), y = midpoint, ymin = lb, ymax = ub, colour = label, group = interaction(id, Mbar_rounded))) +
  geom_errorbar(width = error_bar_width, size = error_bar_thickness, position = position_dodge(width = dodge_width)) +
  #geom_point(position = position_dodge(width = dodge_width), size = 0) +
  geom_hline(yintercept = 0, color = "red", linetype = "solid", size = 0.3, alpha = 0.7) +
  labs(x = "Event Time", y = "In 2020 Euros", colour = expression(bar(M)), title = "A: Annual Gross Labor Income") +
  theme_single_honestdid +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_color_manual(values = my_palette) +
  scale_x_discrete(labels = c("1" = "0", "2" = "1", "3" = "2", "4" = "3", "5" = "4", "6" = "5")) +
  scale_y_continuous(limits = c(-14000, 10000), breaks = seq(-14000, 10000, 4000), expand = c(0, 0))  # no padding

plot_2

# Combine the plots without the legend
final_plot_asp <- cowplot::plot_grid(
  plot_2 + theme(legend.position = "none"),  # Remove legend from plot_2
  plot_1 + theme(legend.position = "none"),  # Remove legend from plot_1
  #plot_3 + theme(legend.position = "none"),  # Remove legend from plot_2
  ncol = 1,  # Change this to 1 for 1 column
  align = 'v',  # Change this to 'v' for vertical alignment
  labels = c("", "")  # Adjust this to have 3 labels
)


# Extract legend
#legend_plot <- get_legend(plot_1 + theme(legend.position = "bottom"))
legend_plot <- cowplot::get_plot_component(plot_1+ theme(legend.position = "bottom") , "guide-box", return_all = TRUE)[[3]]


# Combine the plots with the legend
final_plot_asp_with_legend <- cowplot::plot_grid(
  final_plot_asp,
  legend_plot,
  ncol = 1,
  rel_heights = c(1, 0.1)
)


ggsave(file.path(paste0("HonestDID", ".pdf")), width = 5, height = 5, device=cairo_pdf)



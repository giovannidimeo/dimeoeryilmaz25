#_________________________________________________________________________________#
# The Impacts of Health Shocks on Household Labor Supply and Domestic Production
# Di Meo & Eryilmaz, 2025 	 	 	 	 	 	 	 	 	   
#_________________________________________________________________________________#

# SET DIRECTORY WHERE RESULTS ARE STORED
setwd(results)

#LOAD RESULTS IF NOT ALREADY LOADED
load("AllResults.RData")


#_________________________________________________________________________________#
# DEFINE FUNCTION
#---------------------------------------------------------------------------------#

myround <- function(x) {
  ifelse(x < 1 & x > -1, round(x, 4), round(x, 2) )
}

i <- 1L

stars <- function(df) {
  df$tstat[i] <- 0
  df$tstatatt[i] <- 0
  for (i in 1:11) {
    print(i)
    df$tstat[i] <-   abs(df$effect[i]/df$se[i])
    if (!is.na(df$tstat[i]) &  df$tstat[i] >= df$critval99[i]) {
      df$stars[i] <- "***"
    } else if ( !is.na(df$tstat[i])  &  df$tstat[i] < df$critval99[i] & df$tstat[i] >= df$critval95[i]) {
      df$stars[i] <- "**"
    } else if (!is.na(df$tstat[i])  &  df$tstat[i] < df$critval95[i] & df$tstat[i] >= df$critval90[i]) {
      df$stars[i] <- "*"
    } else { 
      df$stars[i] <- "" }
    
    df$tstatatt[i] <-abs(df$att[i]/df$attse[i])
    
    if (!is.na(df$tstatatt[i]) &  df$tstatatt[i] >= qnorm(1-0.01/2)) {
      df$starsatt[i] <- "***"
    } else if ( !is.na(df$tstatatt[i])  &  df$tstatatt[i] < qnorm(1-0.01/2) & df$tstatatt[i] >= qnorm(1-0.05/2)) {
      df$starsatt[i] <- "**"
    } else if (!is.na(df$tstatatt[i])  &  df$tstatatt[i] < qnorm(1-0.05/2) & df$tstatatt[i] >= qnorm(1-0.1/2)) {
      df$starsatt[i] <- "*"
    } else { 
      df$starsatt[i] <- "" }
    
  }
  
  for (i in 1:11) {
    df$effect[i] <-  paste0(toString(myround(as.numeric(df$effect[i]))),  df$stars[i], sep = "")
    df$att[i] <-  paste0(toString(myround(as.numeric(df$att[i]))),  df$starsatt[i], sep = "")
    
  }
  return(df)
}

tab_function <- function(df, name){  
  df <- stars(df)
  table_tex1 <-   data.frame("Dependent variable" = name,
                             `-5` = df$effect[1],
                             `-4` = df$effect[2],
                             `-3` = df$effect[3],
                             `-2` = df$effect[4],
                             `0` = df$effect[6],
                             `1` = df$effect[7],
                             `2` = df$effect[8],
                             `3` = df$effect[9],
                             `4` = df$effect[10],
                             `5` = df$effect[11],
                             Overall = df$att[1],
                             "Unique obs." = df$unique_pids[1], 
                             check.names = FALSE)
  
  table_tex2 <- data.frame("Dependent variable" = "",
                           `-5` = paste0("(", myround(df$se[1]), ")"),
                           `-4` = paste0("(", myround(df$se[2]), ")"), 
                           `-3` = paste0("(", myround(df$se[3]), ")"),
                           `-2` = paste0("(", myround(df$se[4]), ")"),
                           `0` = paste0("(", myround(df$se[6]), ")"), 
                           `1` = paste0("(", myround(df$se[7]), ")"),
                           `2` = paste0("(", myround(df$se[8]), ")"), 
                           `3` = paste0("(", myround(df$se[9]), ")"),
                           `4` = paste0("(", myround(df$se[10]), ")"),
                           `5` = paste0("(", myround(df$se[11]), ")"),
                           Overall = paste0("(", myround(df$attse[1]) , ")"),
                           "Unique obs." =  "", 
                           check.names = FALSE)
  
  tab <- rbind(table_tex1, table_tex2)
  
  return(tab)
}

generate_table <- function(list) {
  columns = c("Dependent variable","-5", "-4", "-3", "-2", "0", "1", "2", "3", "4", "5", "Overall", "Unique obs.") 
  tab_final <- data.frame(matrix(nrow = 0, ncol = length(columns))) 
  colnames(tab_final) = columns
  
  for (v in 1:length(list)) {
    print(names(list[v]))
    tab_temp <- tab_function(list[[v]], names(list[v]))
    tab_final <- rbind(tab_final, tab_temp)
  }
  
  print(xtable(tab_final, type = "latex", align = "lccccccccccccc"),
        #file = file.path(directory, "tot_treated.tex")),
        include.rownames=FALSE)
  print(tab_final)
}

###############################################################################
#Treated
var_list_treated <- list( "Annual gross labor income" = MainResults_Megaloop$self_i11110,
                          "Labor force participation" = MainResults_Megaloop$self_quitjob_pequiv, 
                          "Full-time employment" = MainResults_Megaloop$self_fulltime_pequiv, 
                          "Part-time employment" = MainResults_Megaloop$self_parttime_pequiv,
                          "Weekly hours" = MainResults_Megaloop$self_e11101_weekly, 
                          "Unemployment with benefits" = MainResults_Megaloop$self_unemployed_pequiv,
                          "Unemployment w/o benefits" = MainResults_Megaloop$self_nonemployed_pequiv, 
                          "Disability pension" = MainResults_Megaloop$self_er_rente_pequiv,
                          "HH pre-government income" = MainResults_Megaloop$self_i11101,
                          "HH post-government income" = MainResults_Megaloop$self_i11102,
                          "HH pre-government income equivalent" = MainResults_Megaloop$self_i11101_adj,
                          "HH post-government income equivalent" = MainResults_Megaloop$self_i11102_adj,
                          "Childcare (hrs/day)" = MainResults_Megaloop$self_time_childcare_weekdays,
                          "Household chores (hrs/day)" = MainResults_Megaloop$self_time_chores_weekdays)

generate_table(var_list_treated)

###############################################################################
#Spouses
var_list_spouses <- list("Annual gross labor income" = MainResults_Megaloop$partner_i11110,
                         "Labor force participation" = MainResults_Megaloop$partner_quitjob_pequiv, 
                         "Full-time employment" = MainResults_Megaloop$partner_fulltime_pequiv, 
                         "Part-time employment" = MainResults_Megaloop$partner_parttime_pequiv,
                         "Weekly hours" = MainResults_Megaloop$partner_e11101_weekly, 
                         "Childcare (hrs/day)" = MainResults_Megaloop$partner_time_childcare_weekdays,
                         "Household chores (hrs/day)" = MainResults_Megaloop$partner_time_chores_weekdays)

generate_table(var_list_spouses)

###############################################################################
# Treated Men vs. Women 

var_list_treated_gender <- list("Annual gross labor income" = MainResults_Megaloop$female_i11110,
                        "Labor force participation" = MainResults_Megaloop$female_quitjob_pequiv, 
                        "Full-time employment" = MainResults_Megaloop$ female_fulltime_pequiv, 
                        "Part-time employment" = MainResults_Megaloop$female_parttime_pequiv,
                        "Childcare (hrs/day)" = MainResults_Megaloop$female_time_childcare_weekdays,
                        "Household chores (hrs/day)" = MainResults_Megaloop$female_time_chores_weekdays,
                        "Annual gross labor income" = MainResults_Megaloop$male_i11110,
                        "Labor force participation" = MainResults_Megaloop$male_quitjob_pequiv, 
                        "Full-time employment" = MainResults_Megaloop$ male_fulltime_pequiv, 
                        "Part-time employment" = MainResults_Megaloop$male_parttime_pequiv,
                        "Childcare (hrs/day)" = MainResults_Megaloop$male_time_childcare_weekdays,
                        "Household chores (hrs/day)" = MainResults_Megaloop$male_time_chores_weekdays)

generate_table(var_list_treated_gender)

###############################################################################
# Spouses, Men vs. Women
var_list_spouses_gender <- list( "Annual gross labor income" = MainResults_Megaloop$female_partner_i11110,
                                 "Labor force participation" = MainResults_Megaloop$female_partner_quitjob_pequiv, 
                                 "Full-time employment" = MainResults_Megaloop$ female_partner_fulltime_pequiv, 
                                 "Part-time employment" = MainResults_Megaloop$female_partner_parttime_pequiv,
                                 "Childcare (hrs/day)" = MainResults_Megaloop$female_partner_time_childcare_weekdays,
                                 "Household chores (hrs/day)" = MainResults_Megaloop$female_partner_time_chores_weekdays,
                                 "Annual gross labor income" = MainResults_Megaloop$male_partner_i11110,
                                 "Labor force participation" = MainResults_Megaloop$male_partner_quitjob_pequiv, 
                                 "Full-time employment" = MainResults_Megaloop$ male_partner_fulltime_pequiv, 
                                 "Part-time employment" = MainResults_Megaloop$male_partner_parttime_pequiv,
                                 "Childcare (hrs/day)" = MainResults_Megaloop$male_partner_time_childcare_weekdays,
                                 "Household chores (hrs/day)" = MainResults_Megaloop$male_partner_time_chores_weekday)

generate_table(var_list_spouses_gender)

###############################################################################
# Reform 
var_list_reform <- list("Annual gross labor income" = MainResults_Megaloop$control_i11110,
                        "Labor force participation" = MainResults_Megaloop$control_quitjob_pequiv, 
                        "Unemployment w/o benefits" = MainResults_Megaloop$control_nonemployed_pequiv, 
                        "Disability pension" = MainResults_Megaloop$control_er_rente_pequiv,
                        "Annual gross labor income" = MainResults_Megaloop$reform_i11110,
                        "Labor force participation" = MainResults_Megaloop$reform_quitjob_pequiv, 
                        "Unemployment w/o benefits" = MainResults_Megaloop$reform_nonemployed_pequiv, 
                        "Disability pension" = MainResults_Megaloop$reform_er_rente_pequiv)

generate_table(var_list_reform)

###############################################################################
# No recent births
var_list_norecentbirhts <- list ("Childcare (hrs/day)" = MainResults_Megaloop$female_NoRecentBirths_time_childcare_weekdays,
                         "Household chores (hrs/day)" = MainResults_Megaloop$female_NoRecentBirths_time_chores_weekdays)

generate_table(var_list_norecentbirhts)

###############################################################################
# Leisure activities 

var_list_leisure <-  list ("Leisure activities (hrs/day)" = MainResults_Megaloop$self_time_leisure_weekdays,
                           "Leisure activities (hrs/day)" = MainResults_Megaloop$partner_time_leisure_weekdays, 
                           "Informal care (hrs/day)" = MainResults_Megaloop$partner_time_caring_weekdays)

generate_table(var_list_leisure)

###############################################################################
# Health outcomes
var_list_health <- list ("Overnight Stays at Hospital" = MainResults_Megaloop$self_hospital_staysnights,
                                 "Satisfaction with Own Health" = MainResults_Megaloop$self_satisfaction_health,
                         "Subjective Health" = MainResults_Megaloop$self_health_now,
                         "Disability Status" = MainResults_Megaloop$self_handicap_EWB_pequiv)

generate_table(var_list_health)




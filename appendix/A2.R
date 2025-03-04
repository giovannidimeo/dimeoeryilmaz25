#_________________________________________________________________________________#
# The Impacts of Health Shocks on Household Labor Supply and Domestic Production
# Di Meo & Eryilmaz, 2025	 	 	 	 	 	 	 	 	   
#_________________________________________________________________________________#

# Define time window for the event study plot, confidence levels
span_top <- 5
span_bottom <- 5
levels <- c(0.01, 0.05, 0.1)

# Define the cs_fun function
cs_fun <- function(y, df_data, xformla = ~ age + age_sq + gender) {
  set.seed(1215)
  
  print(y)
  
  # Store results for different significance levels
  es_01 <- NULL
  es_05 <- NULL
  es_10 <- NULL
  
  # Run for each significance level
  for(l in levels) {
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
                     alp = l,
                     base_period = "universal",
                     data = df_data)
    
    # Aggregate to ES coefficients
    es <- aggte(result, type = "dynamic", na.rm = TRUE, min_e = -span_bottom, max_e = span_top)
    
    # Store based on significance level
    if(l == 0.01) es_01 <- es
    else if(l == 0.05) es_05 <- es
    else if(l == 0.1) es_10 <- es
  }
  
  # Create table
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
  
  return(list(es = es_05, table_data = table_data))
}

# Split data into earlier and later periods
data_earlier <- data %>% 
  select(pid, syear, shock, event_time, gender, age, age_sq, quitjob_pequiv, i11110, nonemployed_pequiv, er_rente_pequiv) %>% 
  filter(shock <= 2000)

data_later <- data %>% 
  select(pid, syear, shock, event_time, gender, age, age_sq, quitjob_pequiv, i11110, nonemployed_pequiv, er_rente_pequiv) %>% 
  filter(shock > 2000)

# Define formula for controls
xformla <- ~ age + age_sq + gender

# Run estimations for earlier period
quitjob_pequiv_early <- cs_fun("quitjob_pequiv", data_earlier, xformla)
i11110_early <- cs_fun("i11110", data_earlier, xformla)
nonemployed_pequiv_early <- cs_fun("nonemployed_pequiv", data_earlier, xformla)
er_rente_pequiv_early <- cs_fun("er_rente_pequiv", data_earlier, xformla)

# Run estimations for later period
quitjob_pequiv_late <- cs_fun("quitjob_pequiv", data_later, xformla)
i11110_late <- cs_fun("i11110", data_later, xformla)
nonemployed_pequiv_late <- cs_fun("nonemployed_pequiv", data_later, xformla)
er_rente_pequiv_late <- cs_fun("er_rente_pequiv", data_later, xformla)


# Store all results in a list
results_list <- list(
  
  quitjob_pequiv_early = quitjob_pequiv_early$table_data,
  quitjob_pequiv_late = quitjob_pequiv_late$table_data,
  
  i11110_early = i11110_early$table_data,
  i11110_late = i11110_late$table_data,
  
  nonemployed_pequiv_early = nonemployed_pequiv_early$table_data,
  nonemployed_pequiv_late = nonemployed_pequiv_late$table_data,
  
  er_rente_pequiv_early = er_rente_pequiv_early$table_data,
  er_rente_pequiv_late = er_rente_pequiv_late$table_data
  
)


test_coef_differences_bh <- function(early, late, n_bootstrap = 1000) {
  # Input validation
  if(!all(c("effect", "se") %in% names(early)) || 
     !all(c("effect", "se") %in% names(late))) {
    stop("Input data frames must contain 'effect' and 'se' columns")
  }
  
  # Remove NA rows (maintaining your original approach)
  valid_rows <- !is.na(early$se) & !is.na(late$se)
  early <- early[valid_rows, ]
  late <- late[valid_rows, ]
  
  # Calculate differences and pooled standard errors
  diff_df <- data.frame(
    group = early$group,
    diff = early$effect - late$effect,
    se_pooled = sqrt(early$se^2 + late$se^2)
  )
  
  # Bootstrap procedure
  boot_diffs <- matrix(NA, nrow = n_bootstrap, ncol = nrow(early))
  set.seed(1215)  # Maintaining your original seed
  
  for(i in 1:n_bootstrap) {
    early_boot <- early$effect + rnorm(nrow(early), 0, early$se)
    late_boot <- late$effect + rnorm(nrow(late), 0, late$se)
    boot_diffs[i,] <- early_boot - late_boot
  }
  
  # Calculate unadjusted p-values
  diff_df$p_value <- sapply(1:ncol(boot_diffs), function(j) {
    2 * min(mean(boot_diffs[,j] >= 0), mean(boot_diffs[,j] <= 0))
  })
  
  # Add BH-adjusted p-values
  diff_df$p_value_bh <- p.adjust(diff_df$p_value, method = "BH") #BH is for multple-hypothesis testing; the difference to not adjusting is minimal...
  
  # Add significance indicators
  diff_df$sig_05 <- diff_df$p_value_bh < 0.05
  diff_df$sig_01 <- diff_df$p_value_bh < 0.01
  
  return(diff_df)
}



# For quitjob_pequiv
quitjob_diff <- test_coef_differences_bh(quitjob_pequiv_early$table_data, 
                                         quitjob_pequiv_late$table_data)

i11110_diff <- test_coef_differences_bh(i11110_early$table_data, 
                                        i11110_late$table_data)

nonemployed_pequiv_diff <- test_coef_differences_bh(nonemployed_pequiv_early$table_data, 
                                                    nonemployed_pequiv_late$table_data)

er_rente_pequiv_diff <- test_coef_differences_bh(er_rente_pequiv_early$table_data, 
                                                 er_rente_pequiv_late$table_data)


#---------------------------------------------------------------------------------#

# Define functions
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
    #df$effect[i] <-  myround(df$effect[i])
    df$effect[i] <-  paste0(toString(myround(as.numeric(df$effect[i]))),  df$stars[i], sep = "")
    df$att[i] <-  paste0(toString(myround(as.numeric(df$att[i]))),  df$starsatt[i], sep = "")
    
  }
  return(df)
}


# Adjusted create_info_text function to eliminate spaces around parentheses
create_info_text <- function(df) {
  df_processed <- stars(df)
  
  # Extract 'att' value and its stars separately
  att_value <- as.numeric(gsub("[^0-9.-]", "", df_processed$att[1])) # Keeps numeric value, including negative
  att_stars <- gsub("[0-9.-]", "", df_processed$att[1]) # Keeps stars
  
  # Apply myround to 'att' numeric value and combine with stars
  att_rounded_with_stars <- paste0(myround(att_value), att_stars)
  
  # Apply myround to 'attse' directly
  attse_rounded <- myround(as.numeric(df_processed$attse[1]))
  
  # Extract number of unique individuals
  unique <- df_processed$unique_pids[1]

  info_text <- paste0("", "ATT: ", att_rounded_with_stars, "  \n (", attse_rounded, ")")
  
  return(info_text)
}

# Apply this adjusted function to each table in MainResults_Megaloop and collect the results
info_texts <- lapply(results_list, create_info_text)




# Set y-labels
#-----------------------------------------------------------------------
income <- "In 2020 Euros"
hours <- "Hours"
hoursweekdays <- "Hours on Weekdays"
share <- "Share"
zscore <- "z-score"



# labor_supply_reform
#---------------------------------------------------------
data_list_quitjob_pequiv <- list(
  
  `1984-2000` = quitjob_pequiv_early$table_data,
  `2001-2020` = quitjob_pequiv_late$table_data
  
)

data_list_i11110 <- list(
  
  `1984-2000` = i11110_early$table_data,
  `2001-2020` = i11110_late$table_data
  
)

data_list_nonemployed_pequiv <- list(
  
  `1984-2000` = nonemployed_pequiv_early$table_data,
  `2001-2020` = nonemployed_pequiv_late$table_data
  
)

data_list_er_rente_pequiv <- list(
  
  `1984-2000` = er_rente_pequiv_early$table_data,
  `2001-2020` = er_rente_pequiv_late$table_data
  
)


info_quitjob_pequiv <- paste0("\n", "1984-2000 ", info_texts$quitjob_pequiv_early, "\n", "2001-2020 ", info_texts$quitjob_pequiv_late, sep = "   ")

info_i11110 <- paste0("\n", "1984-2000 ", info_texts$i11110_early, "\n", "2001-2020 ", info_texts$i11110_late, sep = "   ")

info_nonemployed_pequiv <- paste0("\n", "1984-2000 ", info_texts$nonemployed_pequiv_early, "\n", "2001-2020 ", info_texts$nonemployed_pequiv_late, sep = "   ")

info_er_rente_pequiv <- paste0("\n", "1984-2000 ", info_texts$er_rente_pequiv_early, "\n", "2001-2020 ", info_texts$er_rente_pequiv_late, sep = "   ")


plot_quitjob_pequiv <- create_multplot(
  data_list = data_list_quitjob_pequiv, 
  title = "B: Labor Force Participation", 
  y_label = share, 
  my_theme = theme_single, 
  color_map = c("1984-2000" = "black", "2001-2020" = "#00798c"),
  y_axis_limits = c(-0.5, 0.2),
  y_axis_breaks = seq(-0.5, 0.2, by = 0.1),
  info_text = info_quitjob_pequiv  
  
)

plot_i11110 <- create_multplot(
  data_list = data_list_i11110, 
  title = "A: Annual Gross Labor Income", 
  y_label = income, 
  my_theme = theme_single, 
  color_map = c("1984-2000" = "black", "2001-2020" = "#00798c"),
  y_axis_limits = c(-10000, 4000),
  y_axis_breaks = seq(-10000, 4000, by = 2000),  
  info_text = info_i11110 
)

plot_nonemployed_pequiv <- create_multplot(
  data_list = data_list_nonemployed_pequiv, 
  title = "C: Unemployed w/o Benefits", 
  y_label = share, 
  my_theme = theme_single, 
  color_map = c("1984-2000" = "black", "2001-2020" = "#00798c"),
  y_axis_limits = c(-0.1, 0.4),
  info_text = info_nonemployed_pequiv 
)

plot_er_rente_pequiv <- create_multplot(
  data_list = data_list_er_rente_pequiv, 
  title = "D: Disability Pension", 
  y_label = share, 
  my_theme = theme_single, 
  color_map = c("1984-2000" = "black", "2001-2020" = "#00798c"),
  y_axis_limits = c(-0.05, 0.2),
  info_text = info_er_rente_pequiv 
)


# Combine the plots with the legend
final_plot_asp <- cowplot::plot_grid(
  plot_i11110, plot_quitjob_pequiv, plot_nonemployed_pequiv, plot_er_rente_pequiv,
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
  color_map = c("1984-2000" = "black", "2001-2020" = "#00798c"),
  include_legend = TRUE
)


# Extract legend
legend_plot <- cowplot::get_plot_component(plot_for_legend, "guide-box", return_all = TRUE)[[3]]


# Combine the plots with the legend
final_plot_asp_with_legend <- cowplot::plot_grid(
  final_plot_asp,
  legend_plot,
  ncol = 1,
  rel_heights = c(1, 0.05)
)

print(final_plot_asp_with_legend)


setwd("results")
ggsave(file.path(paste0("simple_splits_2x2", ".pdf")), width = 10, height = 10, device = cairo_pdf)



# Combine the plots with the legend
final_plot_asp <- cowplot::plot_grid(
  plot_i11110, plot_quitjob_pequiv,
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
  color_map = c("1984-2000" = "black", "2001-2020" = "#00798c"),
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


setwd("results")

ggsave(file.path(paste0("simple_splits_1x2", ".pdf")), width = 10, height = 5, device = cairo_pdf)


#---------------------------------------------------------------------------------#
# Reform: results estimated in main scripts
#_________________________________________________________________________________#
# The Impacts of Health Shocks on Household Labor Supply and Domestic Production
# Di Meo & Eryilmaz, 2025	 	 	 	 	 	 	 	 	 	   
#_________________________________________________________________________________#

rm(list = ls(all.names = TRUE))

# --- LOAD PACKAGES, FONTS SET UP ENVIRONMENT ---------------------------------------------------

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
library(knitr)

options(datatable.rbindlist.check = "none") # To silence the rbindlist() related warning. The warning has no effect on the estimation, and occurs only because "did" uses data.table in the latest version.


# --- WRITE CUSTOM CSDID FUNCTION ---------------------------------------------------

# Define time window for the event study plot, confidence levels
span_top <- 5
span_bottom <- 5
levels <- c(0.01, 0.05, 0.1)

# cs_fun requires (1) outcome variable, (2) data set, (3) object name. It estimates the ATT_gt for a given outcome and produces (a) tex table of the event study output and (2) event study plot objects used in <<3_Plot.R>>.

cs_fun <- function(var, data_df, alpha) {
  
  set.seed(1215) # Setting seed in the function. This should prevent us generating results w/o explicitly setting the right seed. 
  
  # Estimate the ATT(g,t)
  result <- att_gt(yname = var,
                   gname = "first_shock_year",
                   idname = "pid",
                   tname = "syear",
                   est_method = "ipw",
                   control_group = "notyettreated",
                   xformla =  ~ age + age_sq + gender,
                   panel = TRUE,
                   allow_unbalanced_panel = TRUE,
                   pl = FALSE,
                   cores = 12,
                   alp = alpha,
                   base_period = "universal",
                   data = data_df)
  
  # Aggregate to ES coefficients
  es <- aggte(result, type = "dynamic", na.rm = TRUE, min_e = -span_bottom, max_e = span_top)
  
  return(list(result = result, es = es))
}

# --- LOAD AND PREPARE DATA ---------------------------------------------------

setwd(datasets)
data <- read_dta("R_soeponly_full.dta")

first_shock_data <- data %>%
  filter(hospital_staysnights > 9) %>%
  arrange(pid, syear)%>%
  group_by(pid) %>%
  count() %>%
  filter(n == 1) %>%
  select(pid)

data_nyt <- data %>%
  right_join(first_shock_data, by = "pid")  %>%
  mutate(first_shock_year = ifelse(hospital_staysnights > 9 & !is.na(hospital_staysnights), syear, 0))  %>%
  group_by(pid) %>% 
  mutate(first_shock_year = max(first_shock_year))  %>%
  filter(first_shock_year>0) %>%
  mutate(age_shock = ifelse(syear == first_shock_year, age, 0))%>%
  group_by(pid) %>%
  mutate(age_shock = max(age_shock)) %>%
  ungroup() %>%
  filter(age_shock >= 25 & age_shock <= 55) %>%
  mutate(in_relationship = ifelse(syear == first_shock_year & couple_cont == 1, 1, 0)) %>%
  group_by(pid) %>%
  mutate(in_relationship = max(in_relationship)) %>%
  ungroup() %>%
  filter(in_relationship == 1) %>%
  mutate(event_time = syear - first_shock_year)

unique_pid_count <- data_nyt %>%
  filter(event_time == -1) %>%
  distinct(pid) %>%
  nrow()

unique_pid_count 

# PARTNER
pids <- data_nyt %>% 
  filter(event_time == -1)  %>% 
  filter(!is.na(pgpartnr) == 1) %>% 
  select(., c("pid", "syear", "pgpartnr", "first_shock_year"))  %>% 
  mutate(., pid_sick = pid) %>% 
  mutate(., pid = pgpartnr) %>%
  select(., c("pid", "syear", "pid_sick", "first_shock_year"))

data_partner <- left_join(data, pids, by = c("pid", "syear")) %>% 
  group_by(pid) %>%
  mutate(., first_shock_year = max(first_shock_year, na.rm = TRUE))  %>% 
  filter(!is.na(first_shock_year) & first_shock_year != -Inf)  %>% 
  ungroup()  %>%
  mutate(age_shock_partner = ifelse(syear == first_shock_year, age, 0)) %>%
  group_by(pid) %>%
  mutate(., age_shock_partner = max(age_shock_partner)) %>%
  filter(age_shock_partner >= 25 & age_shock_partner <= 55)

first_shock_data <- data_partner %>%
  filter(hospital_staysnights > 9) %>%
  arrange(pid, syear)%>%
  group_by(pid) %>%
  count() %>%
  filter(n == 1) %>%
  select(pid) %>% pull(pid)

data_partner <- data_partner %>% 
  subset(!pid %in% first_shock_data) %>% 
  mutate(event_time = syear - first_shock_year)

unique_pid_count_partner <- data_partner %>%
  filter(event_time == -1) %>%
  distinct(pid) %>%
  nrow()

unique_pid_count_partner


# --- DEFINE PLOT THEMES ---------------------------------------------------

theme_single <- theme_bw() +  # Start with theme_bw() for the white background
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, family = "CMU Serif"),
    axis.title = element_text(size = 16, family = "CMU Serif"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "lightgray", size = 0.3),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(size = 12, color = "black", family = "CMU Serif"),
    axis.text.y = element_text(size = 12, color = "black", family = "CMU Serif"),
    axis.title.x = element_text(size = 14, color = "black", family = "CMU Serif"),
    axis.title.y = element_text(size = 14, color = "black", family = "CMU Serif"),
    legend.text = element_text(size = 14, family = "CMU Serif"),
    panel.border = element_blank(),
    axis.ticks = element_line(size = 0.4),
    axis.line = element_line(color = "black", size = 0.1),
    plot.caption = element_text(size = 14, hjust = 0.5, family = "CMU Serif")
  )

# New create_multiplot(); now with customization for legend, explicit settings for axis-breaks on the y-axis
create_multplot <- function(data_list, title, y_label, my_theme, color_map, y_axis_limits = NULL, y_axis_breaks = NULL, info_text = "", include_legend = FALSE) {
  plot_data <- do.call(rbind, lapply(names(data_list), function(name) {
    transform(data_list[[name]], dataset = name)
  }))
  
  plot <- ggplot(plot_data, aes(x = group, y = effect, color = dataset)) +
    geom_point(position = position_dodge(width = 0.3), size = 0.8, shape = 16) +
    geom_line(position = position_dodge(width = 0.3), size = 0.3) +
    geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.3), width = 0.1, size = 0.3) +
    labs(title = title, x = "Event Time", y = y_label, caption = info_text) +
    theme_single +
    scale_x_continuous(breaks = unique(plot_data$group)) +
    geom_vline(xintercept = 0, color = "red", linetype = "solid", size = 0.3, alpha = 0.7) +
    scale_color_manual(name = "", values = color_map)
  
  # Explicitly set both limits and breaks together
  if (!is.null(y_axis_limits) && !is.null(y_axis_breaks)) {
    # Ensure breaks are within limits to avoid masking
    y_axis_breaks <- y_axis_breaks[y_axis_breaks >= min(y_axis_limits) & y_axis_breaks <= max(y_axis_limits)]
    plot <- plot + scale_y_continuous(limits = y_axis_limits, breaks = y_axis_breaks, expand = c(0, 0))
  } else if (!is.null(y_axis_breaks)) {
    plot <- plot + scale_y_continuous(breaks = y_axis_breaks, expand = c(0, 0))
  } else if (!is.null(y_axis_limits)) {
    plot <- plot + scale_y_continuous(limits = y_axis_limits, expand = c(0, 0))
  }
  
  if (!include_legend) {
    plot <- plot + theme(legend.position = "none")
  } else {
    plot <- plot + theme(
      legend.position = "bottom",
      legend.spacing.x = unit(2, "cm"),  # Increase spacing between Male and Female
      legend.margin = margin(t = 0.2, r = 0, b = 0.2, l = 0, unit = "cm"),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.key.size = unit(0.8, "cm"),  # Reduce the size of the legend key
      legend.text = element_text(margin = margin(r = 0.4, unit = "cm"))  # Add right margin to legend text to avoid them overlappig
    ) +
      guides(color = guide_legend(
        override.aes = list(
          size = 1,  # Increase the size of the dots in the legend here!
          linewidth = 0.5  # This is for the thickness of the lines next fo Male and Female
        )
      ))
  }
  
  return(plot)
}


# Set y-labels
share <- "Share"
income <- "In 2020 Euros"

# --- DESCIRPITVE ANALYSIS: AVERAGES PER EVENT-TIME ---------------------------------------------------
average_data <- data_nyt %>%
  filter(first_shock_year != 0) %>% 
  group_by(event_time) %>%
  summarize(average_hospital_staysnights = mean(hospital_staysnights, na.rm = TRUE),
            average_satisfaction_health = mean(satisfaction_health, na.rm = TRUE),
            average_health_now = mean(health_now, na.rm = TRUE),
            average_m11124 = mean(m11124, na.rm = TRUE),
            average_sickdays = mean(sickdays, na.rm = TRUE),
            average_quitjob_pequiv = mean(quitjob_pequiv, na.rm = TRUE),
            average_i11110 = mean(i11110, na.rm = TRUE),
            average_i11101 = mean(i11101, na.rm = TRUE),
            average_i11103 = mean(i11103, na.rm = TRUE),
            average_e11101_weekly = mean(e11101_weekly, na.rm = TRUE),
            average_fulltime_pequiv = mean(fulltime_pequiv, na.rm = TRUE),
            average_parttime_pequiv = mean(parttime_pequiv, na.rm = TRUE)
            
            
  ) %>% 
  ungroup()

average_partner_i11110 <- data_partner %>%
  filter(first_shock_year != 0) %>% 
  group_by(event_time) %>%
  summarize( average_i11110 = mean(i11110, na.rm = TRUE)) %>% 
  ungroup()

# LABOR FORCE PARTICIPATION
lfp <- ggplot(average_data, aes(x = event_time, y = average_quitjob_pequiv)) +
  geom_point() +  # Scatter plot
  geom_line(color = "black", position = position_dodge(width = 0.3), size = 0.3) +
  geom_vline(xintercept = 0, color = "red", linetype = "solid", linewidth = 0.4, alpha = 0.7) +
  labs(x = "Event Time", y = share,
       title = "B: Labor Force Participation") +
  theme_single  +
  scale_x_continuous(limits=c(-5, 5), breaks=seq(-5, 5, 1)) +
  scale_y_continuous(limits = c(0.6, 1), breaks = seq(0.6, 1, 0.1), expand = c(0, 0))  # no padding

plot(lfp)

# ANNUAL GROSS LABOR INCOME
i11110 <- ggplot(average_data, aes(x = event_time, y = average_i11110)) +
  geom_point() +  # Scatter plot
  geom_line(color = "black", position = position_dodge(width = 0.3), size = 0.3) +
  geom_vline(xintercept = 0, color = "red", linetype = "solid", linewidth = 0.4, alpha = 0.7) +
  labs(x = "Event Time", y = income,
       title = "A: Annual Gross Labor Income") +
  theme_single  +
  scale_x_continuous(limits=c(-5, 5), breaks=seq(-5, 5, 1)) +
  scale_y_continuous(limits = c(15000, 25000), breaks = seq(15000, 25000, 2500), expand = c(0, 0))  # no padding

plot(i11110)

# ANNUAL HOUSEHOULD INCOME PRETAX
i11101 <- ggplot(average_data, aes(x = event_time, y = average_i11101)) +
  geom_point() +  # Scatter plot
  geom_line(color = "black", position = position_dodge(width = 0.3), size = 0.3) +
  geom_vline(xintercept = 0, color = "red", linetype = "solid", linewidth = 0.4, alpha = 0.7) +
  labs(x = "Event Time", y = income,
       title = "C: Household Pre-Government Income") +
  theme_single  +
  scale_x_continuous(limits=c(-5, 5), breaks=seq(-5, 5, 1)) +
  scale_y_continuous(limits = c(40000, 60000), breaks = seq(40000, 60000, 5000), expand = c(0, 0))  # no padding

plot(i11101)

# SPOUSAL ANNUAL GROSS LABOR INCOME
i11110_partner <- ggplot(average_partner_i11110, aes(x = event_time, y = average_i11110)) +
  geom_point() +  # Scatter plot
  geom_line(color = "black", position = position_dodge(width = 0.3), size = 0.3) +
  geom_vline(xintercept = 0, color = "red", linetype = "solid", linewidth = 0.4, alpha = 0.7) +
  labs(x = "Event Time", y = income,
       title = "D: Spousal Annual Gross Labor Income") +
  theme_single  +
  scale_x_continuous(limits=c(-5, 5), breaks=seq(-5, 5, 1)) +
  scale_y_continuous(limits = c(20000, 40000), breaks = seq(20000, 40000, 5000), expand = c(0, 0))  # no padding

plot(i11110_partner)

# ANNUAL GROSS LABOR INCOME - DETRENDED
# Step 1: Remove yearly trends
detrended_data <- data %>%
  group_by(syear) %>%
  summarize(yearly_mean_i11110 = mean(i11110, na.rm = TRUE))

data_nyt <- left_join(data_nyt, detrended_data, by = "syear")  %>%
  mutate(detrended_i11110 = i11110 - yearly_mean_i11110)

# Step 2: Calculate averages per event-time
average_detrended_data <- data_nyt %>%
  group_by(event_time) %>%
  summarize(average_i11110 = mean(detrended_i11110, na.rm = TRUE)) %>%
  ungroup()

# Step 3: Create the plot
i11110_detrended <- ggplot(average_detrended_data, aes(x = event_time, y = average_i11110)) +
  geom_point() +  # Scatter plot
  geom_line(color = "black", position = position_dodge(width = 0.3), size = 0.3) +
  geom_vline(xintercept = 0, color = "red", linetype = "solid", linewidth = 0.4, alpha = 0.7) +
  labs(x = "Event Time", y = income, 
       title = "B: Annual Gross Labor Income - Detrended") +
  theme_single  +
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 1)) +
  scale_y_continuous(limits = c(1000, 5000), breaks = seq(1000, 5000, 1000), expand = c(0, 0))  # no padding

print(i11110_detrended)


final_plot_asp <- cowplot::plot_grid(
  plot_grid(i11110, lfp, nrow = 1, ncol = 2),
  plot_grid(i11101, i11110_partner, nrow = 1, ncol = 2),
  nrow = 2
)


print(final_plot_asp)

# Save the combined plot
setwd(results)
ggsave(file.path(paste0("desc_hospitalizations_treated_1s", ".pdf")), 
       final_plot_asp, width = 10, height = 10, device=cairo_pdf)



# --- EMPIRICAL ANALYSIS ---------------------------------------------------

levels <- c(0.01, 0.05, 0.1)

# Specify the datasets and variables for main and partner analyses
analysis_settings <- list(
  
  quitjob_pequiv = list(
    var = "quitjob_pequiv",
    data = data_nyt
  ),
  
  i11110 = list(
    var = "i11110",
    data = data_nyt
  ),
  
  i11101 = list(
    var = "i11101",
    data = data_nyt
  ),
  
  i11110_partner = list(
    var = "i11110",
    data = data_partner
  )
)

# Initialize an empty list to store results for all variables and significance levels
Hospitalizations <- list()

# Use future_map to iterate over analysis types in parallel
results_list <- future_map(names(analysis_settings), ~{
  type <- .x
  current_setting <- analysis_settings[[type]]
  current_variable <- current_setting$var
  current_data <- current_setting$data
  
  # Initialize a list to store results for the current analysis type
  analysis_results <- list()
  
  es_01 <- NULL
  es_05 <- NULL
  es_10 <- NULL
  
  # Run the analysis for each significance level
  for (l in levels) {
    result <- cs_fun(current_variable, current_data, l)
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
  table_name <- paste0(type, "_", current_variable)
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
  analysis_results
}, .progress = TRUE) 

# Combine results from all analyses into a single list
Hospitalizations <- do.call(c, results_list)


# --- ESTIMATE ATTs ---------------------------------------------------

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
  
  # Create the info_text string using paste0 to avoid unnecessary spaces
  info_text <- paste0("ATT: ", att_rounded_with_stars, " \n (", attse_rounded, ")")
  #info_text <- paste0("\n", "ATT: ", att_rounded_with_stars, "  \n (", attse_rounded, ")")
  
  return(info_text)
}

# Apply this adjusted function to each table in OtherTreatment and collect the results
info_texts <- lapply(Hospitalizations, create_info_text)

# Name the elements of info_texts for easy reference
names(info_texts) <- names(Hospitalizations)

# Print or use info_texts as needed
info_texts


# --- GENERATE PLOTS ---------------------------------------------------

data_list_hosp <- list(
  `Annual Gross Labor Income` = Hospitalizations$i11110_i11110
)

plot_i11110 <- create_multplot(
  data_list = data_list_hosp, 
  title = "A: Annual Gross Labor Income", 
  y_label = income, 
  my_theme = theme_single, 
  color_map = c(`Annual Gross Labor Income` = "black"),
  y_axis_limits = c(-6000, 4000), 
  y_axis_breaks = seq(-6000, 4000, by = 2000),  
  info_text = info_texts$i11110_i11110
  
)

print(plot_i11110)


data_list_hosp <- list(
  `Labor Force Participation` = Hospitalizations$quitjob_pequiv_quitjob_pequiv
)

plot_pequiv  <- create_multplot(
  data_list = data_list_hosp, 
  title = "B: Labor Force Participation", 
  y_label = share, 
  my_theme = theme_single, 
  color_map = c(`Labor Force Participation` = "black"),
  y_axis_limits = c(-0.4, 0.1),
  y_axis_breaks = seq(-0.4, 0.1, by = 0.1), 
  info_text = info_texts$quitjob_pequiv_quitjob_pequiv
  
)

print(plot_pequiv)


data_list_hosp <- list(
  `Household Pre-Government Income` = Hospitalizations$i11101_i11101
)

plot_i11101  <- create_multplot(
  data_list = data_list_hosp, 
  title = "C: Household Pre-Government Income", 
  y_label = share, 
  my_theme = theme_single, 
  color_map = c(`Household Pre-Government Income` = "black"),
  y_axis_limits = c(-6000, 4000), 
  y_axis_breaks = seq(-6000, 4000, by = 2000),  
  info_text = info_texts$i11101_i11101
  
)

print(plot_i11101)


data_list_hosp <- list(
  `Spousal Annual Gross Labor Income` = Hospitalizations$i11110_partner_i11110
)

plot_partner_i11110  <- create_multplot(
  data_list = data_list_hosp, 
  title = "D: Spousal Annual Gross Labor Income", 
  y_label = share, 
  my_theme = theme_single, 
  color_map = c(`Spousal Annual Gross Labor Income` = "black"),
  y_axis_limits = c(-4000, 6000), 
  y_axis_breaks = seq(-4000, 6000, by = 2000),  
  info_text = info_texts$i11110_partner_i11110
  
)

print(plot_partner_i11110)

#Combine the plots with the legend
final_plot_asp <- cowplot::plot_grid(
  plot_i11110, plot_pequiv, plot_i11101, plot_partner_i11110,
  ncol = 2,
  align = 'vh',
  labels = c("", ""),
  rel_heights = c(1)
)

print(final_plot_asp)

# Save the combined plot
setwd(results)
ggsave(file.path(paste0("cs_hospitalizations_treated_1s", ".pdf")), 
       final_plot_asp, width = 10, height = 10, device=cairo_pdf)


#_________________________________________________________________________________#
# WRITE TABLES
#---------------------------------------------------------------------------------#

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
var_list_treated <- list( "Annual gross labor income" = Hospitalizations$i11110_i11110,
                          "Labor force participation" = Hospitalizations$quitjob_pequiv_quitjob_pequiv,
                          "Household Pre-Government Income" = Hospitalizations$i11101_i11101,
                          "Spousal Annual Gross Labor Income" = Hospitalizations$i11110_partner_i11110)
generate_table(var_list_treated)








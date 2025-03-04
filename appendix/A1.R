#_________________________________________________________________________________#
# The Impacts of Health Shocks on Household Labor Supply and Domestic Production
# Di Meo & Eryilmaz, 2025	 	 	 	 	 	 	 	 	   
#_________________________________________________________________________________#

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


setwd(datasets)

data <- read_dta("R_soeponly.dta")

data_female <- data %>% filter(., gender == 1)


# Set # of cores to be used for parallelization
plan(multisession, workers = future::availableCores() - 1) # leave one for other system tasks

# Define time window for the event study plot, confidence levels
span_top <- 5
span_bottom <- 5

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

cs_fun <- function(ant, alpha) {
  
  set.seed(1215) # Setting seed in the function. This should prevent us generating results w/o explicitly setting the right seed. 
  
  # Estimate the ATT(g,t)
  result <- att_gt(yname = "quitjob_pequiv",
                   gname = "shock",
                   idname = "pid",
                   tname = "syear",
                   est_method = "ipw",
                   control_group = "notyettreated",
                   xformla =  ~ age + age_sq,
                   panel = TRUE,
                   allow_unbalanced_panel = TRUE,
                   anticipation = ant,
                   pl = FALSE,
                   cores = 12,
                   alp = alpha,
                   base_period = "universal",
                   data = data_female)
  
  # Aggregate to ES coefficients
  es <- aggte(result, type = "dynamic", na.rm = TRUE, min_e = -span_bottom, max_e = span_top)
  
  return(list(result = result, es = es))
}

#_________________________________________________________________________________#
# ESTIMATE
#---------------------------------------------------------------------------------#
levels <- c(0.01, 0.05, 0.1)

# Specify the datasets and variables for main and partner analyses
analysis_settings <- list(

  noant = list(
    anticipation = 0 
  ),
  
  twoant = list(
    anticipation = 2 
  )
)


# Initialize an empty list to store results for all variables and significance levels
MeanReversionFemale <- list()

# Use future_map to iterate over analysis types in parallel
results_list <- future_map(names(analysis_settings), ~{
  type <- .x
  current_setting <- analysis_settings[[type]]
  current_anticipation <- current_setting$anticipation

  # Initialize a list to store results for the current analysis type
  analysis_results <- list()
  
  es_01 <- NULL
  es_05 <- NULL
  es_10 <- NULL
  
  # Run the analysis for each significance level
  for (l in levels) {
    result <- cs_fun(current_anticipation, l)
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
  table_name <- paste0(type, "_", current_anticipation)
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
MeanReversionFemale <- do.call(c, results_list)


#_________________________________________________________________________________#
# ESTIMATE ATTs
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
  
  # Create the info_text string using paste0 to avoid unnecessary spaces
  info_text <- paste0("ATT: ", att_rounded_with_stars, " \n (", attse_rounded, ")")
  #info_text <- paste0("\n", "ATT: ", att_rounded_with_stars, "  \n (", attse_rounded, ")")
  
  return(info_text)
}

# Apply this adjusted function to each table in MeanReversionFemale and collect the results
info_texts <- lapply(MeanReversionFemale, create_info_text)

# Name the elements of info_texts for easy reference
names(info_texts) <- names(MeanReversionFemale)

# Print or use info_texts as needed
info_texts


#_________________________________________________________________________________#
# GENERATE PLOT
#---------------------------------------------------------------------------------#

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
#-----------------------------------------------------------------------
share <- "Share"

# IndividualLFP
#---------------------------------------------------------

data_list_quitjob_noant <- list(
  `No Anticipation` = MeanReversionFemale$noant_0
)

plot_noant <- create_multplot(
  data_list = data_list_quitjob_noant, 
  title = "A: Reference period -1", 
  y_label = share, 
  my_theme = theme_single, 
  color_map = c(`No Anticipation` = "black"),
  y_axis_limits = c(-0.4, 0.1),
  y_axis_breaks = seq(-0.4, 0.1, by = 0.1), 
  info_text = info_texts$noant_0
)

print(plot_noant)


data_list_quitjob_twoant <- list(
  `Two Anticipation` = MeanReversionFemale$twoant_2
)

plot_twoant <- create_multplot(
  data_list = data_list_quitjob_twoant, 
  title = "B: Reference period -3", 
  y_label = share, 
  my_theme = theme_single, 
  color_map = c(`Two Anticipation` = "black"),
  y_axis_limits = c(-0.4, 0.1),
  y_axis_breaks = seq(-0.4, 0.1, by = 0.1), 
  info_text = info_texts$twoant_2
)

print(plot_twoant)



# Combine the plots with the legend
final_plot_asp <- cowplot::plot_grid(
  plot_noant, plot_twoant,
  ncol = 2,
  align = 'vh',
  labels = c("", ""),
  rel_heights = c(1)
)

print(final_plot_asp)

# Save the combined plot
setwd(results)
ggsave(file.path(paste0("quitjob_women_anticipatoin", ".pdf")), 
       final_plot_asp, width = 10, height = 5, device=cairo_pdf)


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
                             `-1` = df$effect[5],
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
                           `-1` = paste0("(", myround(df$se[5]), ")"),
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
  columns = c("Dependent variable","-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3", "4", "5", "Overall", "Unique obs.") 
  tab_final <- data.frame(matrix(nrow = 0, ncol = length(columns))) 
  colnames(tab_final) = columns
  
  for (v in 1:length(list)) {
    print(names(list[v]))
    tab_temp <- tab_function(list[[v]], names(list[v]))
    tab_final <- rbind(tab_final, tab_temp)
  }
  
  print(xtable(tab_final, type = "latex", align = "lcccccccccccccc"),
        #file = file.path(directory, "tot_treated.tex")),
        include.rownames=FALSE)
  print(tab_final)
}


var_list_treated <- list( "Labor force participation -- Reference period $-1$" = MeanReversionFemale$noant_0,
                          "Labor force participation -- Reference period $-3$" = MeanReversionFemale$twoant_2)
generate_table(var_list_treated)


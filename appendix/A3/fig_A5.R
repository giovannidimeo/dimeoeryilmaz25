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
library(patchwork)

# DEFINE DIRECTORY WHERE DATA ARE STORED
datasets <- ("C:/Users/gdimeo/polybox/Shared/16_DiMeo_Eryilmaz/03_datasets")
setwd(datasets)

# LOAD THE THEME
data <- read_dta("R_soeponly.dta")

setwd(results)

# Set y-labels
#-----------------------------------------------------------------------
income <- "In 2020 Euros"
hours <- "Hours"
hoursweekdays <- "Hours on Weekdays"
share <- "Share"
zscore <- "z-score"

#-----------------------------------------------------------------------

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

#-----------------------------------------------------------------------
# Overall trends (no separation by gender)
average_data_overall_1 <- data %>%
  filter(shock != 0) %>% 
  group_by(syear) %>%
  summarize(
    average_quitjob_pequiv = mean(quitjob_pequiv, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(data_source = "data", gender = "Overall")

# Gender-specific trends
average_data_by_gender_1 <- data %>%
  filter(shock != 0) %>% 
  group_by(syear, gender) %>%
  summarize(
    average_quitjob_pequiv = mean(quitjob_pequiv, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(data_source = "data", gender = ifelse(gender == 1, "Female", "Male"))


# Combine all data
combined_data <- bind_rows(
  average_data_overall_1,
  average_data_by_gender_1
)

lfp_plot_for_appendix <- ggplot(combined_data, aes(
  x = syear, 
  y = average_quitjob_pequiv, 
  color = gender
)) +
  geom_line(position = position_dodge(width = 0.3), size = 0.3) +
  geom_point(position = position_dodge(width = 0.3), size = 0.8, shape = 16) +
  geom_vline(xintercept = 0, color = "red", linetype = "solid", size = 0.4, alpha = 0.7) +
  labs(
    x = "Year", 
    y = "Share", 
    title = ""
  ) +
  # Custom colors for genders
  scale_color_manual(
    values = c("Overall" = "black", "Female" = "darkgray", "Male" = "#69b3a2"),
    name = NULL
  ) +
  theme_single +
  scale_x_continuous(limits = c(1984, 2020), breaks = seq(1984, 2020, 4)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), expand = c(0, 0)) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.spacing.x = unit(2, "cm"),
    legend.margin = margin(t = 0.2, r = 0, b = 0.2, l = 0, unit = "cm"),
    legend.key.size = unit(0.8, "cm"),
    legend.text = element_text(margin = margin(r = 0.4, unit = "cm"))
  )

plot(lfp_plot_for_appendix)


# Save the plot
ggsave(file.path(paste0("lfp_over_time_for_appendix", ".pdf")), lfp_plot_for_appendix, width = 5, height = 5, device=cairo_pdf)



# === TRAJECTORY OF SHOCKS OVER CALENDAR YEARS ==================================================

filtered_data2 <- data %>% 
  select(pid, shock, syear, sickdays) %>% 
  filter(shock != 0, 
         syear != 1989 & syear != 1992) %>% 
  mutate(shock_binary = ifelse(syear == shock, 1, 0))


# Summarize raw shocks and population growth
shocks_population_data <- filtered_data2 %>%
  filter(syear != 1989 & syear != 1992) %>% # to remove the gaps, as this looks weird (visually)
  group_by(syear) %>%
  summarize(
    total_shocks = sum(shock_binary, na.rm = TRUE),
    total_population = n_distinct(pid),             
    .groups = "drop"
  )



# Plot raw shocks with line for "population growth"
shocks_population_plot <- ggplot(shocks_population_data, aes(x = factor(syear))) +
  # Shocks
  geom_bar(aes(y = total_shocks), stat = "identity", fill = "black", position = "dodge") +
  # Simple line for "population growth"
  geom_line(aes(y = total_population / max(total_population) * max(total_shocks), group = 1, color = "red"), size = 1) +
  # Scale y-axis
  scale_y_continuous(
    name = "Count of Shocks",
    sec.axis = sec_axis(~ . * max(shocks_population_data$total_population) / max(shocks_population_data$total_shocks), 
                        name = "Total Population")
  ) +
  labs(
    x = "Year", 
    title = "A: Frequency of Health Shocks"
  ) +
  theme_single +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  guides(color = "none") +
  scale_x_discrete(breaks = c("1984", "1990", "1995", "2000", "2005", "2010", "2015", "2020"))

print(shocks_population_plot)


# Pool cohorts into groups of 3
average_data_pooled <- data %>%
  filter(event_time >= -5 & event_time <= 5) %>%
  mutate(pooled_cohort = (shock - min(shock)) %/% 3 + 1) %>%  # Group cohorts into bins of 3
  group_by(pooled_cohort, event_time) %>%
  summarize(
    average_hospital_staysnights = mean(hospital_staysnights, na.rm = TRUE),
    average_satisfaction_health = mean(satisfaction_health, na.rm = TRUE),
    average_health_now = mean(health_now, na.rm = TRUE),
    average_sickdays= mean(sickdays, na.rm = TRUE),
    
    
    .groups = "drop"
  )

# Label pooled cohorts with shock ranges
pooled_labels <- data %>%
  mutate(pooled_cohort = (shock - min(shock)) %/% 3 + 1) %>%
  group_by(pooled_cohort) %>%
  summarize(
    label = paste0(min(shock), "-", max(shock)),
    .groups = "drop"
  )

# Join labels with pooled data
average_data_pooled <- average_data_pooled %>%
  left_join(pooled_labels, by = "pooled_cohort")



average_data <- data %>%
  filter(event_time>=-5 & event_time<=5) %>%
  group_by(event_time) %>%
  summarize(average_hospital_staysnights = mean(hospital_staysnights, na.rm = TRUE),
            average_satisfaction_health = mean(satisfaction_health, na.rm = TRUE),
            average_health_now = mean(health_now, na.rm = TRUE),
            average_m11124 = mean(m11124, na.rm = TRUE),
            average_sickdays = mean(sickdays, na.rm = TRUE)
  )



# Plot pooled cohorts with overall average as reference
host_pooled <- ggplot(average_data_pooled, aes(x = event_time, y = average_hospital_staysnights, color = label)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  # Add overall average as reference
  geom_line(data = average_data, aes(x = event_time, y = average_hospital_staysnights), color = "black", size = 0.5, inherit.aes = FALSE) +
  geom_point(data = average_data, aes(x = event_time, y = average_hospital_staysnights), shape = 15, size = 1, color = "black", inherit.aes = FALSE) +
  geom_vline(xintercept = 0, color = "red", linetype = "solid", size = 0.4, alpha = 0.7) +
  labs(
    x = "Event Time", 
    y = "Average Per Event Time", 
    title = "C: Overnight Hospital Stays",
    color = "Cohort Range"
  ) +
  theme_single +
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 1)) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 2), expand = c(0, 0)) + # no padding
  theme(legend.position = "none")

print(host_pooled)


# Plot satisfaction with health
satisfaction_health <- ggplot(average_data_pooled, aes(x = event_time, y = average_satisfaction_health, color = label)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  # Add overall average as reference
  geom_line(data = average_data, aes(x = event_time, y = average_satisfaction_health), color = "black", size = 0.5, inherit.aes = FALSE) +
  geom_point(data = average_data, aes(x = event_time, y = average_satisfaction_health), shape = 15, size = 1, color = "black", inherit.aes = FALSE) +
  geom_vline(xintercept = 0, color = "red", linetype = "solid", size = 0.4, alpha = 0.7) +
  labs(
    x = "Event Time", 
    y = "Average Z-Score Per Event Time", 
    title = "D: Satisfaction with Own Health",
    color = "Cohort Range"
  ) +
  theme_single +
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 1)) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5), expand = c(0, 0)) +
  theme(legend.position = "none")

print(satisfaction_health)



# Plot subjective health
health_now_legend <- ggplot(average_data_pooled, aes(x = event_time, y = average_health_now, color = label)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  # Add overall average as reference
  geom_line(data = average_data, aes(x = event_time, y = average_health_now), color = "black", size = 0.5, inherit.aes = FALSE) +
  geom_point(data = average_data, aes(x = event_time, y = average_health_now), shape = 15, size = 1, color = "black", inherit.aes = FALSE) +
  geom_vline(xintercept = 0, color = "red", linetype = "solid", size = 0.4, alpha = 0.7) +
  labs(
    x = "Event Time", 
    y = "Average Z-Score Per Event Time", 
    title = "D: Subjective Health ",
    color = "Cohort Range"
  ) +
  theme_single +
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 1)) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5), expand = c(0, 0)) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.direction = "horizontal",
    legend.title = element_blank() 
  ) 


print(health_now_legend)


# Plot subjective health
health_now <- ggplot(average_data_pooled, aes(x = event_time, y = average_health_now, color = label)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  # Add overall average as reference
  geom_line(data = average_data, aes(x = event_time, y = average_health_now), color = "black", size = 0.5, inherit.aes = FALSE) +
  geom_point(data = average_data, aes(x = event_time, y = average_health_now), shape = 15, size = 1, color = "black", inherit.aes = FALSE) +
  geom_vline(xintercept = 0, color = "red", linetype = "solid", size = 0.4, alpha = 0.7) +
  labs(
    x = "Event Time", 
    y = "Average Z-Score Per Event Time", 
    title = "D: Subjective Health ",
    color = "Cohort Range"
  ) +
  theme_single +
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 1)) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5), expand = c(0, 0)) +
  theme(legend.position = "none")


print(health_now)


# Plot sickdays
sickdays <- ggplot(average_data_pooled, aes(x = event_time, y = average_sickdays, color = label)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  # Add overall average as reference
  geom_line(data = average_data, aes(x = event_time, y = average_sickdays), color = "black", size = 0.5, inherit.aes = FALSE) +
  geom_point(data = average_data, aes(x = event_time, y = average_sickdays), shape = 15, size = 1, color = "black", inherit.aes = FALSE) +
  geom_vline(xintercept = 0, color = "red", linetype = "solid", size = 0.4, alpha = 0.7) +
  labs(
    x = "Event Time", 
    y = "Average Per Event Time", 
    title = "B: Days on Sick Leave",
    color = "Cohort Range"
  ) +
  theme_single +
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 1)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25), expand = c(0, 0)) +
  theme(legend.position = "none")


print(sickdays)


# Extract legend
legend_plot <- cowplot::get_plot_component(health_now_legend, "guide-box", return_all = TRUE)[[3]]

# Combine the plots with the legend
final_plot <- cowplot::plot_grid(
  shocks_population_plot, sickdays, host_pooled, satisfaction_health,
  ncol = 2,
  align = 'vh',
  labels = c("", "", "", "", "", ""),
  rel_heights = c(1, 1, 1)
)

print(final_plot)

# Combine the plots with the legend
final_plot_with_legend <- cowplot::plot_grid(
  final_plot,
  legend_plot,
  ncol = 1,
  rel_heights = c(1, 0.1)
)


print(final_plot_with_legend)

# Save the combined plot explicitly
ggsave(file.path("changes_over_time.pdf"), plot = final_plot_with_legend, width = 12, height = 10, device=cairo_pdf)



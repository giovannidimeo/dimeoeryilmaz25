#_________________________________________________________________________________#
# The Impacts of Health Shocks on Household Labor Supply and Domestic Production
# Di Meo & Eryilmaz, 2025	 	 	 	 	 	 	 	 	 	   
#_________________________________________________________________________________#

# SET DIRECTORY WHERE RESULTS ARE STORED
setwd(results)

#LOAD RESULTS IF NOT ALREADY LOADED
load("AllResults.RData")


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

  info_text <- paste0("\n", "ATT: ", att_rounded_with_stars, "  \n (", attse_rounded, ")")
  
  return(info_text)
}

# Apply this adjusted function to each table in MainResults_Megaloop and collect the results
info_texts <- lapply(MainResults_Megaloop, create_info_text)

# Name the elements of info_texts for easy reference
names(info_texts) <- names(MainResults_Megaloop)

# Print or use info_texts as needed
info_texts

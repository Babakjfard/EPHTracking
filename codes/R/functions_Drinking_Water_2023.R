# ==================== University of Nebraska Medical Center (UNMC) ===========
# ========================= College of Public Health (COPH) ===================
# ==================== Water, Climate, and Health (WCH) program ===============
# ======================== Babak J.Fard, Jun 2023 =============================

# Disclaimer
# This code was written for the prepartation of the Water Summary product for
# CDC Tracking Spring 2023 Data Call. All responsibilities for using this code is 
# upon the user. 
#---------
library(tidyverse)
library(lubridate)
library(ggplot2)

`%notin%` <- Negate(`%in%`)
Analytes <- tibble(Name=c('Nitrate', 'Atrazine', 'DEHP', 'PCE', 'TCE', 'Arsenic',
                              'TTHM', 'HAA5', 'Combined Radium', 'Uranium'),
                       code=c(1040, 2050, 2039, 2987, 2984, 1005, 2950, 2456, 4010, 4006))

# Initial required columns to do the summarization
required_columns <- c("PWSIDNumber", "Year", "AnalyteCode", "ConcentrationUnits",
                      "Concentration",  "DateSampled", "SamplePointID")


# This is to check
# Function to calculate the averages over each CWS. This is common
# in both methods suggested by HTG 2023, as the second step in averaging
# Parameters:
#   - sampling: Input dataframe containing the sampling data (that are averaged in the first step)
#   - Analytes: List of 4-digit integer values of analytes to filter
# Returns:
#   - Dataframe containing the summary for the specified Analytes

calculate_summary_by_CWS <- function(sampling){

    summary_mean <- sampling %>%
    group_by(PWSIDNumber, SummaryTimePeriod, AnalyteCode) %>%
    summarise(Year= unique(Year),
              DateSampled = max(DateSampled),
              AggregationType = 'X',
              Concentration = mean(Concentration),
              ConcentrationUnits = unique(ConcentrationUnits),
              NumSamplingLocations = sum(NumSamplingLocations),
              SummaryTimePeriod = unique(SummaryTimePeriod),
              NumSamples = sum(NumSamples),
              NumNonDetects = sum(NumNonDetects))
  return(summary_mean)

}


# Function to calculate annual summary for specified Analytes
# It follows HTG 2023 for Drinking Water Special note for Steps II.b.1 & II.b.2
# This is for the 8 analytes that first need to be averaged over each sampling station
# Parameters:
#   - sampling: Input dataframe containing the sampling data
# Returns:
#   - Dataframe containing the annual summary for the specified Analytes
calculate_summary_stations_first <- function(sampling) {
  # Filter sampling based on Analytes
  filtered_sampling <- sampling %>%
    filter(AnalyteCode %notin% c(2456, 2950))
  
  # Step 1: Average concentration values by sampling station
  annual_mean_station <- filtered_sampling %>%
    group_by(PWSIDNumber, SummaryTimePeriod, AnalyteCode, SamplePointID) %>%
    summarise(DateSampled = max(DateSampled),
              Year= unique(Year),
              AggregationType = 'X',
              Concentration = mean(Concentration),
              ConcentrationUnits = unique(ConcentrationUnits),
              NumSamplingLocations = length(unique(SamplePointID)),
              SummaryTimePeriod = unique(SummaryTimePeriod),
              NumSamples = sum(NumSamples),
              NumNonDetects = sum(NumNonDetects))
  return(annual_mean_station)
}


# Function to calculate annual summary for disinfection-by-products (TTHM and HAA5)
# It follows HTG 2023 for Drinking Water Special note for Steps II.b.1 & II.b.2
# First need to be averaged by day
# Parameters:
#   - sampling: Input dataframe containing the sampling data
# Returns:
#   - Dataframe containing the annual summary for the specified Analytes

calculate_summary_days_first <- function(sampling) {
  # For disinfection-by-products (TTHM: 2950 and HAA5:2456) annual and quarterly
  # average concentration values are derived from first averaging by day, 
  
  # Filter sampling based on Analytes
  filtered_sampling <- sampling %>%
    filter(AnalyteCode %in% c(2456, 2950))
  
  annual_mean_disinfection <- filtered_sampling %>%
    group_by(PWSIDNumber, SummaryTimePeriod, AnalyteCode, DateSampled) %>%
    summarise(Year=unique(Year),
              AggregationType ='X', 
              Concentration=mean(Concentration), 
              ConcentrationUnits=unique(ConcentrationUnits),
              NumSamplingLocations= length(unique(SamplePointID)),
              SummaryTimePeriod = unique(SummaryTimePeriod),
              NumSamples =sum(NumSamples), 
              NumNonDetects = sum(NumNonDetects))
  
  # then by CWS
  
}

# 3- Annual maximum for all 10 analytes
# Maximums for all 10 analytes are derived by taking the annual maximum for each CWS
# Following HTG 2023
annual_summary_max <- function(sampling){
  
  summary_max <- sampling %>% group_by(PWSIDNumber,SummaryTimePeriod, AnalyteCode) %>% 
  summarise(Year=unique(Year),
            DateSampled=max(DateSampled),
            AggregationType ='MX',
            Concentration=max(Concentration),
            ConcentrationUnits=unique(ConcentrationUnits),
            NumSamplingLocations= length(unique(SamplePointID)), 
            SummaryTimePeriod = unique(SummaryTimePeriod),
            NumSamples =sum(NumSamples),
            NumNonDetects = sum(NumNonDetects))
  return(summary_max)
}


# Function to extract quarters(Winter, Spring, Summer, and Fall) from a dataframe
# Parameters:
#   - theDataframe: Input dataframe containing the data
# Returns:
#   - Dataframe with added SummaryTimePeriod columns (in the format suggested 
#       by HTG-2023), and removed theYear
extract_Quarters <- function(theDataframe) {
  theDataframe %>%
    mutate(
      theYear = year(DateSampled),  # Extract the year from DateSampled
      Quarter = case_when(  # Assign the corresponding quarter based on the month of DateSampled
        month(DateSampled) %in% 1:3 ~ "1",
        month(DateSampled) %in% 4:6 ~ "2",
        month(DateSampled) %in% 7:9 ~ "3",
        month(DateSampled) %in% 10:12 ~ "4"
      ),
      SummaryTimePeriod = paste(theYear, Quarter, sep = "-")  # Create SummaryTimePeriod by combining theYear and Quarter
    ) %>%
    select(-c(theYear, Quarter))  # Remove theYear and Quarter columns from the dataframe
}


uranium_units_change <- function(theDataframe){
  units_check <- unique(theDataframe[c('AnalyteCode', 'ConcentrationUnits')])
  
  if (4006 %in% units_check$AnalyteCode){
    if (units_check[units_check$AnalyteCode==4006, "ConcentrationUnits"] == 'pci/l'){
      
      theDataframe[theDataframe$AnalyteCode==4006, ] <- theDataframe %>% filter(AnalyteCode==4006) %>%
        mutate(Concentration = Concentration*1.49, ConcentrationUnits='ug/l')
   
    }
  }
  return(theDataframe)
}

# Followed Appendix G of HTG 2023, checked the Concentration values against their
# Lowest Detection Limit (LDL) and replaced with the suggested values where lower

apply_half_LDL <- function(df) {
  df_new <- df %>%
    mutate(Concentration = case_when(
      AnalyteCode == 1005 & Concentration < 0.02  ~ 0.01,
      AnalyteCode == 2050 & Concentration < 0.003 ~ 0.002,
      AnalyteCode == 2456 & Concentration < 0.5   ~ 0.25,
      AnalyteCode == 2950 & Concentration < 0.01  ~ 0.005,
      AnalyteCode == 2039 & Concentration < 0.46  ~ 0.23,
      AnalyteCode == 1040 & Concentration < 0.002 ~ 0.001,
      AnalyteCode == 2987 & Concentration < 0.01  ~ 0.005,
      AnalyteCode == 2984 & Concentration < 0.002 ~ 0.001,
      AnalyteCode == 4010 & Concentration < 0.03  ~ 0.02,
      AnalyteCode == 4006 & Concentration < 0.001 ~ 0.0005,
      TRUE ~ Concentration
    ))
  
  num_rows_changed <- sum(df_new$Concentration != df$Concentration)
  
  return(list(df_new, num_rows_changed))
}

# Averaged duplicates into one values. Checked for duplicated of same analyte
# sampled in the same day from same point location, and averaged them into one
# value. It contained repeatitions for values in columns (PWSIDNumber, Year,
# AnalyteCode, DateSampled, SamplePointID). Added a new column "NumSamples" that 
# can account for these number of samples. (The highest was 28 for one analyte 
# sampling in the same point in the same date)

average_duplicates <- function(df){
  sampling_dps <- df %>%
    group_by(PWSIDNumber, Year, AnalyteCode, DateSampled, SamplePointID) %>%
    summarize(ConcentrationUnits= unique(ConcentrationUnits), 
              NumSamples=n(), 
              NumNonDetects=sum(NonDetectFlag), 
              Concentration=mean(Concentration),
              .groups = "drop")
    return(sampling_dps)
}


# Example dataset
# Create accordion plots for the analytes
plot_concentrations <- function(theData){
  the_plot <- ggplot(theData, aes(x = AnalyteCode, y = Concentration)) +
    geom_violin(lwd=0.5, draw_quantiles = c(0.25, 0.5, 0.75), scale = "width") +
    geom_boxplot(lwd=1, width = 0.3, fill = "white", outlier.shape = NA) +
    facet_wrap(~ AnalyteCode, ncol = 3, scales = "free") +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = 12, face = "bold"),
          axis.text.x = element_blank()
    )
  return(the_plot)
  
}
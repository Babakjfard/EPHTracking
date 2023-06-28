###################################################################################################
#
# Title: State Level NCDM Development for Drinking Water . Following HTG-2023 section III
# Nebraska Environmental Public Health Tracking (EPHT) Program.
# Date Created: 6/28/2023
# Description: Following HTG 2023 part III.
#             (This set of processing steps further summarizes the CWS-level measures (calculated in 
#               Step II) into statewide frequencies of water systems and summed population-served by 
#               analyte-specific concentration categories. The data produced in this step are not 
#               submitted to CDC.)
###################################################################################################

######################### INSTRUCTIONS FOR USE ####################################################
#
# 1. Goto section "Adjustable Code"
#
# 2. Update the address of the Water Quality Summary file 
#    
# 3. Update the address of the Water Systems information file
#    
# 3. Update the adresses of the output csv files
#    
# 4. Run the code
#    
# 5. The output files will be saved in the determined locations, and you can save the plots.
#    
#
###################################################################################################

###################################################################################################


library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)

# vvv Functions =====================================================================
#
addCategory <- function(WQS, standards) {
  #' Add Category to WQS dataframe based on standards
  #'
  #' This function merges the summarized WQS dataframe with the Analyte standards.
  #' and adds a Category column to the merged dataframe. The Category is determined by
  #' comparing Concentration with LDL and MCL values from the standards.
  #'
  #' @param WQS The input dataframe containing AnalyteCode and Concentration columns.
  #' @param standards The standards table containing AnalyteCode, LDL, and MCL columns.
  #'
  #' @return The merged dataframe with an additional Category column.
  #' @import dplyr
  
  merged <- merge(WQS, standards, by = "AnalyteCode", all.x = TRUE)
  merged$Category <- ifelse(merged$Concentration < merged$LDL, "No_detect",
                            ifelse(merged$Concentration <= merged$MCL, "below_MCL", "above_MCL"))
  return(merged)
}



# ======= Plot function

plot_timeseries_barplot <- function(data) {
  #' Plot Timeseries Barplot
  #'
  #' This function generates a timeseries bar plot that shows each percentage of the contaminant.
  #'
  #' @param data The dataframe containing the required columns.
  #'             The dataframe must contain columns 'above_MCL', 'below_MCL', 'No_detect' with this
  #'             same naming and order.
  #'             The dataframe is usually the output of addCategory() function
  #'
  #' @return A bar plot visualizing the timeseries data.
  #'
  #' @import ggplot2
  #' @import tidyr
  #' @importFrom dplyr gather
  #' @importFrom ggplot2 ggplot, geom_bar, labs, theme_minimal, facet_grid
  #'
  #' @export
  # Reshape the data to long format
  data_long <- gather(data, key = Category, value = Percentage, above_MCL:No_detect)
  
  # Create the bar plot
  ggplot(data_long, aes(x = SummaryTimePeriod, y = Percentage, fill = Category)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_grid(AnalyteCode ~ AggregationType, scales = "free_y", space = "free") +
    labs(x = "Year", y = "Percentage", fill = "Category") +
    theme_minimal()
}

# ^^^ End of functions ======================================================================

# vvv Adjustable Code =====================================================================

WQS_file <- 'Data/Water_Data/Summaries_Calculated_20230505.csv'                    # the address of the Water quality summarized file
PWS_file <- 'Data/Water_Data/PWSInventory.csv'                                     # the address of the Water Systems information file
analytes_standards <- 'numbers/analytes_2023.csv'                                  # the address of the csv file containing standards for the analytes
output_PWS_percentage <- 'Data/Water_Data/NCDM_state_level_2.csv'                    # the address of the output csv file for percent of CWS by category
output_population_affected <- 'Data/Water_Data/NCDM_state_level_population.csv'    # the address of the output csv file for affected population by category

WQS <- read_csv(WQS_file)
PWS <- read_csv(PWS_file)
analytes <- read_csv(analytes_standards)

# Converting MCL for Uranium from pci/l to ug/L (Because it was what supposed to 
# do during data preparation)
analytes[analytes['AnalyteCode']==4010, 'MCL'] <- analytes[analytes['AnalyteCode']==4010, 'MCL'] * 1.49
analytes[analytes['AnalyteCode']==4010, 'LDL'] <- analytes[analytes['AnalyteCode']==4010, 'LDL'] * 1.49

# Categorizing each concentration values based on the suggested LDL and MCL from CDC Tracking
categorized_WQS <- addCategory(WQS, analytes)


# ========== Part 1: Calculating the number of CWS in each of the three categories for each analyte
# Now calculating annual percent of CWS in each category
annual_CWS_category <- categorized_WQS %>% 
  group_by(AnalyteCode, SummaryTimePeriod, AggregationType, Category) %>%
  summarise(Total= n()) %>% 
  pivot_wider(names_from = Category, values_from = Total, values_fill = 0) %>%
  mutate(Total = above_MCL+ below_MCL + No_detect) %>%
  mutate(across(c(above_MCL, below_MCL, No_detect), ~ round(. *100/ Total, 2)))

write_csv(annual_CWS_category, output_PWS_percentage)

# plot barplots 
thePlot <- plot_timeseries_barplot(annual_CWS_category) # For better result separate the dataset and show in different plots
thePlot

# ========== Part 2: Calculating the affected populations served by CWS in each of the three categories for each analyte
# Calculating the percentage of population in each category
categorized_WQS_with_population <- categorized_WQS %>% filter(Year %in% unique(PWS$YearAssociatedTo))

# It returned zero! Because our PWS data contains only 2022 for which we don't have any test samples  
# TODO : Finish the code for the population proportions for the case that there are enough CWS data.  
# merge(categorized_WQS, PWS[,c("PWSIDNumber", "SystemPopulation")], by = "PWSIDNumber", all.x = tr)
  
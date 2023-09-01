###################################################################################################
#
# Title: State Level NCDM Development for Drinking Water . Calculating population affected in each
# County.
# Nebraska Environmental Public Health Tracking (EPHT) Program.
# Date Created: 6/28/2023
# Description: Following HTG 2023 part III.
#             (This set of processing steps further summarizes the CWS-level measures into county 
#               level of population for three distinguished levels of the contaminants
#
#              *** Note 1: The population are calculated from the population served by each CWS
#                         and aggreagated for their representative county. The sum of the population
#                         may not agree with census, or other data sources. The responsbility for 
#                         checking and interpreting the differences in total populations is upon user.
#
#             *** The population for each CWS are all from 2021 and are assumed similar for all years.
#                  This is only because of the unavailibity of these populations for those other years.
#                 Interpreting the results should be with cautious.
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
 col_names <- c('PrincipalCountyServed_FIPS',	'AnalyteCode',	'Year',	'SummaryTimePeriod',	'AggregationType',	'PrincipalCountyServedName',
 'num_people_below_MCL',	'num_people_above_MCL',	'num_people_no_detect',	'perc_people_below_MCL',	'perc_people_above_MCL',
 'perc_people_no_detect',	'total_population_served')

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
  merged$Category <- ifelse(merged$Concentration < merged$LDL, "people_No_detect",
                            ifelse(merged$Concentration <= merged$MCL, "people_below_MCL", "people_above_MCL"))
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
output_county_pops_affected <- 'Data/Water_Data/NCDM_county_level_population_affected.csv'                    # the address of the output csv file for percent of CWS by category
# output_population_affected <- 'Data/Water_Data/NCDM_state_level_population.csv'    # the address of the output csv file for affected population by category
output_PWS_percentage_by_County <- 'Data/Water_Data/NCDM_county_level_population.csv'

WQS <- read_csv(WQS_file)
PWS <- read_csv(PWS_file)
analytes <- read_csv(analytes_standards)

# Calclate total poulation in each county served
county_pops_served <- PWS %>% group_by(`PrincipalCountyServed FIPS`, YearAssociatedTo)%>%
                      summarise(Population_served = sum(SystemPopulation))

# Converting MCL for Uranium from pci/l to ug/L (Because it was what supposed to 
# do during data preparation)
analytes[analytes['AnalyteCode']==4010, 'MCL'] <- analytes[analytes['AnalyteCode']==4010, 'MCL'] * 1.49
analytes[analytes['AnalyteCode']==4010, 'LDL'] <- analytes[analytes['AnalyteCode']==4010, 'LDL'] * 1.49

# Categorizing each concentration values based on the suggested LDL and MCL from CDC Tracking
categorized_WQS <- addCategory(WQS, analytes)

# Attaching populations 
categorized_WQS <- merge(categorized_WQS, PWS[c('PWSIDNumber', 'PrincipalCountyServed FIPS', 'PrincipalCountyServedName', 
                                                'SystemPopulation')], by = 'PWSIDNumber', all.x = TRUE)

# ========== Part 1: Calculating the number of CWS in each of the three categories for each analyte
# Now calculating annual percent of CWS in each category
annual_CWS_category <- categorized_WQS %>% 
  group_by(`PrincipalCountyServed FIPS`, PrincipalCountyServedName, AnalyteCode, SummaryTimePeriod, AggregationType, Category) %>%
  summarise(popualation= sum(SystemPopulation)) %>% 
  pivot_wider(names_from = Category, values_from = popualation, values_fill = 0) %>%
  mutate(total_population_served = people_above_MCL+ people_below_MCL + people_No_detect) %>%
  mutate(across(c(people_above_MCL, people_below_MCL, people_No_detect), 
                .fns = list(perc = ~ round(. *100/ total_population_served, 2)))) %>%
  relocate(total_population_served, .after = last_col())

write_csv(annual_CWS_category, output_county_pops_affected)

# A required data to give cautious when using population affected
unique_CWS <- WQS %>% group_by(Year, SummaryTimePeriod, AnalyteCode) %>% 
  summarise(unique_numbers_of_CWS = length(unique(PWSIDNumber)))

unique_CWS_file <- "Data/Water_Data/unique_CWS_numbers"
write_csv(unique_CWS, file = unique_CWS_file)

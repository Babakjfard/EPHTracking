# ==================== University of Nebraska Medical Center (UNMC) ===========
# ========================= College of Public Health (COPH) ===================
# ==================== Water, Climate, and Health (WCH) program ===============
# ======================== Babak J.Fard, Jun 2023 =============================

# Disclaimer
# This code was written for the prepartation of the Water Summary product for
# CDC Tracking Spring 2023 Data Call. All responsibilities for using this code is 
# upon the user. 
rm(list = ls()) # clear the memory

library(rtf)

print("We suggest to run the validation test before this step.")
source('codes/R/functions_Drinking_Water_2023.R') # Getting required functions

input_file <- 'Data/Water_Data/Sample_results.csv'
output_csv <- paste0('Data/water_summarized_', Sys.time(),".csv")


sampling <- read_csv(input_file)  #

# Check if the dataframe includes the bare minimum columns
if (sum(required_columns %notin%  names(sampling)) != 0){
  print('*** Looks like your dataset does not include all the required columns. ***')
  print('Check for the following columns (Case Sensitive):')
  print(required_columns)
  stop()
}

# This is the main script to create the summary from the raw data files
# Drinking
## Creating a log file for the outputs and the process steps
report_file <- paste0('reports/water_summarized_', Sys.time(),".doc")
rtffile <- RTF(report_file)  # this can be an .rtf or a .doc
addHeader(rtffile, " Water summary report following HTG 2023")
addParagraph(rtffile,  "Created on: ", Sys.time(), "/n")


# check for and change the unit for uranium if needed
sampling <- uranium_units_change(sampling)

addParagraph(rtffile,  "* Checked the Uranium units/n")

# vvv Check for the rows with NonDetectFlag. If this column is not available will create it!
# ***** This column name is also case sensitive. ******
if ('NonDetectFlag' %in% names(sampling)){
  sampling[sampling$NonDetectFlag==1, "Concentration"] <- 0.0
} else {
  sampling$NonDetectFlag <- 0
}
# Also set the flag for Concentration == 0
sampling[sampling$Concentration == 0.0 , "NonDetectFlag"] <- 1

# Correct for the Half Detection Limit following HTG
result <- apply_half_LDL(sampling)
sampling <- result[[1]]

addParagraph(rtffile,'* Checked for the values below the detection limits.')
addParagraph(rtffile,'Number of revised below LDL values:', result[[2]])
# ^^^

# vvv Check and average duplicates for the same (Analyte, Date, station)
addParagraph(rtffile, "Total number of samples: ", dim(sampling)[1])

sampling <- average_duplicates(sampling)
addParagraph(rtffile, "Total number of samples after averaging duplicates: ", dim(sampling)[1])
# ^^^

# vvv before starting the summarization, we took a look into maximum concentration
# value for each analyte compared to its median over all dataset

maximums_table <-  sampling %>% 
  group_by(AnalyteCode) %>% 
  summarise(maximum = max(Concentration),
            median = median(Concentration)) %>% 
  left_join(sampling, by = c("AnalyteCode", "maximum"= "Concentration")) %>% 
  select(AnalyteCode, maximum, median, DateSampled)

addParagraph(rtffile,"")
addParagraph(rtffile, "Table 1: maximum concentration value and its corresponding date for each analyte compared
             to its median over all dataset")

addTable(rtffile, maximums_table)
# ^^^

Analytes_plot <- plot_concentrations(sampling)
addPlot(rtffile, plot.fun=print,width=6,height=6,res=300, Analytes_plot)
addParagraph(rtffile, "Figure 1. The accordion plot of the concentrarion values of the analytes")
addParagraph(rtffile,"")

# ************ Starting the Summarization ******************
# vvv Annual averages for the 8 analytes (Except the two disinfection-by-products (TTHM and HAA5))
sampling$SummaryTimePeriod <- as.character(sampling$Year)

annual_mean_8_analytes <- calculate_summary_stations_first(sampling)
annual_mean_8 <- calculate_summary_by_CWS(annual_mean_8_analytes)
addParagraph(rtffile, "* Calculated annual average of 8 analytes following HTG 2023")
# ^^^

# vvv Annual averages for two disinfection-by-products (TTHM and HAA5))
mean_disinfects <- calculate_summary_days_first(sampling)
annual_mean_disinfect <- calculate_summary_by_CWS(mean_disinfects)
addParagraph(rtffile, "* Calculated annual average of TTHM and HAA5")
# ^^^

# vvv Annual maximum for all analytes
annual_max <- annual_summary_max(sampling)
addParagraph(rtffile, "* Calculated annual max values per cws.")
# ^^^

#vvv ======== Calculating quarterly means
sampling_Q <- sampling %>% filter(AnalyteCode %in% c(2050, 1040, 2456, 2950))
sampling_Q <- extract_Quarters(sampling_Q)

q_attr_nitr <- calculate_summary_stations_first(sampling_Q)
quarter_mean_attr_nitr <- calculate_summary_by_CWS(q_attr_nitr)

q_disinf <- calculate_summary_days_first(sampling_Q)
quarter_mean_disinf <- calculate_summary_by_CWS(q_disinf)
addParagraph(rtffile, "* Quarterly mean values calculated.")
# ^^^

# vvv ==== Aggregating all 5 tables into a final dataframe
Whole_Summaries <- bind_rows(annual_mean_8, annual_mean_disinfect, 
                             annual_max, quarter_mean_attr_nitr,
                             quarter_mean_disinf)

col_order <- c("PWSIDNumber", "Year", "AnalyteCode", "ConcentrationUnits", "Concentration",
               "DateSampled", "AggregationType", "NumSamplingLocations",
               "SummaryTimePeriod", "NumSamples", "NumNonDetects" )

Whole_Summaries <- Whole_Summaries[, col_order]

# Writing the result into the output csv
write_csv(Whole_Summaries, output_csv)
addParagraph(rtffile, "--- Finished creating the summary")
addParagraph(rtffile, "---- The result saved in:", output_csv)

done.RTF(rtffile)
closeAllConnections()

#=============== END of file
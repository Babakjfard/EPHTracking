# Description

This repository contains files that are used to process raw data into summaries to be submitted to CDC Data calls


# Last Update

The latest data call was spring 2023 that contained Drinking Water and Radeon data.

# Description of files

- ***functions_Drinking_Water_2023.R:*** includes required functions to create the summary file from raw data
- ***summarize_Drinking_Water_2023.R:*** the main executable file that will create the summary file along with a .doc report.
   The user can change the address of the input and output files in the code.
- ***Water_2023_NCDM_State_Level.R:*** a self contained file to create state level NCDM for water data call following HTG 2023 section III. The input csv files are supposed to follow the HTG guide of columns order and naming.

   Note: The raw data is expected to have a certain structure of minimum required columns with specific namings. ***summarize_Drinking_Water_2023.R:*** checks for this requirement at the start and will stop (with a message) if the raw data does not include the required columns. We suggest to do the validation and correct the potential problems before summarizing your data. The related codes are in the ***python*** folder.


## DISCLAIMER

**PLEASE NOTE THAT THE USE OF THIS CODE IS AT YOUR OWN RISK. THE AUTHORS AND CONTRIBUTORS OF THIS PROJECT ASSUME NO RESPONSIBILITY OR LIABILITY FOR ANY ERRORS OR CONSEQUENCES RESULTING FROM THE USE OF THE CODE.**




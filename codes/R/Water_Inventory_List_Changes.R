# This is to compare the differences between Inventory files of this year and the year before
library(dplyr)
library(tidyr)

water_Inventory_24 <- readxl::read_excel('/Users/babak.jfard/projects/EPHTracking/Data/Water_Data_2024_2025/PWSInventory2022_2023.xlsx')
water_Inventory_25 <- readxl::read_excel('/Users/babak.jfard/projects/EPHTracking/Data/Water_Data_2024_2025/PWSInventory_2024.xlsx')
# Let's say your tibbles are called df1 and df2

# 1. Inner join on the key
joined <- inner_join(water_Inventory_24, water_Inventory_25, by = "PWSIDNumber", suffix = c("_2023", "_2024"))

# 2. Get all columns (except the key) that are common to both
common_cols <- intersect(names(water_Inventory_24), names(water_Inventory_25))
common_cols <- setdiff(common_cols, "PWSIDNumber")

# 3. Compare the values across the columns
differences <- joined %>%
  rowwise() %>%
  mutate(diff_cols = list(
    setdiff(common_cols, common_cols[sapply(common_cols, function(col) {
      identical(cur_data()[[paste0(col, "_2023")]], cur_data()[[paste0(col, "_2024")]])
    })])
  )) %>%
  ungroup() %>%
  filter(lengths(diff_cols) > 0)

# 4. Optional: Expand to see exactly which values differ
detailed_differences <- differences %>%
  select(PWSIDNumber, diff_cols, ends_with("_2023"), ends_with("_2024"))

detailed_differences <- detailed_differences %>%
  rowwise() %>%
  mutate(diff_cols_str = paste(diff_cols, collapse = ", ")) %>%
  select(-c(diff_cols))
  ungroup()
  
detailed_differences <- detailed_differences %>%
    select(PWSIDNumber, diff_cols_str, everything())
  


write_csv(detailed_differences, '/Users/babak.jfard/projects/EPHTracking/Data/Water_Data_2024_2025/toSubmit_2025/PWS_changes_2024to2023.csv')

# ======================================

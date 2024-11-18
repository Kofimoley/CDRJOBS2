devtools::install_github("Kofimoley/CDRJOBS2")
library(dplyr)

library(rgcam)

library(CDRJOBS2)

results <- calculate_cdr_jobs(
  db_path = "D:/gcam-cdr_1.0.2/output",               # Path to the GCAM database
  db_name = "trade",                  # Name of the database
  dat_file = "CDR_jobs",                     # File name (without the .dat extension)
  scenario_list = c("REP-A", "CAP-A"),# List of scenarios to consider
  region_list = c("USA", "China"), # List of regions to consider
  output_path = "E:/SUBMISSIONS/hosting/CDRJOBS",             # Path to save the CSV files or plots
  output_type = c("csv", "list"),             # Specify "csv" to save results as CSVs or "list" to return as R objects
  create_plots = TRUE                         # Set to TRUE to generate plots
)

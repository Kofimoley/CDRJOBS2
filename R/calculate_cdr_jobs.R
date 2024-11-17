#' Function to calculate CDR Jobs
#'
#' This function estimates the number of jobs created by CDR technologies based on a given GCAM database.
#'
#' @param db_path Path to the GCAM database.
#' @param db_name Name of the GCAM database.
#' @param dat_file Name of the .dat file to load.
#' @param scenario_list List of scenarios to include.
#' @param output_path Path to save output files.
#' @param output_type Output format, either "csv" or "list".
#' @param create_plots Logical, if TRUE, generates plots for the results.
#' @return A list containing the estimated jobs.
#' @import dplyr rgcam ggplot2
#' @export
calculate_cdr_jobs <- function(db_path, db_name, dat_file, scenario_list, output_path, output_type = c("csv", "list"), create_plots = TRUE) {
# Load necessary libraries
  library(dplyr)
  library(rgcam)
  library(ggplot2)

  # Validate output_type
  output_type <- match.arg(output_type, several.ok = TRUE)

  # Define the CDR query directly in the function
  CDR_query <- "<?xml version='1.0'?>
  <queries>
    <aQuery>
      <all-regions/>
      <supplyDemandQuery title='CDR by tech'>
        <axis1 name='technology'>technology</axis1>
        <axis2 name='Year'>physical-output[@vintage]</axis2>
        <xPath buildList='true' dataName='output' group='false' sumAll='false'>
          *[@type='sector' and @name='CDR_regional']/*[@type='subsector']/*[@type='technology' and not(@name='unsatisfied CDR demand')]/
          *[@type='output']/physical-output/node()
        </xPath>
        <comments>Excludes unsatisfied CDR demand</comments>
      </supplyDemandQuery>
    </aQuery>
  </queries>"

  # Create a temporary XML file for the query
  query_file <- tempfile(fileext = ".xml")
  writeLines(CDR_query, query_file)

  # Establish database connection and add scenario
  CDR_tech <- addScenario(localDBConn(db_path, db_name), paste0(dat_file, ".dat"), scenario_list, query_file)

  # Query the database for CDR outputs
  CDR_Output <- getQuery(CDR_tech, "CDR by tech")

  # Ensure the job intensity dataset exists
  if (!exists("CDR_Job_Inten", envir = asNamespace("CDRJOBS2"))) {
    stop("CDR_Job_Inten dataset is not available in the CDRJOBS2 package.")
  }
  CDR_Job_Inten <- CDRJOBS2::CDR_Job_Inten

  # Helper function to calculate job estimates
  calculate_jobs <- function(data) {
    data %>%
      group_by(across(-value)) %>%
      summarize(
        mean_Jobs = sum(mean_int * 3.667 * 10^6 * value, na.rm = TRUE),
        min_Jobs = sum(min_int * 3.667 * 10^6 * value, na.rm = TRUE),
        max_Jobs = sum(max_int * 3.667 * 10^6 * value, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # Join the CDR Output with job intensity values
  joined_data <- dplyr::left_join(CDR_Output, CDR_Job_Inten, by = c("technology" = "CDR"))

  # Calculate jobs by year and technology
  Job_total_year <- calculate_jobs(joined_data %>% group_by(scenario, region, year))
  Job_by_tech_year <- calculate_jobs(joined_data %>% group_by(scenario, region, technology, year))
  Job_cum_tech <- calculate_jobs(joined_data %>% group_by(scenario, region, technology))
  Job_cum_total <- calculate_jobs(joined_data %>% group_by(scenario, region))

  # Output results based on the selected output type
  if ("csv" %in% output_type) {
    write.csv(Job_total_year, file.path(output_path, "Job_total_year.csv"), row.names = FALSE)
    write.csv(Job_by_tech_year, file.path(output_path, "Job_by_tech_year.csv"), row.names = FALSE)
    write.csv(Job_cum_tech, file.path(output_path, "Job_cum_tech.csv"), row.names = FALSE)
    write.csv(Job_cum_total, file.path(output_path, "Job_cum_total.csv"), row.names = FALSE)
  }

  results <- list(
    Job_total_year = Job_total_year,
    Job_by_tech_year = Job_by_tech_year,
    Job_cum_tech = Job_cum_tech,
    Job_cum_total = Job_cum_total
  )

  if ("list" %in% output_type || length(output_type) == 0) {
    return(results)
  }

  # Create plots if requested
  if (create_plots) {
    plot_cdr_jobs(results, output_path)
  }
}

# create logger
log_file <- paste0(output_folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

### PSSA
results_pssa <- getPSSA(cdm = cdm,
                        index = list(c("amiodarone", "ingredient")), 
                        marker = list(c("levothyroxine", "ingredient")), 
                        table_name = "pssa_amiodarone_levothyroxine",
                        study_time = 730) 

### plots
getHistogram(results_pssa, "days")

### Waiting Time Distribution
getWaitingTimeDistribution(cdm = cdm,
                           drug= list(c("amiodarone", "ingredient")),
                           table_name = "wtd_amiodarone", 
                           start_date = "2010-01-01", 
                           end_date = "2022-12-31", 
                           prior_obs = 365)

### Subset by JSON
cohortSet <- readCohortSet(
  path = here("1_InstantiateCohorts", "CohortPSSA")
)
cdm <- generateCohortSet(
  cdm = cdm, cohortSet = cohortSet, name = "pssa_subset",
  overwrite = TRUE
)

results_subset <- getPSSASubset(cdm = cdm, 
                                index = list(c("amiodarone", "ingredient")), 
                                marker = list(c("levothyroxine", "ingredient")),
                                subset_name = "pssa_subset",
                                subset_id = 1,
                                study_time = 365)

### subset by sex and/or age

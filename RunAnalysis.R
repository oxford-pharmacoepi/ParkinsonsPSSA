# create logger
log_file <- paste0(output_folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# generate table names
info(logger, "GENERATE TABLE NAMES")
cohortTableName <- paste0(stem_table, "_cohorts_to_instantiate")

# instantiate necessary cohorts
info(logger, "INSTANTIATE COHORTS")
cohortSet <- readCohortSet(
  path = here("1_InstantiateCohorts", "Cohorts")
)
cdm <- generateCohortSet(
  cdm = cdm,
  cohortSet = cohortSet,
  cohortTableName = cohortTableName,
  overwrite = TRUE
)

study_results <- list()

info(logger, "WRITING CSV FILES")
lapply(names(study_results), function(x) {
  result <- study_results[[x]]
  utils::write.csv(
    result, file = paste0(output_folder, "/", x, ".csv"), row.names = FALSE
  )
})
info(logger, "ZIPPING RESULTS")
output_folder <- basename(output_folder)
zip(
  zipfile = file.path(paste0(output_folder, "/Results_", db_name, ".zip")),
  files = list.files(output_folder, full.names = TRUE)
)
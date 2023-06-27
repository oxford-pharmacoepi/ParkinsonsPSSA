# create logger
log_file <- paste0(output_folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# generate table names
info(logger, "GENERATE TABLE NAMES")
cohortTableNameDrug <- paste0(stem_table, "drug")

# instantiate necessary cohorts
info(logger, "INSTANTIATE COHORTS")
cohortSet <- readCohortSet(
  path = here("1_InstantiateCohorts", "Cohorts")
)

cdm <- generateCohortSet(
  cdm = cdm,
  cohortSet = cohortSet,
  name = cohortTableNameDrug,
  overwrite = TRUE
)


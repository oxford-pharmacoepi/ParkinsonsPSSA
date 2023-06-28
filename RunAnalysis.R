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

table <- cdm[[cohortTableNameDrug]] %>% 
  select(cohort_definition_id, subject_id, cohort_start_date) %>%
  pivot_wider(names_from = cohort_definition_id, values_from = cohort_start_date) %>%
  collect()

colnames(table) <- c("subject_id", "dateIndexDrug", "dateMarkerDrug")

table <- tableCleaning(table, 730)

asr(summ_dat(table, patid = "subject_id", dateA = "dateIndexDrug", dateB = "dateMarkerDrug"))

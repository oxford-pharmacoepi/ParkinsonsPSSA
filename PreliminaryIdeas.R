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

### Method 1: Use pre-defined JSONs
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

### Method 2: Use Capr
markerCohort <- generatePSSACohortDefinitions(1501700)
indexCohort <- generatePSSACohortDefinitions(1309944)

path <- file.path(tempdir(), "pssa_cohorts")
dir.create(path)
writeCohort(indexCohort, file.path(path, "indexCohort.json"))
writeCohort(markerCohort, file.path(path, "markerCohort.json"))
pssa <- readCohortSet(path = path)
cdm <- generateCohortSet(
  cdm,
  pssa,
  name = "pssa",
  overwrite = TRUE
)

table <- cdm[[cohortTableNameDrug]] %>% 
  select(cohort_definition_id, subject_id, cohort_start_date) %>%
  pivot_wider(names_from = cohort_definition_id, values_from = cohort_start_date) %>%
  collect()

colnames(table) <- c("subject_id", "dateIndexDrug", "dateMarkerDrug")

table <- tableCleaning(table, 730)

asr(summ_dat(table, patid = "subject_id", dateA = "dateIndexDrug", dateB = "dateMarkerDrug"))

### Method 3: Use Drug Utilisation package - more complete attempt

# 1. Generate the required drugID
cdm <- CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = "public_100k",
  write_schema = results_database_schema
)

indexId <- getDrugIngredientCodes(cdm, "amiodarone")
markerId <- getDrugIngredientCodes(cdm, "levothyroxine")

table_name_pssa <- "pssa_amiodarone_levothyroxine"

cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = table_name_pssa,
  conceptSetList = c(indexId, markerId),
  summariseMode = "FirstEra"
)

csr(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))

asr(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))

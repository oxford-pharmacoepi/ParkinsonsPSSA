devtools::install_github("oxford-pharmacoepi/CohortSymmetry")
library(CDMConnector)
library(here)
library(dbplyr)
library(dplyr)
library(RPostgres)
library(DBI)
library(CodelistGenerator)
library(DrugUtilisation)
library(CohortSymmetry)

# database metadata and connection details -----
# The name/ acronym for the database
db.name<-"CPRD_GOLD"

# Set output folder location -----
# the path to a folder where the results from this analysis will be saved
# to set the location within the project with folder called "ouput, we can use: here("output")
# but this file path could be set to somewhere else
output.folder<-here("Results", db.name)

# Specify databaseConnector connection details -----
# database connection details
# connect to database
user<-Sys.getenv("DB_USER")
password<- Sys.getenv("DB_PASSWORD")
port<-Sys.getenv("DB_PORT") 
host<-Sys.getenv("DB_HOST") 
server_dbi<-Sys.getenv("DB_SERVER_cdm_gold_202207_dbi") 
dbmsName <- "postgresql"

# Specify cdm_reference via DBI connection details -----
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below (see https://dbi.r-dbi.org/articles/dbi for more details)
# you may need to install another package for this (although RPostgres is included with renv in case you are using postgres)
db <- dbConnect(RPostgres::Postgres(),
                dbname = server_dbi,
                port = port,
                host = host, 
                user = user, 
                password = password)

# Set database details -----
# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema<-"public"

# The name of the schema that contains the vocabularies 
# (often this will be the same as cdm_database_schema)
vocabulary_database_schema<-cdm_database_schema

# The name of the schema where results tables will be created 
results_database_schema<- "results"

# create cdm reference ----
cdm <- CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = "public_100k",
  write_schema = results_database_schema
)
cdm$person %>% 
  tally() %>% 
  CDMConnector::computeQuery()

cdm <- CDMConnector::cdm_from_con(con = db,
                                  cdm_schema = "public_100k",
                                  write_schema = results_database_schema,
                                  cohort_tables = c("cohort_amiodarone", "cohort_levothyroxine"))

# create drug cohorts
conceptList <- getDrugIngredientCodes(cdm, "amiodarone")
cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = "cohort_amiodarone",
  conceptSet = conceptList
)

conceptList <- getDrugIngredientCodes(cdm, "levothyroxine")
cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = "cohort_levothyroxine",
  conceptSet = conceptList
)

cdm <- CohortSymmetry::getCohortSequence(cdm, 
                                         name = "output",
                                         dateRange = as.Date(c("2000-01-01", "2020-01-01")),
                                         indexTable = "cohort_amiodarone",
                                         markerTable = "cohort_levothyroxine",
                                         daysPriorObservation = 0,
                                         indexWashout = 0,
                                         markerWashout = 0,
                                         timeGap = 365)

pssaResult <- CohortSymmetry::getSequenceRatios(cdm,
                                                outcomeTable = "output")

cdm <- CohortSymmetry:::getCohortSequence2(cdm, 
                                           name = "output",
                                           dateRange = as.Date(c("2000-01-01", "2020-01-01")),
                                           indexTable = "cohort_amiodarone",
                                           markerTable = "cohort_levothyroxine",
                                           daysPriorObservation = 0,
                                           indexWashout = 0,
                                           markerWashout = 0,
                                           timeGap = 365)

###########
cdm$cohort_acetaminophen %>% 
  PatientProfiles::addSex() %>%
  dplyr::filter(sex == "Female")

ageGroup <- list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150))

cdm$cohort_acetaminophen %>%
  PatientProfiles::addAge(cdm = cdm, indexDate = "cohort_start_date", ageGroup = ageGroup) %>%
  dplyr::group_by(cohort_definition_id, subject_id, age_group) %>%
  dbplyr::window_order(cohort_start_date) %>%
  dplyr::filter(dplyr::row_number()==1) %>%
  dplyr::ungroup() %>%
  dbplyr::window_order()

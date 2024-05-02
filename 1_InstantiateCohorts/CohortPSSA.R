#### pull back instantiated cohorts
cdm <- CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = c(schema = results_database_schema, prefix = stem_table), 
  cohort_tables = c("parkinson_subtypes", 
                    "amiodarone", 
                    "levothyroxine", 
                    "allopurinol", 
                    "levodopa",
                    "dopamine_agonists",
                    "amantadine",
                    "maob_inhibitors",
                    "comt_inhibitors",
                    "ccb",
                    "dopamine_depleters",
                    "atypical_antipsychotics",
                    "antiemetics")
)

###### Parkinsonism subtypes
print(paste0("Instantiating Parkinsonism subtypes cohorts at ", Sys.time()))
outcome_cohorts_subtypes <- readCohortSet(here(
  "ParkinsonsPSSA",
  "1_InstantiateCohorts",
  "CohortPSSA"
))

cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = outcome_cohorts_subtypes,
                         name = "parkinson_subtypes", 
                         overwrite = TRUE
)

print(paste0("Instantiating antiparkinsonian drug cohorts at ", Sys.time()))
# Levodopa
print(paste0("Generating levodopa cohort at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "levodopa",
                                                    ingredient = "levodopa")
print(paste0("Generated levodopa cohort at ", Sys.time()))

# Dopamine Agonists
print(paste0("Generating dopamine agonists at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm, 
                                             name = "dopamine_agonists", 
                                             atcName = "dopamine agonists", 
                                             level = c("ATC 4th"))
print(paste0("Generated dopamine agonists at ", Sys.time()))

# Amantadine
print(paste0("Generating amantadine cohort at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "amantadine",
                                                    ingredient = "amantadine")
print(paste0("Generated amantadine cohort at ", Sys.time()))

#MAOB
print(paste0("Generating MAOB cohort at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm, 
                                             name = "maob_inhibitors", 
                                             atcName = "Monoamine oxidase B inhibitors", 
                                             level = c("ATC 4th"))
print(paste0("Generated MAOB cohort at ", Sys.time()))

#COMT
print(paste0("Generating COMT cohort at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm, 
                                             name = "comt_inhibitors", 
                                             atcName = "levodopa, decarboxylase inhibitor and COMT inhibitor; oral", 
                                             level = c("ATC 5th"))
print(paste0("Generated COMT cohort at ", Sys.time()))
print(paste0("Instantiated antiparkinsonian drug cohorts at ", Sys.time()))

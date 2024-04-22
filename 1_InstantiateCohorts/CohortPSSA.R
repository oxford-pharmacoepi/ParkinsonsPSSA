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
                    "comt_inhibitors")
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

# #CCB
# print(paste0("Generating potential cascades drug cohorts at ", Sys.time()))
# print(paste0("Generating ccb cohort at ", Sys.time()))
# cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm,
#                                              name = "ccb",
#                                              atcName = "calcium channel blockers",
#                                              level = c("ATC 2nd"))
# print(paste0("Generated ccb cohort at ", Sys.time()))
# 
# #ANTIDEPRESSANTS
# print(paste0("Generating antidepressants cohort at ", Sys.time()))
# cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm,
#                                              name = "antidepressants",
#                                              atcName = "antidepressants",
#                                              level = c("ATC 3rd"))
# #ANTIEMETICS
# cdm <- getSingleDrugCohort(cdm = cdm,
#                            drug = list(c("ANTIEMETICS AND ANTINAUSEANTS", "ATC 2nd")),
#                            table_name = "ANTIEMETICS",
#                            start_date = as.Date("2008-01-01"),
#                            end_date = as.Date("2021-12-31"))
# 
# #ANTIEPILEPTICS
# cdm <- getSingleDrugCohort(cdm = cdm,
#                            drug = list(c("ANTIEPILEPTICS", "ATC 3rd")),
#                            table_name = "ANTIEPILEPTICS",
#                            start_date = as.Date("2008-01-01"),
#                            end_date = as.Date("2021-12-31"))

######## potential antiparkinson cascades
# Antiemetics 
print(paste0("Generating Antiemetics at ", Sys.time()))
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("metoclopramide", "ingredient"), c("levosulpiride", "ingredient"), c("clebopride", "ingredient")),
                           table_name = "antiemetics",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

# Typical antipsychotics 
print(paste0("Generating Typical antipsychotics at ", Sys.time()))
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("chlorpromazine", "ingredient"), 
                                       c("prochlorperazine", "ingredient"), 
                                       c("perphenazine", "ingredient"), 
                                       c("fluphenazine", "ingredient"), 
                                       c("promethazine", "ingredient"), 
                                       c("haloperidol", "ingredient"),
                                       c("pimozide", "ingredient"), 
                                       c("sulpiride", "ingredient")),
                           table_name = "typical_antipsychotics",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

###########################################################################
# Atypical antipsychotics 
print(paste0("Generating Atypical antipsychotics at ", Sys.time()))
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("clozapine", "ingredient"), 
                                       c("quetiapine", "ingredient")),
                           table_name = "atypical_antipsychotics",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

# Mood stabilizer  
print(paste0("Generating Mood stabilizer at ", Sys.time()))
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("lithium", "ingredient")),
                           table_name = "mood_stabilizer",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

# Antidepressant 
print(paste0("Generating Antidepressant at ", Sys.time()))
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("citalopram", "ingredient"),
                                       c("fluoxetine", "ingredient"),
                                       c("praoxetine", "ingredient"),
                                       c("sertraline", "ingredient")),
                           table_name = "antidepressant",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

# Antiepileptic drugs 
print(paste0("Generating Antiepileptic drugs at ", Sys.time()))
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("Volpric Acid...", "ingredient"),
                                       c("phenytoin", "ingredient")),
                           table_name = "antidepressant",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

# Antiemetics Infreqeuncy
print(paste0("Generating Infrequent Antiemetics at ", Sys.time()))
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("domperidone", "ingredient"), c("itopride", "ingredient"), c("clebopride", "ingredient")),
                           table_name = "infreq_antiemetics",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

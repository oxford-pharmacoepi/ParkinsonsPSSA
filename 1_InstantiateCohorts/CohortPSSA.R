###### Parkinsonism subtypes
print(paste0("Instantiating Parkinsonism subtypes cohorts at ", Sys.time()))
outcome_cohorts_subtypes <- readCohortSet(here(
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
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("Dopamine agonists", "ATC 4th")),
                           table_name = "dopamine_agonists",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))
print(paste0("Generated dopamine agonists at ", Sys.time()))

# Amantadine
print(paste0("Generating amantadine cohort at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "amantadine",
                                                    ingredient = "amantadine")
print(paste0("Generated amantadine cohort at ", Sys.time()))

#MAOB
print(paste0("Generating MAOB at ", Sys.time()))
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("Monoamine oxidase B inhibitors", "ATC 4th")),
                           table_name = "MAOB_inhibitors",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))
print(paste0("Generated MAOB at ", Sys.time()))

#COMT
print(paste0("Generating COMT at ", Sys.time()))
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("levodopa, decarboxylase inhibitor and COMT inhibitor; oral", "ATC 5th")),
                           table_name = "COMT_inhibitors",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))
print(paste0("Generated COMT at ", Sys.time()))

#CCB
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("CALCIUM CHANNEL BLOCKERS", "ATC 2nd")),
                           table_name = "ccb_atc2",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))
# 
# #ANTIDEPRESSANTS
# cdm <- getSingleDrugCohort(cdm = cdm,
#                            drug = list(c("ANTIDEPRESSANTS", "ATC 3rd")),
#                            table_name = "antidepressants",
#                            start_date = as.Date("2008-01-01"),
#                            end_date = as.Date("2021-12-31"))
# 
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
# CCB
print(paste0("Generating CCB at ", Sys.time()))
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("cinnarizine", "ingredient"), c("flunarizine", "ingredient")),
                           table_name = "ccb",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

# Dopamine depleters 
print(paste0("Generating Dopamine Depleters at ", Sys.time()))
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("tetrabenazine", "ingredient"), c("reserpine", "ingredient")),
                           table_name = "dopamine_depleters",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

# Atypical antipsychotics 
print(paste0("Generating Atypical Antipsychotics at ", Sys.time()))
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("risperidone", "ingredient"), c("olanzapine", "ingredient"), c("ziprasidone", "ingredient"), c("aripiprazole", "ingredient")),
                           table_name = "atypical_antipsychotics",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

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

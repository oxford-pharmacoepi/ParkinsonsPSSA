#### admiodarone to levothyroxine 

###### Parkinsonism subtypes
outcome_cohorts_subtypes <- readCohortSet(here(
  "1_InstantiateCohorts",
  "CohortPSSA"
))

cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = outcome_cohorts_subtypes,
                         name = "parkinson_subtypes", 
                         overwrite = TRUE
)

# functions
getSingleDrugCohort <- function(cdm, drug, table_name, start_date, end_date){
drug_name <- list()

for (i in (1: length(drug))){
  if (drug[[i]][2] == "ingredient"){
    drug_name[[i]] <- getDrugIngredientCodes(cdm = cdm, name = drug[[i]][1])
  } else {
    drug_name[[i]] <- getATCCodes(cdm = cdm, name = drug[[i]][1], level = c(drug[[i]][2]))
  }
}

conceptSet <- c()
for (i in (1:length(drug_name))){
  conceptSet <- c(conceptSet, drug_name[[i]])
}

cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = table_name,
  conceptSet = conceptSet,
  cohortDateRange = as.Date(c(start_date, end_date))
)

return(cdm)
}
#################################################################################
####positive control
# amiodarone
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("amiodarone", "ingredient")),
                           table_name = "amiodarone",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

# levothyroxine
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("levothyroxine", "ingredient")),
                           table_name = "levothyroxine",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

# Levodopa
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("levodopa", "ingredient")),
                           table_name = "levodopa",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

# Dopamine Agonists
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("Dopamine agonists", "ATC 4th")),
                           table_name = "Dopamine_agonists",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))


# Amantadine
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("amantadine", "ingredient")),
                           table_name = "amantadine",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

#MAOB
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("Monoamine oxidase B inhibitors", "ATC 4th")),
                           table_name = "MAOB_inhibitors",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

#COMT
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("levodopa, decarboxylase inhibitor and COMT inhibitor; oral", "ATC 5th")),
                           table_name = "COMT_inhibitors",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

################################################################################################
#lithium
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("lithium", "ATC 4th")),
                           table_name = "lithium",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

#CCB
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("CALCIUM CHANNEL BLOCKERS", "ATC 2nd")),
                           table_name = "ccb",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

#ANTIDEPRESSANTS
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("ANTIDEPRESSANTS", "ATC 3rd")),
                           table_name = "antidepressants",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

#ANTIEMETICS
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("ANTIEMETICS AND ANTINAUSEANTS", "ATC 2nd")),
                           table_name = "ANTIEMETICS",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

#ANTIEPILEPTICS
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("ANTIEPILEPTICS", "ATC 3rd")),
                           table_name = "ANTIEPILEPTICS",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))
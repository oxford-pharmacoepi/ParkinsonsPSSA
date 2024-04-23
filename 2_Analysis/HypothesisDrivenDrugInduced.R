print(paste0("Generating drug cohorts that frequently causes parkinson-like symptoms at ", Sys.time()))

# CCB
print(paste0("Generating CCB at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "ccb",
                                                    ingredient = c("cinnarizine", "flunarizine"))

# Dopamine depleters 
print(paste0("Generating Dopamine Depleters at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "dopamine_depleters",
                                                    ingredient = c("tetrabenazine", 
                                                                   "reserpine"))

# Atypical antipsychotics 
print(paste0("Generating Atypical Antipsychotics at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "atypical_antipsychotics",
                                                    ingredient = c("risperidone", 
                                                                   "olanzapine",
                                                                   "ziprasidone",
                                                                   "aripiprazole"))

# Antiemetics 
print(paste0("Generating Antiemetics at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "antiemetics",
                                                    ingredient = c("metoclopramide", 
                                                                   "levosulpiride",
                                                                   "clebopride"))

# Typical antipsychotics 
print(paste0("Generating typical antipsychotics at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "typical_antipsychotics",
                                                    ingredient = c("chlorpromazine", 
                                                                   "prochlorperazine",
                                                                   "perphenazine",
                                                                   "fluphenazine", 
                                                                   "promethazine",
                                                                   "haloperidol",
                                                                   "pimozide", 
                                                                   "sulpiride"))

#####
print(paste0("Generating drug cohorts that infrequently causes parkinson-like symptoms at ", Sys.time()))
# Atypical antipsychotics 
print(paste0("Generating atypical antipsychotics at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "atypical_antipsychotics",
                                                    ingredient = c("clozapine", 
                                                                   "quetiapine"))
# Mood stabilizer  
print(paste0("Generating Mood stabilizer at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "mood_stabilizer",
                                                    ingredient = c("lithium"))

# Antidepressant
print(paste0("Generating antidepressant at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "mood_stabilizer",
                                                    ingredient = c("citalopram",
                                                                   "fluoxetine",
                                                                   "praoxetine",
                                                                   "sertraline"))
# Antiepileptic drugs 
print(paste0("Generating Antiepileptic drugs at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "antiepileptics",
                                                    ingredient = c("phenytoin",
                                                                   "valproate"))

# Antiemetics Infreqeuncy
print(paste0("Generating Antiemetics that infrequently causes DIP at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "antiemetics2",
                                                    ingredient = c("domperidone",
                                                                   "itopride",
                                                                   "clebopride"))
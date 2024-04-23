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


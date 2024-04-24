print(paste0("Generating drug cohorts that frequently causes parkinson-like symptoms at ", Sys.time()))

hypothesise_subfolder <- here(output_folder, "hypothesise")
if (!dir.exists(hypothesise_subfolder)) {
  dir.create(hypothesise_subfolder)
}

hypothesise_results_subfolder <- here(hypothesise_subfolder, "results")
if (!dir.exists(hypothesise_results_subfolder)) {
  dir.create(hypothesise_results_subfolder)
}

hypothesise_gt_subfolder <- here(hypothesise_subfolder, "gt")
if (!dir.exists(hypothesise_gt_subfolder)) {
  dir.create(hypothesise_gt_subfolder)
}

hypothesise_plots_subfolder <- here(hypothesise_subfolder, "plots")
if (!dir.exists(hypothesise_plots_subfolder)) {
  dir.create(hypothesise_plots_subfolder)
}

print(paste0("Starting PSSA for antiparkinsonian drugs at ", Sys.time()))
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
                                                    name = "atypical_antipsychotics2",
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
                                                    name = "antidepressants",
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

# Antiemetics
print(paste0("Generating Antiemetics that infrequently causes DIP at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "antiemetics2",
                                                    ingredient = c("domperidone",
                                                                   "itopride",
                                                                   "clebopride"))

cdm <- omopgenerics::bind(
  cdm$typical_antipsychotics, cdm$atypical_antipsychotics, cdm$dopamine_depleters, cdm$antiemetics, cdm$ccb,
  name = "freq_hypothesise_driven"
)

cdm <- omopgenerics::bind(
  cdm$atypical_antipsychotics2, cdm$mood_stabilizer, cdm$antidepressants, cdm$antiepileptics, cdm$antiemetics2,
  name = "infreq_hypothesise_driven"
)
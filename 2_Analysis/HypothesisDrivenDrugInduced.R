print("Inspired by Shin et al. (2012)")
print(paste0("Generating drug cohorts that frequently causes parkinson-like symptoms at ", Sys.time()))

hypothesis_subfolder <- here(output_folder, "hypothesis")
if (!dir.exists(hypothesis_subfolder)) {
  dir.create(hypothesis_subfolder)
}

hypothesis_subfolder_shin <- here(hypothesis_subfolder, "shin")
if (!dir.exists(hypothesis_subfolder_shin)) {
  dir.create(hypothesis_subfolder_shin)
}

hypothesis_results_subfolder <- here(hypothesis_subfolder_shin, "results")
if (!dir.exists(hypothesis_results_subfolder)) {
  dir.create(hypothesis_results_subfolder)
}

hypothesis_gt_subfolder <- here(hypothesis_subfolder_shin, "gt")
if (!dir.exists(hypothesis_gt_subfolder)) {
  dir.create(hypothesis_gt_subfolder)
}

hypothesis_plots_subfolder <- here(hypothesis_subfolder_shin, "plots")
if (!dir.exists(hypothesis_plots_subfolder)) {
  dir.create(hypothesis_plots_subfolder)
}

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

cdm <- omopgenerics::bind(
  cdm$levodopa, cdm$dopamine_agonists, cdm$maob_inhibitors, cdm$comt_inhibitors, cdm$amantadine,
  name = "parkinson_treatment"
)

print(paste0("Carrying out PSSA using drugs that are known to freqently cause DIP at ", Sys.time()))
cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 indexTable = "freq_hypothesise_driven",
                                                 markerTable = "parkinson_treatment",
                                                 name = "freq_hypo",
                                                 cohortDateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                                 daysPriorObservation = 365,
                                                 washoutWindow = 365)

CohortSymmetry::summariseSequenceRatios(cohort = cdm$freq_hypo) |>
  write.xlsx(file = here(hypothesis_results_subfolder, "freq_hypo.xlsx"))

CohortSymmetry::summariseSequenceRatios(cohort = cdm$freq_hypo) |>
  CohortSymmetry::tableSequenceRatios() |> 
  gt::gtsave(filename = here(hypothesis_gt_subfolder, "freq_hypo.docx"))

CohortSymmetry::summariseTemporalSymmetry(cohort = cdm$freq_hypo) |>
  CohortSymmetry::plotTemporalSymmetry() |> 
  ggsave(filename = here(hypothesis_plots_subfolder, "freq_hypo_temporal.png"), width = 30, height = 18)

CohortSymmetry::summariseSequenceRatios(cohort = cdm$freq_hypo) |>
  CohortSymmetry::plotSequenceRatios(onlyaSR = T, 
                                    colours = "black") |>
  ggsave(filename = here(hypothesis_plots_subfolder, "freq_hypo_sr.png"), width = 30, height = 10)

print(paste0("Carrying out PSSA using drugs that are known to infreqently cause DIP at ", Sys.time()))
cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 indexTable = "infreq_hypothesise_driven",
                                                 markerTable = "parkinson_treatment",
                                                 name = "infreq_hypo",
                                                 cohortDateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                                 daysPriorObservation = 365,
                                                 washoutWindow = 365)

CohortSymmetry::summariseSequenceRatios(cohort = cdm$infreq_hypo) |>
  write.xlsx(file = here(hypothesis_results_subfolder, "infreq_hypo.xlsx"))

CohortSymmetry::summariseSequenceRatios(cohort= cdm$infreq_hypo) |>
  CohortSymmetry::tableSequenceRatios() %>% 
  gt::gtsave(filename = here(hypothesis_gt_subfolder, "infreq_hypo.docx"))

CohortSymmetry::summariseTemporalSymmetry(cohort = cdm$infreq_hypo) |>
  CohortSymmetry::plotTemporalSymmetry() |> 
  ggsave(filename = here(hypothesis_plots_subfolder, "infreq_hypo_temporal.png"), width = 30, height = 18)

CohortSymmetry::summariseSequenceRatios(cohort = cdm$infreq_hypo) |>
  CohortSymmetry::plotSequenceRatios(onlyaSR = T, 
                                     colours = "black") |>
  ggsave(filename = here(hypothesis_plots_subfolder, "infreq_hypo_sr.png"), width = 30, height = 10)

################################################################################
print("Inspired by Feldman et al. (2022)")

# Atypical antipsychotics 
print(paste0("Generating Typical Antipsychotics at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "typical_antipsychotics_feldman",
                                                    ingredient = c("prochlorperazine",
                                                                   "levomepromazine",
                                                                   "thioridazine",
                                                                   "perphenazine",
                                                                   "fluphenazine",
                                                                   "flupentixol",
                                                                   "thiothixene",
                                                                   "zuclopenthixol",
                                                                   "pimozide",
                                                                   "loxapine",
                                                                   "amoxapine",
                                                                   "haloperidol",
                                                                   "droperidol",
                                                                   "thioproprazate",
                                                                   "thioproperazine",
                                                                   "thiothixene",
                                                                   "trifluoperazine"))

# Low potency D2R antagonists 
print(paste0("Generating Low potency D2R antagonists at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "low_potency_d2r_feldman",
                                                    ingredient = c("chlorpromazine",
                                                                   "chlorprothixene",
                                                                   "levomepromazine",
                                                                   "mepazine",
                                                                   "mesoridazine",
                                                                   "methoxypromazine",
                                                                   "percyazine",
                                                                   "pimozide",
                                                                   "promazine",
                                                                   "thioridazine"))

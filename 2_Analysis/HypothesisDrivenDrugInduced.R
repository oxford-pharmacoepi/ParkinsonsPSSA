print("For hypothesis driven approach, we will use both ingredient and class level.")
print("For reference, please see Shin et al. (2012) and Feldman et al. (2022)")

hypothesis_subfolder <- here(output_folder, "hypothesis")
if (!dir.exists(hypothesis_subfolder)) {
  dir.create(hypothesis_subfolder)
}

hypothesis_subfolder_ingredient_level <- here(hypothesis_subfolder, "ingredient_level")
if (!dir.exists(hypothesis_subfolder_ingredient_level)) {
  dir.create(hypothesis_subfolder_ingredient_level)
}

hypothesis_results_subfolder <- here(hypothesis_subfolder_ingredient_level, "results")
if (!dir.exists(hypothesis_results_subfolder)) {
  dir.create(hypothesis_results_subfolder)
}

hypothesis_gt_subfolder <- here(hypothesis_subfolder_ingredient_level, "gt")
if (!dir.exists(hypothesis_gt_subfolder)) {
  dir.create(hypothesis_gt_subfolder)
}

hypothesis_plots_subfolder <- here(hypothesis_subfolder_ingredient_level, "plots")
if (!dir.exists(hypothesis_plots_subfolder)) {
  dir.create(hypothesis_plots_subfolder)
}

print(paste0("Generating ingredient level drugs that may induce Parkinson-like symptoms at ", Sys.time()))

ingredient_level <- read_xlsx(
  here::here("2_Analysis", "hypothesis_dip.xlsx")
) |> 
  dplyr::select("ingredient_name") |>
  dplyr::distinct() |>
  dplyr::pull("ingredient_name")

cdm <- omopgenerics::emptyCohortTable(cdm = cdm, 
                                      name = "ingredient_cohort")

### Pulling back if necessary
if (hypothesis_cohort_instantiated == T){
cdm <- CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = c(schema = results_database_schema, prefix = stem_table), 
  cohort_tables = c("amiodarone", 
                    "levothyroxine", 
                    "allopurinol", 
                    "parkinson_treatment",
                    "class_hypothesis")
)} else {
  for (i in (1:length(ingredient_level))){
    cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                        name = ingredient_level[[i]],
                                                        ingredient = ingredient_level[[i]])
    
    cli::cli_alert_success(paste0("- Got ingredient level ", ingredient = ingredient_level[[i]]))
    
    cdm <- omopgenerics::bind(
      cdm$ingredient_cohort,
      cdm[[ingredient_level[[i]]]],
      name = "ingredient_cohort"
    )
  }
}

CohortSymmetry::summariseSequenceRatios(cohort = cdm$freq_hypo, 
                                        minCellCount = minimum_counts) |>
  write.xlsx(file = here(hypothesis_results_subfolder, "freq_hypo.xlsx"))

CohortSymmetry::summariseSequenceRatios(cohort = cdm$freq_hypo,
                                        minCellCount = minimum_counts) |>
  CohortSymmetry::tableSequenceRatios() |> 
  gt::gtsave(filename = here(hypothesis_gt_subfolder, "freq_hypo.docx"))

CohortSymmetry::summariseTemporalSymmetry(cohort = cdm$freq_hypo,
                                          minCellCount = minimum_counts) |>
  CohortSymmetry::plotTemporalSymmetry() |> 
  ggsave(filename = here(hypothesis_plots_subfolder, "freq_hypo_temporal.png"), width = 30, height = 18)

CohortSymmetry::summariseSequenceRatios(cohort = cdm$freq_hypo,
                                        minCellCount = minimum_counts) |>
  CohortSymmetry::plotSequenceRatios(onlyaSR = T, 
                                    colours = "black") |>
  ggsave(filename = here(hypothesis_plots_subfolder, "freq_hypo_sr.png"), width = 30, height = 10)

################################################################################
print("Running PSSA on class level")
hypothesis_subfolder_class_level <- here(hypothesis_subfolder, "class_level")
if (!dir.exists(hypothesis_subfolder_class_level)) {
  dir.create(hypothesis_subfolder_class_level)
}

hypothesis_results_subfolder <- here(hypothesis_subfolder_class_level, "results")
if (!dir.exists(hypothesis_results_subfolder)) {
  dir.create(hypothesis_results_subfolder)
}

hypothesis_gt_subfolder <- here(hypothesis_subfolder_class_level, "gt")
if (!dir.exists(hypothesis_gt_subfolder)) {
  dir.create(hypothesis_gt_subfolder)
}

hypothesis_plots_subfolder <- here(hypothesis_subfolder_class_level, "plots")
if (!dir.exists(hypothesis_plots_subfolder)) {
  dir.create(hypothesis_plots_subfolder)
}

if (instantiate_index == F){
  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = cdm_database_schema,
    write_schema = c(schema = results_database_schema, prefix = stem_table), 
    cohort_tables = c("amiodarone", 
                      "levothyroxine", 
                      "allopurinol", 
                      "parkinson_treatment",
                      "antipsychotics_atc",
                      "antidepressants_atc",
                      "ccb_atc",
                      "antiemetics_atc",
                      "antiepileptics_atc",
                      "propulsives_atc",
                      "antiarrhythmics_atc",
                      "antihypentensives_atc"
    )
  )
} else {
  
#antipsychotics
print(paste0("Generating antipsychotics at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm,
                                             name = "antipsychotics_atc",
                                             atcName = "ANTIPSYCHOTICS",
                                             level = "ATC 3rd")

#antidepressants
print(paste0("Generating antidepressants at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm,
                                             name = "antidepressants_atc",
                                             atcName = "ANTIDEPRESSANTS",
                                             level = "ATC 3rd")

#ccb
print(paste0("Generating ccb at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm,
                                             name = "ccb_atc",
                                             atcName = "CALCIUM CHANNEL BLOCKERS",
                                             level = "ATC 2nd")

#antiemetics
print(paste0("Generating antiemetics at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm,
                                             name = "antiemetics_atc",
                                             atcName = "ANTIEMETICS AND ANTINAUSEANTS",
                                             level = "ATC 2nd")

#antiepileptics
print(paste0("Generating antiepileptics at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm,
                                             name = "antiepileptics_atc",
                                             atcName = "ANTIEPILEPTICS",
                                             level = "ATC 2nd")

#propulsives
print(paste0("Generating propulsives at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm,
                                             name = "propulsives_atc",
                                             atcName = "PROPULSIVES",
                                             level = "ATC 3rd")

#antiarrhythmics
print(paste0("Generating antiarrhythmics at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm,
                                             name = "antiarrhythmics_atc",
                                             atcName = "ANTIARRHYTHMICS, CLASS I AND III",
                                             level = "ATC 3rd")

#antihypertensives
print(paste0("Generating antihypertensives at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm,
                                             name = "antihypentensives_atc",
                                             atcName = "ANTIHYPERTENSIVES",
                                             level = "ATC 2nd")

#anticholinesterases
print(paste0("Generating anticholinesterases at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm,
                                             name = "anticholinesterases_atc",
                                             atcName = "Anticholinesterases",
                                             level = "ATC 4th")

#ssri
print(paste0("Generating ssri at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm,
                                             name = "ssri_atc",
                                             atcName = "Selective Serotonin Reuptake Inhibitors (SSRIs)",
                                             level = "ATC 4th")

#maoi
print(paste0("Generating maoi at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm,
                                             name = "maoi_atc",
                                             atcName = "Monoamine Oxidase Inhibitors (Non-selective and selective)",
                                             level = "ATC 4th")
}

cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 indexTable = "class_hypothesis",
                                                 markerTable = "parkinson_treatment",
                                                 name = "class_hypo",
                                                 cohortDateRange = as.Date(c("2008-01-01", "2021-12-31")))

CohortSymmetry::summariseSequenceRatios(cohort = cdm$class_hypo,
                                        minCellCount = minimum_counts) |>
  write.xlsx(file = here(hypothesis_results_subfolder, "class_hypo.xlsx"))

CohortSymmetry::summariseSequenceRatios(cohort = cdm$class_hypo,
                                        minCellCount = minimum_counts) |>
  CohortSymmetry::tableSequenceRatios() |> 
  gt::gtsave(filename = here(hypothesis_gt_subfolder, "class_hypo.docx"))

CohortSymmetry::summariseTemporalSymmetry(cohort = cdm$class_hypo,
                                          minCellCount = minimum_counts) |>
  CohortSymmetry::plotTemporalSymmetry() |> 
  ggsave(filename = here(hypothesis_plots_subfolder, "class_hypo_temporal.png"), width = 30, height = 18)

CohortSymmetry::summariseSequenceRatios(cohort = cdm$class_hypo) |>
  CohortSymmetry::plotSequenceRatios(onlyaSR = T, 
                                     colours = "black") |>
  ggsave(filename = here(hypothesis_plots_subfolder, "class_hypo_sr.png"), width = 30, height = 10)

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
  cohort_tables = c("parkinson_treatment",
                    "ingredient_cohort")
)} else {
  for (i in (1:length(ingredient_level))){
    ingredient_level_name <- ingredient_level
    ingredient_level_name[ingredient_level_name == "amphotericin B"] <- "amphotericin_b"
    
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

cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 indexTable = "ingredient_cohort",
                                                 markerTable = "parkinson_treatment",
                                                 name = "ingredient_hypo",
                                                 cohortDateRange = as.Date(c("2008-01-01", "2021-12-31")))

res <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$ingredient_hypo, 
                                        minCellCount = minimum_counts) 

res_settings <- omopgenerics::settings(res)

res_count_greater_than_100 <- res |>
  dplyr::group_by(group_level) |>
  dplyr::filter(estimate_name == "count") |>
  dplyr::summarise(n = sum(as.integer(estimate_value), na.rm = T)) |>
  dplyr::filter(n>=100) |>
  dplyr::ungroup() |>
  dplyr::pull("group_level")

res_subsetted <- res |> dplyr::filter(group_level %in% res_count_greater_than_100)

res_subsetted <- res_subsetted |>
  omopgenerics::newSummarisedResult(
    settings = res_settings
  )

#res |> write.xlsx(file = here(hypothesis_results_subfolder, "ingredient_hypo.xlsx"))
res_subsetted |> write.xlsx(file = here(hypothesis_results_subfolder, "ingredient_hypo.xlsx"))

CohortSymmetry::tableSequenceRatios(res_subsetted) |> 
gt::gtsave(filename = here(hypothesis_gt_subfolder, "ingredient_hypo.docx"))

CohortSymmetry::plotSequenceRatios(result = res_subsetted,
                                   onlyaSR = T, 
                                   colours = "black") |>
ggsave(filename = here(hypothesis_plots_subfolder, "ingredient_hypo_sr.png"), width = 30, height = 10)

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

if (hypothesis_cohort_instantiated == F){
  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = cdm_database_schema,
    write_schema = c(schema = results_database_schema, prefix = stem_table), 
    cohort_tables = c("amiodarone", 
                      "levothyroxine", 
                      "allopurinol", 
                      "parkinson_treatment",
                      "class_hypothesis"
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
                                             atcName = "Selective Serotonin Reuptake Inhibitors",
                                             level = "ATC 4th")

#maoi
print(paste0("Generating maoi at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm,
                                             name = "maoi_atc",
                                             atcName = "Monoamine oxidase inhibitors, non-selective",
                                             level = "ATC 4th")

print(paste0("Binding ATC class cohorts at ", Sys.time()))
cdm <- omopgenerics::bind(
  cdm$antipsychotics_atc,
  cdm$antidepressants_atc,
  cdm$ccb_atc,
  cdm$antiemetics_atc,
  cdm$antiepileptics_atc,
  cdm$propulsives_atc,
  cdm$antiarrhythmics_atc,
  cdm$antihypentensives_atc,
  cdm$anticholinesterases_atc,
  cdm$ssri_atc,
  cdm$maoi_atc,
  name = "class_hypothesis"
)

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

class_hypothesis <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$class_hypo)
saveRDS(class_hypothesis, here::here(hypothesis_results_subfolder, "class_hypothesis.rds"))

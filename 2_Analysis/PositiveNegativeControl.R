print(paste0("Running positive and negative controls at ", Sys.time()))
print(paste0("See Pratt et al. (2015)"))

controls_subfolder <- here(output_folder, "positive_negative_controls")
if (!dir.exists(controls_subfolder)) {
  dir.create(controls_subfolder)
}

controls_results_subfolder <- here(controls_subfolder, "results")
if (!dir.exists(controls_results_subfolder)) {
  dir.create(controls_results_subfolder)
}

controls_gt_subfolder <- here(controls_subfolder, "gt")
if (!dir.exists(controls_gt_subfolder)) {
  dir.create(controls_gt_subfolder)
}

controls_plots_subfolder <- here(controls_subfolder, "plots")
if (!dir.exists(controls_plots_subfolder)) {
  dir.create(controls_plots_subfolder)
}

print(paste0("Instantiating positive/negative controls cohorts at ", Sys.time()))
# amiodarone 
print(paste0("Generating amiodarone cohort at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "amiodarone",
                                                    ingredient = "amiodarone")
print(paste0("Generated amiodarone cohort at ", Sys.time()))

# levothyroxine
print(paste0("Generating levothyroxine cohort at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "levothyroxine",
                                                    ingredient = "levothyroxine")
print(paste0("Generated levothyroxine cohort at ", Sys.time()))

# allopurinol
print(paste0("Generating allopurinol cohort at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "allopurinol",
                                                    ingredient = "allopurinol")
print(paste0("Generated allopurinol cohort at ", Sys.time()))

print(paste0("Starting PSSA for amiodarone-levothyroxine (Positive control) at ", Sys.time()))
cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 indexTable = "amiodarone",
                                                 markerTable = "levothyroxine",
                                                 name = "amiodarone_thyroxine",
                                                 cohortDateRange = as.Date(c("2008-01-01", "2021-12-31")))

CohortSymmetry::summariseSequenceRatios(cohort = cdm$amiodarone_thyroxine,
                                        minCellCount = minimum_counts) |>
  write.xlsx(file = here(controls_results_subfolder, "amiodarone_levothyroxine_positive_control.xlsx"))

CohortSymmetry::summariseSequenceRatios(cohort = cdm$amiodarone_thyroxine,
                                        minCellCount = minimum_counts) |>
  CohortSymmetry::tableSequenceRatios() %>% 
  gt::gtsave(filename = here(controls_gt_subfolder, "amiodarone_levothyroxine_positive_control.docx"))

CohortSymmetry::summariseTemporalSymmetry(cohort = cdm$amiodarone_thyroxine,
                                          minCellCount = minimum_counts) |>
  CohortSymmetry::plotTemporalSymmetry() %>% 
  ggsave(filename = here(controls_plots_subfolder, "amiodarone_levothyroxine_positive_control_temporal.png"), width = 8, height = 6)

CohortSymmetry::summariseSequenceRatios(cohort = cdm$amiodarone_thyroxine,
                                        minCellCount = minimum_counts) |>
  CohortSymmetry::plotSequenceRatios(onlyASR = T, 
                                     colours = "black") %>% 
  ggsave(filename = here(controls_plots_subfolder, "amiodarone_levothyroxine_positive_control_sr.png"), width = 8, height = 6)

print(paste0("Starting PSSA for levothyroxine-allopurinol (Negative control) at ", Sys.time()))
cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 indexTable = "levothyroxine",
                                                 markerTable = "allopurinol",
                                                 name = "thyroxine_allopurinol",
                                                 cohortDateRange = as.Date(c("2008-01-01", "2021-12-31")))

CohortSymmetry::summariseSequenceRatios(cohort = cdm$thyroxine_allopurinol,
                                        minCellCount = minimum_counts) |>
  write.xlsx(file = here(controls_results_subfolder, "levothyroxine_allopurinol_negative_control.xlsx"))

CohortSymmetry::summariseSequenceRatios(cohort = cdm$thyroxine_allopurinol,
                                        minCellCount = minimum_counts) |>
  CohortSymmetry::tableSequenceRatios() %>% 
  gt::gtsave(filename = here(controls_gt_subfolder, "levothyroxine_allopurinol_negative_control.docx"))

CohortSymmetry::summariseTemporalSymmetry(cohort = cdm$thyroxine_allopurinol,
                                          minCellCount = minimum_counts) |>
  CohortSymmetry::plotTemporalSymmetry() %>% 
  ggsave(filename = here(controls_plots_subfolder, "levothyroxine_allopurinol_negative_control_temporal.png"), width = 8, height = 6)

CohortSymmetry::summariseSequenceRatios(cohort = cdm$thyroxine_allopurinol,
                                        minCellCount = minimum_counts) |>
  CohortSymmetry::plotSequenceRatios(onlyASR = T, 
                                  colours = "black") %>% 
  ggsave(filename = here(controls_plots_subfolder, "levothyroxine_allopurinol_negative_control_sr.png"), width = 8, height = 6)

print(paste0("Starting PSSA for amiodarone-allopurinol (Negative control) at ", Sys.time()))
cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 indexTable = "amiodarone",
                                                 markerTable = "allopurinol",
                                                 name = "amiodarone_allopurinol",
                                                 cohortDateRange = as.Date(c("2008-01-01", "2021-12-31")))

CohortSymmetry::summariseSequenceRatios(cohort = cdm$amiodarone_allopurinol,
                                        minCellCount = minimum_counts) |>
  write.xlsx(file = here(controls_results_subfolder, "amiodarone_allopurinol_negative_control.xlsx"))

CohortSymmetry::summariseSequenceRatios(cohort = cdm$amiodarone_allopurinol,
                                        minCellCount = minimum_counts) |>
  CohortSymmetry::tableSequenceRatios() %>% 
  gt::gtsave(filename = here(controls_gt_subfolder, "amiodarone_allopurinol_negative_control.docx"))

CohortSymmetry::summariseTemporalSymmetry(cohort = cdm$amiodarone_allopurinol,
                                          minCellCount = minimum_counts) |>
  CohortSymmetry::plotTemporalSymmetry() %>% 
  ggsave(filename = here(controls_plots_subfolder, "amiodarone_allopurinol_negative_control_temporal.png"), width = 8, height = 6)

CohortSymmetry::summariseSequenceRatios(cohort = cdm$amiodarone_allopurinol,
                                        minCellCount = minimum_counts) |>
  CohortSymmetry::plotSequenceRatios(onlyASR = T, 
                                    colours = "black") %>% 
  ggsave(filename = here(controls_plots_subfolder, "amiodarone_allopurinol_negative_control_sr.png"), width = 8, height = 6)

controls_result <- omopgenerics::bind(
  CohortSymmetry::summariseSequenceRatios(cohort = cdm$thyroxine_allopurinol,
                                          minCellCount = minimum_counts),
  CohortSymmetry::summariseSequenceRatios(cohort = cdm$amiodarone_thyroxine,
                                          minCellCount = minimum_counts),
  CohortSymmetry::summariseSequenceRatios(cohort = cdm$amiodarone_allopurinol,
                                          minCellCount = minimum_counts)
)

setting <- omopgenerics::settings(controls_result) |>
  dplyr::mutate(additional = "controls")

controls_result <- controls_result |>
  omopgenerics::newSummarisedResult(settings = setting)

omopgenerics::exportSummarisedResult(controls_result, 
                                     fileName = here::here(controls_subfolder, "controls_results_{cdm_name}.csv"))

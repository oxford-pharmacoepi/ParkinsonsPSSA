#################################################################################
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

amiodarone_levothyroxine <- CohortSymmetry::summariseSequenceRatio(cdm = cdm, 
                                                                   sequenceTable = "amiodarone_thyroxine")

CohortSymmetry::tableSequenceRatios(result = amiodarone_levothyroxine) %>% 
  gt::gtsave(filename = here(controls_gt_subfolder, "amiodarone_levothyroxine_positive_control.docx"), width = 8, height = 6)

CohortSymmetry::plotTemporalSymmetry(cdm = cdm, joinedTable = "amiodarone_thyroxine") %>% 
  ggsave(filename = here(controls_plots_subfolder, "amiodarone_levothyroxine_positive_control_temporal.png"))

print(paste0("Starting PSSA for levothyroxine-allopurinol (Negative control) at ", Sys.time()))
cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 indexTable = "levothyroxine",
                                                 markerTable = "allopurinol",
                                                 name = "thyroxine_allopurinol",
                                                 cohortDateRange = as.Date(c("2008-01-01", "2021-12-31")))

thyroxine_allopurinol <- CohortSymmetry::summariseSequenceRatio(cdm = cdm, 
                                                                   sequenceTable = "thyroxine_allopurinol")

CohortSymmetry::tableSequenceRatios(result = thyroxine_allopurinol) %>% 
  gt::gtsave(filename = here(controls_gt_subfolder, "levothyroxine_allopurinol_negative_control.docx"))

CohortSymmetry::plotTemporalSymmetry(cdm = cdm, joinedTable = "thyroxine_allopurinol") %>% 
  ggsave(filename = here(controls_plots_subfolder, "levothyroxine_allopurinol_negative_control_temporal.png"), width = 8, height = 6)

print(paste0("Starting PSSA for amiodarone-allopurinol (Negative control) at ", Sys.time()))
cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 indexTable = "amiodarone",
                                                 markerTable = "allopurinol",
                                                 name = "amiodarone_allopurinol",
                                                 cohortDateRange = as.Date(c("2008-01-01", "2021-12-31")))

amiodarone_allopurinol <- CohortSymmetry::summariseSequenceRatio(cdm = cdm, 
                                                                   sequenceTable = "amiodarone_allopurinol")

CohortSymmetry::tableSequenceRatios(result = amiodarone_allopurinol) %>% 
  gt::gtsave(filename = here(controls_gt_subfolder, "amiodarone_allopurinol_negative_control.docx"))

CohortSymmetry::plotTemporalSymmetry(cdm = cdm, joinedTable = "amiodarone_allopurinol") %>% 
  ggsave(filename = here(controls_plots_subfolder, "amiodarone_allopurinol_negative_control_temporal.png"), width = 8, height = 6)

treatment_order_subfolder <- here(output_folder, "treatment_order")
if (!dir.exists(treatment_order_subfolder)) {
  dir.create(treatment_order_subfolder)
}

treatment_order_results_subfolder <- here(treatment_order_subfolder, "results")
if (!dir.exists(treatment_order_results_subfolder)) {
  dir.create(treatment_order_results_subfolder)
}

treatment_order_gt_subfolder <- here(treatment_order_subfolder, "gt")
if (!dir.exists(treatment_order_gt_subfolder)) {
  dir.create(treatment_order_gt_subfolder)
}

treatment_order_plots_subfolder <- here(treatment_order_subfolder, "plots")
if (!dir.exists(treatment_order_plots_subfolder)) {
  dir.create(treatment_order_plots_subfolder)
}

print(paste0("Starting PSSA for antiparkinsonian drugs at ", Sys.time()))
cdm <- omopgenerics::bind(
  cdm$levodopa, cdm$dopamine_agonists, cdm$maob_inhibitors, cdm$comt_inhibitors, cdm$amantadine,
  name = "parkinson_treatment"
)

cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 indexTable = "parkinson_treatment",
                                                 markerTable = "parkinson_treatment",
                                                 name = "treatment_order",
                                                 cohortDateRange = as.Date(c("2008-01-01", "2021-12-31")))

CohortSymmetry::summariseSequenceRatios(cohort = cdm$treatment_order,
                                        minCellCount = minimum_counts) |>
  write.xlsx(file = here(treatment_order_results_subfolder, "treatment_order.xlsx"))

CohortSymmetry::summariseSequenceRatios(cohort = cdm$treatment_order,
                                        minCellCount = minimum_counts) |>
  CohortSymmetry::tableSequenceRatios() %>% 
  gt::gtsave(filename = here(treatment_order_gt_subfolder, "treatment_order.docx"))

CohortSymmetry::summariseTemporalSymmetry(cohort = cdm$treatment_order,
                                          minCellCount = minimum_counts) |>
  CohortSymmetry::plotTemporalSymmetry() %>% 
  ggsave(filename = here(treatment_order_plots_subfolder, "treatment_order_temporal.png"), width = 30, height = 10)

CohortSymmetry::summariseSequenceRatios(cohort = cdm$treatment_order,
                                        minCellCount = minimum_counts) |>
  CohortSymmetry::plotSequenceRatios(onlyaSR = T, 
                                    colours = "black") %>% 
  ggsave(filename = here(treatment_order_plots_subfolder, "treatment_order_sr.png"), width = 30, height = 10)

print(paste0("Starting data-driven approach in the context of antiparkinson treatment at ", Sys.time()))

data_driven_subfolder <- here(output_folder, "data_driven")
if (!dir.exists(data_driven_subfolder)) {
  dir.create(data_driven_subfolder)
}

data_driven_results_subfolder <- here(data_driven_subfolder, "results")
if (!dir.exists(data_driven_results_subfolder)) {
  dir.create(data_driven_results_subfolder)
}

data_driven_gt_subfolder <- here(data_driven_subfolder, "gt")
if (!dir.exists(data_driven_gt_subfolder)) {
  dir.create(data_driven_gt_subfolder)
}

data_driven_plots_subfolder <- here(data_driven_subfolder, "plots")
if (!dir.exists(data_driven_plots_subfolder)) {
  dir.create(data_driven_plots_subfolder)
}

cdm <- CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = c(schema = results_database_schema, prefix = stem_table), 
  cohort_tables = c("parkinson_treatment",
                    "data_driven")
)

atc_2 <- CodelistGenerator::getATCCodes(
  cdm = cdm,
  level = c("ATC 2nd"))

atc_3 <- CodelistGenerator::getATCCodes(
  cdm = cdm,
  level = c("ATC 3rd"))

atc_4 <- CodelistGenerator::getATCCodes(
  cdm = cdm,
  level = c("ATC 4th"))

names(atc_2)

for (i in (1:5)){
  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = cdm,
    conceptSet = atc_2[i],
    name = names(atc_2)[[i]]
  )
  print(paste0("Generating cohorts for ", names(atc_2)[[i]], " at ", Sys.time()))
  
}

cdm <- omopgenerics::bind(cdm[[names(atc_2)[[1]]]],
                          cdm[[names(atc_2)[[2]]]],
                          name = "data_driven")

for (i in (6:10)){
  cdm <- omopgenerics::bind(
    cdm[["data_driven"]], cdm[[names(atc_2)[[i]]]],
    name = "data_driven"
  )
}

cdm[["data_driven"]] <- cdm[["data_driven"]] %>% 
  dplyr::compute(name = "data_driven",
                 temporary = F)

cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 indexTable = "data_driven",
                                                 markerTable = "parkinson_treatment",
                                                 name = "data_driven_pssa",
                                                 cohortDateRange = as.Date(c("2008-01-01", "2021-12-31")))

CohortSymmetry::summariseSequenceRatio(cdm = cdm, sequenceTable = "data_driven_pssa") |>
  write.xlsx(file = here(data_driven_results_subfolder, "data_driven_atc2.xlsx"))

data_driven_atc2 <- CohortSymmetry::summariseSequenceRatio(cdm = cdm, sequenceTable = "data_driven_pssa")
CohortSymmetry::tableSequenceRatios(result = data_driven_atc2) %>%
  gt::gtsave(filename = here(data_driven_gt_subfolder, "data_driven_atc2.docx"))

CohortSymmetry::plotSequenceRatio(cdm = cdm,
                                  joinedTable = "data_driven_pssa",
                                  sequenceRatio = data_driven_atc2,
                                  onlyaSR = T,
                                  colours = "black") %>%
  ggsave(filename = here(data_driven_plots_subfolder, "data_driven_atc2_sr.png"), width = 30, height = 10)

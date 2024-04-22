############# ATC 2nd
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("CALCIUM CHANNEL BLOCKERS", "ATC 2nd")),
                           table_name = "ccb_class",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

############# ATC 3rd
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("SELECTIVE CALCIUM CHANNEL BLOCKERS WITH MAINLY VASCULAR EFFECTS", "ATC 3rd")),
                           table_name = "ccb_class_c",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("SELECTIVE CALCIUM CHANNEL BLOCKERS WITH DIRECT CARDIAC EFFECTS", "ATC 3rd")),
                           table_name = "ccb_class_d",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("NON-SELECTIVE CALCIUM CHANNEL BLOCKERS", "ATC 3rd")),
                           table_name = "ccb_class_e",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("CALCIUM CHANNEL BLOCKERS AND DIURETICS", "ATC 3rd")),
                           table_name = "ccb_class_g",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

############# ATC 4th
cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("Dihydropyridine derivatives", "ATC 4th")),
                           table_name = "ccb_class_ca",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("Other selective calcium channel blockers with mainly vascular effects", "ATC 4th")),
                           table_name = "ccb_class_cx",
                           start_date = as.Date("2008-01-01"),
                           end_date = as.Date("2021-12-31"))

################ Analyses
table_names <- names(cdm)[names(cdm) %>% stringr::str_detect("ccb_class")]
for (name in table_names){
  cdm <- singleDrugCohortConditionStrata(cdm = cdm,
                                         outcome_table_name = paste0(name, "_in_pd"),
                                         condition_cohort_table = "parkinson_subtypes",
                                         condition_cohort_name = "parkinsonsdisease",
                                         drug_cohort_table = name,
                                         gap_before = Inf,
                                         gap_after = Inf)
}
cascade_ccb <- data.frame()
for (name in table_names){
  cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                           name = paste0(name, "_antiparkinson"),
                                           dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                           indexTable = paste0(name, "_in_pd"),
                                           markerTable = "parkinson_drugs",
                                           daysPriorObservation = 365,
                                           indexWashout = 365,
                                           markerWashout = 365,
                                           timeGap = 365)
  temp <- CohortSymmetry::getSequenceRatios(cdm = cdm,
                                            outcomeTable = paste0(name, "_antiparkinson")) %>% 
    dplyr::mutate(index_name = name) %>% 
    dplyr::inner_join(drug_cohort_set, by = c("marker_id" = "cohort_definition_id"))
  cascade_ccb <- rbind(cascade_ccb, temp) 
}

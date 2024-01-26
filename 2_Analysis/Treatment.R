cdm <- CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema,
  cohort_tables = c("levodopa", "amantadine", "parkinson_subtypes", "Dopamine_agonists", "MAOB_inhibitors", "COMT_inhibitors", "lithium", "levothyroxine", "amiodarone", "ccb", "antidepressants", "ANTIEMETICS")
)

################################################################################################
#### in parkinsonism patients
## Positive Controls
cdm[["levothyroxine2"]] <- cdm[["parkinson_subtypes"]] %>% 
  dplyr::inner_join(cohortSet(cdm[["parkinson_subtypes"]]), copy = T, by = "cohort_definition_id") %>% 
  dplyr::filter(cohort_name == "Parkinsonism") %>% 
  dplyr::inner_join(cdm[["levothyroxine"]] %>% dplyr::select(-cohort_definition_id) %>% dplyr::rename(drug_start_date = cohort_start_date, drug_end_date = cohort_end_date), by = "subject_id") %>% 
  # dplyr::filter(drug_start_date >= cohort_start_date) %>% 
  dplyr::mutate(cohort_definition_id = 1) %>% 
  dplyr::select(-cohort_start_date, -cohort_end_date, -cohort_name) %>% 
  dplyr::rename(cohort_start_date = drug_start_date, 
                cohort_end_date = drug_end_date) %>% 
  dplyr::mutate(cohort_name = "Levothyroxine") %>% 
  CDMConnector::computeQuery()

cdm[["amiodarone2"]] <- cdm[["parkinson_subtypes"]] %>% 
  dplyr::inner_join(cohortSet(cdm[["parkinson_subtypes"]]), copy = T, by = "cohort_definition_id") %>% 
  dplyr::filter(cohort_name == "Parkinsonism") %>% 
  dplyr::inner_join(cdm[["amiodarone"]] %>% dplyr::select(-cohort_definition_id) %>% dplyr::rename(drug_start_date = cohort_start_date, drug_end_date = cohort_end_date), by = "subject_id") %>% 
  # dplyr::filter(drug_start_date >= cohort_start_date) %>% 
  dplyr::mutate(cohort_definition_id = 1) %>% 
  dplyr::select(-cohort_start_date, -cohort_end_date, -cohort_name) %>% 
  dplyr::rename(cohort_start_date = drug_start_date, 
                cohort_end_date = drug_end_date) %>% 
  dplyr::mutate(cohort_name = "Amiodarone") %>% 
  CDMConnector::computeQuery()

##Antiparkinson drugs
cdm[["levodopa2"]] <- cdm[["parkinson_subtypes"]] %>% 
  dplyr::inner_join(cohortSet(cdm[["parkinson_subtypes"]]), copy = T, by = "cohort_definition_id") %>% 
  dplyr::filter(cohort_name == "Parkinsonism") %>% 
  dplyr::inner_join(cdm[["levodopa"]] %>% dplyr::select(-cohort_definition_id) %>% dplyr::rename(drug_start_date = cohort_start_date, drug_end_date = cohort_end_date), by = "subject_id") %>% 
  # dplyr::filter(drug_start_date >= cohort_start_date) %>% 
  dplyr::mutate(cohort_definition_id = 1) %>% 
  dplyr::select(-cohort_start_date, -cohort_end_date, -cohort_name) %>% 
  dplyr::rename(cohort_start_date = drug_start_date, 
                cohort_end_date = drug_end_date) %>% 
  dplyr::mutate(cohort_name = "Levodopa") %>% 
  CDMConnector::computeQuery()

cdm[["da2"]] <- cdm[["parkinson_subtypes"]] %>% 
  dplyr::inner_join(cohortSet(cdm[["parkinson_subtypes"]]), copy = T, by = "cohort_definition_id") %>% 
  dplyr::filter(cohort_name == "Parkinsonism") %>% 
  dplyr::inner_join(cdm[["Dopamine_agonists"]] %>% dplyr::select(-cohort_definition_id) %>% dplyr::rename(drug_start_date = cohort_start_date, drug_end_date = cohort_end_date), by = "subject_id") %>% 
  # dplyr::filter(drug_start_date >= cohort_start_date) %>% 
  dplyr::mutate(cohort_definition_id = 2) %>% 
  dplyr::select(-cohort_start_date, -cohort_end_date, -cohort_name) %>% 
  dplyr::rename(cohort_start_date = drug_start_date, 
                cohort_end_date = drug_end_date) %>% 
  dplyr::mutate(cohort_name = "Dopamine Agonists") %>% 
  CDMConnector::computeQuery()

cdm[["parkinson_drugs"]] <- union_all(cdm[["levodopa2"]], 
                                      cdm[["da2"]]) %>% 
  CDMConnector::computeQuery()

cdm[["comt2"]] <- cdm[["parkinson_subtypes"]] %>% 
  dplyr::inner_join(cohortSet(cdm[["parkinson_subtypes"]]), copy = T, by = "cohort_definition_id") %>% 
  dplyr::filter(cohort_name == "Parkinsonism") %>% 
  dplyr::inner_join(cdm[["COMT_inhibitors"]] %>% dplyr::select(-cohort_definition_id) %>% dplyr::rename(drug_start_date = cohort_start_date, drug_end_date = cohort_end_date), by = "subject_id") %>% 
  # dplyr::filter(drug_start_date >= cohort_start_date) %>% 
  dplyr::mutate(cohort_definition_id = 3) %>% 
  dplyr::select(-cohort_start_date, -cohort_end_date, -cohort_name) %>% 
  dplyr::rename(cohort_start_date = drug_start_date, 
                cohort_end_date = drug_end_date) %>% 
  dplyr::mutate(cohort_name = "COMT inhibitors") %>% 
  CDMConnector::computeQuery()

cdm[["parkinson_drugs"]] <- union_all(cdm[["parkinson_drugs"]], 
                                      cdm[["comt2"]]) %>% 
  CDMConnector::computeQuery()

cdm[["amantadine2"]] <- cdm[["parkinson_subtypes"]] %>% 
  dplyr::inner_join(cohortSet(cdm[["parkinson_subtypes"]]), copy = T, by = "cohort_definition_id") %>% 
  dplyr::filter(cohort_name == "Parkinsonism") %>% 
  dplyr::inner_join(cdm[["amantadine"]] %>% dplyr::select(-cohort_definition_id) %>% dplyr::rename(drug_start_date = cohort_start_date, drug_end_date = cohort_end_date), by = "subject_id") %>% 
  # dplyr::filter(drug_start_date >= cohort_start_date) %>% 
  dplyr::mutate(cohort_definition_id = 4) %>% 
  dplyr::select(-cohort_start_date, - cohort_end_date, - cohort_name) %>% 
  dplyr::rename(cohort_start_date = drug_start_date, 
                cohort_end_date = drug_end_date) %>% 
  dplyr::mutate(cohort_name = "Amantadine") %>% 
  CDMConnector::computeQuery()

cdm[["parkinson_drugs"]] <- union_all(cdm[["parkinson_drugs"]], 
                                      cdm[["amantadine2"]]) %>% 
  CDMConnector::computeQuery()

cdm[["maob2"]] <- cdm[["parkinson_subtypes"]] %>% 
  dplyr::inner_join(cohortSet(cdm[["parkinson_subtypes"]]), copy = T, by = "cohort_definition_id") %>% 
  dplyr::filter(cohort_name == "Parkinsonism") %>% 
  dplyr::inner_join(cdm[["MAOB_inhibitors"]] %>% dplyr::select(-cohort_definition_id) %>% dplyr::rename(drug_start_date = cohort_start_date, drug_end_date = cohort_end_date), by = "subject_id") %>% 
  # dplyr::filter(drug_start_date >= cohort_start_date) %>% 
  dplyr::mutate(cohort_definition_id = 5) %>% 
  dplyr::select(-cohort_start_date, - cohort_end_date, - cohort_name) %>% 
  dplyr::rename(cohort_start_date = drug_start_date, 
                cohort_end_date = drug_end_date) %>% 
  dplyr::mutate(cohort_name = "MAO-B inhibitors") %>% 
  CDMConnector::computeQuery()

cdm[["parkinson_drugs"]] <- union_all(cdm[["parkinson_drugs"]], 
                                      cdm[["maob2"]]) %>% 
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "drug_pathway_parkinson",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "parkinson_drugs",
                                         markerTable = "parkinson_drugs",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 730)

pathway_results <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                     outcomeTable = "drug_pathway_parkinson")
cohort_set <- cdm[["parkinson_drugs"]] %>% 
  dplyr::select(cohort_definition_id, cohort_name) %>% 
  dplyr::distinct() %>% 
  dplyr::collect()

pathway_results <- pathway_results %>% 
  dplyr::inner_join(cohort_set %>% dplyr::rename("index_name" = "cohort_name"),
                    by = c("index_id" = "cohort_definition_id")) %>% 
  dplyr::inner_join(cohort_set %>% dplyr::rename("marker_name" = "cohort_name"),
                    by = c("marker_id" = "cohort_definition_id")) %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCI, upperCI, cdm_name)
  
pathway_results <- pathway_results %>% 
  dplyr::mutate(csr = round(csr, digit = 2),
                asr = round(asr, digits = 2),
                lowerCI = round(lowerCI, digits = 2),
                upperCI = round(upperCI, digits = 2))
#################################################################################################
#################################################################################################
### in general population
cdm[["parkinson_drugs_overall"]] <- union_all(cdm[["levodopa"]] %>% dplyr::mutate(cohort_definition_id = 1, cohort_name = "Levodopa"), 
                                      cdm[["Dopamine_agonists"]] %>% dplyr::mutate(cohort_definition_id = 2, cohort_name = "Dopamine Agonists")) %>% 
  CDMConnector::computeQuery()

cdm[["parkinson_drugs_overall"]] <- union_all(cdm[["parkinson_drugs_overall"]],
                                              cdm[["COMT_inhibitors"]] %>% dplyr::mutate(cohort_definition_id = 3, cohort_name = "COMT Inhibitors")) %>% 
  CDMConnector::computeQuery()

cdm[["parkinson_drugs_overall"]] <- union_all(cdm[["parkinson_drugs_overall"]],
                                              cdm[["amantadine"]] %>% dplyr::mutate(cohort_definition_id = 4, cohort_name = "Amantadine")) %>% 
  CDMConnector::computeQuery()

cdm[["parkinson_drugs_overall"]] <- union_all(cdm[["parkinson_drugs_overall"]],
                                              cdm[["MAOB_inhibitors"]] %>% dplyr::mutate(cohort_definition_id = 5, cohort_name = "MAO-B Inhibitors")) %>% 
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "drug_pathway_parkinson_overall",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "parkinson_drugs_overall",
                                         markerTable = "parkinson_drugs_overall",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 730)

pathway_results_overall <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                     outcomeTable = "drug_pathway_parkinson_overall")

cohort_set_overall <- cdm[["parkinson_drugs_overall"]] %>% 
  dplyr::select(cohort_definition_id, cohort_name) %>% 
  dplyr::distinct() %>% 
  dplyr::collect()

pathway_results_overall <- pathway_results_overall %>% 
  dplyr::inner_join(cohort_set_overall %>% dplyr::rename("index_name" = "cohort_name"),
                    by = c("index_id" = "cohort_definition_id")) %>% 
  dplyr::inner_join(cohort_set_overall %>% dplyr::rename("marker_name" = "cohort_name"),
                    by = c("marker_id" = "cohort_definition_id")) %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCI, upperCI, cdm_name)

pathway_results_overall <- pathway_results_overall %>% 
  dplyr::mutate(csr = round(csr, digit = 2),
                asr = round(asr, digits = 2),
                lowerCI = round(lowerCI, digits = 2),
                upperCI = round(upperCI, digits = 2))

write.xlsx(pathway_results_overall, "PathwayResults_gen_pop.xlsx")
write.xlsx(pathway_results, "PathwayResults_Parkinsonism.xlsx")

###############################################################################################
#Positive control
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "amiodarone_thyroxine",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "amiodarone",
                                         markerTable = "levothyroxine",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

amiodarone_levothyroxin <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                             outcomeTable = "amiodarone_thyroxine")

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "amiodarone_thyroxine_parkinson",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "amiodarone2",
                                         markerTable = "levothyroxine2",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

amiodarone_levothyroxin_parkinson <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                             outcomeTable = "amiodarone_thyroxine_parkinson")

###############################################################################################
### others
cdm[["lithium"]] <- cdm[["lithium"]] %>% 
  dplyr::mutate(cohort_name = "Lithium") %>% 
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "drug_lithium_parkinson",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "lithium",
                                         markerTable = "parkinson_drugs",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 730)

results_pssa <- CohortSymmetry::getSequenceRatios(cdm = cdm, outcomeTable = "drug_lithium_parkinson") %>% 
  dplyr::mutate(index_name = "lithium") %>% 
  dplyr::mutate(index_id = 1) %>% 
  dplyr::inner_join(cohort_set_overall %>% dplyr::rename("marker_name" = "cohort_name"),
                           by = c("marker_id" = "cohort_definition_id")) %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCI, upperCI, cdm_name)

cdm[["ccb"]] <- cdm[["ccb"]] %>% 
  dplyr::mutate(cohort_name = "CCB") %>% 
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "drug_ccb_parkinson",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "ccb",
                                         markerTable = "parkinson_drugs",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 730)

ccb_antiparkinson_overall <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                                   outcomeTable = "drug_ccb_parkinson")

results_pssa <- rbind(results_pssa, CohortSymmetry::getSequenceRatios(cdm = cdm, outcomeTable = "drug_ccb_parkinson") %>% 
  dplyr::mutate(index_name = "CCB") %>% 
  dplyr::mutate(index_id = 2) %>% 
  dplyr::inner_join(cohort_set_overall %>% dplyr::rename("marker_name" = "cohort_name"),
                    by = c("marker_id" = "cohort_definition_id")) %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCI, upperCI, cdm_name))

cdm[["antidepressants"]] <- cdm[["antidepressants"]] %>% 
  dplyr::mutate(cohort_name = "antidepressants") %>% 
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "drug_antidepressants_parkinson",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "antidepressants",
                                         markerTable = "parkinson_drugs",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 730)

results_pssa <- rbind(results_pssa, CohortSymmetry::getSequenceRatios(cdm = cdm, outcomeTable = "drug_antidepressants_parkinson") %>% 
                        dplyr::mutate(index_name = "antidepressants") %>% 
                        dplyr::mutate(index_id = 3) %>% 
                        dplyr::inner_join(cohort_set_overall %>% dplyr::rename("marker_name" = "cohort_name"),
                                          by = c("marker_id" = "cohort_definition_id")) %>% 
                        dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCI, upperCI, cdm_name))

cdm[["ANTIEMETICS"]] <- cdm[["ANTIEMETICS"]] %>% 
  dplyr::mutate(cohort_name = "Antiemetics") %>% 
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "drug_antiemetics_parkinson",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "ANTIEMETICS",
                                         markerTable = "parkinson_drugs",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 730)

results_pssa <- rbind(results_pssa, CohortSymmetry::getSequenceRatios(cdm = cdm, outcomeTable = "drug_antiemetics_parkinson") %>% 
                        dplyr::mutate(index_name = "antiemetics") %>% 
                        dplyr::mutate(index_id = 4) %>% 
                        dplyr::inner_join(cohort_set_overall %>% dplyr::rename("marker_name" = "cohort_name"),
                                          by = c("marker_id" = "cohort_definition_id")) %>% 
                        dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCI, upperCI, cdm_name))

write.xlsx(results_pssa, "results_pssa_gen_pop.xlsx")

cdm <- CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = c("schema" = results_database_schema, 
                   "prefix" = stem_table),
  cdm_name = db.name,
  cohort_tables = c("parkinson_subtypes", "amiodarone", "levothyroxine", "allopurinol", "levodopa", "maob_inhibitors", "amantadine", "dopamine_agonists", "comt_inhibitors", "ccb", "dopamine_depleters", "antiemetics", "atypical_antipsychotics", "typical_antipsychotics")
) 

cdm <- CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = c("schema" = results_database_schema, 
                   "prefix" = stem_table),
  cdm_name = db.name,
  cohort_tables = c("amiodarone", "levothyroxine")
) 

################################################################################################
#                                                                                              #
#                                       in PD populations                                      #
#                                                                                              #
################################################################################################
## Positive Controls
singleDrugCohortConditionStrata <- function(cdm, 
                                            outcome_table_name,
                                            condition_cohort_table, 
                                            condition_cohort_name, 
                                            drug_cohort_table,
                                            gap_before,
                                            gap_after){
  if(gap_before == Inf){
    gap_before <- 999999999999999999999
  }
  if(gap_after == Inf){
    gap_after <- 999999999999999999999
  }
  
  cdm[["condition_cohort_temp"]] <- cdm[[condition_cohort_table]] %>% 
    dplyr::inner_join(attributes(cdm[[condition_cohort_table]])$cohort_set, copy = T, by = "cohort_definition_id") %>% 
    dplyr::filter(cohort_name == condition_cohort_name) %>% 
    dplyr::select(cohort_definition_id, cohort_name, subject_id, cohort_start_date, cohort_end_date) %>% 
    dplyr::compute()
  
  cdm[["drug_cohort_temp"]] <- cdm[[drug_cohort_table]] %>% 
    dplyr::inner_join(attributes(cdm[[drug_cohort_table]])$cohort_set, copy = T, by = "cohort_definition_id") %>% 
    dplyr::select(cohort_definition_id, cohort_name, subject_id, cohort_start_date, cohort_end_date) %>% 
    dplyr::compute()
  
  cdm[[outcome_table_name]] <- cdm[["condition_cohort_temp"]] %>% 
    dplyr::rename(condition_cohort_id = cohort_definition_id,
                  condition_cohort_name = cohort_name) %>% 
    dplyr::inner_join(cdm[["drug_cohort_temp"]] %>% dplyr::rename(drug_cohort_name = cohort_name, drug_start_date = cohort_start_date, drug_end_date = cohort_end_date), by = "subject_id") %>% 
    dplyr::mutate(gap_to_drug = cohort_start_date - drug_start_date) %>% 
    dplyr::filter(-gap_before <= gap_to_drug & gap_to_drug <= gap_after) %>% 
    dplyr::select(-cohort_start_date, -cohort_end_date, -gap_to_drug) %>% 
    dplyr::rename(cohort_start_date = drug_start_date,
                  cohort_end_date = drug_end_date) %>% 
    dplyr::compute()
  return(cdm)
}

##amiodarone
cdm <- singleDrugCohortConditionStrata(cdm = cdm,
                                       outcome_table_name = "amiodarone2",
                                       condition_cohort_table = "parkinson_subtypes",
                                       condition_cohort_name = "parkinsonsdisease",
                                       drug_cohort_table = "amiodarone",
                                       gap_before = Inf,
                                       gap_after = Inf)

##levothyroxine
cdm <- singleDrugCohortConditionStrata(cdm = cdm,
                                       outcome_table_name = "levothyroxine2",
                                       condition_cohort_table = "parkinson_subtypes",
                                       condition_cohort_name = "parkinsonsdisease",
                                       drug_cohort_table = "levothyroxine",
                                       gap_before = Inf,
                                       gap_after = Inf)

##allopurinol
cdm <- singleDrugCohortConditionStrata(cdm = cdm,
                                       outcome_table_name = "allopurinol2",
                                       condition_cohort_table = "parkinson_subtypes",
                                       condition_cohort_name = "parkinsonsdisease",
                                       drug_cohort_table = "allopurinol",
                                       gap_before = Inf,
                                       gap_after = Inf)

## Antiparkinson drugs
## Levodopa
cdm <- singleDrugCohortConditionStrata(cdm = cdm,
                                       outcome_table_name = "levodopa2",
                                       condition_cohort_table = "parkinson_subtypes",
                                       condition_cohort_name = "parkinsonsdisease",
                                       drug_cohort_table = "levodopa",
                                       gap_before = Inf,
                                       gap_after = Inf)

cdm$levodopa2 <- cdm$levodopa2 %>% dplyr::mutate(cohort_definition_id = 1)
## DA
cdm <- singleDrugCohortConditionStrata(cdm = cdm,
                                       outcome_table_name = "da2",
                                       condition_cohort_table = "parkinson_subtypes",
                                       condition_cohort_name = "parkinsonsdisease",
                                       drug_cohort_table = "dopamine_agonists",
                                       gap_before = Inf,
                                       gap_after = Inf)
cdm$da2 <- cdm$da2 %>% dplyr::mutate(cohort_definition_id = 2)

cdm[["parkinson_drugs"]] <- union_all(cdm[["levodopa2"]], 
                                      cdm[["da2"]]) %>% 
  CDMConnector::computeQuery()

## COMT inhibitors
cdm <- singleDrugCohortConditionStrata(cdm = cdm,
                                       outcome_table_name = "comt2",
                                       condition_cohort_table = "parkinson_subtypes",
                                       condition_cohort_name = "parkinsonsdisease",
                                       drug_cohort_table = "comt_inhibitors",
                                       gap_before = Inf,
                                       gap_after = Inf)
cdm$comt2 <- cdm$comt2 %>% dplyr::mutate(cohort_definition_id = 3)

cdm[["parkinson_drugs"]] <- union_all(cdm[["parkinson_drugs"]], 
                                      cdm[["comt2"]]) %>% 
  CDMConnector::computeQuery()

## Amantadine
cdm <- singleDrugCohortConditionStrata(cdm = cdm,
                                       outcome_table_name = "amantadine2",
                                       condition_cohort_table = "parkinson_subtypes",
                                       condition_cohort_name = "parkinsonsdisease",
                                       drug_cohort_table = "amantadine",
                                       gap_before = Inf,
                                       gap_after = Inf)
cdm$amantadine2 <- cdm$amantadine2 %>% dplyr::mutate(cohort_definition_id = 4)

cdm[["parkinson_drugs"]] <- union_all(cdm[["parkinson_drugs"]], 
                                      cdm[["amantadine2"]]) %>% 
  CDMConnector::computeQuery()

## MAOB
cdm <- singleDrugCohortConditionStrata(cdm = cdm,
                                       outcome_table_name = "maob2",
                                       condition_cohort_table = "parkinson_subtypes",
                                       condition_cohort_name = "parkinsonsdisease",
                                       drug_cohort_table = "maob_inhibitors",
                                       gap_before = Inf,
                                       gap_after = Inf)
cdm$maob2 <- cdm$maob2 %>% dplyr::mutate(cohort_definition_id = 5)
cohort_definition_id <- c(1:5)
cohort_name <- c("levodopa", "dopamine agonists", "COMT inhibitors", "amantadine", "MAOB inhibitors") 
drug_cohort_set <- tibble(cohort_definition_id, cohort_name)

cdm[["parkinson_drugs"]] <- union_all(cdm[["parkinson_drugs"]], 
                                      cdm[["maob2"]]) %>% 
  dplyr::compute() 

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "drug_pathway_parkinson",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "parkinson_drugs",
                                         markerTable = "parkinson_drugs",
                                         combinationWindow = c(0, 730))

pathway_results <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                     outcomeTable = "drug_pathway_parkinson")
cohort_set <- cdm[["parkinson_drugs"]] %>% 
  dplyr::select(cohort_definition_id, drug_cohort_name) %>% 
  dplyr::distinct() %>% 
  dplyr::collect()

pathway_results <- pathway_results %>% 
  dplyr::inner_join(cohort_set %>% dplyr::rename("index_name" = "drug_cohort_name"),
                    by = c("index_id" = "cohort_definition_id")) %>% 
  dplyr::inner_join(cohort_set %>% dplyr::rename("marker_name" = "drug_cohort_name"),
                    by = c("marker_id" = "cohort_definition_id")) %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCI, upperCI, cdm_name)
  
pathway_results <- pathway_results %>% 
  dplyr::mutate(csr = round(csr, digit = 2),
                asr = round(asr, digits = 2),
                lowerCI = round(lowerCI, digits = 2),
                upperCI = round(upperCI, digits = 2))
write.xlsx(pathway_results, "PathwayResults_PD.xlsx")

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
                                         washoutWindow = 365)

amiodarone_levothyroxine <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
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

positive_control <- amiodarone_levothyroxine %>% 
  dplyr::mutate(comment = "In the general population") %>% 
  rbind(amiodarone_levothyroxin_parkinson %>% dplyr::mutate(comment = "In the Parkinson's population")) %>% 
  dplyr::mutate(index_name = "Amiodarone",
                marker_name = "Levothyroxine") %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCSR_CI, upperCSR_CI, lowerASR_CI, upperASR_CI, comment, cdm_name)

write.xlsx(positive_control, "positive_control_PD.xlsx")

###############################################################################################
#Negative control
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "thyroxine_allopurinol",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "levothyroxine",
                                         markerTable = "allopurinol")

levothyroxine_allopurinol <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                               outcomeTable = "thyroxine_allopurinol")

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "thyroxine_allopurinol_parkinson",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "levothyroxine2",
                                         markerTable = "allopurinol2",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

levothyroxine_allopurinol_parkinson <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                                        outcomeTable = "thyroxine_allopurinol_parkinson")

negative_control <- levothyroxine_allopurinol %>% 
  dplyr::mutate(comment = "In the general population") %>% 
  rbind(levothyroxine_allopurinol_parkinson %>% dplyr::mutate(comment = "In the Parkinson's population")) %>% 
  dplyr::mutate(index_name = "Levothyroxine",
                marker_name = "Allopurinol") %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCSR_CI, upperCSR_CI, lowerASR_CI, upperASR_CI, comment, cdm_name)

########################################################################
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "amiodarone_allopurinol",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "amiodarone",
                                         markerTable = "allopurinol",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

amiodarone_allopurinol <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                              outcomeTable = "amiodarone_allopurinol")

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "amiodarone_allopurinol_parkinson",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "amiodarone2",
                                         markerTable = "allopurinol2",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

amiodarone_allopurinol_parkinson <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                                       outcomeTable = "amiodarone_allopurinol_parkinson")

negative_control2 <- amiodarone_allopurinol %>% 
  dplyr::mutate(comment = "In the general population") %>% 
  rbind(amiodarone_allopurinol_parkinson %>% dplyr::mutate(comment = "In the Parkinson's population")) %>% 
  dplyr::mutate(index_name = "Amiodarone",
                marker_name = "Allopurinol") %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCSR_CI, upperCSR_CI, lowerASR_CI, upperASR_CI, comment, cdm_name)

negative_control <- rbind(negative_control, negative_control2)
write.xlsx(negative_control, here(output_folder,"negative_control_PD.xlsx"))

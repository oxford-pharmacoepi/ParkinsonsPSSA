# 1. CCB
cdm <- singleDrugCohortConditionStrata(cdm = cdm,
                                       outcome_table_name = "ccb2",
                                       condition_cohort_table = "parkinson_subtypes",
                                       condition_cohort_name = "parkinsonsdisease",
                                       drug_cohort_table = "ccb",
                                       gap_before = Inf,
                                       gap_after = Inf)

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ccb_antiparkinson",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "ccb2",
                                         markerTable = "parkinson_drugs",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ccb_antiparkinson <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                       outcomeTable = "ccb_antiparkinson") %>% 
  dplyr::inner_join(attributes(cdm$ccb)$cohort_set %>% 
                      dplyr::select(cohort_definition_id, cohort_name) %>% 
                      dplyr::rename("index_id" = "cohort_definition_id",
                                    "index_name" = "cohort_name"),
                    by = "index_id",
                    copy = T) %>% 
  dplyr::inner_join(drug_cohort_set %>% 
                      dplyr::select(cohort_definition_id, cohort_name) %>% 
                      dplyr::rename("marker_id" = "cohort_definition_id",
                                    "marker_name" = "cohort_name"),
                    by = "marker_id",
                    copy = T) %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCSR_CI, upperCSR_CI, lowerASR_CI, upperASR_CI, cdm_name) %>% 
  dplyr::mutate(comment = "CCB - Antiparkinsons")

# 2. dopamine_depleters
cdm <- singleDrugCohortConditionStrata(cdm = cdm,
                                       outcome_table_name = "dopamine_depleters2",
                                       condition_cohort_table = "parkinson_subtypes",
                                       condition_cohort_name = "parkinsonsdisease",
                                       drug_cohort_table = "dopamine_depleters",
                                       gap_before = Inf,
                                       gap_after = Inf)

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "dopamine_depleters_antiparkinson",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "dopamine_depleters2",
                                         markerTable = "parkinson_drugs",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

dopamine_depleters_antiparkinson <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                       outcomeTable = "dopamine_depleters_antiparkinson") %>% 
  dplyr::inner_join(attributes(cdm$dopamine_depleters)$cohort_set %>% 
                      dplyr::select(cohort_definition_id, cohort_name) %>% 
                      dplyr::rename("index_id" = "cohort_definition_id",
                                    "index_name" = "cohort_name"),
                    by = "index_id",
                    copy = T) %>% 
  dplyr::inner_join(drug_cohort_set %>% 
                      dplyr::select(cohort_definition_id, cohort_name) %>% 
                      dplyr::rename("marker_id" = "cohort_definition_id",
                                    "marker_name" = "cohort_name"),
                    by = "marker_id",
                    copy = T) %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCSR_CI, upperCSR_CI, lowerASR_CI, upperASR_CI, cdm_name)%>% 
  dplyr::mutate(comment = "Dopamine Depleters - Antiparkinsons")

# 3. antiemetics
cdm <- singleDrugCohortConditionStrata(cdm = cdm,
                                       outcome_table_name = "antiemetics2",
                                       condition_cohort_table = "parkinson_subtypes",
                                       condition_cohort_name = "parkinsonsdisease",
                                       drug_cohort_table = "antiemetics",
                                       gap_before = Inf,
                                       gap_after = Inf)

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "antiemetics_antiparkinson",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "antiemetics2",
                                         markerTable = "parkinson_drugs",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

antiemetics_antiparkinson <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                                      outcomeTable = "antiemetics_antiparkinson") %>% 
  dplyr::inner_join(attributes(cdm$antiemetics)$cohort_set %>% 
                      dplyr::select(cohort_definition_id, cohort_name) %>% 
                      dplyr::rename("index_id" = "cohort_definition_id",
                                    "index_name" = "cohort_name"),
                    by = "index_id",
                    copy = T) %>% 
  dplyr::inner_join(drug_cohort_set %>% 
                      dplyr::select(cohort_definition_id, cohort_name) %>% 
                      dplyr::rename("marker_id" = "cohort_definition_id",
                                    "marker_name" = "cohort_name"),
                    by = "marker_id",
                    copy = T) %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCSR_CI, upperCSR_CI, lowerASR_CI, upperASR_CI, cdm_name)%>% 
  dplyr::mutate(comment = "Antiemetics - Antiparkinsons")


# 4. typical_antipsychotics
cdm <- singleDrugCohortConditionStrata(cdm = cdm,
                                       outcome_table_name = "typical_antipsychotics2",
                                       condition_cohort_table = "parkinson_subtypes",
                                       condition_cohort_name = "parkinsonsdisease",
                                       drug_cohort_table = "typical_antipsychotics",
                                       gap_before = Inf,
                                       gap_after = Inf)

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "typical_antipsychotics_antiparkinson",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "typical_antipsychotics2",
                                         markerTable = "parkinson_drugs",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

typical_antipsychotics_antiparkinson <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                               outcomeTable = "typical_antipsychotics_antiparkinson") %>% 
  dplyr::inner_join(attributes(cdm$typical_antipsychotics)$cohort_set %>% 
                      dplyr::select(cohort_definition_id, cohort_name) %>% 
                      dplyr::rename("index_id" = "cohort_definition_id",
                                    "index_name" = "cohort_name"),
                    by = "index_id",
                    copy = T) %>% 
  dplyr::inner_join(drug_cohort_set %>% 
                      dplyr::select(cohort_definition_id, cohort_name) %>% 
                      dplyr::rename("marker_id" = "cohort_definition_id",
                                    "marker_name" = "cohort_name"),
                    by = "marker_id",
                    copy = T) %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCSR_CI, upperCSR_CI, lowerASR_CI, upperASR_CI, cdm_name)%>% 
  dplyr::mutate(comment = "Typical Antipsychotics - Antiparkinsons")

# 5. atypical_antipsychotics
cdm <- singleDrugCohortConditionStrata(cdm = cdm,
                                       outcome_table_name = "atypical_antipsychotics2",
                                       condition_cohort_table = "parkinson_subtypes",
                                       condition_cohort_name = "parkinsonsdisease",
                                       drug_cohort_table = "atypical_antipsychotics",
                                       gap_before = Inf,
                                       gap_after = Inf)

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "atypical_antipsychotics_antiparkinson",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "atypical_antipsychotics2",
                                         markerTable = "parkinson_drugs",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

atypical_antipsychotics_antiparkinson <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                                          outcomeTable = "atypical_antipsychotics_antiparkinson") %>% 
  dplyr::inner_join(attributes(cdm$atypical_antipsychotics)$cohort_set %>% 
                      dplyr::select(cohort_definition_id, cohort_name) %>% 
                      dplyr::rename("index_id" = "cohort_definition_id",
                                    "index_name" = "cohort_name"),
                    by = "index_id",
                    copy = T) %>% 
  dplyr::inner_join(drug_cohort_set %>% 
                      dplyr::select(cohort_definition_id, cohort_name) %>% 
                      dplyr::rename("marker_id" = "cohort_definition_id",
                                    "marker_name" = "cohort_name"),
                    by = "marker_id",
                    copy = T) %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCSR_CI, upperCSR_CI, lowerASR_CI, upperASR_CI, cdm_name)%>% 
  dplyr::mutate(comment = "Atypical Antipsychotics - Antiparkinsons")

########################################################################################
cdm[["parkinson_drugs_total"]] <- cdm[["parkinson_drugs"]] %>% 
  dplyr::mutate(cohort_definition_id = 1)
cdm[["ccb_total"]] <- cdm[["ccb2"]] %>% 
  dplyr::mutate(cohort_definition_id = 1)
cdm[["dopamine_depleters_total"]] <- cdm[["dopamine_depleters2"]] %>% 
  dplyr::mutate(cohort_definition_id = 1)
cdm[["antiemetics_total"]] <- cdm[["antiemetics2"]] %>% 
  dplyr::mutate(cohort_definition_id = 1)
cdm[["typical_antipsychotics_total"]] <- cdm[["typical_antipsychotics2"]] %>% 
  dplyr::mutate(cohort_definition_id = 1)
cdm[["atypical_antipsychotics_total"]] <- cdm[["atypical_antipsychotics2"]] %>% 
  dplyr::mutate(cohort_definition_id = 1)

### CCB
#total v no
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ccb_total_antiparkinson",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "ccb_total",
                                         markerTable = "parkinson_drugs",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ccb_total_antiparkinson <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                       outcomeTable = "ccb_total_antiparkinson") %>% 
  dplyr::mutate(index_name = "CCB") %>% 
  dplyr::inner_join(drug_cohort_set %>% 
                      dplyr::select(cohort_definition_id, cohort_name) %>% 
                      dplyr::rename("marker_id" = "cohort_definition_id",
                                    "marker_name" = "cohort_name"),
                    by = "marker_id",
                    copy = T) %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCSR_CI, upperCSR_CI, lowerASR_CI, upperASR_CI, cdm_name) %>% 
  dplyr::mutate(comment = "CCB - Antiparkinsons")

#no vs total
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ccb_antiparkinson_total",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "ccb2",
                                         markerTable = "parkinson_drugs_total",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ccb_antiparkinson_total <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                             outcomeTable = "ccb_antiparkinson_total") %>% 
  dplyr::inner_join(attributes(cdm$ccb)$cohort_set %>% 
                      dplyr::select(cohort_definition_id, cohort_name) %>% 
                      dplyr::rename("index_id" = "cohort_definition_id",
                                    "index_name" = "cohort_name"),
                    by = "index_id",
                    copy = T) %>% 
  dplyr::mutate(marker_name = "Antiparkinson Drugs") %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCSR_CI, upperCSR_CI, lowerASR_CI, upperASR_CI, cdm_name) %>% 
  dplyr::mutate(comment = "CCB - Antiparkinsons")

#total v total
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ccb_total_antiparkinson_total",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "ccb_total",
                                         markerTable = "parkinson_drugs_total",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ccb_total_antiparkinson_total <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                             outcomeTable = "ccb_total_antiparkinson_total") %>% 
  dplyr::mutate(index_name = "CCB") %>% 
  dplyr::mutate(marker_name = "Antiparkinson Drugs") %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCSR_CI, upperCSR_CI, lowerASR_CI, upperASR_CI, cdm_name) %>% 
  dplyr::mutate(comment = "CCB - Antiparkinsons")

ccb_results <- rbind(ccb_antiparkinson,
                     ccb_total_antiparkinson,
                     ccb_antiparkinson_total,
                     ccb_total_antiparkinson_total)

### dopamine_depleters
#total v no
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "dopamine_depleters_total_antiparkinson",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "dopamine_depleters_total",
                                         markerTable = "parkinson_drugs",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

dopamine_depleters_total_antiparkinson <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                             outcomeTable = "dopamine_depleters_total_antiparkinson") %>% 
  dplyr::mutate(index_name = "dopamine_depleters") %>% 
  dplyr::inner_join(drug_cohort_set %>% 
                      dplyr::select(cohort_definition_id, cohort_name) %>% 
                      dplyr::rename("marker_id" = "cohort_definition_id",
                                    "marker_name" = "cohort_name"),
                    by = "marker_id",
                    copy = T) %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCSR_CI, upperCSR_CI, lowerASR_CI, upperASR_CI, cdm_name) %>% 
  dplyr::mutate(comment = "Dopamine Depleters - Antiparkinsons")

#no vs total
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "dopamine_depleters_antiparkinson_total",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "dopamine_depleters2",
                                         markerTable = "parkinson_drugs_total",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

dopamine_depleters_antiparkinson_total <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                             outcomeTable = "dopamine_depleters_antiparkinson_total") %>% 
  dplyr::inner_join(attributes(cdm$dopamine_depleters)$cohort_set %>% 
                      dplyr::select(cohort_definition_id, cohort_name) %>% 
                      dplyr::rename("index_id" = "cohort_definition_id",
                                    "index_name" = "cohort_name"),
                    by = "index_id",
                    copy = T) %>% 
  dplyr::mutate(marker_name = "Antiparkinson Drugs") %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCSR_CI, upperCSR_CI, lowerASR_CI, upperASR_CI, cdm_name) %>% 
  dplyr::mutate(comment = "Dopamine Depleters - Antiparkinsons")

#total v total
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "dopamine_depleters_total_antiparkinson_total",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "dopamine_depleters_total",
                                         markerTable = "parkinson_drugs_total",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

dopamine_depleters_total_antiparkinson_total <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                                   outcomeTable = "dopamine_depleters_total_antiparkinson_total") %>% 
  dplyr::mutate(index_name = "dopamine_depleters") %>% 
  dplyr::mutate(marker_name = "Antiparkinson Drugs") %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCSR_CI, upperCSR_CI, lowerASR_CI, upperASR_CI, cdm_name) %>% 
  dplyr::mutate(comment = "Dopamine Depleters - Antiparkinsons")

dopamine_depleters_results <- rbind(dopamine_depleters_antiparkinson,
                     dopamine_depleters_total_antiparkinson,
                     dopamine_depleters_antiparkinson_total,
                     dopamine_depleters_total_antiparkinson_total)

### antiemetics
#total v no
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "antiemetics_total_antiparkinson",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "antiemetics_total",
                                         markerTable = "parkinson_drugs",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

antiemetics_total_antiparkinson <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                             outcomeTable = "antiemetics_total_antiparkinson") %>% 
  dplyr::mutate(index_name = "antiemetics") %>% 
  dplyr::inner_join(drug_cohort_set %>% 
                      dplyr::select(cohort_definition_id, cohort_name) %>% 
                      dplyr::rename("marker_id" = "cohort_definition_id",
                                    "marker_name" = "cohort_name"),
                    by = "marker_id",
                    copy = T) %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCSR_CI, upperCSR_CI, lowerASR_CI, upperASR_CI, cdm_name) %>% 
  dplyr::mutate(comment = "Antiemetics - Antiparkinsons")

#no vs total
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "antiemetics_antiparkinson_total",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "antiemetics2",
                                         markerTable = "parkinson_drugs_total",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

antiemetics_antiparkinson_total <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                             outcomeTable = "antiemetics_antiparkinson_total") %>% 
  dplyr::inner_join(attributes(cdm$antiemetics)$cohort_set %>% 
                      dplyr::select(cohort_definition_id, cohort_name) %>% 
                      dplyr::rename("index_id" = "cohort_definition_id",
                                    "index_name" = "cohort_name"),
                    by = "index_id",
                    copy = T) %>% 
  dplyr::mutate(marker_name = "Antiparkinson Drugs") %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCSR_CI, upperCSR_CI, lowerASR_CI, upperASR_CI, cdm_name) %>% 
  dplyr::mutate(comment = "Antiemetics - Antiparkinsons")

#total v total
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "antiemetics_total_antiparkinson_total",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "antiemetics_total",
                                         markerTable = "parkinson_drugs_total",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

antiemetics_total_antiparkinson_total <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                                   outcomeTable = "antiemetics_total_antiparkinson_total") %>% 
  dplyr::mutate(index_name = "antiemetics") %>% 
  dplyr::mutate(marker_name = "Antiparkinson Drugs") %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCSR_CI, upperCSR_CI, lowerASR_CI, upperASR_CI, cdm_name) %>% 
  dplyr::mutate(comment = "Antiemetics - Antiparkinsons")

antiemetics_results <- rbind(antiemetics_antiparkinson,
                     antiemetics_total_antiparkinson,
                     antiemetics_antiparkinson_total,
                     antiemetics_total_antiparkinson_total)

### typical_antipsychotics
#total v no
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "typical_antipsychotics_total_antiparkinson",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "typical_antipsychotics_total",
                                         markerTable = "parkinson_drugs",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

typical_antipsychotics_total_antiparkinson <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                                     outcomeTable = "typical_antipsychotics_total_antiparkinson") %>% 
  dplyr::mutate(index_name = "typical_antipsychotics") %>% 
  dplyr::inner_join(drug_cohort_set %>% 
                      dplyr::select(cohort_definition_id, cohort_name) %>% 
                      dplyr::rename("marker_id" = "cohort_definition_id",
                                    "marker_name" = "cohort_name"),
                    by = "marker_id",
                    copy = T) %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCSR_CI, upperCSR_CI, lowerASR_CI, upperASR_CI, cdm_name) %>% 
  dplyr::mutate(comment = "Typical Antipsychotics - Antiparkinsons")

#no vs total
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "typical_antipsychotics_antiparkinson_total",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "typical_antipsychotics2",
                                         markerTable = "parkinson_drugs_total",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

typical_antipsychotics_antiparkinson_total <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                                     outcomeTable = "typical_antipsychotics_antiparkinson_total") %>% 
  dplyr::inner_join(attributes(cdm$typical_antipsychotics)$cohort_set %>% 
                      dplyr::select(cohort_definition_id, cohort_name) %>% 
                      dplyr::rename("index_id" = "cohort_definition_id",
                                    "index_name" = "cohort_name"),
                    by = "index_id",
                    copy = T) %>% 
  dplyr::mutate(marker_name = "Antiparkinson Drugs") %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCSR_CI, upperCSR_CI, lowerASR_CI, upperASR_CI, cdm_name) %>% 
  dplyr::mutate(comment = "Typical Antipsychotics - Antiparkinsons")

#total v total
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "typical_antipsychotics_total_antiparkinson_total",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "typical_antipsychotics_total",
                                         markerTable = "parkinson_drugs_total",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

typical_antipsychotics_total_antiparkinson_total <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                                           outcomeTable = "typical_antipsychotics_total_antiparkinson_total") %>% 
  dplyr::mutate(index_name = "typical_antipsychotics") %>% 
  dplyr::mutate(marker_name = "Antiparkinson Drugs") %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCSR_CI, upperCSR_CI, lowerASR_CI, upperASR_CI, cdm_name) %>% 
  dplyr::mutate(comment = "Typical Antipsychotics - Antiparkinsons")

typical_antipsychotics_results <- rbind(typical_antipsychotics_antiparkinson,
                             typical_antipsychotics_total_antiparkinson,
                             typical_antipsychotics_antiparkinson_total,
                             typical_antipsychotics_total_antiparkinson_total)

### atypical_antipsychotics
#total v no
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "atypical_antipsychotics_total_antiparkinson",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "atypical_antipsychotics_total",
                                         markerTable = "parkinson_drugs",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

atypical_antipsychotics_total_antiparkinson <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                                                outcomeTable = "atypical_antipsychotics_total_antiparkinson") %>% 
  dplyr::mutate(index_name = "atypical_antipsychotics") %>% 
  dplyr::inner_join(drug_cohort_set %>% 
                      dplyr::select(cohort_definition_id, cohort_name) %>% 
                      dplyr::rename("marker_id" = "cohort_definition_id",
                                    "marker_name" = "cohort_name"),
                    by = "marker_id",
                    copy = T) %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCSR_CI, upperCSR_CI, lowerASR_CI, upperASR_CI, cdm_name) %>% 
  dplyr::mutate(comment = "Atypical Antipsychotics - Antiparkinsons")

#no vs total
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "atypical_antipsychotics_antiparkinson_total",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "atypical_antipsychotics2",
                                         markerTable = "parkinson_drugs_total",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

atypical_antipsychotics_antiparkinson_total <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                                                outcomeTable = "atypical_antipsychotics_antiparkinson_total") %>% 
  dplyr::inner_join(attributes(cdm$atypical_antipsychotics)$cohort_set %>% 
                      dplyr::select(cohort_definition_id, cohort_name) %>% 
                      dplyr::rename("index_id" = "cohort_definition_id",
                                    "index_name" = "cohort_name"),
                    by = "index_id",
                    copy = T) %>% 
  dplyr::mutate(marker_name = "Antiparkinson Drugs") %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCSR_CI, upperCSR_CI, lowerASR_CI, upperASR_CI, cdm_name) %>% 
  dplyr::mutate(comment = "Atypical Antipsychotics - Antiparkinsons")

#total v total
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "atypical_antipsychotics_total_antiparkinson_total",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "atypical_antipsychotics_total",
                                         markerTable = "parkinson_drugs_total",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

atypical_antipsychotics_total_antiparkinson_total <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                                                      outcomeTable = "atypical_antipsychotics_total_antiparkinson_total") %>% 
  dplyr::mutate(index_name = "atypical_antipsychotics") %>% 
  dplyr::mutate(marker_name = "Antiparkinson Drugs") %>% 
  dplyr::select(index_id, index_name, marker_id, marker_name, index_first, marker_first, csr, asr, lowerCSR_CI, upperCSR_CI, lowerASR_CI, upperASR_CI, cdm_name) %>% 
  dplyr::mutate(comment = "Atypical Antipsychotics - Antiparkinsons")

atypical_antipsychotics_results <- rbind(atypical_antipsychotics_antiparkinson,
                                        atypical_antipsychotics_total_antiparkinson,
                                        atypical_antipsychotics_antiparkinson_total,
                                        atypical_antipsychotics_total_antiparkinson_total)


cascade_res <- rbind(ccb_results,
                     dopamine_depleters_results,
                     antiemetics_results,
                     typical_antipsychotics_results,
                     atypical_antipsychotics_results)

write.xlsx(cascade_res, here(output_folder, "cascade_results.xlsx"))

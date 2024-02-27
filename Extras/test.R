start <- Sys.time()
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "amiodarone_thyroxine",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "amiodarone",
                                         markerTable = "levothyroxine",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = Inf)
end <- Sys.time()
(end-start)

start <- Sys.time()
cdm[["amiodarone2"]] <- cdm[["amiodarone"]] %>% 
  dplyr::mutate(cohort_definition_id = 17) %>% 
  dplyr::compute()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "amiodarone_thyroxine",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "amiodarone2",
                                         markerTable = "levothyroxine",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = Inf)
end <- Sys.time()
(end-start)

start <- Sys.time()
cdm[["levothyroxine2"]] <- cdm[["levothyroxine"]] %>% 
  dplyr::mutate(cohort_definition_id = 171) %>% 
  dplyr::compute()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "amiodarone_thyroxine",
                                         dateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                         indexTable = "amiodarone2",
                                         markerTable = "levothyroxine2",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = Inf)
end <- Sys.time()
(end-start)

#############################################################

  # checks
  checkInputGetSequenceRatios(cdm = cdm,
                              outcomeTable = outcomeTable,
                              confidenceIntervalLevel = confidenceIntervalLevel,
                              restriction = restriction)
  

  
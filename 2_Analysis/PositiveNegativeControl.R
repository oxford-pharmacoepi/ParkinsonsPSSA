#################################################################################
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

# cdm <- CDMConnector::cdm_from_con(
#   con = db,
#   cdm_schema = cdm_database_schema,
#   write_schema = c("schema" = results_database_schema,
#                    "prefix" = stem_table),
#   cohort_tables = c("amiodarone", "levothyroxine")
# )
print(paste0("Starting PSSA for amiodarone-levothyroxine (Positive control) at ", Sys.time()))
cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 indexTable = "amiodarone",
                                                 markerTable = "levothyroxine",
                                                 name = "amiodarone_thyroxine",
                                                 cohortDateRange = as.Date(c("2008-01-01", "2021-12-31")))

amiodarone_levothyroxine <- CohortSymmetry::summariseSequenceRatio(cdm = cdm, 
                                                                   sequenceTable = "amiodarone_thyroxine")

gt_amiodarone_levothyroxine <- CohortSymmetry::tableSequenceRatios(result = amiodarone_levothyroxine)
plot_amiodarone_levothyroxine <- CohortSymmetry::plotTemporalSymmetry(cdm = cdm,
                                                                      joinedTable = "amiodarone_thyroxine")

print(paste0("Starting PSSA for levothyroxine-allopurinol (Negative control) at ", Sys.time()))
cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 indexTable = "levothyroxine",
                                                 markerTable = "allopurinol",
                                                 name = "thyroxine_allopurinol",
                                                 cohortDateRange = as.Date(c("2008-01-01", "2021-12-31")))

thyroxine_allopurinol <- CohortSymmetry::summariseSequenceRatio(cdm = cdm, 
                                                                   sequenceTable = "thyroxine_allopurinol")

gt_thyroxine_allopurinol <- CohortSymmetry::tableSequenceRatios(result = thyroxine_allopurinol)
plot_thyroxine_allopurinol <- CohortSymmetry::plotTemporalSymmetry(cdm = cdm,
                                                                      joinedTable = "thyroxine_allopurinol")

print(paste0("Starting PSSA for amiodarone-allopurinol (Negative control) at ", Sys.time()))
cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 indexTable = "amiodarone",
                                                 markerTable = "allopurinol",
                                                 name = "amiodarone_allopurinol",
                                                 cohortDateRange = as.Date(c("2008-01-01", "2021-12-31")))

amiodarone_allopurinol <- CohortSymmetry::summariseSequenceRatio(cdm = cdm, 
                                                                   sequenceTable = "amiodarone_allopurinol")

gt_amiodarone_allopurinol <- CohortSymmetry::tableSequenceRatios(result = amiodarone_allopurinol)
plot_amiodarone_allopurinol <- CohortSymmetry::plotTemporalSymmetry(cdm = cdm,
                                                                   joinedTable = "amiodarone_allopurinol")

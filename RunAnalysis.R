# create logger
log_file <- paste0(output_folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# generating cohorts for PSSA
info(logger, "GENERATING COHORTS FOR PSSA")
print(paste0("Generating cohorts for PSSA at ", Sys.time()))
source(here("1_InstantiateCohorts", "CohortPSSA.R"))
info(logger, "GENERATING COHORTS FOR PSSA IS DONE")
print(paste0("Generating cohorts for PSSA is done at ", Sys.time()))

if (run_controls == T){
  # Running positive and negative controls
  info(logger, "RUNNING POSITIVE AND NEGATIVE CONTROLS")
  print(paste0("Running positive and negative controls at ", Sys.time()))
  source(here("2_Analysis", "PositiveNegativeControl.R"))
  info(logger, "RUNNING POSITIVE AND NEGATIVE CONTROLS IS DONE")
  print(paste0("Finishing positive and negative controls at ", Sys.time()))
}

if (run_treatment_order == T){
  # running treatments for PD
  info(logger, "RUNNING TREATMENTS ORDER ANALYSIS")
  print(paste0("Running treatment order analysis (antiparkinsonian drugs) at ", Sys.time()))
  source(here("2_Analysis", "TreatmentOrder.R"))
  info(logger, "RUNNING TREATMENTS ORDER ANALYSIS IS DONE")
  print(paste0("Finishing treatment order analysis at ", Sys.time()))
}

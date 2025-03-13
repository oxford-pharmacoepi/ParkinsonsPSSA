# create logger
log_file <- paste0(output_folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

results <- list()
results[["snapshot"]] <- OmopSketch::summariseOmopSnapshot(cdm)
results[["obs_period"]] <- OmopSketch::summariseObservationPeriod(cdm$observation_period)

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

if (run_hypothesis_driven == T){
  info(logger, "RUNNING HYPOTHESIS DRIVEN ANALYSIS")
  print(paste0("Running hypothesis driven analysis at ", Sys.time()))
  source(here("2_Analysis", "HypothesisDrivenDrugInduced.R"))
  info(logger, "RUNNING HYPOTHESIS DRIVEN ANALYSIS DONE")
  print(paste0("Finishing hypothesis driven analysis at ", Sys.time()))
}

if (run_characterisation == T){
  info(logger, "CHARACTERISATION STARTS")
  print(paste0("Running characterisation at ", Sys.time()))
  source(here("2_Analysis", "Characterisation.R"))
  info(logger, "CHARACTERISATION DONE")
  print(paste0("Finishing characterisation at ", Sys.time()))
}

results <- results |>
  vctrs::list_drop_empty() |>
  omopgenerics::bind() |>
  omopgenerics::newSummarisedResult()

exportSummarisedResult(results,
                       minCellCount = minimum_counts,
                       path = output_folder
)

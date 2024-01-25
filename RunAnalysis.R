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

# running treatments for PD
info(logger, "RUNNING TREATMENTS FOR PD")
print(paste0("RUNNING TREATMENTS FOR PD at ", Sys.time()))
source(here("1_InstantiateCohorts", "CohortPSSA.R"))
info(logger, "RUNNING TREATMENTS FOR PD IS DONE")
print(paste0("RUNNING TREATMENTS FOR PD is done at ", Sys.time()))
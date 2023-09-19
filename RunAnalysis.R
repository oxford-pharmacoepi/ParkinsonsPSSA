# create logger
log_file <- paste0(output_folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

### PSSA
results_pssa <- getPSSA(cdm = cdm,
                        index = list(c("amiodarone", "ingredient")), 
                        marker = list(c("levothyroxine", "ingredient")), 
                        table_name = "pssa_amiodarone_levothyroxine",
                        study_time = 730) 

### plots
getHistogram(results_pssa, "days")

### Waiting Time Distribution

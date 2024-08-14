renv::activate() 
renv::restore() # this should prompt you to install the various packages required for the study
library(CDMConnector)
library(DBI)
library(plyr)
library(log4r)
library(dplyr)
library(dbplyr)
library(here)
library(tidyr)
library(CodelistGenerator)
library(DrugUtilisation)
library(ggplot2)
library(xlsx)
library(CohortSymmetry) #devtools::install_github("oxford-pharmacoepi/CohortSymmetry")

# database metadata and connection details -----
# The name/ acronym for the database
db_name <- "...."

# Set output folder location -----
# the path to a folder where the results from this analysis will be saved
output_folder <- here(paste0("Results_", db_name))
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Database connection details -----
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below
# https://darwin-eu.github.io/CDMConnector/articles/DBI_connection_examples.html 
# for more details.
# you may need to install another package for this 
# eg for postgres 
# db <- dbConnect(
#   RPostgres::Postgres(), 
#   dbname = server_dbi, 
#   port = port, 
#   host = host, 
#   user = user,
#   password = password
# )
db <- dbConnect("....")

# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema <- "...."

# The name of the schema where results tables will be created 
results_database_schema <- "...."

# Whether or not to run negative/positive control (T/F)
run_controls <- TRUE

# Whether or not to run treatment orders (T/F)
run_treatment_order <- FALSE

#Whether or not to run hypothesis driven (using hypotheses found using previous research)
run_hypothesis_driven <- TRUE

# Whether or to instantiate index cohorts prior, only set it to be FALSE if relevant
# parts of HypothesisDrivenDrugInduced.R have been run 
instantiate_index_shin <- TRUE
instantiate_index_feldman <- TRUE
instantiate_index_class <- TRUE

# Name of stem outcome table in the result schema where the outcome cohorts will
# be stored. 
# Notes: 
# - if there is an existing table in your results schema with the same names it
#   will be overwritten
# - more than one cohort will be created
# - name must be lower case
stem_table <- "...."

# minimum counts that can be displayed according to data governance
minimum_counts <- 5

# create cdm reference ----
cdm <- CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = c(schema = results_database_schema,
                   prefix = stem_table)
)
# check database connection
# running the next line should give you a count of your person table
cdm$person %>% 
  tally()

# Run the study ------
source(here("RunAnalysis.R"))
# after the study is run you should have a zip folder in your output folder to share

print("Done!")
print("-- If all has worked, there should now be a zip folder with your results in the output folder to share")
print("-- Thank you for running the study!")
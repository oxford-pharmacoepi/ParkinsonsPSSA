#devtools::install_github("oxford-pharmacoepi/CohortSymmetry")
library(CDMConnector)
library(DBI)
library(plyr)
library(log4r)
library(dplyr)
library(dbplyr)
library(here)
library(devtools)
library(Capr) #devtools::install_github("ohdsi/Capr")
library(tidyr)
library(CodelistGenerator)
library(DrugUtilisation)
library(lubridate)
library(CirceR) #remotes::install_github("ohdsi/CirceR")
library(ggplot2)
library(xlsx)
library(IncidencePrevalence)

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
  write_schema = results_database_schema
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
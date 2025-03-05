# demographics ----
cli::cli_alert_info("Summarising Demographics")

suppressWarnings(
  
  summaryDemographics <- cdm$outcome %>%
    summariseCharacteristics(
      strata = list(c("sex"),
                    c("age_group"),
                    c("age_group", "sex"),
                    c("diag_yr_gp"),
                    c("diag_yr_gp", "sex")),
      ageGroup = list( "18 to 49" = c(18, 49),
                       "50 to 59" = c(50, 59),
                       "60 to 69" = c(60, 69),
                       "70 to 79" = c(70, 79),
                       "80 +" = c(80, 150)),
      tableIntersect = list()
    )
  
)

write_csv(summaryDemographics %>%
            omopgenerics::suppress(minCellCount = 5), here("Results",db_name, paste0(cdmName(cdm),
                                                                                     "_summary_demographics.csv"
            )))

cli::cli_alert_success("Summarising Demographics Complete")

# comorbidities --------
cli::cli_alert_info("Instantiating Comorbidities")

codelistConditions <- CodelistGenerator::codesFromConceptSet(here("2_Study", "1_InstantiateCohorts", "Conditions"), cdm)

cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm, 
                                              conceptSet = codelistConditions,
                                              name = "conditions",
                                              overwrite = TRUE)


cli::cli_alert_info("Summarising Comorbidities")

suppressWarnings(
  
  summaryComorbidity <- cdm$outcome %>%
    summariseCharacteristics(
      strata = list(c("sex"),
                    c("age_group"),
                    c("age_group", "sex"),
                    c("diag_yr_gp"),
                    c("diag_yr_gp", "sex")),
      ageGroup = list( "18 to 49" = c(18, 49),
                       "50 to 59" = c(50, 59),
                       "60 to 69" = c(60, 69),
                       "70 to 79" = c(70, 79),
                       "80 +" = c(80, 150)),
      tableIntersect = list(),
      cohortIntersect = list("Comorbidities" = list(
        targetCohortTable = "conditions",
        value = "flag",
        window = list(c(-999999, -1) ,
                      c(-999999, -366),
                      c(-365, -31),
                      c(-30, -1),
                      c(0, 0))
      )
      )
    )
  
)


write_csv(summaryComorbidity %>%
            omopgenerics::suppress(minCellCount = 5), here("Results",db_name, paste0(cdmName(cdm),
                                                                                     "_summary_comorbidity.csv"
            )))

cli::cli_alert_success("Summarising Comorbidities Complete")

# medications -----
cli::cli_alert_info("Summarising Medications")

# instantiate medications
codelistMedications <- CodelistGenerator::codesFromConceptSet(here("2_Study" ,"1_InstantiateCohorts", "Medications"), cdm)

cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(cdm = cdm, 
                                                         conceptSet = codelistMedications, 
                                                         name = "medications")


suppressWarnings(
  
  summaryMedications <- cdm$outcome %>%
    summariseCharacteristics(
      strata = list(c("sex"),
                    c("age_group"),
                    c("age_group", "sex"),
                    c("diag_yr_gp"),
                    c("diag_yr_gp", "sex")),
      ageGroup = list( "18 to 49" = c(18, 49),
                       "50 to 59" = c(50, 59),
                       "60 to 69" = c(60, 69),
                       "70 to 79" = c(70, 79),
                       "80 +" = c(80, 150)),
      tableIntersect = list(),
      cohortIntersect = list(
        "Medications" = list(
          targetCohortTable = "medications",
          value = "flag",
          window = list(c(-365, -1),
                        c(-365, -31),
                        c(-30, -1),
                        c(0, 0),
                        c(1, 30),
                        c(1, 90),
                        c(1, 365))
        ))
    )
  
)



write_csv(summaryMedications %>%
            omopgenerics::suppress(minCellCount = 5),
          here("Results", db_name, paste0(cdmName(cdm),
                                          "_summary_medications.csv"
          )))

cli::cli_alert_success("Summarising Medications Complete")

cli::cli_alert_success("Characterisation Analysis Complete")
# demographics ----
cli::cli_alert_info("Summarising Demographics")

cdm <- omopgenerics::bind(
  cdm$amiodarone_allopurinol,
  cdm$amiodarone_thyroxine,
  cdm$thyroxine_allopurinol,
  cdm$ingredient_hypo,
  cdm$class_hypo,
  name = "cohort"
)

# Instantiating comorbidities --------
cli::cli_alert_info("Instantiating Comorbidities")
codelistConditions <- CodelistGenerator::codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm)
cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm, 
                                              conceptSet = codelistConditions,
                                              name = "conditions",
                                              overwrite = TRUE)
cli::cli_alert_info("Instantiated Comorbidities")

# Instantiating medications -----
cli::cli_alert_info("Instantiating Medications")
codelistMedications <- CodelistGenerator::codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm)
cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(cdm = cdm, 
                                                         conceptSet = codelistMedications, 
                                                         name = "medications")
cli::cli_alert_info("Instantiated Medications")

cli::cli_alert_info("Start summarising demographics")
cdm$cohort <- cdm$cohort |> 
  PatientProfiles::addDemographics(
    ageGroup = list(
      "age_group" =
        list(
          "< 18" = c(0, 17),
          "18 to 49" = c(18, 49),
          "50 to 59" = c(50, 59),
          "60 to 69" = c(60, 69),
          "70 to 79" = c(70, 79),
          "80+" = c(80, 150)
        )
    )) |> 
  dplyr::mutate(index_or_marker_first = if_else(cohort_start_date == index_date, "Index", "Marker"))

cli::cli_alert_info("Summarising Characteristics")
results[["characteristics"]] <- cdm$cohort |>
  CohortCharacteristics::summariseCharacteristics(
    strata = list(c("index_or_marker_first")),
    ageGroup = list ("< 18" = c(0, 17),
                     "18 to 49" = c(18, 49),
                     "50 to 59" = c(50, 59),
                     "60 to 69" = c(60, 69),
                     "70 to 79" = c(70, 79),
                     "80 +" = c(80, 150)),
    cohortIntersectFlag = list(
      "Conditions prior to index date" = list(
        targetCohortTable = "conditions",
        window = c(-Inf, -1)
      ),
      "Medications 365 days prior to index date" = list(
        targetCohortTable = "medications",
        window = c(-365, -1)
      )
    )
  )
cli::cli_alert_success("Characterisation Analysis Complete")
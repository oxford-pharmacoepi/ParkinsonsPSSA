# shiny is prepared to work with this resultList, please do not change them
resultList <- list(
  "summarise_omop_snapshot" = c(1L),
  "summarise_observation_period" = c(2L),
  "sequence_ratios" = c(3L, 4L),
  "summarise_characteristics" = c(5L)
)

source(file.path(getwd(), "functions.R"))

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "data")) |>
  dplyr::mutate(
    group_level = dplyr::case_when(
      (result_id == 5) ~ stringr::str_replace(group_level, "index_", ""),
      T ~ group_level
    )
  ) |>
  dplyr::mutate(
    group_level = dplyr::case_when(
      (result_id == 5) ~ stringr::str_replace(group_level, "marker", ""),
      T ~ group_level
    )
  ) |>
  dplyr::mutate(
    group_level = dplyr::case_when(
      (result_id == 5) ~ stringr::str_replace(group_level, "levodopa_decarboxylase_inhibitor_and_comt_inhibitor_oral", "comt_inhibitors"),
      T ~ group_level
    )
  ) |>
  dplyr::mutate(
    group_level = dplyr::case_when(
      (result_id == 5) ~ stringr::str_replace(group_level, "monoamine_oxidase_b_inhibitors", "maob_inhibitors"),
      T ~ group_level
    )
  ) |>
  dplyr::mutate(
    group_level = dplyr::case_when(
      (result_id == 5) ~ stringr::str_replace(group_level, "__", " -> "),
      T ~ group_level
    )
  ) 

data <- prepareResult(result, resultList)
filterValues <- defaultFilterValues(result, resultList)

save(data, filterValues, file = file.path(getwd(), "data", "shinyData.RData"))

rm(result, filterValues, resultList, data)

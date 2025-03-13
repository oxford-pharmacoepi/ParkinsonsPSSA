# shiny is prepared to work with this resultList, please do not change them
resultList <- list(
  "summarise_omop_snapshot" = c(1L),
  "summarise_observation_period" = c(2L),
  "sequence_ratios" = c(3L, 4L),
  "summarise_characteristics" = c(5L)
)

source(file.path(getwd(), "functions.R"))

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "data"))
data <- prepareResult(result, resultList)
filterValues <- defaultFilterValues(result, resultList)

save(data, filterValues, file = file.path(getwd(), "data", "shinyData.RData"))

rm(result, filterValues, resultList, data)

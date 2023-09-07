### Input a cdm table and a study period and output a cleaned version of table, so that it can be used in asr
tableCleaning <- function(table, study_time = NULL){
  colChecks(table, c("cohort_definition_id", "subject_id", "cohort_start_date"))
  if (is.null(study_time)){
    dat <- 
      table %>%
      dplyr::select(cohort_definition_id, subject_id, cohort_start_date) %>%
      pivot_wider(names_from = cohort_definition_id, values_from = cohort_start_date) %>% 
      dplyr::rename("dateIndexDrug" = `1`, "dateMarkerDrug" = `2`) %>%
      dplyr::mutate(gap = dateMarkerDrug - dateIndexDrug) %>%
      dplyr::filter(!is.na(gap)) %>%
      dplyr::filter(!gap==0) %>%
      dplyr::select(-gap) %>%
      collect() %>%
      dplyr::select(-subject_id)
  }
  else{
    dat <-
      table %>%
      dplyr::select(cohort_definition_id, subject_id, cohort_start_date) %>%
      pivot_wider(names_from = cohort_definition_id, values_from = cohort_start_date) %>% 
      dplyr::rename("dateIndexDrug" = `1`, "dateMarkerDrug" = `2`) %>%
      dplyr::mutate(gap = dateMarkerDrug - dateIndexDrug) %>%
      dplyr::filter(!is.na(gap)) %>%
      dplyr::filter(!gap==0) %>%
      dplyr::filter(-study_time<= gap & gap <= study_time) %>%
      dplyr::select(-gap) %>%
      collect() %>%
      dplyr::select(-subject_id)
  }
  return(dat)
}

# CI
getConfidenceInterval <- function(table, confidence_interval_level = 0.025){
  colChecks(table, c("marker_first", "index_first"))
  
  counts <- tibble(
    index_first = table %>% pull(index_first) %>% sum(.),
    marker_first = table %>% pull(marker_first) %>% sum(.)
  )
  
  counts$lowerCI <- qbeta(confidence_interval_level, counts$index_first + 0.5, counts$marker_first + 0.5)
  counts$upperCI <- qbeta(1-confidence_interval_level, counts$index_first + 0.5, counts$marker_first + 0.5)
  
  counts$lowerCI <- counts$lowerCI/(1-counts$lowerCI)
  counts$upperCI <- counts$upperCI/(1-counts$upperCI)
  
  return(counts)
}

#Histogram
getHistogram <- function (table, bins = 48){
  colChecks(table, c("dateIndexDrug", "dateMarkerDrug"))
  prep <- table %>%
    mutate(gap = as.integer(dateMarkerDrug - dateIndexDrug)) %>%
    mutate(order = ifelse(dateMarkerDrug>dateIndexDrug, "Marker First", "Index First"))
  
  p <- ggplot(prep, aes(x=gap, color=order, fill=order)) + 
    geom_histogram(bins = bins) +
    geom_vline(xintercept = 0, linewidth = 1, color = "black") +
    labs(title = paste0("Time difference between the initiation of index and mark drugs"))+
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Gap betwen index and marker in days") + ylab("counts")
  return(p)
}

#generating drug cohort in one go
generateDrugCohort <- function(cdm, index, marker, table_name = "pssa"){
  index_drug <- list()
  marker_drug <- list()
  
  for (i in (1: length(index))){
    if (index[[i]][2] == "ingredient"){
      index_drug[[i]] <- getDrugIngredientCodes(cdm = cdm, name = index[[i]][1])
    } else {
      index_drug[[i]] <- getATCCodes(cdm = cdm, name = index[[i]][1], level = c(index[[i]][2]))
    }
  }
  
  for (i in (1: length(marker))){
    if (marker[[i]][2] == "ingredient"){
      marker_drug[[i]] <- getDrugIngredientCodes(cdm = cdm, name = marker[[i]][1])
    } else {
      marker_drug[[i]] <- getATCCodes(cdm = cdm, name = marker[[i]][1], level = c(marker[[i]][2]))
    }
  }
  
  conceptSetList <- c()
  for (i in (1:length(index_drug))){
    conceptSetList <- c(conceptSetList, index_drug[[i]])
  }
  
  for (i in (1: length(marker_drug))){
    conceptSetList <- c(conceptSetList, marker_drug[[i]])
  }
  
  cdm <- generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = table_name,
    conceptSetList = conceptSetList,
    summariseMode = "FirstEra"
  )
  
  index_length <- 0
  for (i in (1:length(index_drug))){
    index_length <- index_length + length(index_drug[[i]])
  }
  
  cdm[[table_name]] <- cdm[[table_name]] %>%
    collect() %>%
    dplyr::mutate(cohort_definition_id = case_when(cohort_definition_id <= index_length ~ 1,
                                                   cohort_definition_id > index_length ~ 2)) %>% 
    dplyr::mutate(cohort_definition_id = as.integer(cohort_definition_id)) %>%
    group_by(cohort_definition_id, subject_id) %>%
    arrange(cohort_start_date, .by_group =T) %>%
    filter(row_number()==1) %>%
    ungroup() %>%
    compute()
  
  raw_table <- cdm[[table_name]] 
  
  return(raw_table)
}

##### getPSSA (complete approach)
getPSSA <- function(cdm, index, marker, table_name = "pssa", study_time = NULL, confidence_interval_level = 0.025){
  table <- generateDrugCohort(cdm = cdm, index = index, marker = marker, table_name = table_name)
  table_cleaned <- tableCleaning(table = table, study_time = study_time)
  csr<-crudeSequenceRatio(summaryTable(table_cleaned))
  asr<-adjustedSequenceRatio(summaryTable(table_cleaned))
  counts <- getConfidenceInterval(summaryTable(table_cleaned))
  
  results <- tibble(name = table_name, 
                    csr = csr, 
                    asr = asr)
  
  results <- cbind(results, counts)
  
  return(results)
}

# ### Intake two IDs and generate two cohort sets using capr
# generatePSSACohortDefinitions <- function (DrugId){
#   cohort(
#     entry = entry(drug(cs(descendants(DrugId))),
#                   primaryCriteriaLimit = "First"),
#     exit = exit(endStrategy = fixedExit(index = "startDate", offsetDays = 0L)))
# }

### Credit to Ty - checking if the dataframe has the required columns
colChecks <- function(df, cols) {

  if(!(("data.frame" %in% class(df))|("GeneratedCohortSet" %in% class(df))))
    stop("df input is not a data.frame or a CohortSet object")

  if(!("character" %in% class(cols)))
    stop("col input is not a atomic character vector")

  col_names <- colnames(df)
  cols_found <- cols %in% col_names
  if (!all(cols_found)) {
    stop(paste0(cols[!cols_found], collapse=", "), " not found in input data.frame")
  } else {
    return(TRUE)
  }
}

### A summary table required to compute asr, csr and nsr
# and produces a summary table with one column indicating how many days it has been since the the very first drug
# and the number of cases where the marker was prescribed first and the index was prescribed first.

summaryTable <- function(table, subject_id = "subject_id", dateIndexDrug = "dateIndexDrug", dateMarkerDrug = "dateMarkerDrug") {
  
  colChecks(table, c(dateIndexDrug, dateMarkerDrug))
  
  # allocating column names
  column_names <- colnames(table)
  column_names[column_names == subject_id] <- "subject_id"
  column_names[column_names == dateIndexDrug] <- "dateIndexDrug"
  column_names[column_names == dateMarkerDrug] <- "dateMarkerDrug"
  colnames(table) <- column_names
  
  # creating order, orderBA is TRUE if index is AFTER marker
  table <-
    table %>%
    dplyr::filter((!is.na(dateIndexDrug)) & (!is.na(dateMarkerDrug))) %>%
    mutate(orderBA = dateIndexDrug >= dateMarkerDrug)
  
  # min date of any drug start
  date_start <- min(pull(table, dateIndexDrug), pull(table, dateMarkerDrug))
  
  # lubridate package used, days(1), as_date() and %--%.
  table <-
    table %>%
    mutate(
      date_first = lubridate::as_date(ifelse(orderBA, dateMarkerDrug, dateIndexDrug)), # setting which date is first and which is second
      date_second = lubridate::as_date(ifelse(orderBA, dateIndexDrug, dateMarkerDrug)),
      days_first = as.integer((date_start %--% date_first) / lubridate::days(1)), # gap between the first drug of a person and the first drug of the whole population
      days_second = as.integer((date_first %--% date_second) / lubridate::days(1)) # gap betwen two drugs of a person
    )
  
  #max number of digits in days_first
  max_dig <- nchar(as.character(max(pull(table, days_first))))
  ch_format <- paste0("%0", max_dig, ".0f")
  
  table <-
    table %>%
    mutate(
      days_first_ch_format = sprintf(ch_format, days_first) # make a column with days_first having the same number of digits - Why?
    ) %>%
    arrange(days_first)
 
  ### final output, a dataframe with four columns, days_first_ch_format, days_first, marker_first and index_first.
  # days_first_ch_format: days_first in ch format
  # days_first: gap between the first drug date of an individual to the first drug date of everyone
  # marker_first: for a given days_first, how many were marker first
  # index_first: for a given days_first, how many were index first
  dat <-
    table %>%
    group_by(days_first_ch_format, days_first) %>%
    summarise(marker_first = sum(orderBA), index_first = sum(!orderBA), .groups = "drop") %>%
    ungroup()
  
  return(dat)
}

### ASR
adjustedSequenceRatio <- function(table) {
  
  return(crudeSequenceRatio(table) / nullSequenceRatio(table))
  
}

### CSR
crudeSequenceRatio <- function(table) {
  
  colChecks(table, c("days_first", "index_first", "marker_first"))
  
  n_index_before_marker <- table %>% pull(index_first) %>% sum(.) #how many occasions are there that index was taken before marker
  n_marker_before_index <- table %>% pull(marker_first) %>% sum(.) #how many occasions are there that index was taken after marker
  
  crudeSequenceRatio <- n_index_before_marker / n_marker_before_index
  
  return(crudeSequenceRatio)
  
}

### NSR
nullSequenceRatio <- function(table, restriction = 548) {
  
  colChecks(table, c("days_first", "marker_first", "index_first"))
  
  n_index_before_marker <- table %>% pull(index_first) %>% sum(.)
  n_marker_before_index <- table %>% pull(marker_first) %>% sum(.)
  
  numer <- 0
  denom <- 0
  
  if (is.finite(restriction)) {
    
    table <-
      table %>%
      mutate(
        marker_cumsum_fwd = deltaCumulativeSum(marker_first, days_first, restriction, backwards = FALSE),
        marker_cumsum_bwd = deltaCumulativeSum(marker_first, days_first, restriction, backwards = TRUE),
        numerator = index_first * marker_cumsum_fwd,
        denominator = index_first * (marker_cumsum_bwd + marker_cumsum_fwd - marker_first),
      )
    
    numer <- table %>% pull(numerator) %>% sum(.)
    denom <- table %>% pull(denominator) %>% sum(.)
    
  } else {
    
    numer <-
      table %>%
      mutate(
        marker_cumsum = n_marker_before_index - cumsum(marker_first),
        numerator = index_first * marker_cumsum
      ) %>%
      pull(numerator)
    
    numer <- sum(numer)
    denom <- n_marker_before_index * n_index_before_marker
    
  }
  
  if (numer < 1)
    warning("NSR numerator is 0, which results in a NSR == 0, proceed with caution")
  
  if (denom < 1)
    stop("NSR denominator is 0, suggesting no Marker Drug -> Index Drug or Index Drug -> Marker Drug events")
  
  a <- numer / denom
  
  nullSequenceRatio <- a / (1 - a)
  
  return(nullSequenceRatio)
  
}
### For each i in 1 through length(x), it finds the minimum j such that x[i]-x[j]<=delta. 
# x needs to be non decreasing. 
# i.e., y[i] = min{j: x[i]-x[j]<=delta}
indexDeltaBackward <- function(x, delta) {

  n <- length(x)
  y <- rep(0, n) #create y which is the same size of x
  
  y[1] <- 1

  j <- 1
  i <- 2

  while (i <= n) {

    if ((x[i] - x[j]) <= delta) {
      y[i] <- j
      i <- i + 1
    } else {
      j <- j + 1
    }

  }

  return(y)

}

### For each i in 1 through length(x), it finds the max j such that x[j]-x[i]<=delta. 
# x needs to be non decreasing.
# i.e., y[i] = max{j: x[j]-x[i]<=delta}
indexDeltaForward <- function(x, delta) {
  
  n <- length(x)
  y <- rep(0, n)
  
  j <- 2
  i <- 1
  
  while (j <= n) {
    
    if ((x[j] - x[i]) > delta) {
      y[i] <- j - 1
      i <- i + 1
    } else {
      j <- j + 1
    }
    
  }
  
  y[i:n] <- n
  
  return(y)
  
}

#### Not sure what is going on 
# y and t are vectors of the same length
# t is again no decreasing
# when backward is T, the output is again a vector of the same length, the jth entry is the sum of y_i from max{i<j:t[j]-t[i]>delta}+1 to (j-1)
# similarly for backward is F.
deltaCumulativeSum <- function(y, t, delta, backwards = TRUE) {
  
  if (is.null(y)) {
    y <- t
  }
  y_cumsum <- cumsum(y)
  
  if (backwards) {
    
    n_y <- length(y)
    prior_cumsum <- c(0, y_cumsum[-n_y])
    bwd_cumsum <- rep(0, n_y)
    t_delta_bwd <- indexDeltaBackward(t, delta) - 1
    look_bwds <- t_delta_bwd > 0
    bwd_cumsum[look_bwds] <- y_cumsum[t_delta_bwd[look_bwds]]
    
    return(prior_cumsum - bwd_cumsum)
    
  } else {
    t_delta_fwd <- indexDeltaForward(t, delta)
    fwd_cumsum <- y_cumsum[t_delta_fwd]
    
    return(fwd_cumsum - y_cumsum)
    
  }
  
}


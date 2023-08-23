### Input a cdm table and a study period and output a cleaned version of table, so that it can be used in asr
tableCleaning <- function(table, study_time){
  colChecks(table, c("cohort_definition_id", "subject_id", "cohort_start_date"))
  table %>%
    select(cohort_definition_id, subject_id, cohort_start_date) %>%
    pivot_wider(names_from = cohort_definition_id, values_from = cohort_start_date) %>% 
    rename("dateIndexDrug" = `1`, "dateMarkerDrug" = `2`) %>%
    mutate(gap = dateMarkerDrug - dateIndexDrug) %>%
    dplyr::filter(!is.na(gap)) %>%
    dplyr::filter(!gap==0) %>%
    dplyr::filter(-study_time<= gap & gap <= study_time) %>%
    select(-gap) %>%
    collect()
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

  if(!("data.frame" %in% class(df)))
    stop("df input is not a data.frame object")

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
# Basically, it takes in patients drug history table, the one produced after using the tableCleaning function
# and produces a summary table with one column indicating how many days it has been since the the very first drug
# and the number of cases where the marker was prescribed first and the index was prescribed first.

summaryTable <- function(table, subject_id = "subject_id", dateIndexDrug = "dateIndexDrug", dateMarkerDrug = "dateMarkerDrug") {
  
  colChecks(table, c(subject_id, dateIndexDrug, dateMarkerDrug))
  
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
    summarise(marker_first = sum(orderBA), index_first = sum(!orderBA)) %>%
    ungroup()
  
  return(dat)
}

### ASR
adjustedSequenceRatio <- function(summaryTable) {
  
  return(crudeSequenceRatio(summaryTable) / nullSequenceRatio(summaryTable))
  
}

### CSR
crudeSequenceRatio <- function(summaryTable) {
  
  colChecks(summaryTable, c("days_first", "index_first", "marker_first"))
  
  n_index_before_marker <- summaryTable %>% pull(index_first) %>% sum(.) #how many occasions are there that index was taken before marker
  n_marker_before_index <- summaryTable %>% pull(marker_first) %>% sum(.) #how many occasions are there that index was taken after marker
  
  crudeSequenceRatio <- n_index_before_marker / n_marker_before_index
  
  return(crudeSequenceRatio)
  
}

### NSR
nullSequenceRatio <- function(summaryTable, restriction = 548) {
  
  colChecks(summaryTable, c("days_first", "marker_first", "index_first"))
  
  n_index_before_marker <- summaryTable %>% pull(index_first) %>% sum(.)
  n_marker_before_index <- summaryTable %>% pull(marker_first) %>% sum(.)
  
  numer <- 0
  denom <- 0
  
  if (is.finite(restriction)) {
    
    summaryTable <-
      summaryTable %>%
      mutate(
        marker_cumsum_fwd = deltaCumulativeSum(marker_first, days_first, restriction, backwards = FALSE),
        marker_cumsum_bwd = deltaCumulativeSum(marker_first, days_first, restriction, backwards = TRUE),
        numerator = index_first * marker_cumsum_fwd,
        denominator = index_first * (marker_cumsum_bwd + marker_cumsum_fwd - marker_first),
      )
    
    numer <- summaryTable %>% pull(numerator) %>% sum(.)
    denom <- summaryTable %>% pull(denominator) %>% sum(.)
    
  } else {
    
    numer <-
      summaryTable %>%
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

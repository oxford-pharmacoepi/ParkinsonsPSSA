### Input a cdm table and a study period and output a cleaned version of table, so that it can be used in asr
tableCleaning <- function(table, study_time){
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

### Intake two IDs and generate two cohort sets using capr
generatePSSACohortDefinitions <- function (DrugId){
  cohort(
    entry = entry(drug(cs(descendants(DrugId))),
                  primaryCriteriaLimit = "First"),
    exit = exit(endStrategy = fixedExit(index = "startDate", offsetDays = 0L)))
}

# ### Credit to Ty - checking if the dataframe has the required columns
# colChecks <- function(df, cols) {
#   
#   if(!("data.frame" %in% class(df)))
#     stop("df input is not a data.frame object")
#   
#   if(!("character" %in% class(cols)))
#     stop("col input is not a atomic character vector")
#   
#   cns <- colnames(df)
#   cols_found <- cols %in% cns
#   if (!all(cols_found)) {
#     stop(paste0(cols[!cols_found], collapse=", "), " not found in input data.frame")
#   } else {
#     return(TRUE)
#   }
#   
# }

# ### Credit to Ty - a summary table required to compute asr
# summaryTable <- function(table, subject_id = "subject_id", dateIndexDrug = "dateIndexDrug", datedMarkerDrug = "datedMarkerDrug") {
#   
#   colChecks(table, c(subject_id, dateIndexDrug, datedMarkerDrug))
#   
# # allocating column names  
#   cnms <- colnames(table)
#   cnms[cnms == subject_id] <- "subject_id"
#   cnms[cnms == dateIndexDrug] <- "dateIndexDrug"
#   cnms[cnms == datedMarkerDrug] <- "datedMarkerDrug"
#   colnames(table) <- cnms
# 
# # creating order
#   table <-
#     table %>%
#     mutate(drugAB = (!is.na(dateIndexDrug)) & (!is.na(datedMarkerDrug))) %>%
#     dplyr::filter(drugAB) %>%
#     mutate(orderBA = dateIndexDrug >= datedMarkerDrug)
# 
# # min date of any drug start  
#   date_start <- min(pull(table, dateIndexDrug), pull(table, datedMarkerDrug))
#   
#   table <-
#     table %>%
#     mutate(
#       date_first = as_date(ifelse(orderBA, datedMarkerDrug, dateIndexDrug)),
#       date_second = as_date(ifelse(orderBA, dateIndexDrug, datedMarkerDrug)),
#       days_first = as.integer((date_start %--% date_first) / days(1)),
#       days_second = as.integer((date_first %--% date_second) / days(1))
#     )
#   
#   #max numbr of digits in days_first
#   max_dig <- nchar(as.character(max(pull(table, days_first))))
#   ch_format <- paste0("%0", max_dig, ".0f")
#   
#   table <-
#     table %>%
#     mutate(
#       df_ch = sprintf(ch_format, days_first)
#     ) %>%
#     arrange(days_first)
#   
#   dat <-
#     table %>%
#     group_by(df_ch, days_first) %>%
#     summarise(b_first = sum(orderBA), a_first = sum(!orderBA)) %>%
#     ungroup()
#   
#   # mutate(b_cumsum = n_ba - cumsum(b_first))
#   
#   return(dat)
#   
# }
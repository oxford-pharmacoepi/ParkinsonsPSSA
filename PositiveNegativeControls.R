source(here("Functions", "functions.R"))

### subset of cprd gold 100k
cdm <- CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = "public_100k",
  write_schema = results_database_schema
)

### creating plots folder
plotsFolder <- here(output_folder, "plots")
if (!dir.exists(plotsFolder)) {
  dir.create(plotsFolder)
}

################################################################################################ 
#                                                                                              #
#                                       Positive control                                       #
#                                                                                              #  
################################################################################################
#1. Amiodarone to Levothyroxine
indexId <- getDrugIngredientCodes(cdm, "amiodarone")
markerId <- getDrugIngredientCodes(cdm, "levothyroxine")

table_name_pssa <- "pssa_amiodarone_levothyroxine"

cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = table_name_pssa,
  conceptSetList = c(indexId, markerId),
  summariseMode = "FirstEra"
)

csr<-crudeSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
asr<-adjustedSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
results <- tibble(name = table_name_pssa, 
                  csr = csr, 
                  asr = asr)

counts <- getConfidenceInterval(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))

results <-cbind(results, counts)

getHistogram(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730))

# 2. Direct factor Xa inhibitors to antidepressants
indexId <- getATCCodes(cdm, level = c("ATC 4th"), name = "Direct factor Xa inhibitors")
markerId <- getATCCodes(cdm, level = c("ATC 3rd"), name = "ANTIDEPRESSANTS")

table_name_pssa <- "pssa_dfx_antidepressants"

cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = table_name_pssa,
  conceptSetList = c(indexId, markerId),
  summariseMode = "FirstEra"
)

csr <- crudeSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
asr <- adjustedSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
counts <- getConfidenceInterval(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))

results <- results %>%
  union_all(cbind(tibble(name = table_name_pssa, 
                  csr = csr, 
                  asr = asr), counts))

# 3. CCB to Diuretics
indexId <- getATCCodes(cdm, level = c("ATC 2nd"), name = "CALCIUM CHANNEL BLOCKERS")
markerId <- getATCCodes(cdm, level = c("ATC 2nd"), name = "DIURETICS")

table_name_pssa <- "pssa_ccb_diuretics"

cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = table_name_pssa,
  conceptSetList = c(indexId, markerId),
  summariseMode = "FirstEra"
)

csr <- crudeSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
asr <- adjustedSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
counts <- getConfidenceInterval(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))

results <- results %>%
  union_all(cbind(tibble(name = table_name_pssa, 
                         csr = csr, 
                         asr = asr), counts))

# 4. antipsychotics to antiparkinson drugs
indexId <- getATCCodes(cdm, level = c("ATC 3rd"), name = "ANTIPSYCHOTICS")
markerId <- getATCCodes(cdm, level = c("ATC 2nd"), name = "ANTI-PARKINSON DRUGS")

table_name_pssa <- "pssa_antipsychotics_antiparkinsonian"

cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = table_name_pssa,
  conceptSetList = c(indexId, markerId),
  summariseMode = "FirstEra"
)

csr <- crudeSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
asr <- adjustedSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
counts <- getConfidenceInterval(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))

results <- results %>%
  union_all(cbind(tibble(name = table_name_pssa, 
                         csr = csr, 
                         asr = asr), counts))

# 5. Benzodiazepine to Cholinesterase Inhibitor or memantine (complication)
indexId <- getATCCodes(cdm, level = c("ATC 4th"), name = "Benzodiazepine derivatives")
markerId <- getATCCodes(cdm, level = c("ATC 4th"), name = "Anticholinesterases")
markerId2 <- getDrugIngredientCodes(cdm, "memantine")

table_name_pssa <- "pssa_benzodiazepine_cholinesterase_memantine"

cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = table_name_pssa,
  conceptSetList = c(indexId, markerId, markerId2),
  summariseMode = "FirstEra"
)

updated_pssa_table <- cdm[[table_name_pssa]] %>% collect()
updated_pssa_table <- updated_pssa_table %>% 
  dplyr::mutate(cohort_definition_id = case_when(cohort_definition_id == 1 | cohort_definition_id == 2 | cohort_definition_id == 3 ~ 1,
                          cohort_definition_id == 4 | cohort_definition_id == 5 | cohort_definition_id == 6 ~ 2))
updated_pssa_table <- updated_pssa_table %>% mutate(cohort_definition_id = as.integer(cohort_definition_id))
  
updated_pssa_table <- updated_pssa_table %>% 
  group_by(subject_id) %>% 
  arrange(cohort_start_date, .by_group =T) %>%
  filter(row_number()==1)
  
csr <- crudeSequenceRatio(summaryTable(tableCleaning(updated_pssa_table, 730)))
asr <- adjustedSequenceRatio(summaryTable(tableCleaning(updated_pssa_table, 730)))

# 6. Benzodiazepine to antipsychotics
indexId <- getATCCodes(cdm, level = c("ATC 4th"), name = "Benzodiazepine derivatives")
markerId <- getATCCodes(cdm, level = c("ATC 3rd"), name = "ANTIPSYCHOTICS")

table_name_pssa <- "pssa_benzodiazepine_antipsychotics"

cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = table_name_pssa,
  conceptSetList = c(indexId, markerId),
  summariseMode = "FirstEra"
)

updated_pssa_table <- cdm[[table_name_pssa]] %>% collect()
updated_pssa_table <- updated_pssa_table %>% 
  dplyr::mutate(cohort_definition_id = case_when(cohort_definition_id == 1 | cohort_definition_id == 2 | cohort_definition_id == 3 ~ 1,
                                                 cohort_definition_id == 4 ~ 2))
updated_pssa_table <- updated_pssa_table %>% mutate(cohort_definition_id = as.integer(cohort_definition_id))

updated_pssa_table <- updated_pssa_table %>% 
  group_by(subject_id) %>% 
  arrange(cohort_start_date, .by_group =T) %>%
  filter(row_number()==1)

csr <- crudeSequenceRatio(summaryTable(tableCleaning(updated_pssa_table, 730)))
asr <- adjustedSequenceRatio(summaryTable(tableCleaning(updated_pssa_table, 730)))

results <- results %>%
  union_all(tibble(name = table_name_pssa, 
                   csr = csr, 
                   asr = asr))

# 7. Rosiglitazone to furosemide
indexId <- getDrugIngredientCodes(cdm, name = "rosiglitazone")
markerId <- getDrugIngredientCodes(cdm, name = "furosemide")

table_name_pssa <- "pssa_rosiglitazone_furosemide"

cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = table_name_pssa,
  conceptSetList = c(indexId, markerId),
  summariseMode = "FirstEra"
)

csr <- crudeSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
asr <- adjustedSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
counts <- getConfidenceInterval(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))

results <- results %>%
  union_all(cbind(tibble(name = table_name_pssa, 
                         csr = csr, 
                         asr = asr), counts))

# 8. SGLT2 inhibitors to antifungal 
indexId <- getATCCodes(cdm, level = c("ATC 4th"), name = "Sodium-glucose co-transporter 2 (SGLT2) inhibitors")
markerId <- getATCCodes(cdm, level = c("ATC 2nd"), name = "ANTIFUNGALS FOR DERMATOLOGICAL USE")

table_name_pssa <- "pssa_sglt2_antifungal"

cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = table_name_pssa,
  conceptSetList = c(indexId, markerId),
  summariseMode = "FirstEra"
)

csr <- crudeSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
asr <- adjustedSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
counts <- getConfidenceInterval(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))

results <- results %>%
  union_all(cbind(tibble(name = table_name_pssa, 
                         csr = csr, 
                         asr = asr), counts))

# 9. Statins to antibiotics
indexId <- getATCCodes(cdm, level = c("ATC 4th"), name = "HMG CoA reductase inhibitors")
markerId <- getATCCodes(cdm, level = c("ATC 2nd"), name = "ANTIBACTERIALS FOR SYSTEMIC USE")

table_name_pssa <- "pssa_statins_antibiotics"

cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = table_name_pssa,
  conceptSetList = c(indexId, markerId),
  summariseMode = "FirstEra"
)

csr <- crudeSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
asr <- adjustedSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
counts <- getConfidenceInterval(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))

results <- results %>%
  union_all(cbind(tibble(name = table_name_pssa, 
                         csr = csr, 
                         asr = asr), counts))

# 10. Statins to antidepressants
indexId <- getATCCodes(cdm, level = c("ATC 4th"), name = "HMG CoA reductase inhibitors")
markerId <- getATCCodes(cdm, level = c("ATC 3rd"), name = "ANTIDEPRESSANTS")

table_name_pssa <- "pssa_statins_antidepressants"

cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = table_name_pssa,
  conceptSetList = c(indexId, markerId),
  summariseMode = "FirstEra"
)

csr <- crudeSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
asr <- adjustedSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
counts <- getConfidenceInterval(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))

results <- results %>%
  union_all(cbind(tibble(name = table_name_pssa, 
                         csr = csr, 
                         asr = asr), counts))

# 11. Statins to quinine
indexId <- getATCCodes(cdm, level = c("ATC 4th"), name = "HMG CoA reductase inhibitors")
markerId <- getDrugIngredientCodes(cdm, "quinine")

table_name_pssa <- "pssa_statins_quinine"

cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = table_name_pssa,
  conceptSetList = c(indexId, markerId),
  summariseMode = "FirstEra"
)

csr <- crudeSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
asr <- adjustedSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
counts <- getConfidenceInterval(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))

results <- results %>%
  union_all(cbind(tibble(name = table_name_pssa, 
                         csr = csr, 
                         asr = asr), counts))
################################################################################################ 
#                                                                                              #
#                                       Negative control                                       #
#                                                                                              #  
################################################################################################
# 1. amiodarone to allopurinol
indexId <- getDrugIngredientCodes(cdm, "amiodarone")
markerId <- getDrugIngredientCodes(cdm, "allopurinol")
table_name_pssa <- "pssa_amiodarone_allopurinol"

cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = table_name_pssa,
  conceptSetList = c(indexId, markerId),
  summariseMode = "FirstEra"
)

csr <- crudeSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
asr <- adjustedSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
counts <- getConfidenceInterval(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))

results <- results %>%
  union_all(cbind(tibble(name = table_name_pssa, 
                         csr = csr, 
                         asr = asr), counts))

# 2. levothyroxine to allopurinol
indexId <- getDrugIngredientCodes(cdm, "levothyroxine")
markerId <- getDrugIngredientCodes(cdm, "allopurinol")
table_name_pssa <- "pssa_levothyroxine_allopurinol"

cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = table_name_pssa,
  conceptSetList = c(indexId, markerId),
  summariseMode = "FirstEra"
)

csr <- crudeSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
asr <- adjustedSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
counts <- getConfidenceInterval(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))

results <- results %>%
  union_all(cbind(tibble(name = table_name_pssa, 
                         csr = csr, 
                         asr = asr), counts))

# 3. Rosuvastatin to levothyroxine
indexId <- getDrugIngredientCodes(cdm, "rosuvastatin")
markerId <- getDrugIngredientCodes(cdm, "levothyroxine")
table_name_pssa <- "pssa_rosuvastatin_levothyroxine"

cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = table_name_pssa,
  conceptSetList = c(indexId, markerId),
  summariseMode = "FirstEra"
)

csr <- crudeSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
asr <- adjustedSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
counts <- getConfidenceInterval(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))

results <- results %>%
  union_all(cbind(tibble(name = table_name_pssa, 
                         csr = csr, 
                         asr = asr), counts))

# 4. levothyroxine to loop diuretics
indexId <- getDrugIngredientCodes(cdm, "levothyroxine")
markerId <- getATCCodes(cdm, level = c("ATC 3rd"), name = "high-ceiling diuretics")
table_name_pssa <- "pssa_levothyroxine_loop_diuretics"

cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = table_name_pssa,
  conceptSetList = c(indexId, markerId),
  summariseMode = "FirstEra"
)

csr <- crudeSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
asr <- adjustedSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
counts <- getConfidenceInterval(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))

results <- results %>%
  union_all(cbind(tibble(name = table_name_pssa, 
                         csr = csr, 
                         asr = asr), counts))

# 5. ace-inhibitors to loop diuretics
indexId <- getATCCodes(cdm, level = c("ATC 3rd"), name = "ACE INHIBITORS, PLAIN")
markerId <- getATCCodes(cdm, level = c("ATC 3rd"), name = "high-ceiling diuretics")
table_name_pssa <- "pssa_ace_inhibitors_loop_diuretics"

cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = table_name_pssa,
  conceptSetList = c(indexId, markerId),
  summariseMode = "FirstEra"
)

csr <- crudeSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
asr <- adjustedSequenceRatio(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))
counts <- getConfidenceInterval(summaryTable(tableCleaning(cdm[[table_name_pssa]] %>% collect(), 730)))

results <- results %>%
  union_all(cbind(tibble(name = table_name_pssa, 
                         csr = csr, 
                         asr = asr), counts))

write.csv(results, "results.csv")

#### plots results
pos_results <- results[1:9,] %>% 
  mutate(across(c('name'), substr, 6, nchar(name))) %>%
  mutate()

pos_res_plots <- ggplot(pos_results, aes(name, asr)) + geom_point(shape = 4, color="darkred", size=3) + 
  geom_errorbar(aes(ymin = lowerCI, ymax = upperCI)) +
  ggtitle("ASR and confidence intervals for different sequences") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Sequences") + ylab("Adjusted Sequence Ratio") + geom_hline(yintercept=1, linetype="dashed", color = "red")

pdf(here(plotsFolder, "pos_results.pdf"),
    width = 18, height = 10)
print(pos_res_plots, newpage = FALSE)
dev.off()


negative_results <- results[10:14,] %>% 
  mutate(across(c('name'), substr, 6, nchar(name))) %>%
  mutate()

negative_res_plots <- ggplot(negative_results, aes(name, asr)) + geom_point(shape = 4, color="darkred", size=3) + 
  geom_errorbar(aes(ymin = lowerCI, ymax = upperCI)) +
  ggtitle("ASR and confidence intervals for different sequences") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Sequences") + ylab("Adjusted Sequence Ratio") + geom_hline(yintercept=1, linetype="dashed", color = "red")

pdf(here(plotsFolder, "negative_results.pdf"),
    width = 18, height = 10)
print(negative_res_plots, newpage = FALSE)
dev.off()

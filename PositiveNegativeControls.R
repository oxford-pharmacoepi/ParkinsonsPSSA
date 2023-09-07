source(here::here("Functions", "functions.R"))

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
#step 1: generate the necessary drug cohort using generateDrugCohort()
drugCohort <- generateDrugCohort(index = list(c("amiodarone", "ingredient")), marker = list(c("levothyroxine", "ingredient")))

#step 2: create CSR, ASR and CI
csr<-crudeSequenceRatio(summaryTable(tableCleaning(drugCohort, 730)))
asr<-adjustedSequenceRatio(summaryTable(tableCleaning(drugCohort, 730)))
counts <- getConfidenceInterval(summaryTable(tableCleaning(drugCohort, 730)))

results <- tibble(name = "pssa_amiodarone_levothyroxine", 
                  csr = csr, 
                  asr = asr)

results <-cbind(results, counts)

#step 3: produce a histogram plot
getHistogram(tableCleaning(drugCohort, 730), "days")
getHistogram(tableCleaning(drugCohort, 730), "weeks")
getHistogram(tableCleaning(drugCohort, 730), "months")

#### Alternatively, step 1 and step 2 can be done using one step
results_pssa <- getPSSA(index = list(c("amiodarone", "ingredient")), 
                   marker = list(c("levothyroxine", "ingredient")), 
                   table_name = "pssa_amiodarone_levothyroxine",
                   study_time = 730) 

# 2. Direct factor Xa inhibitors to antidepressants
results_pssa <- results_pssa %>%
  rbind(getPSSA(index = list(c("Direct factor Xa inhibitors", "ATC 4th")), 
                marker = list(c("ANTIDEPRESSANTS", "ATC 3rd")), 
                table_name = "pssa_dfx_antidepressants",
                study_time = 730))

# 3. CCB to Diuretics
results_pssa <- results_pssa %>%
  rbind(getPSSA(index = list(c("CALCIUM CHANNEL BLOCKERS", "ATC 2nd")), 
                marker = list(c("DIURETICS", "ATC 2nd")), 
                table_name = "pssa_ccb_diuretics",
                study_time = 730))

# 4. antipsychotics to antiparkinson drugs
results_pssa <- results_pssa %>%
  rbind(getPSSA(index = list(c("ANTIPSYCHOTICS", "ATC 3rd")), 
                marker = list(c("ANTI-PARKINSON DRUGS", "ATC 2nd")), 
                table_name = "pssa_antipsychotics_antiparkinsonian",
                study_time = 730))

# 5. Benzodiazepine to Cholinesterase Inhibitor or memantine (complication)
results_pssa <- results_pssa %>%
  rbind(getPSSA(index = list(c("Benzodiazepine derivatives", "ATC 4th")), 
                marker = list(c("Anticholinesterases", "ATC 4th"), c("memantine", "ingredient")), 
                table_name = "pssa_benzodiazepine_cholinesterase_memantine",
                study_time = 730))

# 6. Benzodiazepine to antipsychotics
results_pssa <- results_pssa %>%
  rbind(getPSSA(index = list(c("Benzodiazepine derivatives", "ATC 4th")), 
                marker = list(c("ANTIPSYCHOTICS", "ATC 3rd")), 
                table_name = "pssa_benzodiazepine_antipsychotics",
                study_time = 730))

# 7. Rosiglitazone to furosemide
results_pssa <- results_pssa %>%
  rbind(getPSSA(index = list(c("rosiglitazone", "ingredient")), 
                marker = list(c("furosemide", "ingredient")), 
                table_name = "pssa_rosiglitazone_furosemide",
                study_time = 730))

# 8. SGLT2 inhibitors to antifungal 
results_pssa <- results_pssa %>%
  rbind(getPSSA(index = list(c("Sodium-glucose co-transporter 2 (SGLT2) inhibitors", "ATC 4th")), 
                marker = list(c("ANTIFUNGALS FOR DERMATOLOGICAL USE", "ATC 2nd")), 
                table_name = "pssa_sglt2_antifungal",
                study_time = 730))

# 9. Statins to antibiotics
results_pssa <- results_pssa %>%
  rbind(getPSSA(index = list(c("HMG CoA reductase inhibitors", "ATC 4th")), 
                marker = list(c("ANTIBACTERIALS FOR SYSTEMIC USE", "ATC 2nd")), 
                table_name = "pssa_statins_antibiotics",
                study_time = 730))

# 10. Statins to antidepressants
results_pssa <- results_pssa %>%
  rbind(getPSSA(index = list(c("HMG CoA reductase inhibitors", "ATC 4th")), 
                marker = list(c("ANTIDEPRESSANTS", "ATC 3rd")), 
                table_name = "pssa_statins_antidepressants",
                study_time = 730))

# 11. Statins to quinine
results_pssa <- results_pssa %>%
  rbind(getPSSA(index = list(c("HMG CoA reductase inhibitors", "ATC 4th")), 
                marker = list(c("quinine", "ingredient")), 
                table_name = "pssa_statins_quinine",
                study_time = 730))

##plots 
positive_results <- results_pssa

positive_results1 <- positive_results %>% 
  mutate(across(c('name'), substr, 6, nchar(name))) %>%
  mutate()

positive_results_plots <- ggplot(positive_results1, aes(name, asr)) + geom_point(shape = 4, color="darkred", size=3) + 
  geom_errorbar(aes(ymin = lowerCI, ymax = upperCI)) +
  ggtitle("ASR and confidence intervals for different sequences") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 0.6) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Sequences") + ylab("Adjusted Sequence Ratio") + geom_hline(yintercept=1, linetype="dashed", color = "red")

pdf(here(plotsFolder, "positive_results.pdf"),
    width = 18, height = 10)
print(positive_results_plots, newpage = FALSE)
dev.off()

write.xlsx(positive_results, "positive_results.xlsx")
################################################################################################ 
#                                                                                              #
#                                       Negative control                                       #
#                                                                                              #  
################################################################################################
# 1. amiodarone to allopurinol
results_pssa <- results_pssa %>%
  rbind(getPSSA(index = list(c("amiodarone", "ingredient")), 
                marker = list(c("allopurinol", "ingredient")), 
                table_name = "pssa_amiodarone_allopurinol",
                study_time = 730))

# 2. levothyroxine to allopurinol
results_pssa <- results_pssa %>%
  rbind(getPSSA(index = list(c("levothyroxine", "ingredient")), 
                marker = list(c("allopurinol", "ingredient")), 
                table_name = "pssa_levothyroxine_allopurinol",
                study_time = 730))

# 3. Rosuvastatin to levothyroxine
results_pssa <- results_pssa %>%
  rbind(getPSSA(index = list(c("rosuvastatin", "ingredient")), 
                marker = list(c("levothyroxine", "ingredient")), 
                table_name = "pssa_rosuvastatin_levothyroxine",
                study_time = 730))

# 4. levothyroxine to loop diuretics
results_pssa <- results_pssa %>%
  rbind(getPSSA(index = list(c("levothyroxine", "ingredient")), 
                marker = list(c("high-ceiling diuretics", "ATC 3rd")), 
                table_name = "pssa_levothyroxine_loop_diuretics",
                study_time = 730))

# 5. ace-inhibitors to loop diuretics
results_pssa <- results_pssa %>%
  rbind(getPSSA(index = list(c("ACE INHIBITORS, PLAIN", "ATC 3rd")), 
                marker = list(c("high-ceiling diuretics", "ATC 3rd")), 
                table_name = "pssa_ace_inhibitors_loop_diuretics",
                study_time = 730))

#### plots
negative_results <- results_pssa %>%
  anti_join(positive_results, by = "name") %>%
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

#### export
write.csv(results_pssa, "results_pssa.csv")

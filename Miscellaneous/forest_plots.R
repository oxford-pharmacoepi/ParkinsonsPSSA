library(readxl)
library(ggplot2)
library(CohortSymmetry)
library(here)
library(tidyr)

### Positive Negative Controls
amiodarone_allopurinol_negative_control <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$amiodarone_allopurinol,
                                                                                   minCellCount = minimum_counts)
amiodarone_levothyroxine_positive_control <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$amiodarone_thyroxine,
                                                                                     minCellCount = minimum_counts)

controls <- rbind(amiodarone_allopurinol_negative_control, amiodarone_levothyroxine_positive_control) |>
  omopgenerics::newSummarisedResult()

controls <- controls |>
  dplyr::mutate(across(everything(), gsub, pattern = "amiodarone", replacement = "Amiodarone"),
                across(everything(), gsub, pattern = "levothyroxine", replacement = "Levothyroxine"),
                across(everything(), gsub, pattern = "allopurinol", replacement = "Allopurinol"))

control_forest_plot <- plotSequenceRatios(result = controls,
                       onlyaSR = T,
                       colours = "black",
                       labs = c("Adjusted Sequence Ratios", "Drug Pairs")) +
  labs(caption="Figure 1: ASRs on Positive and Negative Control") + 
  theme(axis.text.x = element_text(hjust=1, size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face="bold"),
        panel.background = element_blank() ,
        axis.line = element_line(colour = "black", size = 1) ,
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.text=element_text(size=20, face = "bold"),
        plot.caption = element_text(hjust = 0.5, size=20)) 

ControlPlotName <- paste0("ControlPlots", ".png")
png(here(output_folder, ControlPlotName), width = 18, height = 8, units = "in", res = 1500)
print(control_forest_plot, newpage = FALSE)
dev.off()

### 
infreq_hypo <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$infreq_hypo,
                                                       minCellCount = minimum_counts)
freq_hypo <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$freq_hypo,
                                                     minCellCount = minimum_counts)
shin <- rbind(infreq_hypo, freq_hypo) |>
  omopgenerics::newSummarisedResult()

res <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$class_hypo,
                                               minCellCount = minimum_counts)

plotASRForestPlot <- function(result,
                              title, 
                              level = 10){
  sub_result <- result %>% 
    dplyr::filter(estimate_name == "count") %>% 
    pivot_wider(names_from = "variable_name",
                values_from = "estimate_value") %>% 
    dplyr::mutate(index = as.numeric(index),
                  marker = as.numeric(marker)) %>% 
    dplyr::mutate(n = index + marker) %>% 
    dplyr::arrange(desc(n)) %>% 
    head(level) %>% 
    dplyr::select(group_level)
  
  subset_low_to_high <- result %>% 
    dplyr::inner_join(sub_result, by = "group_level") %>% 
    dplyr::filter(variable_name == "adjusted", estimate_name == "point_estimate") %>% 
    dplyr::arrange(estimate_value) %>% 
    dplyr::pull(group_level)
  
  sr_tidy <- result %>% 
    dplyr::filter(group_level %in% subset_low_to_high) %>% 
    visOmopResults::splitGroup() %>% 
    visOmopResults::filterSettings(.data$result_type == "sequence_ratios") %>%
    dplyr::select(-c("cdm_name", "strata_name", "strata_level", "variable_level")) %>%
    visOmopResults::splitAdditional() %>%
    tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value") %>%
    dplyr::mutate(group = paste0(.data$index_cohort_name, " -> ", .data$marker_cohort_name)) %>%
    dplyr::select(-c("index_cohort_name", "marker_cohort_name")) %>%
    dplyr::mutate(
      point_estimate = as.numeric(.data$point_estimate),
      lower_CI = as.numeric(.data$lower_CI),
      upper_CI = as.numeric(.data$upper_CI),
      variable_name = as.factor(.data$variable_name)
    ) %>%
    dplyr::select(tidyselect::where( ~ dplyr::n_distinct(.) > 1)|.data$group) %>%
    dplyr::rename(
      "Adjusted Sequence Ratios" := "point_estimate",
      "Drug Pairs" := "group"
    )
  
  colours = c("adjusted" = "black")
  
  sr_forest_plot <- sr_tidy %>%
    dplyr::filter(.data$variable_name == "adjusted") %>% 
    dplyr::mutate(`Drug Pairs` = reorder(`Drug Pairs`, `Adjusted Sequence Ratios`)) %>% 
    ggplot(aes(x=`Adjusted Sequence Ratios`, y = `Drug Pairs`, xmin = lower_CI, xmax = upper_CI)) + 
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$lower_CI, xmax = .data$upper_CI, colour = .data$variable_name), height = 0.2) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$variable_name, shape = .data$variable_name), size = 3) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = 1), linetype = 2) +
    ggplot2::scale_shape_manual(values = rep(19, 5)) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::theme_bw() +
    labs(caption = title) + 
    ggplot2::theme(axis.text.x = element_text(hjust = 1, size = 20, face = "bold"),
                   axis.text.y = element_text(size = 20, face = "bold"),
                   axis.title.x = element_text(size = 20, face = "bold"),
                   axis.title.y = element_text(size = 20, face="bold"),
                   panel.background = element_blank() ,
                   axis.line = element_line(colour = "black", size = 1) ,
                   panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                   legend.key = element_rect(fill = "transparent", colour = "transparent"),
                   legend.text=element_text(size=20, face = "bold"),
                   legend.title=element_blank(),
                   plot.caption = element_text(hjust = 1, size=20, vjust = 0.0000000001))
  return(sr_forest_plot)
}

res <- res |>
  dplyr::mutate(across(everything(), gsub, pattern = "amantadine", replacement = "Amantadine"),
                across(everything(), gsub, pattern = "dopamine_agonists", replacement = "Dopamine Agonists"),
                across(everything(), gsub, pattern = "levodopa", replacement = "Levodopa"),
                across(everything(), gsub, pattern = "levodopa_decarboxylase_inhibitor_and_comt_inhibitor_oral", replacement = "COMT Inhibitors"),
                across(everything(), gsub, pattern = "monoamine_oxidase_b_inhibitors", replacement = "MAO-B Inhibitors"),
                across(everything(), gsub, pattern = "antidepressants", replacement = "Antidepressants"),
                across(everything(), gsub, pattern = "antiemetics_and_antinauseants", replacement = "Antiemetics"),
                across(everything(), gsub, pattern = "antiepileptics", replacement = "Antiepileptics"),
                across(everything(), gsub, pattern = "atypical_antipsychotics", replacement = "Atypical Antipsychotics"),
                across(everything(), gsub, pattern = "calcium_channel_blockers", replacement = "Calcium Channel Blockers"),
                across(everything(), gsub, pattern = "typical_antipsychotics", replacement = "Typical Antipsychotics")
  )

sr_forest_plot <- plotASRForestPlot(result = res, 
                                    title = "Figure 2: ASRs on Top 10 Most Observed Pairs Between Class-level Index Drugs and Marker Drugs")
SRPlotName <- paste0("SRPlotsClass", ".png")
png(here(output_folder, SRPlotName), width = 18, height = 8, units = "in", res = 1000)
print(sr_forest_plot, newpage = FALSE)
dev.off()

shin <- shin |>
  dplyr::mutate(across(everything(), gsub, pattern = "amantadine", replacement = "Amantadine"),
                across(everything(), gsub, pattern = "dopamine_agonists", replacement = "Dopamine Agonists"),
                across(everything(), gsub, pattern = "levodopa", replacement = "Levodopa"),
                across(everything(), gsub, pattern = "levodopa_decarboxylase_inhibitor_and_comt_inhibitor_oral", replacement = "COMT Inhibitors"),
                across(everything(), gsub, pattern = "monoamine_oxidase_b_inhibitors", replacement = "MAO-B Inhibitors"),
                across(everything(), gsub, pattern = "citalopram", replacement = "Citalopram"),
                across(everything(), gsub, pattern = "clozapine", replacement = "Clozapine"),
                across(everything(), gsub, pattern = "domperidone", replacement = "Domperidone"),
                across(everything(), gsub, pattern = "fluoxetine", replacement = "Fluoxetine"),
                across(everything(), gsub, pattern = "phenytoin", replacement = "Phenytoin"),
                across(everything(), gsub, pattern = "quetiapine", replacement = "Quetiapine"),
                across(everything(), gsub, pattern = "sertraline", replacement = "Sertraline"),
                across(everything(), gsub, pattern = "valproate", replacement = "Valproate"),
                across(everything(), gsub, pattern = "aripiprazole", replacement = "Aripiprazole"),
                across(everything(), gsub, pattern = "chlorpromazine", replacement = "Chlorpromazine"),
                across(everything(), gsub, pattern = "cinnarizine", replacement = "Cinnarizine"),
                across(everything(), gsub, pattern = "haloperidol", replacement = "Haloperidol"),
                across(everything(), gsub, pattern = "metoclopramide", replacement = "Metoclopramide"),
                across(everything(), gsub, pattern = "olanzapine", replacement = "Olanzapine"),
                across(everything(), gsub, pattern = "perphenazine", replacement = "Perphenazine"),
                across(everything(), gsub, pattern = "prochlorperazine", replacement = "Prochlorperazine"),
                across(everything(), gsub, pattern = "promethazine", replacement = "Promethazine"),
                across(everything(), gsub, pattern = "risperidone", replacement = "Risperidone"),
                across(everything(), gsub, pattern = "sulpiride", replacement = "Sulpiride"),
                across(everything(), gsub, pattern = "tetrabenazine", replacement = "Tetrabenazine")
                )

sr_forest_plot <- plotASRForestPlot(result = shin, 
                                    title = "Figure 3: ASRs on Top 10 Most Observed Pairs Between Ingredient-level Index Drugs and Marker Drugs")
  SRPlotName <- paste0("SRPlotsIngredient", ".png")
  png(here(output_folder, SRPlotName), width = 18, height = 8, units = "in", res = 1000)
  print(sr_forest_plot, newpage = FALSE)
  dev.off()
  
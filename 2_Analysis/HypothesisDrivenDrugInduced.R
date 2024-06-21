print("Inspired by Shin et al. (2012)")
print(paste0("Generating drug cohorts that frequently causes parkinson-like symptoms at ", Sys.time()))

hypothesis_subfolder <- here(output_folder, "hypothesis")
if (!dir.exists(hypothesis_subfolder)) {
  dir.create(hypothesis_subfolder)
}

hypothesis_subfolder_shin <- here(hypothesis_subfolder, "shin")
if (!dir.exists(hypothesis_subfolder_shin)) {
  dir.create(hypothesis_subfolder_shin)
}

hypothesis_results_subfolder <- here(hypothesis_subfolder_shin, "results")
if (!dir.exists(hypothesis_results_subfolder)) {
  dir.create(hypothesis_results_subfolder)
}

hypothesis_gt_subfolder <- here(hypothesis_subfolder_shin, "gt")
if (!dir.exists(hypothesis_gt_subfolder)) {
  dir.create(hypothesis_gt_subfolder)
}

hypothesis_plots_subfolder <- here(hypothesis_subfolder_shin, "plots")
if (!dir.exists(hypothesis_plots_subfolder)) {
  dir.create(hypothesis_plots_subfolder)
}

# CCB
print(paste0("Generating CCB at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "ccb",
                                                    ingredient = c("cinnarizine", "flunarizine"))

# Dopamine depleters 
print(paste0("Generating Dopamine Depleters at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "dopamine_depleters",
                                                    ingredient = c("tetrabenazine", 
                                                                   "reserpine"))

# Atypical antipsychotics 
print(paste0("Generating Atypical Antipsychotics at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "atypical_antipsychotics",
                                                    ingredient = c("risperidone", 
                                                                   "olanzapine",
                                                                   "ziprasidone",
                                                                   "aripiprazole"))

# Antiemetics 
print(paste0("Generating Antiemetics at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "antiemetics",
                                                    ingredient = c("metoclopramide", 
                                                                   "levosulpiride",
                                                                   "clebopride"))

# Typical antipsychotics 
print(paste0("Generating typical antipsychotics at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "typical_antipsychotics",
                                                    ingredient = c("chlorpromazine", 
                                                                   "prochlorperazine",
                                                                   "perphenazine",
                                                                   "fluphenazine", 
                                                                   "promethazine",
                                                                   "haloperidol",
                                                                   "pimozide", 
                                                                   "sulpiride"))

#####
print(paste0("Generating drug cohorts that infrequently causes parkinson-like symptoms at ", Sys.time()))
# Atypical antipsychotics 
print(paste0("Generating atypical antipsychotics at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "atypical_antipsychotics2",
                                                    ingredient = c("clozapine", 
                                                                   "quetiapine"))
# Mood stabilizer  
print(paste0("Generating Mood stabilizer at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "mood_stabilizer",
                                                    ingredient = c("lithium"))

# Antidepressant
print(paste0("Generating antidepressant at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "antidepressants",
                                                    ingredient = c("citalopram",
                                                                   "fluoxetine",
                                                                   "praoxetine",
                                                                   "sertraline"))
# Antiepileptic drugs 
print(paste0("Generating Antiepileptic drugs at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "antiepileptics",
                                                    ingredient = c("phenytoin",
                                                                   "valproate"))

# Antiemetics
print(paste0("Generating Antiemetics that infrequently causes DIP at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "antiemetics2",
                                                    ingredient = c("domperidone",
                                                                   "itopride",
                                                                   "clebopride"))

cdm <- omopgenerics::bind(
  cdm$typical_antipsychotics, cdm$atypical_antipsychotics, cdm$dopamine_depleters, cdm$antiemetics, cdm$ccb,
  name = "freq_hypothesise_driven"
)

cdm <- omopgenerics::bind(
  cdm$atypical_antipsychotics2, cdm$mood_stabilizer, cdm$antidepressants, cdm$antiepileptics, cdm$antiemetics2,
  name = "infreq_hypothesise_driven"
)

cdm <- omopgenerics::bind(
  cdm$levodopa, cdm$dopamine_agonists, cdm$maob_inhibitors, cdm$comt_inhibitors, cdm$amantadine,
  name = "parkinson_treatment"
)

print(paste0("Carrying out PSSA using drugs that are known to freqently cause DIP at ", Sys.time()))
cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 indexTable = "freq_hypothesise_driven",
                                                 markerTable = "parkinson_treatment",
                                                 name = "freq_hypo",
                                                 cohortDateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                                 daysPriorObservation = 365,
                                                 washoutWindow = 365)

CohortSymmetry::summariseSequenceRatios(cohort = cdm$freq_hypo, 
                                        minCellCount = minimum_counts) |>
  write.xlsx(file = here(hypothesis_results_subfolder, "freq_hypo.xlsx"))

CohortSymmetry::summariseSequenceRatios(cohort = cdm$freq_hypo,
                                        minCellCount = minimum_counts) |>
  CohortSymmetry::tableSequenceRatios() |> 
  gt::gtsave(filename = here(hypothesis_gt_subfolder, "freq_hypo.docx"))

CohortSymmetry::summariseTemporalSymmetry(cohort = cdm$freq_hypo,
                                          minCellCount = minimum_counts) |>
  CohortSymmetry::plotTemporalSymmetry() |> 
  ggsave(filename = here(hypothesis_plots_subfolder, "freq_hypo_temporal.png"), width = 30, height = 18)

CohortSymmetry::summariseSequenceRatios(cohort = cdm$freq_hypo,
                                        minCellCount = minimum_counts) |>
  CohortSymmetry::plotSequenceRatios(onlyaSR = T, 
                                    colours = "black") |>
  ggsave(filename = here(hypothesis_plots_subfolder, "freq_hypo_sr.png"), width = 30, height = 10)

print(paste0("Carrying out PSSA using drugs that are known to infreqently cause DIP at ", Sys.time()))
cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 indexTable = "infreq_hypothesise_driven",
                                                 markerTable = "parkinson_treatment",
                                                 name = "infreq_hypo",
                                                 cohortDateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                                 daysPriorObservation = 365,
                                                 washoutWindow = 365)

CohortSymmetry::summariseSequenceRatios(cohort = cdm$infreq_hypo,
                                        minCellCount = minimum_counts) |>
  write.xlsx(file = here(hypothesis_results_subfolder, "infreq_hypo.xlsx"))

CohortSymmetry::summariseSequenceRatios(cohort= cdm$infreq_hypo,
                                        minCellCount = minimum_counts) |>
  CohortSymmetry::tableSequenceRatios() %>% 
  gt::gtsave(filename = here(hypothesis_gt_subfolder, "infreq_hypo.docx"))

CohortSymmetry::summariseTemporalSymmetry(cohort = cdm$infreq_hypo,
                                          minCellCount = minimum_counts) |>
  CohortSymmetry::plotTemporalSymmetry() |> 
  ggsave(filename = here(hypothesis_plots_subfolder, "infreq_hypo_temporal.png"), width = 30, height = 18)

CohortSymmetry::summariseSequenceRatios(cohort = cdm$infreq_hypo,
                                        minCellCount = minimum_counts) |>
  CohortSymmetry::plotSequenceRatios(onlyaSR = T, 
                                     colours = "black") |>
  ggsave(filename = here(hypothesis_plots_subfolder, "infreq_hypo_sr.png"), width = 30, height = 10)

################################################################################
hypothesis_subfolder_feldman <- here(hypothesis_subfolder, "feldman")
if (!dir.exists(hypothesis_subfolder_feldman)) {
  dir.create(hypothesis_subfolder_feldman)
}

hypothesis_results_subfolder <- here(hypothesis_subfolder_feldman, "results")
if (!dir.exists(hypothesis_results_subfolder)) {
  dir.create(hypothesis_results_subfolder)
}

hypothesis_gt_subfolder <- here(hypothesis_subfolder_feldman, "gt")
if (!dir.exists(hypothesis_gt_subfolder)) {
  dir.create(hypothesis_gt_subfolder)
}

hypothesis_plots_subfolder <- here(hypothesis_subfolder_feldman, "plots")
if (!dir.exists(hypothesis_plots_subfolder)) {
  dir.create(hypothesis_plots_subfolder)
}
print("Inspired by Feldman et al. (2022)")
cdm <- CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = c(schema = results_database_schema, prefix = stem_table), 
  cohort_tables = c("parkinson_subtypes", 
                    "amiodarone", 
                    "levothyroxine", 
                    "allopurinol", 
                    "levodopa",
                    "dopamine_agonists",
                    "amantadine",
                    "maob_inhibitors",
                    "comt_inhibitors",
                    "typical_antipsychotics_feldman",
                    "low_potency_d2r_feldman",
                    "atypical_antipsychotics_feldman",
                    "non-antipsychoticneuroleptics1_feldman",
                    "non-antipsychoticneuroleptics2_feldman",
                    "non-antipsychoticneuroleptics3_feldman",
                    "antiarrhythmics_feldman",
                    "antibiotics_feldman",
                    "anticonvulsants_feldman",
                    "antidepressants_feldman",
                    "ssri_feldman",
                    "maoi_feldman",
                    "antidepressants2_feldman",
                    "verapamil_feldman",
                    "diltiazem_feldman",
                    "captopril_feldman",
                    "flunarizine_feldman",
                    "cinnarizine_feldman",
                    "monoamine_depleters_feldman",
                    "sympatholytic_feldman",
                    "anticholinester_feldman",
                    "miscellaneous_feldman"
                    )
)


# Typical antipsychotics 
print(paste0("Generating Typical Antipsychotics at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "typical_antipsychotics_feldman",
                                                    ingredient = c("levomepromazine",
                                                                   "perphenazine",
                                                                   "fluphenazine",
                                                                   "flupentixol",
                                                                   "zuclopenthixol",
                                                                   "loxapine",
                                                                   "amoxapine",
                                                                   "haloperidol",
                                                                   "droperidol",
                                                                   "thioproprazate",
                                                                   "thioproperazine",
                                                                   "thiothixene",
                                                                   "trifluoperazine"))

# Low potency D2R antagonists 
print(paste0("Generating Low potency D2R antagonists at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "low_potency_d2r_feldman",
                                                    ingredient = c("chlorpromazine",
                                                                   "chlorprothixene",
                                                                   "levomepromazine",
                                                                   "mepazine",
                                                                   "mesoridazine",
                                                                   "methoxypromazine",
                                                                   "percyazine",
                                                                   "pimozide",
                                                                   "promazine",
                                                                   "thioridazine"))

# Atypical antipsychotics 
print(paste0("Generating Atypical Antipsychotics at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "atypical_antipsychotics_feldman",
                                                    ingredient = c("olanzapine",
                                                                   "risperidone",
                                                                   "zotepine",
                                                                   "aripiprazole",
                                                                   "brexpiprazole",
                                                                   "ziprasidone",
                                                                   "quetiapine",
                                                                   "clozapine",
                                                                   "asenapine",
                                                                   "iloperidone",
                                                                   "lurasidone",
                                                                   "paliperidone"))

# Non-antipsychoticneuroleptics 
print(paste0("Generating Non-antipsychoticneuroleptics at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "non-antipsychoticneuroleptics1_feldman",
                                                    ingredient = c("metoclopramide",
                                                                   "clebopride",
                                                                   "prochlorperazine",
                                                                   "promethazine",
                                                                   "domperidone"))

cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "non-antipsychoticneuroleptics2_feldman",
                                                    ingredient = c("hydroxyzine", 
                                                                   "alimemazine", 
                                                                   "aceprometazine"))

cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "non-antipsychoticneuroleptics3_feldman",
                                                    ingredient = c("sulpiride",
                                                                   "tiapride",
                                                                   "cisapride",
                                                                   "veralipride"))
# Antiarrhythmics
print(paste0("Generating Antiarrhythmics at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "antiarrhythmics_feldman",
                                                    ingredient = c("amiodarone",
                                                                   "procaine"))

# Antibiotics
print(paste0("Generating Antibiotics at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "antibiotics_feldman",
                                                    ingredient = c("rifampin",
                                                                   "acyclovir",
                                                                   "amphotericin B",
                                                                   "trimethoprim",
                                                                   "sulfamethoxazole"))

# Anticonvulsants
print(paste0("Generating Anticonvulsants at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "anticonvulsants_feldman",
                                                    ingredient = c("valproate",
                                                                   "phenytoin",
                                                                   "oxcarbazepine"))

# Antidepressants
print(paste0("Generating Antidepressants at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "antidepressants_feldman",
                                                    ingredient = c("imipramine",
                                                                   "amitriptyline",
                                                                   "clomipramine",
                                                                   "dosulepin"))

print(paste0("Generating SSRI at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm,
                                             name = "ssri_feldman",
                                             atcName = 	"selective serotonin reuptake inhibitors",
                                             level = c("ATC 4th"))

print(paste0("Generating MAOI at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm,
                                             name = "maoi_feldman",
                                             atcName = 	"Monoamine oxidase A inhibitors",
                                             level = c("ATC 4th"))

print(paste0("Generating Antidepressants at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "antidepressants2_feldman",
                                                    ingredient = c("venlafaxine",
                                                                   "mirtazapine"))
# verapamil
print(paste0("Generating verapamil at ", Sys.time()))
verapamil_code <- CodelistGenerator::getDrugIngredientCodes(cdm = cdm, name = "verapamil")
names(verapamil_code) <- c("verapamil_1", "verapamil_2")

for (i in (1:length(verapamil_code))){
  cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
    cdm = cdm,
    name = names(verapamil_code)[i],
    conceptSet = verapamil_code[i]
  )
}

cdm <- omopgenerics::bind(cdm$verapamil_1, cdm$verapamil_2, name = "verapamil_feldman")
cdm$verapamil_feldman <- CohortConstructor::unionCohorts(cdm$verapamil_feldman)

# Diltiazem
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "diltiazem_feldman",
                                                    ingredient = c("diltiazem"))

# Captopril
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "captopril_feldman",
                                                    ingredient = c("captopril"))

# Flunarizine
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "flunarizine_feldman",
                                                    ingredient = c("flunarizine"))

# Cinnarizine
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "cinnarizine_feldman",
                                                    ingredient = c("cinnarizine"))


# Monoamine depleters
print(paste0("Generating Monoamine depleters at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "monoamine_depleters_feldman",
                                                    ingredient = c("valbenazine",
                                                                   "deutetrabenazine",
                                                                   "tetrabenazine",
                                                                   "reserpine"))
   
# Sympatholytic
print(paste0("Generating Sympatholytic at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "sympatholytic_feldman",
                                                    ingredient = c("methyldopa"))  

# other ingredient
print(paste0("Generating Miscellaneous causative drugs at ", Sys.time()))
cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm,
                                                    name = "miscellaneous_feldman",
                                                    ingredient = c("lithium",
                                                                   "cyclosporine", 
                                                                   "tacrolimus",
                                                                   "thalidomide", 
                                                                   "ifosfamide", 
                                                                   "vincristine", 
                                                                   "tamoxifen",
                                                                   "levothyroxine",
                                                                   "halothane",
                                                                   "quinine"))  

anticholinesterases_code <- CodelistGenerator::getATCCodes(cdm = cdm,
                                                           level = c("ATC 4th"),
                                                           name = "anticholinesterases")

names(anticholinesterases_code) <- c("anticholinesterases_1", "anticholinesterases_2")

cdm <- DrugUtilisation::generateConceptCohortSet(cdm = cdm,
                                                 conceptSet = anticholinesterases_code,
                                                 name = "anticholinester_feldman")

cdm$anticholinester_feldman <- CohortConstructor::unionCohorts(cdm$anticholinester_feldman)

cdm <- omopgenerics::bind(
  cdm$typical_antipsychotics_feldman, cdm$low_potency_d2r_feldman,               
  cdm$atypical_antipsychotics_feldman, cdm$`non-antipsychoticneuroleptics1_feldman`,
  cdm$`non-antipsychoticneuroleptics2_feldman`, cdm$`non-antipsychoticneuroleptics3_feldman`,
  cdm$antiarrhythmics_feldman, cdm$antibiotics_feldman,                   
  cdm$anticonvulsants_feldman, cdm$antidepressants_feldman,               
  cdm$ssri_feldman,                           cdm$maoi_feldman,                          
  cdm$antidepressants2_feldman,               cdm$verapamil_feldman,                     
  cdm$diltiazem_feldman,                      cdm$captopril_feldman,                    
  cdm$flunarizine_feldman,                    cdm$cinnarizine_feldman,                   
  cdm$monoamine_depleters_feldman,            cdm$sympatholytic_feldman,                 
  cdm$anticholinester_feldman,                cdm$miscellaneous_feldman,   
  name = "feldman_hypothesis"
)

cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 indexTable = "feldman_hypothesis",
                                                 markerTable = "parkinson_treatment",
                                                 name = "feldman_hypo",
                                                 cohortDateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                                 daysPriorObservation = 365,
                                                 washoutWindow = 365)

CohortSymmetry::summariseSequenceRatios(cohort = cdm$feldman_hypo,
                                        minCellCount = minimum_counts) |>
  write.xlsx(file = here(hypothesis_results_subfolder, "feldman_hypo.xlsx"))

CohortSymmetry::summariseSequenceRatios(cohort = cdm$feldman_hypo,
                                        minCellCount = minimum_counts) |>
  CohortSymmetry::tableSequenceRatios() |> 
  gt::gtsave(filename = here(hypothesis_gt_subfolder, "feldman_hypo.docx"))

CohortSymmetry::summariseTemporalSymmetry(cohort = cdm$feldman_hypo,
                                          minCellCount = minimum_counts) |>
  CohortSymmetry::plotTemporalSymmetry() |> 
  ggsave(filename = here(hypothesis_plots_subfolder, "feldman_hypo_temporal.png"), width = 30, height = 18)

CohortSymmetry::summariseSequenceRatios(cohort = cdm$feldman_hypo) |>
  CohortSymmetry::plotSequenceRatios(onlyaSR = T, 
                                     colours = "black") |>
  ggsave(filename = here(hypothesis_plots_subfolder, "feldman_hypo_sr.png"), width = 30, height = 10)

################################################################################
hypothesis_subfolder_class_level <- here(hypothesis_subfolder, "class_level")
if (!dir.exists(hypothesis_subfolder_class_level)) {
  dir.create(hypothesis_subfolder_class_level)
}

hypothesis_results_subfolder <- here(hypothesis_subfolder_class_level, "results")
if (!dir.exists(hypothesis_results_subfolder)) {
  dir.create(hypothesis_results_subfolder)
}

hypothesis_gt_subfolder <- here(hypothesis_subfolder_class_level, "gt")
if (!dir.exists(hypothesis_gt_subfolder)) {
  dir.create(hypothesis_gt_subfolder)
}

hypothesis_plots_subfolder <- here(hypothesis_subfolder_class_level, "plots")
if (!dir.exists(hypothesis_plots_subfolder)) {
  dir.create(hypothesis_plots_subfolder)
}

#Antipsychotics
print(paste0("Generating Antipsychotics at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm,
                                             name = "antipsychotics_atc",
                                             atcName = "ANTIPSYCHOTICS",
                                             level = "ATC 3rd")

# ANTIDEPRESSANTS
print(paste0("Generating ANTIDEPRESSANTS at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm,
                                             name = "antidepressants_atc",
                                             atcName = "ANTIDEPRESSANTS",
                                             level = "ATC 3rd")

# CCB
print(paste0("Generating CCB at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm,
                                             name = "ccb_atc",
                                             atcName = "CALCIUM CHANNEL BLOCKERS",
                                             level = "ATC 2nd")

#ANTIEMETICS AND ANTINAUSEANTS
print(paste0("Generating ANTIEMETICS at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm,
                                             name = "antiemetics_atc",
                                             atcName = "ANTIEMETICS AND ANTINAUSEANTS",
                                             level = "ATC 2nd")

#ANTIEPILEPTICS
print(paste0("Generating ANTIEMETICS at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm,
                                             name = "antiepileptics_atc",
                                             atcName = "ANTIEPILEPTICS",
                                             level = "ATC 2nd")

#PROPULSIVES
print(paste0("Generating PROPULSIVES at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm,
                                             name = "propulsives_atc",
                                             atcName = "PROPULSIVES",
                                             level = "ATC 3rd")

#Antiarrhythmics
print(paste0("Generating Antiarrhythmics at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm,
                                             name = "antiarrhythmics_atc",
                                             atcName = "ANTIARRHYTHMICS, CLASS I AND III",
                                             level = "ATC 3rd")

#ANTIHYPERTENSIVES
print(paste0("Generating ANTIHYPERTENSIVES at ", Sys.time()))
cdm <- DrugUtilisation::generateAtcCohortSet(cdm = cdm,
                                             name = "antihypentensives_atc",
                                             atcName = "ANTIHYPERTENSIVES",
                                             level = "ATC 2nd")

cdm <- omopgenerics::bind(
  cdm$antipsychotics_atc,                     cdm$antidepressants_atc,                          
  cdm$ccb_atc,                                cdm$antiemetics_atc,                     
  cdm$antiepileptics_atc,                     cdm$propulsives_atc,                    
  cdm$antiarrhythmics_atc,                    cdm$antihypentensives_atc,   
  name = "class_hypothesis"
)

cdm <- CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = c(schema = results_database_schema, prefix = stem_table), 
  cohort_tables = c("parkinson_subtypes", 
                    "amiodarone", 
                    "levothyroxine", 
                    "allopurinol", 
                    "parkinson_treatment",
                    "class_hypothesis"
  )
)

cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 indexTable = "class_hypothesis",
                                                 markerTable = "parkinson_treatment",
                                                 name = "class_hypo",
                                                 cohortDateRange = as.Date(c("2008-01-01", "2021-12-31")),
                                                 daysPriorObservation = 365,
                                                 washoutWindow = 365)

CohortSymmetry::summariseSequenceRatios(cohort = cdm$class_hypo,
                                        minCellCount = minimum_counts) |>
  write.xlsx(file = here(hypothesis_results_subfolder, "class_hypo.xlsx"))

CohortSymmetry::summariseSequenceRatios(cohort = cdm$class_hypo,
                                        minCellCount = minimum_counts) |>
  CohortSymmetry::tableSequenceRatios() |> 
  gt::gtsave(filename = here(hypothesis_gt_subfolder, "class_hypo.docx"))

CohortSymmetry::summariseTemporalSymmetry(cohort = cdm$class_hypo,
                                          minCellCount = minimum_counts) |>
  CohortSymmetry::plotTemporalSymmetry() |> 
  ggsave(filename = here(hypothesis_plots_subfolder, "class_hypo_temporal.png"), width = 30, height = 18)

CohortSymmetry::summariseSequenceRatios(cohort = cdm$class_hypo) |>
  CohortSymmetry::plotSequenceRatios(onlyaSR = T, 
                                     colours = "black") |>
  ggsave(filename = here(hypothesis_plots_subfolder, "class_hypo_sr.png"), width = 30, height = 10)

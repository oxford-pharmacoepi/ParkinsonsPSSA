# Prescription Sequence Symmetry Analysis for Antiparkinsonian Treatments
<img src="https://img.shields.io/badge/Study%20Status-Started-blue.svg" alt="Study Status: Started">

- **Study title**: Prescription Sequence Symmetry Analysis for Antiparkinsonian Treatments
- **ShinyApp Main Study**: TBC
- **Publications**: TBC

## Introduction
This study is focussing detecting adverse drug events/prescription cascades using PSSA (Prescription Sequence Symmetry Analysis) for drugs to treat the symptoms of parkinsonism:
* Levodopa
* Dopamine Agonists
* MAO-B Inhibitors
* COMT Inhibitors
* Amantadine

## Repository organisation
This repo is organised as follows:
- [2_Analysis](https://github.com/oxford-pharmacoepi/ParkinsonsPSSA/tree/main/2_Analysis): contains PositiveNegativeControls, HypothesisDrivenDrugInduced and TreatmentOrder.
- [3_DataDriven](https://github.com/oxford-pharmacoepi/ParkinsonsPSSA/tree/main/3_DataDriven): Under construction.

## Running the analysis
1) Download this entire repository (you can download as a zip folder using Code -> Download ZIP, or you can use GitHub Desktop). 
2) Open the project <i>ParkinsonsPSSA.Rproj</i> in RStudio (when inside the project, you will see its name on the top-right of your RStudio session)
3) Open and work though the <i>CodeToRun.R</i> file which should be the only file that you need to interact with. Run the lines in the file, adding your database specific information and so on (see comments within the file for instructions). The last line of this file will run the study <i>(source(here("RunStudy.R"))</i>.     
4) After running you should then have a zip folder with results to share in your output folder.

print(paste0("Starting data-driven approach in the context of antiparkinson treatment at ", Sys.time()))

data_driven_subfolder <- here(output_folder, "data_driven")
if (!dir.exists(data_driven_subfolder)) {
  dir.create(data_driven_subfolder)
}

data_driven_results_subfolder <- here(data_driven_subfolder, "results")
if (!dir.exists(data_driven_results_subfolder)) {
  dir.create(data_driven_results_subfolder)
}

data_driven_gt_subfolder <- here(data_driven_subfolder, "gt")
if (!dir.exists(data_driven_gt_subfolder)) {
  dir.create(data_driven_gt_subfolder)
}

data_driven_plots_subfolder <- here(data_driven_subfolder, "plots")
if (!dir.exists(data_driven_plots_subfolder)) {
  dir.create(data_driven_plots_subfolder)
}

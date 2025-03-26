# This file stages the 'Targets' controller to run analysis of documents in parallel, speeding up overall computational time

# _targets.R file
# Load targets library
library(targets)
# Tell targets where the required functions are located
source("R/FEGS_Doc_Reader_Functions_Targets_v20240214.R")
# Create additional "workers" to run targets - workers will activate after they are idle for more than 1 second
controller <- crew::crew_controller_local(name = "my_controller", workers = 20, seconds_idle = 1, launch_max=Inf)
# Set targets options
# Indicate the required packages and the controller settings
tar_option_set(packages = c("dplyr", "pdfsearch", "purrr", "tidyr", "usethis", "utils"), controller = controller)
# Create the targets pipeline
# Target 1 (synonym_list): Create a tracked file for synonym_list
# Target 2 (synonym_list_all): Create a tracked file for synonym_list_all
# Target 3 (keyword_matches): Find keywords matches
# # Setting the pattern argument to map() allows individual loop runs to be evaluated simultaneously by the "workers" created in tar_option_set
list(tar_target(created_synonym_list_all, "data/synonym_list_all.rda", format = "file"),
     tar_target(synonym_list_all, get(load(created_synonym_list_all, envir = environment()))),
     tar_target(docs, "data/documents_df.rda", format = "file"),
     tar_target(documents_df, get(load(docs, envir = environment()))),
     tar_target(keyword_matches, synonym_searcher(documents_df, synonym_list_all, 1), pattern = map(documents_df)))

# Store default metadata for internal use.

#library(magrittr)
library(rlang)
#library(jsonlite)
#library(devtools)

print("making .rfret")
.rfret <- rlang::new_environment(data = list(metadata=list()))
print(class(.rfret))
print(class(.rfret$metadata))
# create_environment <- function(metadata_json = NULL){
#     .rfret <- rlang::new_environment(data = list("metadata"=NULL))
#
#     # Read default metadata from JSON file and populate this environment with
#     # the default key-value pairs (values are symbols).
#
#     metadata <- metadata_json %>% #"../data-raw/default_metadata.json" %>%
#     jsonlite::read_json() %>%
#     rlang::syms()
#
#     # Build environment and store metadata in it.
#     assign(x = "metadata", value = metadata, envir = .rfret)
#
#     return(.rfret)
# }
#
#
# metadata_json <- "../data-raw/default_metadata.json"
#
# # Read default metadata from JSON file and populate this environment with
# # the default key-value pairs (values are symbols).
# metadata <- metadata_json %>% #"../data-raw/default_metadata.json" %>%
#     jsonlite::read_json() %>%
#     rlang::syms()
#
# # Build environment and store metadata in it.
# .rfret <- rlang::new_environment()
# assign(x = "metadata", value = metadata, envir = .rfret)
#
# # Save environment in the internal data store.
# devtools::use_data(internal = TRUE,
#                    overwrite = TRUE,
#                    .rfret)
#
# # Clean up workspace.
# remove(metadata, .rfret)

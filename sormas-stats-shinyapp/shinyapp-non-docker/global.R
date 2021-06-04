

# Loading packages -----
source(file.path(".", "loading_packages.R"))

# load binary function files
source(file.path(".", "loading_functions.R"))

## loading data
## loading_data connects to sormas db, pull all the non sensitive data needed by sormas-stats and disconnect when done
source(file.path(".", "loading_data.R"))

# Defining parameters to be used to sample data from external database (sormas database)
delay = 365 # number of days to count backward from today for entities without specific delay parameters,  default = 90 days in the past
fromDate = as.character(Sys.Date() - delay - 1) #  you can directly define fromDate as: fromDate = as.character("yyyy-mm-dd")
toDate = as.character(Sys.Date() + 1) # or toDate = as.character("yyyy-mm-dd"), +1 is added because between sql commant does not consider end of intervals

# Defining connection to db
DB_USER = "sormas_user"
DB_PASS = "password"
DB_HOST = "127.0.0.1"
DB_PORT = "5432"
DB_NAME = "sormas"
## end of configuration

# Loading packages -----
base::source(file.path(".", "loading_packages.R"))
  
# load binary files for functions -----
# source(file.path(".", "loading_functions.R")) # Use this version of sourcing the binary version of functions
base::source(file.path(".", "loading_functions.R"))

## loading data ----
## loading_data connects to sormas db, pull all the non sensitive data needed by sormas-stats and disconnect when done
base::source(file.path(".", "loading_data.R"))

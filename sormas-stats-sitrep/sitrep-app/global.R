# CONFIGURATION FOR PULLING DATA AT THE TIME OF GENERATING REPORT

reportDate = Sys.Date() # date which appears on the report
toDate = reportDate -1 # latest date of the data being queried
toDateSQL = toDate +1 # +1 is added because between sql command does not consider end of interval

duration = 1000 # number of consecutive dates used for the report
fromDate = toDate - duration + 1 # earliest date of the data being queried 
fromDateSQL = fromDate - 1 # -1 is added because between sql command does not consider beginning of interval

# Defining connection to db
DB_USER = "postgres"
DB_PASS = "HZIsormas"
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

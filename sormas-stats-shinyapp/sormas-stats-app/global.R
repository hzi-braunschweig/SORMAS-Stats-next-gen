
# Defining parameters to be used to sample data from external database (sormas database)
delay = 700 # number of days to count backward from today for entities without specific delay parameters,  default = 90 days in the past
event_delay = 700  # delay for events
fromDate = as.character(Sys.Date() - delay - 1) #  you can directly define fromDate as: fromDate = as.character("yyyy-mm-dd")
toDate = as.character(Sys.Date() + 1) # or toDate = as.character("yyyy-mm-dd"), +1 is added because between sql commant does not consider end of intervals
uniquePersonPersonContact = TRUE # or FALSE to keep only one contact between the same 2 persons ( case person and contact person)
event_fromDate = as.character(Sys.Date() - event_delay - 1) #  you can directly define fromDate as: fromDate = as.character("yyyy-mm-dd")
event_toDate = as.character(Sys.Date() + 1) # or toDate = as.character("yyyy-mm-dd"), +1 is added because between sql commant does not consider end of intervals

# User authentication
authenticat_user_global = FALSE

# defining "Report date" delay to be used as default on front end
delay_default_UI = 700 # This value should be changed to say 14 on production servers
# Defining colours to be used by dashboard icons for entities
colCont = "green"  # contact colour
colCase="red"   # case colour 
colEvent = "blue"   # event colour
colPerson = "black"   # contact person or event participant person colour
colEdge = "black"
 
# Defining connection to db
DB_USER = "sormas_user"
DB_PASS = "password"
DB_HOST = "127.0.0.1"
DB_PORT = "5432"
DB_NAME= "sormas"
## end of configuratiion

# Loading packages -----
source(file.path(".", "loading_packages.R"))
  
# load binary files for functions -----
# source(file.path(".", "loading_functions.R")) # Use this version of sourcing the binary version of functions
source(file.path(".", "loading_functions_source.R"))

## loading data ----
## loading_data connects to sormas db, pull all the non sensitive data needed by sormas-stats and disconnect when done
source(file.path(".", "loading_data.R"))

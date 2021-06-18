
# Defining parameters to be used to sample data
delay = 365 # number of days to count backward from today for entities without specific delay parameters,  default = 90 days in the past
event_delay = 365  # delay for events
fromDate = as.character(Sys.Date() - delay - 1) #  you can directly define fromDate as: fromDate = as.character("yyyy-mm-dd")
toDate = as.character(Sys.Date() + 1) # or toDate = as.character("yyyy-mm-dd"), +1 is added because between sql commant does not consider end of intervals
uniquePersonPersonContact = TRUE # or FALSE to keep only one contact between the same 2 persons ( case person and contact person)
event_fromDate = as.character(Sys.Date() - event_delay - 1) #  you can directly define fromDate as: fromDate = as.character("yyyy-mm-dd")
event_toDate = as.character(Sys.Date() + 1) # or toDate = as.character("yyyy-mm-dd"), +1 is added because between sql commant does not consider end of intervals

# Defining colours to be used by dashboard icons for entities
colCont = "green"  # contact colour
colCase="red"   # case colour
colEvent = "blue"   # event colour
colPerson = "black"   # contact person or event participant person colour
colEdge = "black"

# Loading packages -----
source(file.path(".", "loading_packages.R"))

# load binary files for functions -----
source(file.path(".", "loading_functions.R"))

## loading data ----
## loading_data connects to sormas db, pull all the non sensitive data needed by sormas-stats and disconnect when done
source(file.path(".", "loading_data.R"))

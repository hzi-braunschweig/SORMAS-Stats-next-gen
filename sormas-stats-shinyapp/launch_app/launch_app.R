## configuration
### The codes in this file connnect to sormas db and import the required data into R

# Defining parameters to be used to sample data
delay = 365 # number of days to count backward from today,  default from date = 90 days in the past
event_delay = 1000
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

# Defining connection to db
DB_USER = "sormas_user"
DB_PASS = "password"
DB_HOST = "127.0.0.1"
DB_PORT = "5432"
DB_NAME= "sormas"


mypath = "/home/bsi17/SORMAS-Stats-next-gen/sormas-stats-shinyapp" # This should be the address of the shinyapp folder of your sustem
setwd(mypath)
source("loading_packages.R")
source("loading_functions.R")
source("loading_data.R" )
runApp(appDir = mypath, launch.browser = TRUE, host = "127.0.0.1")



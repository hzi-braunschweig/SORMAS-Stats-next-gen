## configuration -----
### The codes in this file connnect to sormas db and import the required data into R
# This run each time the app loads

# Defining parameters to be used to sample data
delay = 365 # number of days to count backward from today for entities without specific delay parameters,  default = 90 days in the past
event_delay = 1000  # delay for events
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
# Change this according to your sormas-stats user in sormas
# No function in sormas-stats read personal or sensitive data from sormas. However, uuid of entities are read
DB_USER = "sormas_user"
DB_PASS = "password"
DB_HOST = "127.0.0.1"
DB_PORT = "5432"
DB_NAME= "sormas"
## end of configuratiion

# Loading global environment varaibles ad defined in global.R file
# more information on renv here https://rstudio.github.io/renv/articles/renv.html
renv::consent(provided = TRUE)
renv::restore()  # to restore the state and versions of packages of your project from renv.lock OR to revert back to previous state 
# renv::init()   # to initialize, do not run in active session
# renv::snapshot()  #  save the state of your project to renv.lock  , do not run in active session 

shiny::runApp(appDir = getwd(), launch.browser = TRUE, host = "127.0.0.1")    



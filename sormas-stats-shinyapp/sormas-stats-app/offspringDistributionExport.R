# Defining parameters to be used to sample data from external database (sormas database)
# Defining connection to db
DB_USER = "sormas_user"
DB_PASS = "password"
DB_HOST = "127.0.0.1"
DB_PORT = "5432"
DB_NAME= "sormas"

# set working directory if needed
# This should be the directory where the server.r and ui.r files are stored
# setwd("/home/bsi17/SORMAS-Stats-next-gen/sormas-stats-shinyapp/sormas-stats-app")

# Loading packages -----
source(file.path(".", "loading_packages.R"))

# Loading functions
source(file.path(".", "loading_functions_source.R"))

### Export  with wider report date interval
fromDate =  as.character("2021-01-01") #  as.character(Sys.Date() - delay - 1) #  you can directly define fromDate as: fromDate = as.character("yyyy-mm-dd")
toDate =  as.character("2021-12-17") #as.character(Sys.Date() + 1) # or toDate = as.character("yyyy-mm-dd"), +1 is added because between sql commant does not consider end of intervals

# connect to sormas_db and export data
sormas_db = dbConnect(PostgreSQL(), user=DB_USER,  dbname=DB_NAME, password = DB_PASS, host=DB_HOST, port=DB_PORT)
infectorInfecteeData = infectorInfecteeExport(sormas_db, fromDate = fromDate, toDate = toDate)

# Saving result
write.csv(infectorInfecteeData,  file.path(".", "dispersion_analysis/infectorInfecteeData.csv"),
          quote = FALSE, row.names = FALSE)



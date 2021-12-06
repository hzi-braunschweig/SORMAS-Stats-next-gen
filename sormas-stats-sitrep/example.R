# Loading packages needed
library(DBI)
library(dplyr)
library(sodium)
library(RPostgres)
library(tidyr)
library(sf)

# Defining parameters to be used to sample data
delay = 365 # number of days to count backward from today for entities without specific delay parameters,  default = 90 days in the past
fromDate = as.character(Sys.Date() - delay - 1) #  you can directly define fromDate as: fromDate = as.character("yyyy-mm-dd")
toDate = as.character(Sys.Date() + 1) # or toDate = as.character("yyyy-mm-dd"), +1 is added because between sql commant does not consider end of intervals

# Sourcing utils
source("utils/caseExport.R")
source("utils/geoPopExport.R")

# Defining USER, PASSWORD, HOST, PORT and Name to connnect to PostgreSQL database
DB_USER = "postgres"
DB_PASS = "HZIsormas"
DB_HOST = "localhost"
DB_PORT = "5432"
DB_NAME= "sormas"

# Connecting to sormas_db
sormas_db = DBI::dbConnect(RPostgres::Postgres(),
                      user=DB_USER,
                      dbname=DB_NAME,
                      password = DB_PASS,
                      host=DB_HOST,
                      port=DB_PORT
                      )
# Extracting case data, Hashing Passwords with sodium
cases = caseExport(sormas_db, fromDate, toDate)

# Exporting population and geo shapes data
pop_geo_data = geoPopExport(sormas_db, fromDate, toDate)

# Population data
population_data <- pop_geo_data$population_data

# Geo data
geo_data <- pop_geo_data$geo_data

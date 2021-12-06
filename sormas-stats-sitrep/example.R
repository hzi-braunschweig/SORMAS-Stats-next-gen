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
sormas_data = caseExport(sormas_db = sormas_db)

# Line listing of cases
cases <- sormas_data$line_list_cases

# Population and geo shapes data
population_geo <- sormas_data$population_geo_data

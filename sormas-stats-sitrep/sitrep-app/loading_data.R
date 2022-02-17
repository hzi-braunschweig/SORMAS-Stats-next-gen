# This file extract data from sormas, preprocess and import it in sitrep-app for analysis

# Connect to sormas_db
sormas_db = DBI::dbConnect(RPostgres::Postgres(),
                      user=DB_USER,
                      dbname=DB_NAME,
                      password = DB_PASS,
                      host=DB_HOST,
                      port=DB_PORT)

# Import case data line listing
case_data_line_list = ExportCaseLineList(sormas_db = sormas_db, fromDate = fromDate, toDate = toDate)

# Import population data, districts and regions
export_list = ExportPopulation(sormas_db = sormas_db)

# Population data
population_data = export_list$population_data

# Geographic units
geographic_units = export_list$geographic_units

# Import shape files
geoshapes_data = ExportGeoshapes(sormas_db = sormas_db)

# Disconnect from sormas_db ---- 
dbDisconnect(sormas_db)



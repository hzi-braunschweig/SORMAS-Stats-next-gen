# This file extract data from sormas, preprocess and import it in sitrep-app for analysis

# Connect to sormas_db
sormas_db = DBI::dbConnect(RPostgres::Postgres(),
                      user=DB_USER,
                      dbname=DB_NAME,
                      password = DB_PASS,
                      host=DB_HOST,
                      port=DB_PORT)

# Import case data line listing
case_data_list = CaseExportLineList(sormas_db = sormas_db, fromDate = fromDate, toDate = toDate)

# Import population data
population_data = PopulationExport(sormas_db = sormas_db)

# Import shape files
geoshapes_data = GeoshapesExport(sormas_db = sormas_db)

# Disconnect from sormas_db ---- 
dbDisconnect(sormas_db)



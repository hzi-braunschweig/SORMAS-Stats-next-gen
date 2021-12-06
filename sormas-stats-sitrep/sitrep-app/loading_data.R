# This file extract data from sormas, preprocess and import it in sitrep-app for analysis

# Connect to sormas_db
sormas_db = dbConnect(PostgreSQL(), user=DB_USER,  dbname=DB_NAME, password = DB_PASS, host=DB_HOST, port=DB_PORT)

# Import cases
caseData = caseExport(sormas_db = sormas_db, fromDate = fromDate, toDate = toDate)

# Import population data

# Import shape files


# Disconnect from sormas_db ---- 
dbDisconnect(sormas_db)



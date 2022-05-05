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


# Import text and references
text_files<- readtext::readtext("data/text/*.docx")
text <- data.frame(text_files[,-1], row.names=text_files[,1])
# Preparing data

# Epidemic overview
epid_base = GetEpidBase(data_line_list = case_data_line_list)

# Death counts per agegroup and gender
death_counts_agegroup_gender = GetDeathCountsPerAgegroupGender(data_line_list = case_data_line_list,
                                                               cause_of_death = "EPIDEMIC_DISEASE")

# Timeseries data 
timeseries_data <- GetTimeseriesData(case_data_line_list, as.Date(fromDate), as.Date(toDate))

# Overview epid table
overview_table <- GetOverviewEpidTable(epid_data = epid_base)

# Regional epid table
region_table <- GetRegionEpidTable(epid_data = epid_base)

# insert dynamic references to table cells
text <- GetDynamicText(text = text,
                       overview_table = overview_table,
                       region_table = region_table,
                       geographic_units = geographic_units)
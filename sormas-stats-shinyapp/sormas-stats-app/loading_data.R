
# connect to sormas_db
sormas_db = dbConnect(PostgreSQL(), user=DB_USER,  dbname=DB_NAME, password = DB_PASS, host=DB_HOST, port=DB_PORT)

# Extracting user data, Hashing Passwords with sodium
users = userExport(sormas_db=sormas_db)

## Extracting eventData -----
eventData = eventExport(sormas_db,fromDate = event_fromDate, toDate = event_toDate)

# compute location_category varaible for events
eventData = compute_eventlocation_category(eventData = eventData)

## Extracting infectorInfecteeData -----
infectorInfecteeData = infectorInfecteeExport(sormas_db, fromDate = fromDate, toDate = toDate)


## Extracting contact data ---- 
# mergingDataFromDB extracts network data, default contact data and serial interval data
importDataFrontEndOutput = mergingDataFromDB(sormas_db = sormas_db, fromDate = fromDate, toDate = toDate , uniquePersonPersonContact = TRUE)

contRegionDist = importDataFrontEndOutput$contRegionDist
nodeLineList = importDataFrontEndOutput$nodeLineList  # id here is person id
elist = importDataFrontEndOutput$elist  # id here is contact id
#siDat = importDataFrontEndOutput$siDat 

## The code below would need to be transformed in to independent functions later
#### loading case -----
## reading raw data from db based on time (fromDate, toDate) specified
dataCombined = ImportingUnformatedDataFromDB(sormas_db, fromDate, toDate)

# Assigning DOB to person with only year of birth using January 1. This is an estimates birth date
person = dataCombined$person
perTemp = person[is.na(person$birthdate_yyyy) == F, ]
perTemp2 = person[is.na(person$birthdate_yyyy) == T, ]

dm = rep(1,nrow(perTemp))
perTemp$birthDate = as.Date(with(perTemp, paste(birthdate_yyyy, dm, dm, sep = "-")))
perTemp2$birthDate = rep(NA,nrow(perTemp2)) 
person = rbind(perTemp, perTemp2)

personVar=c( "person_id", "sex","occupationtype","presentcondition", "birthDate")  
person = person[, colnames(person) %in% personVar]

## mergig case and person table
casePerson = base::merge(dataCombined$case , person, by=  "person_id",  all.x = T, all.y = F ) # to ge the casevaraibles of contacts. Contacts that
# calculating age at point that the person was a case
casePerson$age = as.numeric(round((casePerson$reportdate - casePerson$birthDate)/365))

# merging casePerson with region
casePersonRegion = base::merge(casePerson, dataCombined$region, by = "region_id", all.x = T, all.y = F)

## Adding week, month and year  as individual colums using date of report
#casePersonRegion = casePersonRegion[casePersonRegion$reportdate > as.Date("2017-05-01"),] # deleting cases with date errors 

casePersonRegion$total = rep(1, nrow(casePersonRegion))
casePersonRegion$reportweek = week(casePersonRegion$reportdate)
casePersonRegion$reportmonth = month(casePersonRegion$reportdate)
casePersonRegion$reportyear = year(casePersonRegion$reportdate)
casePersonRegion$total = rep(1, nrow(casePersonRegion))

### merging casePersonRegion with district  ##
casePersonRegionDist = base::merge(casePersonRegion, dataCombined$district, by = "district_id", all.x = T, all.y = F)

#### event and event participant data merging -----
## source data is event table and we mergr in this order: event*location*region*district*evetnPartucipant*Person
eventLoc = base::merge(dataCombined$event, dataCombined$location, by.x =  "eventlocation_id", by.y = "location_id", all.x = T, all.y = F)
eventLocReg = base::merge(eventLoc, dataCombined$region, by.x =  "region_id", by.y = "region_id", all.x = T, all.y = F)
eventLocRegDist = base::merge(eventLocReg, dataCombined$district, by.x =  "district_id", by.y = "district_id", all.x = T, all.y = F)
eventLocRegDistParticipant = base::merge(eventLocRegDist, dataCombined$eventParticipant, by.x =  "event_id", by.y = "event_id", all.x = T, all.y = F)

##loading and cleaning shap files for Nigeria----
# You only need to load one set of shap files for your country. 
# You can download shapfiles for your country at https://www.diva-gis.org/datadown
# This cleaning will depend on the country and the source of teh shap file, thus codes needto be adjusted in this section.
# This default code uses the shapfile for Nigeria
# districtShapes <- rgdal::readOGR(dsn = "shapefiles", layer = "LGAs_Aug28")
districtShapes <- rgdal::readOGR(dsn = file.path("./data/shapefiles"), layer = "LGAs_Aug28")
regionShapes <- rgdal::readOGR(dsn = file.path("./data/shapefiles"), layer = "State_Aug28") 
# renaming region names to match shapfiles, do this for all names that are not the same
regionShapes@data$StateName = as.character(regionShapes@data$StateName)
regionShapes@data$StateName[regionShapes@data$StateName == "Fct, Abuja"] = "FCT"
regionShapes@data$StateName[regionShapes@data$StateName == "Akwa Ibom"] = "Akwa-Ibom"
 
# loading shapfiles for France
# This is a general version of the app and that is whdy we load shapfiles from different countries
regionShapesFrance = rgdal::readOGR(dsn = file.path("./data/shapefiles_france"), layer = "a_reg2019")
departementShapesFrance = rgdal::readOGR(dsn = file.path("./data/shapefiles_france"), layer = "a_dep_2019")
CommuneFrance = rgdal::readOGR(dsn = file.path("./data/shapefiles_france"), layer = "a_arm2020")
#renaming region and district names.
# exporting data to correct names or regions, districts and community
# library("rio")
# export(regionShapesFrance@data, "regionFrance.csv")
# export(departementShapesFrance@data, "departementFrance.csv")
# export(CommuneFrance@data, "CommuneFrance.csv")

# Replaceing names in shapfile with correct names from data earlier exported
regionShapesFrance@data = read.csv2( file = file.path("./data/shapefiles_france/regionFrance.csv"), sep = "," )
departementShapesFrance@data = read.csv2( file = file.path("./data/shapefiles_france/departementFrance.csv"), sep = "," )
CommuneFrance@data = read.csv2( file = file.path("./data/shapefiles_france/CommuneFrance.csv"), sep = "," )

# adding random lat, lng, and ep to event to event data
#  to be added to event data export later
eventData = eventData %>%
  dplyr::mutate(n_ep = rep(5,nrow(eventData)), lat = 10.53128 + rnorm(nrow(eventData)), long = 52.21099 + nrow(eventData) ) %>%
  dplyr::select(-c(latitude, longitude ))

eventData = as.data.frame(eventData)

## creating event_variable_data dateset that maps event variables to their categories. This mapping would be used for selecting columns on event table by jurisdiction
event_variable_data = event_variable_category_maper(cuntbyRegionTableEvent = cuntbyRegionDistrictEvent(data = eventData , byRegion = TRUE ))


#disconnect from db ---- 
dbDisconnect(sormas_db)


### The codes in this file connnect to sormas db and import the required data into R

# Defining parameters to be used to sample data
delay = 1000 # number of days to count backward from today,  default from date = 90 days in the past
fromDate = as.character(Sys.Date() - delay - 1) #  you can directly define fromDate as: fromDate = as.character("yyyy-mm-dd")
toDate = as.character(Sys.Date() + 1) # or toDate = as.character("yyyy-mm-dd"), +1 is added because between sql commant does not consider end of intervals
uniquePersonPersonContact = TRUE # or FALSE to keep only one contact between the same 2 persons ( case person and contact person)

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

## Extracting eventData -----
sormas_db = dbConnect(PostgreSQL(), user=DB_USER,  dbname=DB_NAME, password = DB_PASS, host=DB_HOST, port=DB_PORT)
eventData = eventExport(sormas_db,fromDate = fromDate, toDate = toDate)



# Defining parameters to be used to sample case and contact data
delay = 700 # number of days to count backward from today,  default from date = 90 days in the past
fromDate = as.character(Sys.Date() - delay - 1) #  you can directly define fromDate as: fromDate = as.character("yyyy-mm-dd")
toDate = as.character(Sys.Date() + 1) # or toDate = as.character("yyyy-mm-dd"), +1 is added because between sql commant does not consider end of intervals
uniquePersonPersonContact = TRUE # or FALSE to keep only one contact between the same 2 persons ( case person and contact person)




## Extracting infectorInfecteeData -----
infectorInfecteeData = infectorInfecteeExport(sormas_db, fromDate = fromDate, toDate = toDate)
 
## The code below would need to be transformed in to independent functions later

## Extracting contact data ----
# mergingDataFromDB extracts network data, default contact data and serial interval data
importDataFrontEndOutput = mergingDataFromDB(sormas_db = sormas_db, fromDate = fromDate, toDate = toDate , uniquePersonPersonContact = TRUE)

contRegionDist = importDataFrontEndOutput$contRegionDist
nodeLineList = importDataFrontEndOutput$nodeLineList  # id here is person id
elist = importDataFrontEndOutput$elist  # id here is contact id
siDat = importDataFrontEndOutput$siDat

##loading and cleaning shap files ----
# You can download shapfiles for your country at https://www.diva-gis.org/datadown
# This cleaning will depend on the country and the source of teh shap file, thus codes needto be adjusted in this section.
# This default code uses the shapfile for Nigeria
districtShapes <- rgdal::readOGR(dsn = "Shapefiles", layer = "LGAs_Aug28")
regionShapes <- rgdal::readOGR(dsn = "Shapefiles", layer = "State_Aug28") 
# renaming region names to match shapfiles, do this for all names that are not the same
regionShapes@data$StateName = as.character(regionShapes@data$StateName)
regionShapes@data$StateName[regionShapes@data$StateName == "Fct, Abuja"] = "FCT"
regionShapes@data$StateName[regionShapes@data$StateName == "Akwa Ibom"] = "Akwa-Ibom"


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
casePerson = merge(dataCombined$case , person, by=  "person_id",  all.x = T, all.y = F ) # to ge the casevaraibles of contacts. Contacts that
# calculating age at point that the person was a case
casePerson$age = as.numeric(round((casePerson$reportdate - casePerson$birthDate)/365))

# merging casePerson with region
casePersonRegion = merge(casePerson, dataCombined$region, by = "region_id", all.x = T, all.y = F)

## Adding week, month and year  as individual colums using date of report
#casePersonRegion = casePersonRegion[casePersonRegion$reportdate > as.Date("2017-05-01"),] # deleting cases with date errors 

casePersonRegion$total = rep(1, nrow(casePersonRegion))
casePersonRegion$reportweek = week(casePersonRegion$reportdate)
casePersonRegion$reportmonth = month(casePersonRegion$reportdate)
casePersonRegion$reportyear = year(casePersonRegion$reportdate)
casePersonRegion$total = rep(1, nrow(casePersonRegion))

### merging casePersonRegion with district  ##
casePersonRegionDist = merge(casePersonRegion, dataCombined$district, by = "district_id", all.x = T, all.y = F)

#### event and event participant data merging -----
## source data is event table and we mergr in this order: event*location*region*district*evetnPartucipant*Person
eventLoc = merge(dataCombined$event, dataCombined$location, by.x =  "eventlocation_id", by.y = "location_id", all.x = T, all.y = F)
eventLocReg = merge(eventLoc, dataCombined$region, by.x =  "region_id", by.y = "region_id", all.x = T, all.y = F)
eventLocRegDist = merge(eventLocReg, dataCombined$district, by.x =  "district_id", by.y = "district_id", all.x = T, all.y = F)
eventLocRegDistParticipant = merge(eventLocRegDist, dataCombined$eventParticipant, by.x =  "event_id", by.y = "event_id", all.x = T, all.y = F)


# loading shapfiles for France
# You only need to load one set of shap files for your country. 
# This is a general version of the app and that is whdy we load shapfiles from different countries
regionShapesFrance = rgdal::readOGR(dsn = "shapefiles_France", layer = "a_reg2019")
departementShapesFrance = rgdal::readOGR(dsn = "shapefiles_France", layer = "a_dep_2019")
CommuneFrance = rgdal::readOGR(dsn = "shapefiles_France", layer = "a_arm2020")
#renaming region names. This is needed to be done to the districts also
temp = regionShapesFrance@data
temp$libgeo = as.character(temp$libgeo)
temp$libgeo[temp$libgeo == "Bourgogne-Franche-Comt\xe9" ] = "Bourgogne-Franche-Comte"
temp$libgeo[temp$libgeo == "Provence-Alpes-C\xf4te d'Azur" ] = "Provence-Alpes-d'Azur"
temp$libgeo[temp$libgeo == "\xcele-de-France" ] = "xcele-de-France"
temp$libgeo[temp$libgeo == "La R\xe9union" ] = "La Reunion"
regionShapesFrance@data = temp

# renaming districts   to be added
#
# adding random lat, lng, and ep to event to event data
#  to be added to event data export later
eventData = eventData %>%
  dplyr::mutate(n_ep = rep(5,nrow(eventData)), lat = 10.53128 + rnorm(nrow(eventData)), long = 52.21099 + nrow(eventData) ) %>%
  dplyr::select(-c(latitude, longitude ))


## creating event_variable_data dateset that maps event variables to their categories. This mapping would be used for selecting columns on event table by jurisdiction
event_variable_data = event_variable_category_maper(cuntbyRegionTableEvent = cuntbyRegionDistrictEvent(data = eventData , byRegion = TRUE ))


#disconnect from db ---- 
dbDisconnect(sormas_db)


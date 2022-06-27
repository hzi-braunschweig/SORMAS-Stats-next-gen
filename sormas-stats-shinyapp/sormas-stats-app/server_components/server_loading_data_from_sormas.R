###### Extracting data from sormas database ##########
# All back-end methods related to surveillance (sormas) data  extraction should be added in this file
# non surveillance data like shape files are not to be added in this file but managed by the loading_data.R file

showModal(modalDialog(title = NULL, "Loading data from the database.", "Just a minute.", tags$br(), "This message will close automatically when this has finished.", easyClose = TRUE, footer = NULL))
# connect to sormas_db
sormas_db = DBI::dbConnect(PostgreSQL(), user=DB_USER,  dbname=DB_NAME, password = DB_PASS, host=DB_HOST, port=DB_PORT)

# Extracting user data, Hashing Passwords with sodium ----
users = userExport(sormas_db=sormas_db, authenticat_user= authenticat_user_global)

## Extracting event Data -----
eventData = eventExport(sormas_db,fromDate = event_fromDate, toDate = event_toDate)
# adding random lat, lng, and ep to event to event data
#  to be added to event data export later
eventData = eventData %>%
  dplyr::mutate(n_ep = rep(5,nrow(eventData)), lat = 10.53128 + rnorm(nrow(eventData)), long = 52.21099 + nrow(eventData) ) %>%
  dplyr::select(-c(latitude, longitude ))
eventData = as.data.frame(eventData)
## creating event_variable_data dataset that maps event variables to their categories. This mapping would be used for selecting columns on event table by jurisdiction
event_variable_data = event_variable_category_maper(cuntbyRegionTableEvent = cuntbyRegionDistrictEvent(data = eventData , byRegion = TRUE ))
# compute location_category variable for events
eventData = compute_eventlocation_category(eventData = eventData)

## Extracting infectorInfecteeData -----
infectorInfecteeData = infectorInfecteeExport(sormas_db, fromDate = fromDate, toDate = toDate)
## Extracting contact data ---- 
# only data frames that matched the configuration in loading_data_config_vector are exported
# mergingDataFromDB extracts network data, default contact data and serial interval data
importDataFrontEndOutput = mergingDataFromDB(sormas_db = sormas_db, fromDate = fromDate, 
                                             toDate = toDate , uniquePersonPersonContact = TRUE)
if("contRegionDist" %in% loading_data_config_vector){contRegionDist = importDataFrontEndOutput$contRegionDist}
nodeLineList = importDataFrontEndOutput$nodeLineList  # id here is person id
elist = importDataFrontEndOutput$elist  # id here is contact id
#### loading case data-----
casePersonRegionDist = case_export(sormas_db, fromDate, toDate)
## Loading sample data ----
#this code run only if the feature is activated
if("sample_table" %in% loading_data_config_vector){sample_table = sample_export(sormas_db, fromDate, toDate)}
#disconnect from db
dbDisconnect(sormas_db)
removeModal()

## Updating UI filters -----
# elist
updatePickerInput(session = session, inputId = "regionNetworkUi", choices = sort(levels(as.factor(elist$region_name))))
updatePickerInput(session = session, inputId = "contactEntitiyTypeUi", choices = sort(unique(elist$entityType)))
updatePickerInput(session = session, inputId = "relationCaseUi", choices = sort(levels(as.factor(elist$relationtocase))))
updatePickerInput(session = session, inputId = "eventstatusUI", choices = sort(levels(as.factor(elist$eventstatus))))
updatePickerInput(session = session, inputId = "risklevelUI", choices = sort(levels(as.factor(elist$risklevel_event))))
# casePersonRegionDist
updatePickerInput(session = session, inputId = "regionCaseMapUi", choices = sort(levels(as.factor(casePersonRegionDist$region_name))))
updatePickerInput(session = session, inputId = "regionCaseUi", choices = sort(levels(as.factor(casePersonRegionDist$region_name))))
updatePickerInput(session = session, inputId = "classificationCaseUi", choices = sort(levels(as.factor(casePersonRegionDist$caseclassification))))
updatePickerInput(session = session, inputId = "facilityCaseUi", choices = sort(levels(as.factor(casePersonRegionDist$healthfacility_id))))
updatePickerInput(session = session, inputId = "diseaseCaseUi", choices = sort(levels(as.factor(casePersonRegionDist$disease))), selected = c("CORONAVIRUS"))
# event_variable_data
updatePickerInput(session = session, inputId = "eventTableColumnVaribleUi", choices = c(levels(as.factor(event_variable_data$event_variable))), selected = c("Name", "Total", "Event status"))
# eventData
updatePickerInput(session = session, inputId = "twoByTwotableEventVariblesUi", choices = c(levels(as.factor(colnames(eventData)))))
updatePickerInput(session = session, inputId = "piechartEventVaribleUi", choices = c("Event Status",levels(as.factor(colnames(eventData)))), selected = "Event Status")
updatePickerInput(session = session, inputId = "barplotEventVaribleUi", choices = c("Location category",levels(as.factor(colnames(eventData)))), selected = "Location category")
updatePickerInput(session = session, inputId = "regionEventUi", choices = sort(levels(as.factor(eventData$region_name))))
updatePickerInput(session = session, inputId = "eventIdentificationSourceUi", choices = sort(levels(as.factor(eventData$event_identification_source))))  
updateSliderInput(session = session, inputId = "range", min =  min(eventData$n_ep), max = max(eventData$n_ep), value = range(eventData$n_ep))
updatePickerInput(session = session, inputId = "diseaseEventUi", choices = sort(levels(as.factor(eventData$disease_event))), selected = c("CORONAVIRUS"))
# contRegionDist
updatePickerInput(session = session, inputId = "regionContactUi", choices = sort(levels(as.factor(contRegionDist$region_name))))
# sample_table
updatePickerInput(session = session, inputId = "diseaseSampleUi", choices = sort(levels(as.factor(sample_table$disease_sample))), selected = c("CORONAVIRUS"))
updatePickerInput(session = session, inputId = "reportdateSampleUi", choices = sort(levels(as.factor(sample_table$date_of_report))))
updatePickerInput(session = session, inputId = "regionSampleUi", choices = sort(levels(as.factor(sample_table$region_name))))
updatePickerInput(session = session, inputId = "bargraphSampleVariableUi", choices = sort(c("pathogentestresult","samplingreason","samplepurpose","shipped","received",
                                                                                            "specimencondition","samplesource","samplematerial"))  )
updatePickerInput(session = session, inputId = "samplepurposeUi", choices = sort(levels(as.factor(sample_table$samplepurpose))))
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

## FIXES IN ORDER TO HAVE THE UI PART STILL WORK INSTEAD OF REFACTORING TO NULL
elist <- list()
casePersonRegionDist <- list()
casePerson <- list()
event_variable_data <- list()
eventData <- data.frame()
contRegionDist <- list()
users <- data.frame(username = character(), password = character(), stringsAsFactors = FALSE)
sample_table <- data.frame()

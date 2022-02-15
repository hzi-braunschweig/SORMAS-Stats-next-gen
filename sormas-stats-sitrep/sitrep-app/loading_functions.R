## This file contains all the functions needed by the sitrep-app

## General functions ----

## Functions to export data from sormas ----
# base::source(file.path("./utils", "function_name.R"))
base::source("utils/ExportCaseLineList.R")
base::source("utils/ExportPopulation.R")
base::source("utils/ExportGeoshapes.R")

## Functions for data analysis ----
base::source("utils/AggregateCountsByVariable.R")
base::source("utils/AggregateCountsByDate.R")
base::source("utils/GetTotalCountsPerDistrict.R")
base::source("utils/GetNewCountsPerDistrict.R")
base::source("utils/GetEpidBase.R")
base::source("utils/GetCompleteDistrictsCategoriesDf.R")









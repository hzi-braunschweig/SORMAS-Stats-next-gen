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
base::source("utils/CalculateBirthdateAge.R")
base::source("utils/GetAgegroupGenderDf.R")
base::source("utils/GetDeathCountsPerAgegroupGender.R")
base::source("utils/GetTimeseriesDfs.R")
base::source("utils/GetTimeseriesData.R")

## Functions for plots and displaying data
base::source("utils/GetRegionEpidTable.R")
base::source("utils/GetOverviewEpidTable.R")
base::source("utils/BarplotCountsPerAgegroup.R")
base::source("utils/DisplayDeathCountsPerAgegroupGender.R")
base::source("utils/DisplayEpidDistrictMap.R")
base::source("utils/TimeseriesGraph.R")
base::source("utils/DisplayTimeseriesGraphs.R")
base::source("utils/GetTextTableKeys.R")
base::source("utils/GetTextReplacements.R")
base::source("utils/GetDynamicText.R")
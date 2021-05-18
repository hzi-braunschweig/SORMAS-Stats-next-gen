
mypath = "/home/bsi17/SORMAS-Stats-next-gen/sormas-stats-shinyapp" # This should be the address of the shinyapp folder of your sustem
setwd(mypath)
source("loading_packages.R")
source("loading_functions.R")
source("loading_data.R" )
runApp(appDir = mypath, launch.browser = TRUE, host = "127.0.0.1")




mypath = "/home/bsi17/Dropbox/sormas_2021/sormasDevelopment/dashboardShiny/sormas/sormasApp"
#sep = ";"
setwd(mypath)
source("loading_packages.R")
source("loading_functions.R")
source("loading_data.R" )
runApp(appDir = mypath, launch.browser = TRUE, host = "127.0.0.1")


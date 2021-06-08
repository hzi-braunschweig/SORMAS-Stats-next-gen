  # Install required R packages for shiny
install.packages('shiny', version='1.6.0', repos='http://cran.us.r-project.org')
install.packages('shinydashboard', version='0.7.1', repos='http://cran.us.r-project.org')
install.packages('shinyWidgets', version='0.6.0', repos='http://cran.us.r-project.org')
install.packages('shinythemes', version='1.2.0', repos='http://cran.us.r-project.org')
install.packages('shinyjs', version='2.0.0', repos='http://cran.us.r-project.org')
install.packages('shinycssloaders', version='1.0.0', repos='http://cran.us.r-project.org')
#install.packages('V8', version='3.0.2', repos='http://cran.us.r-project.org')

# Install required R packages for the program
# install.packages("devtools", version='2.3.1', repos='http://cran.us.r-project.org')  # provided by rocker/shiny
# install_github("r-lib/devtools")
#packages needed by app
install.packages('renv', version='0.13.2', repos='https://cloud.r-project.org')
install.packages('dplyr', version='1.0.5', repos='http://cran.us.r-project.org')
install.packages('ggplot2', version='3.3.3', repos='http://cran.us.r-project.org')
install.packages('RColorBrewer', version='1.1-2', repos='http://cran.us.r-project.org')
install.packages('plotly', version='4.9.3', repos='http://cran.us.r-project.org')
install.packages('rpart', version='4.1-15', repos='http://cran.us.r-project.org')
install.packages('visNetwork', version='2.0.9', repos='http://cran.us.r-project.org')
install.packages('extrafont', version='0.17', repos='http://cran.us.r-project.org')
install.packages('extrafontdb', version='1.0', repos='http://cran.us.r-project.org')
install.packages('webshot', version='0.5.2', repos='http://cran.us.r-project.org')
install.packages('ggthemes', version='4.2.4', repos='http://cran.us.r-project.org')
install.packages('lubridate', version='1.7.10', repos='http://cran.us.r-project.org')
install.packages('Hmisc', version='4.5-0', repos='http://cran.us.r-project.org')
install.packages('gdata', version='2.18.0', repos='http://cran.us.r-project.org')
install.packages('scales', version='1.1.1', repos='http://cran.us.r-project.org')
install.packages('EpiEstim', version='2.2-4', repos='http://cran.us.r-project.org')
install.packages('incidence', version='1.7.3', repos='http://cran.us.r-project.org')
install.packages('tidyr', version='1.1.3', repos='http://cran.us.r-project.org')
install.packages('tidyverse', version='1.3.1', repos='http://cran.us.r-project.org')

#install.packages('xml2', version='1.3.2', repos='http://cran.us.r-project.org')  # needed fro tmaptools
remotes::install_version(package = "XML", version = "3.99-0.3")
devtools::install_github("rspatial/raster")  # dependency of tmap 
install.packages('rgdal', version='1.5-21', repos='http://cran.us.r-project.org')
install.packages('maps', version='3.3.0', repos='http://cran.us.r-project.org')
install.packages('maptools', version='1.1-1', repos='http://cran.us.r-project.org')
install.packages('mapdata', version='2.3.0', repos='http://cran.us.r-project.org')
install.packages('foreign', version='0.8-76', repos='http://cran.us.r-project.org')
install.packages('sp', version='1.4-5', repos='http://cran.us.r-project.org')
install.packages('broom', version='0.7.6', repos='http://cran.us.r-project.org')
install.packages('ggmap', version='3.0.0', repos='http://cran.us.r-project.org')
install.packages('rgeos', version='0.5-5', repos='http://cran.us.r-project.org')
install.packages('tmap', version='3.3-1', repos='http://cran.us.r-project.org')
install.packages('lattice', version='0.20-44', repos='http://cran.us.r-project.org')
devtools::install_github("rstudio/fontawesome") # does not exist on cran thus installed from git repot
install.packages('fitdistrplus', version='1.1-5', repos='http://cran.us.r-project.org')
install.packages('leaflet', version='2.0.4.1', repos='http://cran.us.r-project.org')
install.packages('igraph', version='1.2.6', repos='http://cran.us.r-project.org')
install.packages('RPostgreSQL', version='0.6-2', repos='http://cran.us.r-project.org')




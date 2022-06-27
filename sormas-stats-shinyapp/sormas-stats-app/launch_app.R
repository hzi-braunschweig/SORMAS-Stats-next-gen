
# Before launching the application using this file, make sure that the "connection to db" section in the global.R file is defined
# with the right posgres user account credentials.
# more information on renv here https://rstudio.github.io/renv/articles/renv.html
 renv::consent(provided = TRUE)
 renv::restore()  # to restore the state and versions of packages of your project from renv.lock OR to revert back to previous state 
# renv::init()   # to initialize, do not run in active session
# renv::init(bare = TRUE) # initialize the project without attempting to discover and install R package dependencies
# renv::snapshot()  #  save the state of your project to renv.lock  , do not run in active session 

shiny::runApp(appDir = getwd(), launch.browser = TRUE, host = "127.0.0.1")    

shinyServer(
function(input, output,session) { 
### LANGUAGE SETTINGS ----   
observeEvent(input$selected_language, {
  # update language in session
  shiny.i18n::update_lang(session, input$selected_language)
})    

#### EXTRACTING DATA FROM SORMAS DATABASE -----
msg <- try({
      base::source(file.path("./server_components","server_loading_data_from_sormas.R"), local = TRUE)$value
      })
if(inherits(msg, "try-error")){
  showModal(modalDialog(title = "Startup error", as.character(msg), easyClose = TRUE))
}
# Call login module 
credentials <- shinyauthr::loginServer(
  id = "login",
  data = users,
  user_col = username,
  pwd_col = password,
  sodium_hashed = TRUE,
  log_out = reactive(logout_init())
)
# call the logout module with reactive trigger to hide/show
logout_init <- shinyauthr::logoutServer(
  id = "logout",
  active = reactive(credentials()$user_auth)
)
#### TRANSMISSION CHAIN ANALYSIS ----
# Sourcing the component of server to render the network tab. To remove this tab, comment this line and restart R
base::source(file.path("./server_components","server_transmission_network.R"), local = TRUE)$value

## CONTACT DATA ANALYSIS-----
base::source(file.path("./server_components","server_contact_data_analysis.R"), local = TRUE)$value 

#### CASE DATA ANALYSIS ----
base::source(file.path("./server_components","server_case_data_analysis.R"), local = TRUE)$value   
    
#### EVETN DATA ANALYSIS ----
base::source(file.path("./server_components","server_event_data_analysis.R"), local = TRUE)$value 

## SAMPLE DATA ANALYSIS -----
base::source(file.path("./server_components","server_sample_data_analysis.R"), local = TRUE)$value 
    
## MODEL SPECIFICATION ----
    output$model_specificationUI <- renderUI(
      includeMarkdown(paste0("Documentation/model_specification_", input$language,".md"))
    )
} 
)
 










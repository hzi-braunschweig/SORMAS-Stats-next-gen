## CONTACT DATA ANALYSIS-----
# This sub section of the server.r file renders the contact data analysis tab
# All back-end methods related to this tab should be added in this file

# Filter contact data  based on region district time and disease
contRegionDistDiseaseDate = reactive({
  req(credentials()$user_auth)
  if(is.null(input$regionContactUi))
  {
    contRegionDist[((contRegionDist$disease == input$diseaseContactUi) & (contRegionDist$reportdatetime >= (min(input$reportdateContactUi))) & (contRegionDist$reportdatetime <= (max(input$reportdateContactUi)) )), ]
  } else{
    contRegionDist[((contRegionDist$region_name %in% input$regionContactUi) & (contRegionDist$disease == input$diseaseContactUi) & (contRegionDist$reportdatetime >= (min(input$reportdateContactUi) )  ) & (contRegionDist$reportdatetime <= (max(input$reportdateContactUi) ))),]
  }
})
# Adding control based on contactDataAnalysisAction icon on ui
# Any output or computation that depend on contRegionDistDiseaseDate would run only when contactDataAnalysisAction is clicked
d = eventReactive(input$contactDataAnalysisAction, { 
  contRegionDistDiseaseDate() 
}, ignoreNULL = FALSE)
# end of contact filter  
# Begin of contact analysis    
## Bar plot
output$plot <- renderPlot({
  if(nrow(d( ) ) == 0)
  {
    plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="Number of contacts", xlab=" ",
         main = "No data exist based on your selection, please choose another selection for which data exist")
  }else{
    if(is.null(input$regionContactUi) )
    {
      par(las=2, mar=c(7,4,4,2)) # make label text perpendicular to axis
      barplot (table(as.factor(d( )$region_name)), ylab = "Number of contacts",main = "Bar plot for number of contacts")
    } else {
      par(las=2, mar=c(7,4,4,2)) # make label text perpendicular to axis
      barplot (table(as.factor(d( )$district_name)), ylab = "Number of contacts",main = "Bar plot for number of contacts")
    }
  }
}, height=700)
## exporting bar plot
output$downloadBarplot = downloadHandler(
  filename = function(){
    paste("barPlotContacts", "png", sep = ".")
  },
  content = function(file){
    png(file)
    ##
    if(is.null(input$regionContactUi) )
    {
      par(las=2, mar=c(7,4,4,2)) # make label text perpendicular to axis
      barplot (table(as.factor(d( )$region_name)), ylab = "Number of contacts",main = "Bar plot for number of contacts")
    } else {
      par(las=2, mar=c(7,4,4,2)) # make label text perpendicular to axis
      barplot (table(as.factor(d( )$district_name)), ylab = "Number of contacts",main = "Bar plot for number of contacts")
    }
    ##
    dev.off()
  }
)

### Beginning of contact KPI ###########
## Row 1 All contacts
output$allCont <- renderInfoBox({
  infoBox(
    "", nrow(d( )), icon = icon("handshake"),
    color = colCont, fill = FALSE , subtitle = "All contacts"   )
})
# confirmed contacts
output$contConfirmed = renderInfoBox({
  temp = d()
  temp = temp[temp$contactclassification == "CONFIRMED" ,]
  infoBox("", nrow(temp), icon = icon("handshake"), color = colCont, fill = FALSE, subtitle = "Confirmed"
  )
})
# Unconfirmed contacts
output$contUnconfirmed = renderInfoBox({
  temp = d()[d()$contactclassification == "UNCONFIRMED" ,]
  infoBox("", nrow(temp), icon = icon("handshake"), color = colCont, fill = FALSE, subtitle = "Unconfirmed"
  )
})
# Not a contact contacts
output$contNot = renderInfoBox({
  temp =  d()[d()$contactclassification == "NO_CONTACT" ,]
  infoBox("", nrow(temp), icon = icon("handshake"), color = colCont, fill = FALSE, subtitle = "Discarded"
  )
})
### Row 2  contact status ####
## Active contacts
output$activeCont = renderInfoBox({
  temp = d()[d()$contactstatus == "ACTIVE" ,]
  infoBox("", nrow(temp), icon = icon("handshake"), color = colCont, fill = FALSE, subtitle = "Active"
  )
})
## converted to case
output$convertedToCase = renderInfoBox({
  temp =  d()[d()$contactstatus == "CONVERTED" ,]
  infoBox( "",nrow(temp), icon = icon("handshake"), color = colCont,
           fill = FALSE,  subtitle = "Converted to case"
  )
  
})
## dropped contacts
output$dropped = renderInfoBox({
  temp =   d()[d()$contactstatus == "DROPPED" ,]
  infoBox("", nrow(temp), icon = icon("handshake"), color = colCont, fill = FALSE, subtitle = "Dropped"
  )
})
## inactive
output$inactiveCont = renderInfoBox({
  temp =  d()[d()$contactstatus == "INACTIVE",]
  infoBox("", nrow(temp), icon = icon("handshake"), color = colCont, fill = FALSE, subtitle = "Inactive"
  )
})
### Row 4, summary of number of contacts per case
p = reactive({
  data.frame(as.table(summary(as.factor(d()$caze_id), maxsum = 5000000)))
})
## min contact per case
output$minConPerCase = renderInfoBox({
  infoBox("",   min(p()$Freq) , icon = icon("handshake"), color = colCont,
          fill = FALSE, subtitle = "Min cont-per-case"
  )
})
## median contact per case
output$medianConPerCase = renderInfoBox({
  infoBox("", median(p()$Freq) , icon = icon("handshake"), color = colCont, fill = FALSE, subtitle = "Med cont-per-case"
  )
})
## mean contact per case
output$meanConPerCase = renderInfoBox({
  infoBox("",  round(mean(p()$Freq), digits = 2 ) , icon = icon("handshake"), color = colCont, fill = FALSE, subtitle = "Mean cont-per-case"
  )
})
## max contact per case
output$maxConPerCase = renderInfoBox({
  infoBox("",  max(p()$Freq)   , icon = icon("handshake"), color = colCont, fill = FALSE, subtitle = "Max cont-per-case"
  )
})
#### end of KPI 
#Contacts per case analysis begin----
if(contact_per_case_plot=="t"){
  minLim = reactive({
    c( min(p()$Freq), max(p()$Freq)+20)
  })
  output$plotContPerCase <- renderPlot({
    if(nrow(d( ) ) == 0)
    {
      plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="Number of contacts", xlab=" ",
           main = "No data exist based on your selection, please choose another selection for which data exist")
    }else{
      par(mar=c(5,4,4,1), mfrow = c(1,2))
      hist(p()$Freq, breaks = 50,  xlim = minLim(), col = "grey", main = "Histogram of number of contacts per case", xlab = "Number of contacts per case")
      plot(sort(p()$Freq), col="black", cex=1, pch=16, xlab = "Case index", ylab = "Number of contacts",
           main = "Number of contacts per case")
    }
  }, height=700)
  ## exporting contact per case plot
  output$downloadContPerCasePlot = downloadHandler(
    filename = function(){
      paste("contactPerCasePlot", "png", sep = ".")
    },
    content = function(file){
      png(file)
      ##
      par(mar=c(5,4,4,1), mfrow = c(1,2))
      hist(p()$Freq, breaks = 50,  xlim = minLim(), col = "grey", main = "Histogram of number of contacts per case", xlab = "Number of contacts per case")
      plot(sort(p()$Freq), col="black", cex=1, pch=16, xlab = "Case index", ylab = "Number of contacts",
           main = "Number of contacts per case")
      ##
      dev.off()
    })}
#Contacts per case analysis ends
#Contacts per case export begin -----
if(contact_per_case_export == "t"){
  conPerCaseExp = reactive({
    p() %>% dplyr::rename(Case_id = Var1, Nunber_of_contacts = Freq )
  })
  output$conPerCaseExpCsv <- downloadHandler(
    filename = function() {
      paste("sormas_contactPerCaseExp_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(conPerCaseExp(), file)})
  output$conPerCaseExpTable <- renderPrint({
    orig <- options(width = 1000)
    print(head(conPerCaseExp(), input$maxrows), row.names = FALSE)
    options(orig)
  })}
#Contacts per case export ends 
#Contact per region export begin ----
conPerGerionExp = reactive({
  temp =  data.frame(table(as.factor(d( )$region_name)))
  colnames(temp) = c("Region_name", "Number_of_Contacts")
  temp = temp[order(temp$Number_of_Contacts, decreasing = T), ]
  return(temp)
})
output$conPerGerionExpCsv <- downloadHandler(
  filename = function() {
    paste("sormas_conPerGerionExp_", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(conPerGerionExp(), file)
  }
)
output$conPerGerionExpTable <- renderPrint({
  orig <- options(width = 1000)
  print(head(conPerGerionExp(), input$maxrowsContByRegion), row.names = FALSE)
  options(orig)
})
#Contact per region export ends
# UI output for "contact data analysis" tab ----
# This UI section was moved to the server section because we wanted to make it configurable
# The complete UI element is build by R
# The output object "contact_analysis_output" should be placed below the computation of elements needed in it
output$contact_analysis_output <- renderUI({
panels <- list(
tabPanel("Contact dashboard",
wellPanel(style = "background: white", 
fluidRow(width = 12,
 column(2, infoBoxOutput("allCont", width = 12)),
 column(2, infoBoxOutput("contConfirmed", width = 12)),
 column(2, infoBoxOutput("contUnconfirmed", width = 12)),
 column(2, infoBoxOutput("contNot", width = 12)),
 column(2,infoBoxOutput("activeCont", width = 12)),
 column(2,infoBoxOutput("convertedToCase", width = 12))
),
fluidRow(width=12,
 column(2,infoBoxOutput("dropped", width = 12)),
 column(2,infoBoxOutput("inactiveCont", width = 12)),
 column(2, infoBoxOutput("minConPerCase", width = 12)),
 column(2,infoBoxOutput("medianConPerCase", width = 12)),
 column(2, infoBoxOutput("meanConPerCase", width = 12)),
 column(2,infoBoxOutput("maxConPerCase", width = 12)) )
),
plotOutput("plot", width = "100%", height = "90vh"),downloadButton("downloadBarplot", "Download this plot")
 , tags$br(),tags$br(),
 " Each bar in this plot represents a region or district and the height of the bar corresponds to the number of contacts 
     in the region or district."
),
tabPanel("Contact per region export", icon = icon("table"),
width = 10,
dashboardPage( # the use of shiny dashboard is to make sure that all icons are fine, do not remove or deactivate this tab.
dashboardHeader( ),
dashboardSidebar(disable = TRUE,
pickerInput("conPerson", "Contact entity type", choices = c("Contact", "Contact person"), # option to view contact or contact person
            selected = c("Contact"),multiple = FALSE) ),
dashboardBody(
 numericInput("maxrowsContByRegion", "Rows to show", 20),
 verbatimTextOutput("conPerGerionExpTable"),
 downloadButton("conPerGerionExpCsv", "Download as CSV"),tags$br(),tags$br(),
 "Each row in this data is a region with corresponding number of contacts.
     The data was obtained by summing the number of contacts in each region.
     The resgion of the source case was used in case the region of the contact was missing." )
 )) )
if(contact_per_case_plot=="t"){
  panels[[3]]  = tabPanel("Contact per case plot", plotOutput("plotContPerCase", width = "100%", height = "90vh"),  
                          downloadButton("downloadContPerCasePlot", "Download this plot"), tags$br(),tags$br(), "Contact per case.")}
if(contact_per_case_export == "t"){
panels[[4]] <-tabPanel("Contacts per case export", numericInput("maxrows", "Rows to show", 20),
 verbatimTextOutput("conPerCaseExpTable"),
 downloadButton("conPerCaseExpCsv", "Download as CSV"),tags$br(),tags$br(),
 "Each row in this data is a case. The data was obtained by summing the number of contacts for each case. Cases with no contact are not included in this table")
}
base::do.call(tabsetPanel, panels)
})  
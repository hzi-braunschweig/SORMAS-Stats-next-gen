# Render infoboxes for multiple panels (contact,event,case,samples)
# input data= reactive data,
# create a list of values to render in the function using the rendervalueBoxfromList function
# icons and colors arguments to rendervalueBoxfromList can be either single character variables 
# or be specifically assigned by passing named list items
KPIValueBox_UI <- function(id) {
  ns <- NS(id)
  tagList(
      uiOutput(ns("boxlist_UI"))
  )
}
KPIValueBox_Server <-
  function(id,data,panel,boxfun) {
    stopifnot(is.reactive(data))
    moduleServer(id,
        function(input, output, session) {
        output$boxlist_UI <-
          renderUI({
          ###' * Contact Panel *
              if (panel=="contact") {
      
                # icon  = list("Min cont-per-case"="greater-than-equal","Med cont-per-case"="minus",
                #              "Mean cont-per-case"="minus","Max cont-per-case"="less-than-equal")                 
                # idData =  data.frame(as.table(summary(as.factor(data()$caze_id), maxsum = 5000000)))
                # KPI <- as.list( c(
                #     "Min cont-per-case" = min(idData$Freq), "Med cont-per-case" = median(idData$Freq),
                #     "Mean cont-per-case" = round(mean(idData$Freq),2),"Max cont-per-case" = max(idData$Freq) ) )
                KPI_tabel<-as.list(
                  c( 
                    "All contacts" = nrow(data()),
                    table(data()$contactclassification),
                    table(data()$contactstatus)
                    )
                  )
                ## render Value boxes
                box(collapsible = TRUE,
                    width=12,
                    title=NULL,
                rendervalueBoxfromList(KPI_tabel,icon="handshake",color="light-blue")
                )
              }
          ###' * Event Panel *
              else if(panel=="event") {
                icon  = list("cogs","user-cog","procedures","CLUSTER"="people-arrows","EVENT"="cog","SIGNAL"="exclamation","SCREENING"="vials")
                color = list("blue", "blue", "red","CLUSTER"="blue","EVENT"="blue","SIGNAL"= "yellow","SCREENING"="navy")      
                ### build KPI list
                KPI <- as.list(
                   c(
                    "Total Events" = nrow(data()),
                    "Event Participants"=sum(data()$eventPart_sum),
                    "Resulting cases"=sum(data()$resulting_case_sum),
                    table(data()$eventstatus)
                  )
                )
                ## render event  Value boxes
                box(collapsible = TRUE,
                    width=12,
                    title=NULL,
                rendervalueBoxfromList(KPI,icon,color)
                 )
              }
          ###' * Case Panel *
              else if(panel=="case") {
                icon  = "procedures"
                color = "red"
                ### build KPI lists:
                classification <- as.list(
                  c("All cases" = nrow(data()), table(data()$caseclassification)))
                quarantine <- as.list(
                  c(table(data()[!is.na(data()$quarantine), ]$quarantine),
                    "Missing" = nrow(data()[is.na(data()$quarantine),])
                    )
                  )
                gender <- as.list(
                  c( table(data()[!is.na(data()$sex),]$sex),
                    "Missing sex" = nrow(data()[is.na(data()$sex),])
                    )
                  )
                age <- as.list(
                  c(
                  "Min age" = min(data()$age[!is.na(data()$age) ]),
                  "Median age" = median(data()$age[!is.na(data()$age)]),
                  "Mean age" = round(mean(data()$age[!is.na(data()$age) ]),2),
                  "Max age" = max(data()$age[!is.na(data()$age) ]),
                  "Missing age" = nrow(data()$age[is.na(data()$age) ])
                  )
                )
                outcome<-as.list( c(table(data()$outcome) ))
                ## render Value boxes in tab Box
              tabBox( id = "tabsetCase",
                      width = 12,
                      title = icon("procedures"),
                      height = "250px",
                  tabPanel("classification","", rendervalueBoxfromList(classification,icon,color)),
                  tabPanel( "quarantine", "", rendervalueBoxfromList(quarantine,
                                                                     icon=list("HOME"= "home","INSTITUTIONELL"="hospital",
                                                                               "NONE"  ="minus","OTHER"= "home","UNKNOWN" ="question",
                                                                               "Missing" = "minus"), color)),
                  tabPanel( "gender", "", rendervalueBoxfromList(gender,icon,color)),
                  tabPanel( "age", "",
                            rendervalueBoxfromList(age,
                                                  icon  = list("Min age"="greater-than-equal",
                                                                "Med age"="minus",
                                                                "Mean age"="minus",
                                                                "Max age"="less-than-equal" ), color)),
                  tabPanel( "outcome","",  rendervalueBoxfromList(outcome,icon=list("procedures","procedures","handshake","question"),color))
                )
              }
          ###' * Samples Panel*
              else if(panel=="samples"){
                icon= "vial"
                color = "purple"
                KPI<- as.list(
                     c(
                    "Total Samples"=nrow(data()),
                    table(data()$case_classification)
                    )
                  )
                tabBox(
                       id = "tabsetSamples",
                       width = 12,
                       title = icon("vials"),
                       height = "250px",
                      tabPanel("classification","", rendervalueBoxfromList( KPI, icon,color)),
                      tabPanel("result","" ,rendervalueBoxfromList( as.list(c(table(data()$pathogentestresult))),icon,color))
                      )
              }
          }) #end of render UI
        }
    )
  }

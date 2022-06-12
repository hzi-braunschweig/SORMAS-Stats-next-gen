# kpi MODULE 
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
           
  ###'  * Contact Panel *
              if (panel=="contact") {
                
                ### icons for mean median min max COntacts per case id
                # icon  = list("Min cont-per-case"="greater-than-equal",
                #              "Med cont-per-case"="minus",
                #              "Mean cont-per-case"="minus",
                #              "Max cont-per-case"="less-than-equal"
                #              )
                # color = c("olive","maroon","maroon","navy")
                # 
                # idData =  data.frame(as.table(summary(as.factor(data()$caze_id), maxsum = 5000000)))
                # 
                # ### calculate mean min max contact per case id
                # KPI <- as.list(
                #   c(
                #     "Min cont-per-case" = min(idData$Freq),
                #     "Med cont-per-case" = median(idData$Freq),
                #     "Mean cont-per-case" = round(mean(idData$Freq),2),
                #     "Max cont-per-case" = max(idData$Freq)
                #     
                #   )
                # )
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
              #  rendervalueBoxfromList(KPI,icon,color),
                rendervalueBoxfromList(KPI_tabel,icon="handshake",color="light-blue")
                
                )
              }
  ###'     * Event Panel *
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
                
              #  icon2 = list("CLUSTER"="people-arrows","EVENT"="cog","SIGNAL"="exclamation","SCREENING"="vials")
              #  color2=list("CLUSTER"="blue","EVENT"="blue","SIGNAL"= "yellow","SCREENING"="navy")
                # KPI_from_table<-as.list(
                #   c(
                #     table(data()$eventstatus)
                #   )
                # )
                ## render Value boxes
                box(collapsible = TRUE,
                    width=12,
                    title=NULL,
                rendervalueBoxfromList(
                  KPI,icon,color)#,
                # rendervalueBoxfromList(
                #   KPI_from_table,icon=icon2, color=color2 )
                 )
              }
  
  ###'      * Case Panel *
              else if(panel=="case") {
                icon  = "procedures"
                color = "red"
                ### build KPI lists:
                classification <- as.list(
                  c("All cases" = nrow(data()), table(data()$caseclassification)))
                quarantine <- as.list(
                  c(table(data()[!is.na(data()$quarantine), ]$quarantine),
                    "Missing" = nrow(data()[is.na(data()$quarantine),])))
                gender <- as.list(
                  c( table(data()[!is.na(data()$sex),]$sex),
                                     "Missing sex" = nrow(data()[is.na(data()$sex),] )))
                age <- as.list(
                  c(
                  "Min age" = min(data()$age[!is.na(data()$age) ]),
                  "Median age" = median(data()$age[!is.na(data()$age)]),
                  "Mean age" = round(mean(data()$age[!is.na(data()$age) ]),2),
                  "Max age" = max(data()$age[!is.na(data()$age) ]),
                  "Missing age" = nrow(data()$age[is.na(data()$age) ])
                ))
                outcome<-as.list(
                  c(table(data()$outcome)))
                # occupation<-as.list(
                #   c(
                #   table(data()[!is.na(data()$occupationtype), ]$occupationtype),
                #   "Missing" = nrow(data()[!is.na(data()$occupationtype), ])
                # ))
        
                ## render Value boxes in tab Box
              tabBox( id = "tabset1",
                      width = 12,
                      title = icon("procedures"),
                      height = "250px",
                  tabPanel("classification","", rendervalueBoxfromList(classification,icon,color)),
                  tabPanel( "quarantine", "", rendervalueBoxfromList(quarantine,
                                                                     icon=list("home","hospital","minus","home","question","minus"),
                                                                     color)),
                  tabPanel( "gender", "", rendervalueBoxfromList(gender,icon,color)),
                  tabPanel( "age", "",
                            rendervalueBoxfromList(age,
                                                  icon  = list("Min cont-per-case"="greater-than-equal",
                                                                "Med cont-per-case"="minus",
                                                                "Mean cont-per-case"="minus",
                                                                "Max cont-per-case"="less-than-equal"
                                                              ),
                                                  color)
                            ),
                  tabPanel( "outcome","",  rendervalueBoxfromList(outcome,icon=list("","procedures","handshake","question"),color))
                #  tabPanel("occupation","",  rendervalueBoxfromList(occupation,icon="users",color))
              
                )# close tabBox
              }

  ###' * samples Panel*
              else if(panel=="samples"){
                icon= "vial"
                color = "purple"
                KPI<- as.list(
                     c(
                    "Total Samples"=nrow(data()),
                    table(data()$case_classification)#,
                  #  table(data()$samplepurpose),
                   # 
                    )
                  )
                tabBox(
                       id = "tabset1",
                       width = 12,
                       title = icon("vials"),
                       height = "250px",
                      tabPanel("classification","", rendervalueBoxfromList( KPI, icon,color)),
                      tabPanel("result","" ,rendervalueBoxfromList(
                        as.list(
                            c(
                              table(data()$pathogentestresult)
                              )
                            ),icon,color
                        )
                      )
                )
              }
                
              
        })
      }
    )
  }

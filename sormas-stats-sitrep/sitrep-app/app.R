# Shiny App with "Download Report" Button for Situation Report
# ui
ui <- fluidPage(downloadButton(outputId = "download",
                               label = "Download Report",
                               icon = shiny::icon("download"))
                )

#server
server <- function(input, output) {
  
  output$download = downloadHandler(filename = "Situation Report.docx",
  content = function(file){
    base::source("global.R")
    rmarkdown::render("SitRep.Rmd",
                      output_file = file)
  })
}
# app
shinyApp(ui = ui, server = server)
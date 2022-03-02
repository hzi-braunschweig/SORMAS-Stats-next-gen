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
    # progress bar
    shiny::withProgress(message = "Downloading..", value = 0,{
      
      shiny::incProgress(1/10)
      # computing output data used in sitrep
      base::source("global.R")
      
      shiny::incProgress(5/10)
      # rendering report
      rmarkdown::render("SitRep.Rmd",
                      output_file = file)
    })
  })
}
# app
shinyApp(ui = ui, server = server)

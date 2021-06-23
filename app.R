library(shiny)
library(shinydashboard)

components <- readRDS("S:/Kozos/Inkluziv_novekedesi_index/Adatok/components.rds")

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    shiny::tableOutput("table1")
  )
)

server <- function(input, output) { 
  
  output$table1 <- shiny::renderTable(
    components %>% 
      .[1:5, 1:5]
  )
  
  }



shinyApp(ui, server)
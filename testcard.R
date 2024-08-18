library(shiny)
library(bslib)
library(bsicons)

ui <- fluidPage(
  value_box(
    style = 'background-color: #DAA520!important;',
    title = "1st value",
    value = "123",
    showcase = bs_icon("bar-chart"),
    p("The 1st detail")
  ),
  value_box(
    title = "2nd value",
    value = "456",
    showcase = bs_icon("graph-up"),
    theme = "teal",
    p("The 2nd detail"),
    p("The 3rd detail")
  ),
  value_box(
    title = "3rd value",
    value = "789",
    showcase = bs_icon("pie-chart"),
    theme = "pink",
    p("The 4th detail"),
    p("The 5th detail"),
    p("The 6th detail")
  )
)

server <- function(input, output, session) {
  # server logic if needed
}

shinyApp(ui, server)

library(shiny)

ui <- fluidPage(
  ## Build what the person sees in the app here
  titlePanel("This is my first shiny app!")
)

server <- function(input, output) {
  ## Build how the app should react to the user/page here
}

shiny::shinyApp(ui=ui, server=server)
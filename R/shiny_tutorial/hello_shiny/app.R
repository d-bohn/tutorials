library(shiny)

# ui <- fluidPage(
#   ## Build what the person sees in the app here
#   titlePanel("This is my first shiny app!"),
#   h3("A brief history of the Penn State logo"),
#   h4("When I first started grad school, Penn State had this logo:"),
#   tags$div(
#     tags$img(src='http://onwardstate.com/wp-content/uploads/2014/11/63', width = 300),
#     style="text-align: center;"),
#   tags$br(),
#   p("According to Wikipedia, the logo above was commisioned in 1983. Fascinating."),
#   h4("Now, Penn State's new logo looks like this:"),
#   tags$div(
#     tags$img(src="https://i.pinimg.com/736x/fb/7d/23/fb7d236255db236434fcb29f800b2ce9--nittany-lion-sports-logos.jpg", width = 300),
#     style="text-align: center;"),
#   tags$br(),
#   p("The university commissioned this logo in 2015.")
# )

# ui <- fluidPage(
#   ## Build what the person sees in the app here
#   titlePanel("This is my first shiny app!"),
#   h3("Oh hey, a level 3 header exists now."),
#   p("Here is some text, but it is left aligned. Curses!"),
#   tags$div(
#     p("Centered text is make me feel better."),
#     style="text-align: center;"
#   )
# )

# ui <- fluidPage(
#   titlePanel("This is my first shiny app!"),
#   sidebarLayout(sidebarPanel(
#     textInput("text", label = h5("What is your name?"), placeholder = "Enter text..."),
#     actionButton('submit','submit')
#   ),
#                 mainPanel(),
#                 position = 'left',
#                 fluid = TRUE)
# )
# 
# server <- function(input, output){
#   ## Build how the app should react to the user/page here
# }

ui <- fluidPage(
  titlePanel("This is my first shiny app!"),
  sidebarLayout(sidebarPanel(
    textInput("text", label = h5("What is your name?"), placeholder = "Enter text..."),
    hr(),
    actionButton('submit','submit')),
    mainPanel(
      textOutput('name')
    ),
    position = 'left',
    fluid = TRUE)
)

## Server without an observer
# server <- function(input, output) {
#   ## Build how the app should react to the user/page here
#     output$name <- renderText({
#       paste0('Hello, ', input$text, '! Have a fantastic day!')
#     })
# }

server <- function(input, output) {
  ## Build how the app should react to the user/page here
  observeEvent(eventExpr = input$submit,handlerExpr = {
    output$name <- renderText({
      paste0('Hello, ', input$text, '! Have a fantastic day!')
    })
  })
}

shiny::shinyApp(ui=ui, server=server)
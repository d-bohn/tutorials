library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("File Upload and Scatter Plot"),
  tabsetPanel(
    tabPanel("Upload File",
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Choose CSV File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 
                 # added interface for uploading data from
                 # http://shiny.rstudio.com/gallery/file-upload.html
                 tags$br(),
                 checkboxInput('header', 'Header', TRUE),
                 radioButtons('sep', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ',')
               ),
               mainPanel(
                 tableOutput('contents')
               )
             )
    ),
    tabPanel("Scatter Plot",
             sidebarLayout(
               sidebarPanel(
              
                 selectInput('xcol', 'X Variable', ""),
                 selectInput('ycol', 'Y Variable', "", selected = "")
                 
               ),
               mainPanel(
                 plotOutput('MyPlot')
               )
             )
    )
    
  )
)

server <- function(input, output, session) {
  # added "session" because updateSelectInput requires it
  
  data <- reactive({ 
    req(input$file1) 
    
    inFile <- input$file1 

    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep)
  
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable', choices = names(df),
                      selected = names(df)[sapply(df, is.numeric)])
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable', choices = names(df),
                      selected = names(df)[sapply(df, is.numeric)])
    
    return(df)
  })
  
  output$contents <- renderTable({
    head(data(), 10)
  })
  
  output$MyPlot <- renderPlot({
    
    x <- data()[, input$xcol]
    y <- data()[, input$ycol]
    newdf <- as.data.frame(cbind(x,y))
    
    ggplot(newdf, aes(x, y)) +
      geom_point() +
      xlab(as.character(input$xcol)) +
      ylab(as.character(input$ycol)) +
      stat_smooth(method = 'lm') +
      theme_bw(base_size = 15)
    
  })
}

shinyApp(ui, server)
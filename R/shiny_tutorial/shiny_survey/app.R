# This Shiny app was taken heavily from:
# http://deanattali.com/2015/06/14/mimicking-google-form-shiny/
# and altered by Daniel Albohn, 08/21/17  

# Define some stuff in the global scope

fieldsMandatory <- c("title")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }
#error { color: red; }"

fieldsAll <- c('title', 'age', 'area', 'rate_this')
responsesDir <- file.path("responses")

epochTime <- function() {
  as.integer(Sys.time())
}

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

# The actual Shiny app

shinyApp(
  ## UI portion
  ui = fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    titlePanel("A Simple shiny Survey"),
    h4("Built using R and Shiny!"),
    
    ## The actual questions
    
    div(id = "form", # Display the questions
        textInput("title", labelMandatory("What is your title?"), "",
                  placeholder = "e.g., Grad, Faculty, etc."),
        
        numericInput("age", label = "What is your age?",
                     value = 0, min = 16, max = 100),
        
        textInput("area", label = "Program area" , "",
                  placeholder = "e.g., Clinical, Developmental, etc."),
        
        # Slider ratings for individual sessions
        sliderInput("rate_this", "How would you rate this tutorial?",
                    min = 1, max = 7, value = 0, ticks = TRUE),
        
        # Submit this and save
        actionButton("submit", "Submit", class = "btn-primary"),
        
        # Submit button and error handling
        shinyjs::hidden(
          span(id = "submit_msg", "Submitting..."),
          div(id = "error",
              div(br(), tags$b("Error: "), span(id = "error_msg"))
          )
        )
    ),
    shinyjs::hidden(
      div(
        id = "thankyou_msg",
        h3("Thanks, your response was submitted successfully!"),
        actionLink("submit_another", "Submit another response")
      )
    ) 
  ),
  
  ## Server portion
  server = function(input, output, session) {
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
      
      formData <- reactive({
        data <- sapply(fieldsAll, function(x) input[[x]])
        data <- c(data, timestamp = epochTime())
        data <- t(data)
        data
      })
      
      saveData <- function(data) {
        fileName <- sprintf("%s_%s.csv",
                            humanTime(),
                            digest::digest(data))
        
        write.csv(x = data, file = file.path(responsesDir, fileName),
                  row.names = FALSE, quote = TRUE)
      }
      
      # action to take when submit button is pressed
      observeEvent(input$submit, {
        observeEvent(input$submit, {
          shinyjs::disable("submit")
          shinyjs::show("submit_msg")
          shinyjs::hide("error")
          
          tryCatch({
            saveData(formData())
            shinyjs::reset("form")
            shinyjs::hide("form")
            shinyjs::show("thankyou_msg")
          },
          error = function(err) {
            shinyjs::html("error_msg", err$message)
            shinyjs::show(id = "error", anim = TRUE, animType = "fade")
          },
          finally = {
            shinyjs::enable("submit")
            shinyjs::hide("submit_msg")
          })
        })
      })
      
      # action to take when submit button is pressed
      observeEvent(input$submit, {
        saveData(formData())
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::show("thankyou_msg")
      })
      
      observeEvent(input$submit_another, {
        shinyjs::show("form")
        shinyjs::hide("thankyou_msg")
      })
    })
    
  }
)
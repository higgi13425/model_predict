library(shiny)
library(shinythemes)

# Define UI 
ui <- shinyUI(fluidPage(
  theme = shinytheme("united"),
  
  # Application title
  titlePanel("Health Insurance Annual Expenses Predictor"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = 'sex', 
                  label = "Select Gender",
                  choices = c("male", "female"),
                  selected = "female"),
      
      numericInput(
        inputId = 'age',
        label = 'Age ',
        30,
        min = 1
      ),
      
      numericInput(inputId = 'bmi', 
          label = 'BMI (Body Mass Index in kg/m2)', 20),
      
      actionButton('predict', "Predict", icon = icon("refresh"))
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
      tabPanel(
        icon = icon("table"),
        "Prediction:",
        tableOutput('table'),
        tags$h3("Annual Medical Expenses in dollars", icon("dollar")),
        tags$h4(textOutput('insurance'))
      ),
      
      tabPanel(
        icon = icon("book"),
        title = "About",
        tags$br(), # inserts line break
        tags$div(
          "This application predicts annual Medical care expenses
          using dynamic input from the user. This prediction uses the following variables:",
          tags$br(), # inserts line break
          tags$br(),
          tags$ul( # starts list
            tags$li( # list item
              "age : An integer indicating the age of the insured.",
              tags$li("sex : The insured person's gender, either male or female"),
              tags$li(
                "bmi : The body mass index (BMI), which provides a sense of how over- or
                under-weight a person is relative to their height. BMI is equal to weight (inkilograms)
                divided by height (in meters) squared. An ideal BMI is within the range of 18.5 to 24.9."
              )
            )
          )
        ),
        tags$br(),
        "The application is built with the" ,
        tags$a("Shiny", href = "https://shiny.rstudio.com/"),
        " framework for the R programming language. Multiple Linear Regression was used to fit the model.",
        tags$br(),
        tags$br(),
        tags$p(
          "Code for the application is available at (github link here). I welcome feedback and suggestions!"),
        
        tags$div()
      )
    ))
  )
))

# Define server logic required to model and predict
server <- shinyServer(function(input, output) {
  # test and validate input types
  test <- reactive({
    age <- as.integer(input$age)
    sex <- as.character(input$sex)
    bmi <- as.integer(input$bmi)
    
    fest <- cbind(age, sex, bmi)
    
    test <-
      cbind.data.frame(age, sex, bmi)
    test <- as.data.frame(test)
    
  })
  

  pred <- eventReactive(input$predict, {
    charges <-  input$age*373.5 + input$bmi*14.7 + 73*(input$sex == "male")
  })
  output$insurance <- renderText(pred())
  output$table <- renderTable(test())
})

# Run the application 
shinyApp(ui = ui, server = server)

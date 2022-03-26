library(shiny)
library(shinythemes)

# Define UI  (User Interface)
ui <- shinyUI(fluidPage(
  theme = shinytheme("spacelab"),
  
  # Application title
  titlePanel("Health Insurance Annual Expenses Predictor"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = 'sex', 
                  label = "Select Gender",
            choices = c("male", "female", "other"),
                  selected = "female"),
      
      sliderInput(
        inputId = 'age',
        label = 'Age ',
        value = 30,
        min = 1,
        max = 115
      ),
      
      sliderInput(inputId = 'bmi', 
          label = 'BMI (Body Mass Index in kg/m2)', 
          value = 20,
          min = 10,
          max = 80),
      
      actionButton('predict', "Predict", icon = icon("briefcase-medical"))
      
    ),
    
    # Show a table of the input values
    mainPanel(tabsetPanel(
      tabPanel(
        icon = icon("table"),
        "Prediction:",
        tags$br(), # inserts line break
        tableOutput('table'),
        tags$h5("Adjust the three input variables in the left sidebar and"),
      tags$h5("Click on the Predict Button in the left sidebar to make a prediction"),
        tags$h3("Annual Medical Expenses in dollars", icon("dollar-sign")),
        tags$h4(textOutput('insurance'))
      ),
      
      tabPanel(
        icon = icon("book"),
        title = "About",
        tags$br(), # inserts line break
        tags$div( # div creates a new section of an HTML doc
       "This application predicts hypothetical annual Medical care expenses using dynamic input from the user. This prediction uses the following variables:",
          tags$br(), # inserts line break
          tags$br(),
          tags$ul( # starts an unordered list (ul)
            tags$li( # list item
              "age : An integer indicating the age of the insured.",
              tags$li("sex : The insured person's gender, either male or female or other"),
              tags$li(
                "bmi : The body mass index (BMI), which is equal to weight (in kilograms)
                divided by height (in meters) squared."
              )
            )
          )
        ),
        tags$br(),
        "The application is built with the" ,
        tags$a("Shiny", href = "https://shiny.rstudio.com/"),
        " framework for the R programming language. Multiple Linear Regression was used to fit the model.", 
       # the a tag creates a link to a webpage
        tags$br(),
        tags$br(),
        "The code for the application is available at this" ,
       tags$a("GitHub site.", href = "https://github.com/higgi13425/model_predict"),
       " Check it out!",
       tags$br(),
       tags$br(),
       "The icons are from the selection of 2009 free icons at ",
       tags$a("FontAwesome", href = "https://fontawesome.com/search?m=free"),
        tags$div() # close the div
      )
    ))
  )
))

# Define server logic required to model and predict
server <- shinyServer(function(input, output) {
  # test and validate input types
  # less necessary with sliderInput and dropdown
  # more important for text or numeric open data entry
  test <- reactive({
    age <- as.integer(input$age)
    sex <- as.character(input$sex)
    bmi <- as.integer(input$bmi)
    
    fest <- cbind(age, sex, bmi)
    
    test <-
      cbind.data.frame(age, sex, bmi)
    test <- as.data.frame(test)
    
  })
  
# make prediction
  pred <- eventReactive(input$predict, {
    charges <-  input$age*373.5 + 
      input$bmi*14.7 + 
      173*(input$sex == "male") + 
      83*(input$sex == "female")
  })
  # render output for the UI (User Interface)
  output$insurance <- renderText(pred())
  output$table <- renderTable(test())
})

# Run the application 
shinyApp(ui = ui, server = server)

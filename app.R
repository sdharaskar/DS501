#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Stroke Predictor - Logistic Regression"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("age",
                        "Age in years:",
                        min = 10,
                        max = 100,
                        value = 30),
            
            radioButtons("Hypertension",
                         label = strong("Suffer from Hypertension ?"),
                         choices = list("Yes" = 1, "No" = 0),
                         selected = 0,
                         inline = TRUE),
            
            radioButtons("HeartDisease",
                         label = strong("Suffer from Heart Disease ?"),
                         choices = list("Yes" = 1, "No" = 0),
                         selected = 0,
                         inline = TRUE),   
            
            numericInput("avgGlucLvl", "Average Glucose Level:", 10,
                         min = 1, max = 500, width = '400px')
            
            #numericInput(inputId, label, value, min = NA, max = NA, step = NA,
                         #width = NULL)
      
                    ), # end sidebar

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("predictTable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$predictTable <- renderTable({
        # load libraries
        library(readr)
        # read the data
        stroke <- read_csv("stroke.csv")
        head(stroke)
        strokeData = stroke[,2:12]
        head(strokeData)

        # display table of the prediction
        strokeLogregModel = glm(stroke ~ age + hypertension + heart_disease + avg_glucose_level, 
                           data=strokeData, 
                           family=binomial(link="logit"))
        
        newData <- data.frame(age = input$age, 
                              hypertension = as.numeric(input$Hypertension), 
                              heart_disease = as.numeric(input$HeartDisease),
                              avg_glucose_level = input$avgGlucLvl)
        
        strokeProb <- predict(strokeLogregModel,
                          newData,
                          type="response")
        
        strokeProb
        
        predicted.stroke <- ifelse(strokeProb > 0.3, "Stroke!", "No Stroke!!")

    })
}

# Run the application 
shinyApp(ui = ui, server = server)

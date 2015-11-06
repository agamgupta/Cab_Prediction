# SHINY APP
#install.packages("shiny")
data_res <- read.csv("data/Kaggle_results_with_atts.csv")
library(shiny)
ui <- navbarPage("",
                 tabPanel("Home",
                          titlePanel(title = em(h2("Predict whether a cab booking will get cancelled", align = "center"))),
                            br(),
                            p(h3("The Problem", color = "grey")),
                            p("The business problem tackled here is trying to improve customer service for YourCabs.com, a cab company in Bangalore. The problem of interest is booking cancellations by the company due to unavailability of a car, dependent on multiple factors as demonstrated in a given dataset. The challenge is that cancellations can occur very close to the trip start time, thereby causing passengers inconvenience."),
                            p(h3("The Goal")),
                            br(),
                            p("The goal is to create a predictive model for classifying new bookings as to whether they will eventually get cancelled due to car unavailability.  This would help the cab company to assure that most of its customers get good service. Thus intrinsically the goal was to deploy a model, which enhances the company’s customer satisfaction level, thereby promoting overall growth."),                       
                            p(h3("My Approach")),
                            p("To tackle the problem professionally, I applied the CRISP-DM methodology.It consists of the following steps: "),
                            tags$ol(
                              tags$li(strong("Business Understanding"), "- Understanding the primary and secondary business objectives of the client."), 
                              tags$li(strong("Data Understanding"), " - Understanding the distribution of the data, trying to look for any overarching patterns."),
                              tags$li(strong("Data Preparation"), " - Cleaning the data so that it makes sense to use it to create a model. This includes removing null values, outliers, and grouping data."), 
                              tags$li(strong("Modeling")," - Choosing an appropriate modeling approach and creating a model based on the relevant factors."), 
                              tags$li(strong("Evaluation"), " - Running the model on the data and evaluating its performance. Seeing if it meets the desired efficiency requirements."),
                              tags$li(strong("Deployment"), " - Presenting the model and its insights for others to utilize.")),
                            p(h3("Conclusion")), 
                            p("Thus using the CRISP-DM methodology, we follow a multistep iterative cycle. We begin with understanding the business requirements and briefly looking over the data. Then we prepare the data to be used in modeling, choose and create an appropriate model, and evaluate its efficiency. In the end, we deploy the model in the form of an application."),
                            p("Following this approach I got a model, with", strong("92.5% efficiency"),", successfully meeting the efficiency requirements of the business.  The application deployed allows the YourCabs company executives to predict on-the-spot whether a new cab booking will be cancelled or not. This allows them to appropriately take booking, which would be retained, thereby minimizing last minute cancellations and the subsequent fall in customer satisfaction.")
                          ), 
                 
                 tabPanel("Statistics",
                          p(h3("Summary of the dataset")), 
                          p("The training dataset, a table consisting of over 43000 bookings, including various parameters such as the date of booking, travel type, from area, to area, along with whether it was cancelled or not, is provided. After treating the data for missing values, outliers, choosing sensible attributes and grouping largely unique columns, we create a refined data set. This data set is used for creating a model. It's summarised below, highlighting the",span(strong("mean, median, min, max, 1st and 3rd quartiles of each parameter"))," in the dataset."), 
                          br(),
                          p(img(src = "statspage.png", align = "center", width = '100%')),
                          br(),
                          br(),
                          hr(),
                          p(h3("Reslationship between cabs cancellation and a given parameter.")),
                          p(em(h4("Choose a feature below to see how it relates to the cancellation."))),
                          br(),
                          fluidRow(
                            column(4, offset = 4, 
                                   wellPanel(
                                     selectInput(inputId = 'indvar1', label = 'Feature 1', choices = c("id" = 1,"vehicle_model_id" = 3, "travel_type_id" = 5,"from_date" =10,"online booking"=12,"mobile_site_booking" =13,"from_lat"=15,"from_long"=16)),
                                     submitButton(text = "Enter"))
                                   )
                            ),
                          fluidRow(
                            column(6,plotOutput("hist1")),
                            column(6,plotOutput("hist2"))
                           )
                        ),
               
                 tabPanel("Predict",
                          p(h3("Make a live prediction")), 
                          em("Enter the following parameters and see whether this booking will get cancelled or not."),
                          p(),
                          fluidRow(
                            column(4,
                                   wellPanel(
                                     numericInput(inputId = "uid",label = "User_id", value = "22179")
                                     )
                                   ),
                            column(4,
                                   wellPanel(
                                     numericInput(inputId = "vmid",label = "Vehicle_model_id", value = "12")
                                   )
                                  ),
                            column(4,
                                   wellPanel(
                                     selectInput(inputId = "ttype", label = "Travel Type:", c("Long Distance (1)","Point to Point (2)", "Hourly Rental (3)"), selectize = FALSE)
                                   )
                                )
                            ),
                          fluidRow(
                            column(4,
                                   wellPanel(numericInput(inputId = "farid",label="from_area_id", value = "1096"))),
                            column(4,
                                  wellPanel(selectInput(inputId = "mob", label ="Mode of Booking", choices = c("Online Booking", "Mobile Booking", "Call Center Booking"), selectize = FALSE))),
                            column(4,
                                  wellPanel(dateInput(inputId = "fdt", label = "Trip start date", value = "2004-01-01")))
                          ),
                          p(),
                          fluidRow(
                            column(4, offset = 6,
                                   submitButton(text = "Predict"))
                          ),
                          hr(),
                          fluidRow(
                            column(12,
                                   wellPanel(textOutput("prediction")))
                          )
                 ),
                 
              collapsible = TRUE)

server <- function(input, output) {
  
  output$prediction <- renderText({
    cornc <- data_res[data_res$user_id == input$uid,]
    ccornc <- as.character(cornc$Car_Cancellation)
    res <- switch(ccornc, 
                  '0' = " a high chance that this booking will not get cancelled :)",
                  '1' = " a high chance that this booking will get cancelled! :(")
    paste("There is  ", res)
    })

  output$hist1 <- renderPlot({
    xvar1 <- as.numeric(input$indvar1)
    hist(data_res[which(data_res[,21] == 0),xvar1], xlab = names(data_res[xvar1]), main = "Car's Not Cancelled", col = "green")
    })  
 
  output$hist2 <- renderPlot({
   xvar1 <- as.numeric(input$indvar1)
   hist(data_res[which(data_res[,21] == 1),xvar1], xlab = names(data_res[xvar1]), main = "Car's Cancelled", col = "red")
 }) 
}

shinyApp(ui = ui, server = server)

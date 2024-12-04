library(shiny)
library(shinyjs)
library(dplyr)

# loading in the complete dataset
complete_dataset = read.csv("vehicle_prices_nafilled.csv")


#this allows for us to use the ranges on how many people can use the car
#this uses that any single person can buy any type of car
vehicle_sizes <- data.frame(
  vehicle_class = c(
    "Compact", "Sport utility vehicle: Small", "Minicompact", "Two-seater",
    "Subcompact", "Mid-size", "Full-size", "Sport utility vehicle: Standard",
    "Pickup truck: Small", "Van: Passenger", "Pickup truck: Standard",
    "Station wagon: Small", "Minivan", "Station wagon: Mid-size",
    "Special purpose vehicle"
  ),
  min_family_size = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  max_family_size = c(4, 5, 2, 2, 4, 5, 5, 7, 5, 10, 6, 5, 8, 7, 5)
)

# Define UI
ui <- fluidPage(
  titlePanel("What Type of Car are you Looking for Today?"),
    mainPanel(
      uiOutput("mainPage")  # Dynamically update content here
    )
  )

# Define Server
server <- function(input, output, session) {
  
  # Reactive value to track the current page
  current_page <- reactiveVal("home")
  
  # Output for main page
  # Output for main page
  output$mainPage <- renderUI({
    if (current_page() == "home") {
      # Front page with buttons
      tagList(
        h2("Choose an Option"),
        actionButton("option1", "New"),
        actionButton("option2", "Used")
      )
    } else if (current_page() == "page1") {
      sidebarLayout(
        sidebarPanel(
          style = "width: 400px",
          sliderInput("priceRange", "Select Price Range:",
                      min = min(complete_dataset$avg_used_price), 
                      max = max(complete_dataset$avg_used_price),
                      value = c(min(complete_dataset$avg_used_price), max(complete_dataset$avg_used_price))),
          sliderInput("yearlyDistance", "Enter how much you drive in a year",
                      min = 5000,
                      max = 100000,
                      value = 5000),
          selectInput("familySize",  "Select how many people are in your family unit",
                      choices = as.list(seq(1,10)), selected = 1),
            selectInput("vehicle_class", "Select Vehicle Class:",
                        choices = NULL), # Initialized as NULL, updated dynamically
          selectInput("Fuel_type", "Select Fuel Type:",
                      choices = c("All", unique(complete_dataset$fuel_type_choice))),
          selectInput("transmission", "Select Transmission Type:",
                      choices = c("All", unique(complete_dataset$transmission_type))),      
          actionButton("filter", "Get Recommendations"),
          actionButton("back", "Go Back"),
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Recommendations", 
                     dataTableOutput("recommendations")),
            tabPanel("Visualization", 
                     plotOutput("vehiclePlot"))
          )
        )
      )
    } else if (current_page() == "page2") {
      sidebarLayout(
        sidebarPanel(
          style = "width: 400px",
          sliderInput("priceRange", "Select Price Range:",
                      min = min(complete_dataset$avg_used_price), 
                      max = max(complete_dataset$avg_used_price),
                      value = c(min(complete_dataset$avg_used_price), max(complete_dataset$avg_used_price))),
          sliderInput("mileageRange", "Select Used Car Mileage Range:",
                      min = min(complete_dataset$avg_mileage), 
                      max = max(complete_dataset$avg_mileage),
                      value = c(min(complete_dataset$avg_mileage), max(complete_dataset$avg_mileage))),
          sliderInput("yearlyDistance", "Enter how much you drive in a year",
                      min = 5000,
                      max = 100000,
                      value = 5000),
          selectInput("familySize",  "Select how many people are in you family unit",
                      choices = as.list(seq(1,10)), selected = 1),
          selectInput("vehicle_class", "Select the type of vehicle you're interested in:",
                      choices = NULL),
          selectInput("Fuel_type", "Select Fuel Type:",
                      choices = c("All", unique(complete_dataset$fuel_type_choice))),
          selectInput("transmission", "Select Transmission Type:",
                      choices = c("All", unique(complete_dataset$transmission_type))),      
          actionButton("filter", "Get Recommendations"),
          actionButton("back", "Go Back")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Recommendations", 
                     dataTableOutput("recommendations")),
            tabPanel("Visualization", 
                     plotOutput("vehiclePlot"))
          )
        )
      )
    }
    
  })
  
  observeEvent(input$familySize, {
    if (current_page() %in% c("page1", "page2")) {
      valid_classes <- vehicle_sizes$vehicle_class[
        vehicle_sizes$min_family_size <= as.numeric(input$familySize) &
          vehicle_sizes$max_family_size >= as.numeric(input$familySize)
      ]
      if (length(valid_classes) == 0) valid_classes <- c("No suitable vehicles")
      updateSelectInput(session, "vehicle_class",
                        choices = valid_classes,
                        selected = valid_classes[1])
    }
  })

  
  observeEvent(input$filter, {
    if (current_page() %in% c("page1", "page2")) {  # Filtering logic for "New" cars
      filtered_data <- complete_dataset %>%
        filter(
          avg_used_price >= input$priceRange[1] & avg_used_price <= input$priceRange[2],
          fuel_type_choice %in% ifelse(input$Fuel_type == "All", unique(fuel_type_choice), input$Fuel_type),
          transmission_type %in% ifelse(input$transmission == "All", unique(transmission_type), input$transmission),
          vehicle_class %in% input$vehicle_class,
          (year %in% 2024 | year %in% NA),
          (month %in% 10 | month %in% NA)
        )
    }
    
    # Debugging: Inspect the filtered data
    print(paste("Number of rows in filtered data:", nrow(filtered_data)))

    # Display recommendations
    output$recommendations <- renderDataTable({
      if (nrow(filtered_data) == 0) {
        data.frame(Message = "No matching vehicles found")
      } else {
        filtered_data %>% 
          select(-transmission_type, -fuel_type_choice, -smog_rating,
                 -avg_fuel_price_yr, -avg_fuel_type_price) %>% 
          arrange(avg_used_price, model_year, make, model)
      }
    })
  })
  
  
  

  # Update current page based on button clicks
  observeEvent(input$option1, { current_page("page1") })
  observeEvent(input$option2, { current_page("page2") })
  observeEvent(input$back, { current_page("home") })
}

# Run the app
shinyApp(ui, server)

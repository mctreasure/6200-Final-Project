# loading required libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
library(zip)

# Creating dataframe and calling loading datasets
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

complete_dataset = read.csv("vehicle_prices_nafilled.csv")


# User Interface Setup
ui = dashboardPage(
  dashboardHeader(title = "Car App Recommender"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Demo Visualisations", tabName = "visualisations", icon = icon("chart-bar")),
      menuItem("New Cars", tabName = "new_cars", icon = icon("car")),
      menuItem("Used Cars", tabName = "used_cars", icon = icon("car-side")),
      menuItem("Download Recommendations", tabName = "recommendations", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "visualisations",
        h2("Demo Visualisations Below. To Explore Data, Select Used or New Cars"),
        uiOutput("dynamicVisualisations")
      ),
      tabItem(
        tabName = "new_cars",
        h2("Explore New Cars"),
        uiOutput("dynamicNewCars")
      ),
      tabItem(
        tabName = "used_cars",
        h2("Explore Used Cars"),
        uiOutput("dynamicUsedCars")
      ),
      tabItem(
        tabName = "recommendations",
        h2("Your Recommendations"),
        downloadButton("downloadData", "Download Recommendations"),
        uiOutput("dynamicRecommendations")
      )
    )
  )
)



# Server Logic
server = function(input, output, session){
  
  # Reactive value to track the current page
  current_page <- reactiveVal("visualisations")

  # Reactive variable to store filtered data
  filtered_data <- reactiveVal(NULL)
  
  # Reactive variable to store filtered data
  filtered_used_data <- reactiveVal(NULL)
  
  
  # Update current_page based on user's tab selection
  observe({
    current_page(input$tabs) # Assumes your `sidebarMenu` has an ID "tabs"
  })
  

  # Render UI based on user selection
  output$dynamicVisualisations = renderUI({
      fluidRow(
        column(6, style = "height: 600px;", plotlyOutput("visualisationplot1")),
        column(6, style = "height: 600px;", plotlyOutput("visualisationplot2")),
        column(6, style = "height: 600px;", plotlyOutput("visualisationplot3")),
        column(6, style = "height: 600px;", plotlyOutput("visualisationplot4"))
      )
    })
  
  
  
  output$visualisationplot1 <- renderPlotly({
    complete_dataset %>%
      filter(vehicle_class %in% c("Compact", "Full-size", "Mid-size", "Minivan", 
                                  "Pickup truck: Standard", "Special purpose vehicle", "Sport utility vehicle: Standard",
                                  "Station wagon: Mid-size", "Two-seater")) %>% 
      group_by(vehicle_class) %>%
      summarise(avg_co2 = mean(co2_emissions_g_km, na.rm = TRUE)) %>%
      arrange(avg_co2) %>% 
      plot_ly(
        x = ~reorder(vehicle_class, avg_co2), 
        y = ~avg_co2,
        color = ~vehicle_class,
        type = "bar",
        name = ~vehicle_class
      ) %>% 
      layout(
        title = list(text = "Average CO2 Emissions by Vehicle Class", font = list(color = "#f8f9fa")),
        xaxis = list(title = "Vehicle Class", tickangle = -20, color = "#f8f9fa"),
        yaxis = list(title = "Average CO2 Emissions (g/km)", color = "#f8f9fa"),
        plot_bgcolor = "#343a40",  # Match darkly background
        paper_bgcolor = "#343a40",
        font = list(color = "#f8f9fa"), # Match text color
        height = 600
      )
  })
  
  
  # Combined dashboard plot for New and Used Vehicle Prices
  output$visualisationplot2 <- renderPlotly({
    complete_dataset %>% 
      filter(!(make %in%  c("Aston Martin", "Lamborghini", "Rolls-Royce", "Bentley"))) %>%
      group_by(make) %>% 
      summarise(
        med_new_price = median(avg_new_price, na.rm = TRUE),
        med_used_price = median(avg_used_price, na.rm = TRUE)
      ) %>% 
      arrange(desc(med_new_price)) %>%
      plot_ly() %>% 
      add_trace(
        x = ~reorder(make, -med_new_price),
        y = ~med_new_price,
        type = "bar",
        name = "Median New Price($)",
        marker = list(color = "purple")
      ) %>% 
      add_trace(
         x = ~reorder(make, -med_new_price),
        y = ~med_used_price,
        type = "bar",
        name = "Median Used Price ($)",
        marker = list(color = "lightgreen")
      ) %>% 
      layout(
        title = list(text = "Market Price of Vehicles Based on Make", font = list(color = "#f8f9fa")),
        xaxis = list(title = "Make", color = "#f8f9fa", tickangle= -66),
        yaxis = list(title = "Median Price ($)", color = "#f8f9fa"),
        barmode = "group",
        plot_bgcolor = "#343a40",  # Match darkly background
        paper_bgcolor = "#343a40",
        font = list(color = "#f8f9fa"),  # Match text color
        height = 600
      )# Group bars for comparison
  })  
  
  
  #dashboard plot for trends in Vehicle Prices
  output$visualisationplot3 <- renderPlotly({
    complete_dataset %>%
      group_by(model_year) %>%
      summarise(
        med_new_price = median(avg_new_price, na.rm = TRUE),
        med_used_price = median(avg_used_price, na.rm = TRUE)
      ) %>%
      plot_ly() %>%
      add_trace(
        x = ~model_year,
        y = ~med_new_price,
        type = "scatter",
        mode = "lines+markers",
        name = "Average New Price ($)",
        line = list(color = "purple"),
        marker = list(color = "purple")
      ) %>%
      add_trace(
        x = ~model_year,
        y = ~med_used_price,
        type = "scatter",
        mode = "lines+markers",
        name = "Average Used Price ($)",
        line = list(color = "lightgreen"),
        marker = list(color = "lightgreen")
      ) %>%
      layout(
        title = list(text = "Trends in Vehicle Prices Over Time", font = list(color = "#f8f9fa")),
        xaxis = list(title = "Model Year", color = "#f8f9fa"),
        yaxis = list(title = "Median Price ($)", color = "#f8f9fa"),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2),
        plot_bgcolor = "#343a40",  # Match darkly background
        paper_bgcolor = "#343a40",
        font = list(color = "#f8f9fa"),
        height = 600
      )
  })
  
  output$visualisationplot4 <- renderPlotly({
    
    summarized_data <- complete_dataset %>%
      mutate(year = as.numeric(year)) %>% 
      group_by(year, fuel_type) %>%
      summarise(
        avg_price = mean(avg_fuel_price_yr, na.rm = TRUE)  # Replace with the actual column for fuel price
      ) 
    
    plot_ly() %>%
      add_trace(
        data = summarized_data %>% filter(fuel_type == "diesel"),
        x = ~year,
        y = ~avg_price,
        name = "Diesel",
        type = "scatter",
        mode = "lines+markers",
        line = list(width = 2, color = "#1f77b4"),
        marker = list(size = 6)
      ) %>%
      add_trace(
        data = summarized_data %>% filter(fuel_type == "regular"),
        x = ~year,
        y = ~avg_price,
        name = "Regular",
        type = "scatter",
        mode = "lines+markers",
        line = list(width = 2, color = "#ff7f0e"),
        marker = list(size = 6)
      ) %>%
      add_trace(
        data = summarized_data %>% filter(fuel_type == "premium"),
        x = ~year,
        y = ~avg_price,
        name = "Premium",
        type = "scatter",
        mode = "lines+markers",
        line = list(width = 2, color = "#2ca02c"),  # Unique color for Premium
        marker = list(size = 6)
      ) %>%
      layout(
        title = list(
          text = "Trends in Fuel Prices over Time by Fuel Type",
          font = list(color = "#f8f9fa")
        ),
        xaxis = list(
          title = "Year",
          color = "#f8f9fa"
        ),
        yaxis = list(
          title = "Average Fuel Price ($)",
          color = "#f8f9fa"
        ),
        legend = list(
          title = list(text = "Fuel Type"),
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.2
        ),
        plot_bgcolor = "#343a40",
        paper_bgcolor = "#343a40",
        font = list(color = "#f8f9fa"),
        height = 600
      )
  })
  
  
  observeEvent(input$familySize, {
    valid_classes <- vehicle_sizes$vehicle_class[
      vehicle_sizes$min_family_size <= as.numeric(input$familySize) &
        vehicle_sizes$max_family_size >= as.numeric(input$familySize)
    ]

    if (length(valid_classes) == 0) valid_classes <- c("No suitable vehicles")

    # Update the selectInput only for the active tab
    if (current_page() == "new_cars") {
      updateSelectInput(session, "vehicle_class",
                        choices = valid_classes,
                        selected = valid_classes[1])
    } else if (current_page() == "used_cars") {
      updateSelectInput(session, "used_vehicle_class",
                        choices = valid_classes,
                        selected = valid_classes[1])
    }
  })
  

  
  output$dynamicNewCars = renderUI({
    sidebarLayout(
      sidebarPanel(
        sliderInput("priceRange", "Select Price Range:",
                    min = min(complete_dataset$avg_used_price), 
                    max = max(complete_dataset$avg_used_price),
                    value = c(min(complete_dataset$avg_used_price), max(complete_dataset$avg_used_price))),
        sliderInput("yearlyDistance", "What is your estimated annual kilometres?",
                    min = 5000,
                    max = 100000,
                    value = 5000),
        selectInput("familySize",  "What is your family size?",
                    # choices = as.list(seq(1,10)), 
                    choices = c("Select Family Size" = "", as.character(1:10)),
                    selected = NULL, 
                    selectize = TRUE),
        selectInput("vehicle_class", "Vehicle Type: select all that apply",
                    choices = NULL, 
                    selected = NULL,
                    multiple = TRUE),
        selectInput("Fuel_type", "Select Fuel Type:",
                    choices = c("All", unique(complete_dataset$fuel_type_choice)),
                    selected = "All",
                    multiple = TRUE),
        selectInput("transmission", "Select Transmission Type:",
                    choices = c("All", unique(complete_dataset$transmission_type))),
        selectInput("new_car_concern", "What is your primary concern?",
                    choices = c("Price", "CO2 Rating", "Fuel Efficiency"),
                    selected = "CO2 Rating"),
        actionButton("new_car_filter", "Get Recommendations")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Recommendations", 
                   dataTableOutput("recommendations")),
          tabPanel("Price", 
                   plotlyOutput("pricePlot")),
          tabPanel("Fuel Efficiency", 
                   plotlyOutput("fuelEfficieny")),
          tabPanel("CO2 Emissions", 
                   plotlyOutput("co2Plot"))
        )
      )
    )
  })
  
  
  observeEvent(input$new_car_filter, {
    if (is.null(input$vehicle_class) || length(input$vehicle_class) == 0) {
      showNotification("Please enter your family size and select at least one vehicle class before clicking 'Get Recommendations'.", type = "error")
      return()  # Exit the observeEvent without executing the rest
    }
    
    
    filtered_new = complete_dataset %>% 
      filter(
        model_year >= 2022,
        avg_mileage <= 100,
        avg_new_price > 0,
        avg_new_price >= input$priceRange[1],
        avg_new_price <= input$priceRange[2],
        (input$Fuel_type == "All" | fuel_type_choice == input$Fuel_type),
        (input$transmission == "All" | transmission_type == input$transmission),
        (vehicle_class == input$vehicle_class | input$vehicle_class == "No suitable vehicles")
      ) %>%
      mutate(avg_yearly_fuel_cost = round(
        case_when(
          fuel_type_choice == "gas/diesel" ~ (input$yearlyDistance * (combined_l_100_km / 100) * (avg_fuel_type_price / 100)),
          fuel_type_choice == "Electric" ~ (input$yearlyDistance * 0.42 * (combined_kwh_100_km / 100)),
          fuel_type_choice == "Hybrid" ~ (((input$yearlyDistance * 0.42 * (combined_kwh_100_km / 100)) + (input$yearlyDistance * (combined_l_100_km / 100) * (avg_fuel_type_price / 100))) / 2),
          TRUE ~ NA_real_
        )
      ),
      avg_new_price = round(avg_new_price))%>% 
      distinct(make, .keep_all = TRUE) %>% 
      arrange(
        case_when(
          input$new_car_concern == "Price" ~ avg_new_price,
          input$new_car_concern == "CO2 Rating" ~ co2_emissions_g_km,
          input$new_car_concern == "Fuel Efficiency" ~ avg_yearly_fuel_cost
        )) %>% 
      head(5)
    
    filtered_data(filtered_new)
    
    # Displaying recommended vehicles in tables
    output$recommendations = renderDataTable({
      data = filtered_data()
      req(data)
      if (nrow(data) > 0){
        data %>% 
          select(avg_new_price, model_year, make, model, vehicle_class, transmission_type, fuel_type, avg_mileage, avg_yearly_fuel_cost, co2_emissions_g_km, co2_rating) %>% 
          rename(
            "Price ($)" = avg_new_price,
            "Model Year" = model_year,
            "Make" = make,
            "Model" = model,
            "Vehicle Class" = vehicle_class,
            "Transmission" = transmission_type,
            "Fuel Type" = fuel_type,
            "Mileage (km)" = avg_mileage,
            "Yearly Fuel Cost ($)" = avg_yearly_fuel_cost,
            "CO2 Emissions (g/km)" = co2_emissions_g_km,
            "CO2 Rating" = co2_rating
          ) 
      } else {
        data.frame(Message = "No Vehicles match your criteria")
      }
    })
    
    
    # Visualizing prices of recommended new vehicles
    output$pricePlot = renderPlotly({
      data = filtered_data()
      req(data)
      
      priceplot <- data %>%  # Replace column names as needed
        select(make, model, avg_new_price) %>%
        arrange(avg_new_price) %>% 
        pivot_longer(cols = c(avg_new_price), 
                     names_to = "price_type", 
                     values_to = "price") %>%
        plot_ly(
          x = ~reorder(make, price),
          y = ~price,
          color = ~price_type,
          type = "bar",
          text = ~paste0(model," - ", "Price: $", round(price, 2)),
          hoverinfo = "text"
        ) %>%
        layout(
          title = list(text = "New Prices of Recommended Vehicles", font = list(color = "#f8f9fa")),
          xaxis = list(title = "Vehicle Name", tickangle = -45, color = "#f8f9fa"),
          yaxis = list(title = "Price ($)", color = "#f8f9fa"),
          legend = list(title = list(text = "Price Type"), orientation = "h", x = 0.5, xanchor = "center", y = -0.2),
          plot_bgcolor = "#343a40",  # Match darkly background
          paper_bgcolor = "#343a40",
          font = list(color = "#f8f9fa"), # Match text color
          height = 600
        )
      priceplot
    })
    
    plot_newCar_data <- eventReactive(input$new_car_filter, {
      gas_prices <- complete_dataset %>%
        select(year, avg_fuel_price_yr) %>%
        distinct(year, avg_fuel_price_yr) %>% 
        arrange(year)
      
      filtered_data() %>% 
        select(model, combined_l_100_km, combined_kwh_100_km, fuel_type_choice) %>% 
        tidyr::expand_grid(year = gas_prices$year) %>% 
        left_join(gas_prices, by = "year") %>% 
        mutate(
          avg_yearly_fuel_cost = case_when(
            fuel_type_choice == "gas/diesel" ~ input$yearlyDistance * (combined_l_100_km / 100) * (avg_fuel_price_yr / 100),
            fuel_type_choice == "Electric" ~ input$yearlyDistance * 0.43 * (combined_kwh_100_km / 100),
            fuel_type_choice == "Hybrid" ~ ((input$yearlyDistance * (combined_l_100_km / 100) * (avg_fuel_price_yr / 100)) +
                                              (input$yearlyDistance * 0.43 * (combined_kwh_100_km / 100))) / 2,
            TRUE ~ NA_real_
          )
        ) %>% 
        filter(year >= 2022)
    })
    
    
    output$fuelEfficieny <- renderPlotly({
      new_car_plot = plot_newCar_data() %>% 
        ggplot(aes(x = year, y = avg_yearly_fuel_cost, color = model)) +
        geom_line() +  # Trend lines
        geom_point() + # Points for each data entry
        labs(
          title = "Fuel Efficiency Costs (2022–2024)",
          x = "Year",
          y = "Estimated Fuel Cost ($CAN)",
          color = "Car Model"
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)
        )
      
      ggplotly(new_car_plot, height = 600)
    })
    
    
    # Add the new CO2 plot output
    output$co2Plot <- renderPlotly({
      data <- filtered_data()
      req(data)
      
      co2_plot <- data %>%
        mutate(
          model_co2 = paste(model, co2_emissions_g_km, "g/km"), 
          co2_rating = paste("rating:", co2_rating, fuel_type)) %>%
        plot_ly(
          x = ~co2_emissions_g_km,
          y = ~reorder(make, -co2_emissions_g_km),
          color = ~model,
          type = "bar",
          text = ~model_co2,
          hoverinfo = "text",
          name = ~co2_rating
        ) %>%
        layout(
          title = list(text = "CO2 Emissions By Vehicle", font = list(color = "#f8f9fa")),
          xaxis = list(title = "CO2 Emissions (g/km)", color = "#f8f9fa"),
          yaxis = list(title = "Make", color = "#f8f9fa"),
          plot_bgcolor = "#343a40",  # Match darkly background
          paper_bgcolor = "#343a40",
          font = list(color = "#f8f9fa"),  # Match text color
          height = 600
        )
      
      co2_plot
    })
    
  })
  
  output$dynamicUsedCars = renderUI({
    sidebarLayout(
      sidebarPanel(
        sliderInput("priceRange", "Select Price Range:",
                    min = min(complete_dataset$avg_used_price),
                    max = max(complete_dataset$avg_used_price),
                    value = c(min(complete_dataset$avg_used_price), max(complete_dataset$avg_used_price))),
        sliderInput("mileageRange", "Select Used Car Mileage Range:",
                    min = min(complete_dataset$avg_mileage), 
                    max = max(complete_dataset$avg_mileage),
                    value = c(min(complete_dataset$avg_mileage), max(complete_dataset$avg_mileage))),
        sliderInput("yearlyDistance", "How much do you drive in a year?",
                    min = 5000,
                    max = 100000,
                    value = 5000),
        selectInput("familySize",  "What is your family size?",
                    # choices = as.list(seq(1,10)), 
                    choices = c("Select Family Size" = "", as.character(1:10)),
                    selected = NULL, 
                    selectize = TRUE),
        selectInput("used_vehicle_class", "Vehicle Type: select all that apply",
                    choices = NULL, 
                    selected = NULL,
                    multiple = TRUE),
        selectInput("Fuel_type", "Select Fuel Type:",
                    choices = c("All", unique(complete_dataset$fuel_type_choice)),
                    selected = "All",
                    multiple = TRUE),
        selectInput("transmission", "Select Transmission Type:",
                    choices = c("All", unique(complete_dataset$transmission_type))),
        selectInput("used_car_concern", "What is your primary concern?",
                    choices = c("Price", "CO2 Rating", "Mileage", "Fuel Efficiency"),
                    selected = "Price"),
        actionButton("used_car_filter", "Get Recommendations")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Recommendations", 
                   dataTableOutput("usedCarRecommend")),
          tabPanel("Price", 
                   plotlyOutput("usedCarPricePlot")),
          tabPanel("Fuel Efficiency", 
                   plotlyOutput("usedCarFuelEfficiency")),
          tabPanel("CO2 Emissions", 
                   plotlyOutput("usedCarCo2"))
        )
      )
    )
  })
  
  
  observeEvent(input$used_car_filter, {
    if (is.null(input$used_vehicle_class) || length(input$used_vehicle_class) == 0) {
      showNotification("Please enter your family size and select at least one vehicle class before clicking 'Get Recommendations'.", type = "error")
      return()  # Exit the observeEvent without executing the rest
    }
    
    
    filtered_used = complete_dataset %>% 
      filter(
        avg_used_price > 0,
        avg_used_price >= input$priceRange[1],
        avg_used_price <= input$priceRange[2],
        avg_mileage >= input$mileageRange[1],
        avg_mileage <= input$mileageRange[2],
        (input$Fuel_type == "All" | fuel_type_choice == input$Fuel_type),
        (input$transmission == "All" | transmission_type == input$transmission),
        (vehicle_class == input$used_vehicle_class | input$used_vehicle_class == "No suitable vehicles")
      ) %>% 
      mutate(avg_yearly_fuel_cost = round(
        case_when(
          fuel_type_choice == "gas/diesel" ~ (input$yearlyDistance * (combined_l_100_km / 100) * (avg_fuel_type_price / 100)),
          fuel_type_choice == "Electric" ~ (input$yearlyDistance * 0.42 * (combined_kwh_100_km / 100)),
          fuel_type_choice == "Hybrid" ~ (((input$yearlyDistance * 0.42 * (combined_kwh_100_km / 100)) + (input$yearlyDistance * (combined_l_100_km / 100) * (avg_fuel_type_price / 100))) / 2),
          TRUE ~ NA_real_
        )
      ),
      avg_new_price = round(avg_new_price),
      avg_used_price = round(avg_used_price)) %>% 
      distinct(make, .keep_all = TRUE) %>% 
      arrange(
        case_when(
          input$used_car_concern == "Price" ~ avg_used_price,
          input$used_car_concern == "CO2 Rating" ~ co2_emissions_g_km,
          input$used_car_concern == "Mileage" ~ avg_mileage,
          input$used_car_concern == "Fuel Efficiency" ~ avg_yearly_fuel_cost
        )
      ) %>% 
      head(5)
    
    filtered_used_data(filtered_used)
  })
  
    # visualising recommended table
    output$usedCarRecommend <- renderDataTable({
      data <- filtered_used_data()
      req(data)
      if (nrow(data) > 0) {
        data %>% 
          select(avg_used_price, avg_new_price, model_year, make, model, vehicle_class, transmission_type, fuel_type, avg_mileage, avg_yearly_fuel_cost, co2_emissions_g_km, co2_rating) %>% 
          rename(
            "Used Price ($)" = avg_used_price,
            "Approximate New Price ($)" = avg_new_price,
            "Model Year" = model_year,
            "Make" = make,
            "Model" = model,
            "Vehicle Class" = vehicle_class,
            "Transmission" = transmission_type,
            "Fuel Type" = fuel_type,
            "Mileage (km)" = avg_mileage,
            "Yearly Fuel Cost ($)" = avg_yearly_fuel_cost,
            "CO2 Emissions (g/km)" = co2_emissions_g_km,
            "CO2 Rating" = co2_rating
          )
      } else {
        data.frame(Message = "No Vehicles match your criteria.")
      }
    })
    
    # visualizing prices of the recommended used vehicles
    output$usedCarPricePlot <- renderPlotly({
      data <- filtered_used_data()
      req(data)
      
      priceplot <- data %>%  # Replace column names as needed
        select(make, model, avg_new_price, avg_used_price) %>% 
        plot_ly() %>%
        add_trace(
          x = ~reorder(make, -avg_new_price),
          y = ~avg_new_price,
          type = "bar",
          name = "Average New Price ($)"
        ) %>%
        add_trace(
          x = ~reorder(make, -avg_used_price),
          y = ~avg_used_price,
          type = "bar",
          name = "Average Used Price ($)"
        ) %>%
        layout(
          title = list(text = "New and Used Prices of Recommended Vehicles", font = list(color = "#f8f9fa")),
          xaxis = list(title = "Vehicle Name", tickangle = -45, color = "#f8f9fa"),
          yaxis = list(title = "Price ($)", color = "#f8f9fa"),
          legend = list(title = list(text = "Price Type"), orientation = "h", x = 0.5, xanchor = "center", y = -0.2),
          plot_bgcolor = "#343a40",  # Match darkly background
          paper_bgcolor = "#343a40",
          font = list(color = "#f8f9fa"),  # Match text color
          height = 700
        )
      priceplot
      
    })
    
    
    plot_usedCar_data <- eventReactive(input$used_car_filter, {
      gas_prices <- complete_dataset %>%
        select(year, avg_fuel_price_yr) %>%
        distinct(year, avg_fuel_price_yr) %>% 
        arrange(year)
      
      filtered_used_data() %>% 
        select(model, combined_l_100_km, combined_kwh_100_km, fuel_type_choice) %>% 
        tidyr::expand_grid(year = gas_prices$year) %>% 
        left_join(gas_prices, by = "year") %>% 
        mutate(
          avg_yearly_fuel_cost = case_when(
            fuel_type_choice == "gas/diesel" ~ input$yearlyDistance * (combined_l_100_km / 100) * (avg_fuel_price_yr / 100),
            fuel_type_choice == "Electric" ~ input$yearlyDistance * 0.43 * (combined_kwh_100_km / 100),
            fuel_type_choice == "Hybrid" ~ ((input$yearlyDistance * (combined_l_100_km / 100) * (avg_fuel_price_yr / 100)) +
                                              (input$yearlyDistance * 0.43 * (combined_kwh_100_km / 100))) / 2,
            TRUE ~ NA_real_
          )
        )
    })
    
    
    output$usedCarFuelEfficiency <- renderPlotly({
      used_car_plot = plot_usedCar_data() %>% 
        ggplot(aes(x = year, y = avg_yearly_fuel_cost, color = model)) +
        geom_line() +  # Trend lines
        geom_point() + # Points for each data entry
        labs(
          title = "Fuel Efficiency Cost (2016–2024)",
          x = "Year",
          y = "Estimated Fuel Costs ($CAN)",
          color = "Car Model"
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5) 
        )
      
      ggplotly(used_car_plot, height = 700)
    })
    
    # Add the new CO2 plot output
    output$usedCarCo2 <- renderPlotly({
      data <- filtered_used_data()
      req(data)
      
      co2_plot <- data %>%
        mutate(
          model_co2 = paste(model, co2_emissions_g_km, "g/km"), 
          co2_rating = paste("rating:", co2_rating, fuel_type)) %>%
        plot_ly(
          x = ~co2_emissions_g_km,
          # y = ~make,
          y = ~reorder(make, -co2_emissions_g_km),
          color = ~model,
          type = "bar",
          text = ~model_co2,
          hoverinfo = "text",
          name = ~co2_rating
        ) %>%
        layout(
          title = list(text = "CO2 Emissions by Vehicle", font = list(color = "#f8f9fa")),
          xaxis = list(title = "CO2 Emissions (g/km)", color = "#f8f9fa"),
          yaxis = list(title = "Make", color = "#f8f9fa"),
          plot_bgcolor = "#343a40",  # Match darkly background
          paper_bgcolor = "#343a40",
          font = list(color = "#f8f9fa"),  # Match text color
          height = 700
          
        )
      
      co2_plot
    })
  
  
  
  
  # output$downloadData <- downloadHandler(
  #   filename = function() { "recommendations.csv" },
  #   content = function(file) {
  #     write.csv(complete_dataset(), file, row.names = FALSE)
  #   }
  # )
    
    output$downloadData <- downloadHandler(
      filename = function() { 
        "car_recommendations.zip" 
      },
      content = function(file) {
        # Create a temporary directory to store the files
        temp_dir <- tempfile()
        dir.create(temp_dir)
        
        # Generate New Car Recommendations CSV
        new_car_data <- filtered_data()
        new_car_file <- file.path(temp_dir, "new_car_recommendations.csv")
        if (!is.null(new_car_data) && nrow(new_car_data) > 0) {
          new_car_data %>%
            select(
              avg_new_price, model_year, make, model, vehicle_class, 
              transmission_type, fuel_type, avg_mileage, avg_yearly_fuel_cost, 
              co2_emissions_g_km, co2_rating
            ) %>%
            rename(
              "Price ($)" = avg_new_price,
              "Model Year" = model_year,
              "Make" = make,
              "Model" = model,
              "Vehicle Class" = vehicle_class,
              "Transmission" = transmission_type,
              "Fuel Type" = fuel_type,
              "Mileage (km)" = avg_mileage,
              "Yearly Fuel Cost ($)" = avg_yearly_fuel_cost,
              "CO2 Emissions (g/km)" = co2_emissions_g_km,
              "CO2 Rating" = co2_rating
            ) %>%
            write.csv(new_car_file, row.names = FALSE)
        } else {
          write.csv(data.frame(Message = "No new car recommendations available"), 
                    new_car_file, row.names = FALSE)
        }
        
        # Generate Used Car Recommendations CSV
        used_car_data <- filtered_used_data()
        used_car_file <- file.path(temp_dir, "used_car_recommendations.csv")
        if (!is.null(used_car_data) && nrow(used_car_data) > 0) {
          used_car_data %>%
            select(
              avg_used_price, avg_new_price, model_year, make, model, vehicle_class, 
              transmission_type, fuel_type, avg_mileage, avg_yearly_fuel_cost, 
              co2_emissions_g_km, co2_rating
            ) %>%
            rename(
              "Used Price ($)" = avg_used_price,
              "Approximate New Price ($)" = avg_new_price,
              "Model Year" = model_year,
              "Make" = make,
              "Model" = model,
              "Vehicle Class" = vehicle_class,
              "Transmission" = transmission_type,
              "Fuel Type" = fuel_type,
              "Mileage (km)" = avg_mileage,
              "Yearly Fuel Cost ($)" = avg_yearly_fuel_cost,
              "CO2 Emissions (g/km)" = co2_emissions_g_km,
              "CO2 Rating" = co2_rating
            ) %>%
            write.csv(used_car_file, row.names = FALSE)
        } else {
          write.csv(data.frame(Message = "No used car recommendations available"), 
                    used_car_file, row.names = FALSE)
        }
        
        # Zip the files
        files_to_zip <- list.files(temp_dir, full.names = TRUE)
        if (length(files_to_zip) > 0) {
          zip::zipr(
            zipfile = file,
            files = files_to_zip,
            recurse = FALSE
          )
        } else {
          stop("No data available to create the zip file.")
        }
      },
      contentType = "application/zip"
    )
    
    
  
}

# Run the app
shinyApp(ui, server)

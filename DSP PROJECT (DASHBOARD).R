

writeLines("
/* Custom styles for FoodPanda Dashboard */
.main-header {
  background-color: pink !important;
}

.main-sidebar {
  background-color: black !important;
}

.content-wrapper,
.right-side {
  background-color: pink !important;
}
", "custom.css")




library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(leaflet)

# Load the dataset
rdata <- read.csv("D:\\UMP\\Sem 4 2022-23\\BSD2223 Data Sc Programming II\\Project\\Dataset\\data.csv")

# dashboard header
header <- dashboardHeader(
  title = span("FoodPanda Dashboard", style = "color: white;")
)

# dashboard sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Customer Preferences", tabName = "customer_pref", icon = icon("utensils")),
    menuItem("ASEAN Tastes", tabName = "asean_tastes", icon = icon("list")),
    menuItem("Delivery Insights", tabName = "delivery_insights", icon = icon("map-marker")),
    menuItem("Rating Analysis", tabName = "boxplot", icon = icon("star"))
  )
)

# dashboard body
body <- dashboardBody(
  # Add this line to include the custom CSS file
  includeCSS("custom.css"),  
  tabItems(
    ############################################################################################# HOME    
    tabItem(
      tabName = "home",
      h2("Welcome to the FoodPanda Dashboard!"),
      fluidRow(
        # Info Boxes
        infoBox("Total restaurants & Shop", "111153", icon = icon("utensils")),
        # Dynamic Info Boxes
        infoBoxOutput("ratingBox"),
        infoBoxOutput("cuisineBox"),
        # Add a styled box around the select inputs
        box(
          title = "Country",
          width = 4,
          solidHeader = TRUE,
          status = "primary",
          background = "maroon",
          selectInput("countryInput", "Select Country:",
                      choices = unique(rdata$country),
                      selected = NULL),
        ), 
        box(
          title = "City",
          width = 4,
          solidHeader = TRUE,
          status = "primary",
          background = "maroon",
          selectInput("cityInput", "Select City:",
                      choices = NULL,
                      selected = NULL),
        ),
        box(
          title = "Cuisine",
          width = 4,
          solidHeader = TRUE,
          status = "primary",
          background = "maroon",
          selectInput("cuisineInput", "Select Cuisine:",
                      choices = NULL,
                      selected = NULL),
        ),
      ),
      fluidRow(
        # Display the map
        box(
          title = "Restaurant Map",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          background = "maroon",
          tabBox(id = "List of restaurant",
                 width = "100%", 
                 tabPanel("Map ",
                          leafletOutput("restaurant_map")),
                 tabPanel("Table", 
                          DT::dataTableOutput("top_restaurants_table"))
          )
        )
      )
    ),
    ############################################################################################# CUST. PREFERENCES
    tabItem(
      tabName = "customer_pref",
      h2("Customer Preferences"),
      fluidRow(
        box(
          title = "Vertical Parent",
          solidHeader = TRUE,
          status = "primary",
          background = "maroon",
          selectInput("verticalparent5Input", "Select Vertical Parent:",
                      choices = unique(rdata$vertical_parent),
                      selected = NULL)
        ),
        box(
          title = "Country",
          #width = 6, 
          solidHeader = TRUE,
          status = "primary",
          background = "maroon",
          selectInput("country5Input", "Select Country: ",
                      choices = unique(rdata$country),
                      selected = NULL)
        )
      ),
      fluidRow(
        box(
          title = "Vertical",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          background = "maroon",
          plotOutput("verticalparent_plot")
        )
      )
    ),
    ############################################################################################# ASEAN TASTES
    tabItem(
      tabName = "asean_tastes",
      h2("Cuisine Types For Asean"),
      fluidRow(
        box(
          title = "Cuisine Types in ASEAN",
          width = 12,
          solidHeader = TRUE,
          status = "info",
          background = "maroon",
          selectInput("countryInput_asean", "Select Country:",
                      choices = c("All", "Singapore", "Malaysia", "Thailand", "Laos", "Philippines", "Myanmar", "Cambodia"),
                      selected = "All"),
          plotlyOutput("asean_tastes_plot")
        )
      )
    ),
    ############################################################################################# DELIVERY INSIGHTS
    tabItem(
      tabName = "delivery_insights",
      h2("Delivery Insights"),
      fluidRow(
        box(title = "Delivery Option",
            status = "primary",
            solidHeader = TRUE,
            width = 3,
            background = "maroon",
            selectInput("deliveryprovider1Input", "Select Delivery Provider",
                        choices = unique(rdata$delivery_provider),
                        selected = NULL)
        ),
        box(title = "Country", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 3,
            background = "maroon",
            selectInput("country1Input", "Select Country",
                        choices = NULL,
                        selected = NULL),
        ),
        box(title = "City", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 3,
            background = "maroon",
            selectInput("city1Input", "Select City",
                        choices = NULL,
                        selected = NULL),
        ),
        box(title = "Main Cuisine", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 3,
            background = "maroon",
            selectInput("maincuisine1Input", "Select Main Cuisine",
                        choices = NULL,
                        selected = NULL)
        )
      ),
      fluidRow(
        box(title = "Minimum of Delivery Time",
            status = "primary",
            width = 12,
            background = "maroon",
            plotlyOutput("mindelivery_plot"),
            solidHeader = TRUE),
      ),
      fluidRow(
        box(title = "Minimum of Pickup Time", 
            status = "primary", 
            width = 12,
            background = "maroon",
            plotlyOutput("minpickup_plot"), 
            solidHeader = TRUE)
      )
    ),
    ############################################################################################# RATING ANALYSIS
    tabItem(
      tabName = "boxplot",
      fluidRow(
        box(
          title = "Rating Analysis",
          width = 12,
          status = "primary",
          background = "maroon",
          selectInput("country2Input", "Select Country:",
                      choices = c("All", unique(rdata$country)),
                      selected = "All"),
          plotOutput("boxplot_output")
        )
      )
    )
  )
)


# UI: Combine the header, sidebar, body & skin color
ui <- dashboardPage(header, sidebar, body, skin = "red")


# Server logic
server <- function(input, output, session) {
  
  ############################################################################################# HOME
  
  # Render Rating Box
  output$ratingBox <- renderInfoBox({
    infoBox(
      "Customer Ratings", 
      "104885", 
      icon = icon("star"),
      color = "purple"
    )
  })
  
  # Render cuisineBox
  output$cuisineBox <- renderInfoBox({
    infoBox(
      "Top Cuisines", 
      "563", 
      icon = icon("list"),
      color = "yellow"
    )
  })
  
  ### RESTAURANT MAP
  # Update city options based on selected country
  observeEvent(input$countryInput, {
    cities <- unique(rdata$city[rdata$country == input$countryInput])
    updateSelectInput(session, "cityInput", choices = cities, selected = NULL)
  })
  
  # Update cuisine options based on selected city
  observeEvent(input$cityInput, {
    cuisines <- unique(rdata$main_cuisine[rdata$city == input$cityInput])
    updateSelectInput(session, "cuisineInput", choices = cuisines, selected = NULL)
  })
  
  # Filter the data based on user inputs
  filtered_data <- reactive({
    data <- rdata
    
    # Filter by country
    if (!is.null(input$countryInput) && input$countryInput != "All") {
      data <- data %>% filter(country == input$countryInput)
    }
    # Filter by city
    if (!is.null(input$cityInput) && input$cityInput != "All") {
      data <- data %>% filter(city == input$cityInput)
    }
    # Filter by cuisine
    if (!is.null(input$cuisineInput) && input$cuisineInput != "All") {
      data <- data %>% filter(main_cuisine == input$cuisineInput)
    }
    return(data)
  })
  
  ### Create  MAP
  output$restaurant_map <- renderLeaflet({
    data <- filtered_data()
    
    # Filter for top 10 restaurants by rating
    top_restaurants <- data %>%
      filter(!is.na(rating)) %>%
      group_by(city, name) %>%
      summarise(rating = ifelse(all(is.na(rating)), NA, max(rating, na.rm = TRUE)), .groups = "drop_last") %>%
      filter(!is.na(rating)) %>%
      arrange(city, desc(rating)) %>%
      group_by(city) %>%
      top_n(10, wt = rating)
    
    # Create a color palette for ratings
    rating_palette <- colorNumeric(palette = "RdYlGn", domain = data$rating)
    
    # Create the leaflet map
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(data = data, 
                       lat = ~latitude, lng = ~longitude,
                       color = ~rating_palette(rating),
                       fillColor = ~rating_palette(rating),
                       fillOpacity = 0.7,
                       radius = 5,
                       popup = ~name) %>%
      addLegend("bottomright", pal = rating_palette,
                values = data$rating, 
                title = "Rating", opacity = 1)
  })
  # Create the table
  output$top_restaurants_table <- DT::renderDataTable({
    filtered_data()
  })
  
  
  ############################################################################################# CUST. PREF.
  
  ### Filter Vertical Parent by Country 
  output$verticalparent_plot <- renderPlot({
    filtered_data5 <- rdata %>%
      filter(country == input$country5Input, vertical_parent == input$verticalparent5Input)
    
    ggplot(filtered_data5, aes(x = vertical)) +
      geom_bar() +
      labs(x = "Vertical Parent", y = "Count") +
      facet_wrap(vars(country)) +
      theme_minimal()
  })  
  
  
  ############################################################################################# ASEAN TASTE
  
  # Render ASEAN Tastes plot
  output$asean_tastes_plot <- renderPlotly({
    # Filter for cuisine counts in the selected ASEAN country
    if (input$countryInput_asean == "All") {
      asean_cuisines <- rdata %>%
        filter(country %in% c("All","Singapore", "Malaysia", "Thailand", "Laos", "Philippines","Myanmar","Cambodia")) %>%
        count(main_cuisine)
    } else {
      asean_cuisines <- rdata %>%
        filter(country == input$countryInput_asean) %>%
        count(main_cuisine)
    }
    
    # Create the bar plot for ASEAN tastes
    asean_tastes_plot <- plot_ly(asean_cuisines, x = ~main_cuisine, y = ~n, type = "bar", colors = "purple") %>%
      layout(title = "ASEAN Tastes", xaxis = list(title = "Cuisine"), yaxis = list(title = "Count"))
    
    return(asean_tastes_plot)
  })
  
  
  ############################################################################################# DELIVERY INSIGHTS
  
  observeEvent(input$deliveryprovider1Input,{
    country1 <- unique(rdata$country[rdata$delivery_provider == input$deliveryprovider1Input])
    updateSelectInput(session, "country1Input", choices = country1, selected = NULL)
  })
  
  observeEvent(input$country1Input,{
    city1 <- unique(rdata$city[rdata$country == input$country1Input])
    updateSelectInput(session, "city1Input", choices = city1, selected = NULL)
  })
  
  observeEvent(input$city1Input,{
    maincuisine1 <- unique(rdata$main_cuisine[rdata$city == input$city1Input])
    updateSelectInput(session, "maincuisine1Input", choices = maincuisine1, selected = NULL)
  })
  
  # Filter the data based on user inputs
  filtered_data1 <- reactive({
    data1 <- rdata
    # Filter by delivery provider
    if (!is.null(input$deliveryprovider1Input) && input$deliveryprovider1Input != "All") {
      data1 <- data1 %>% filter(delivery_provider == input$deliveryprovider1Input)
    }
    # Filter by country
    if (!is.null(input$country1Input) && input$country1Input != "All") {
      data1 <- data1 %>% filter(country == input$country1Input)
    }
    # Filter by city
    if (!is.null(input$city1Input) && input$city1Input != "All") {
      data1 <- data1 %>% filter(city == input$city1Input)
    }
    # Filter by main cuisine
    if (!is.null(input$maincuisine1Input) && input$maincuisine1Input != "All") {
      data1 <- data1 %>% filter(main_cuisine == input$maincuisine1Input)
    }
    return(data1)
  })
  
  
  #### minimum delivery time
  output$mindelivery_plot <- renderPlotly({
    data <- filtered_data1()
    
    mindelivery <- data %>%
      filter(!is.na(minimum_delivery_time)) %>%
      group_by(city, name) %>%
      summarise(minimum_delivery_time = ifelse(all(is.na(minimum_delivery_time)), NA, max(minimum_delivery_time, na.rm = TRUE)), .groups = "drop_last") %>%
      filter(!is.na(minimum_delivery_time)) %>%
      arrange(city, desc(minimum_delivery_time)) %>%
      group_by(city) %>%
      top_n(10, wt = minimum_delivery_time)
    
    # Create the bar plot
    mindelivery_plot <- plot_ly(mindelivery, y = ~name, x = ~minimum_delivery_time, color = ~city, type = "bar", colors = "blue") %>%
      layout(title = "Minimum Delivery Time", yaxis = list(title = "Restaurant"), xaxis = list(title = "Minimum Delivery Time"), bargap = 0.2)
    
    return(mindelivery_plot)
  })
  
  
  #### minimum pickup time
  output$minpickup_plot <- renderPlotly({
    data2 <- filtered_data1()
    
    minpickup <- data2 %>%
      filter(!is.na(minimum_pickup_time)) %>%
      group_by(city, name) %>%
      summarise(minimum_pickup_time = ifelse(all(is.na(minimum_pickup_time)), NA, 
                                             max(minimum_pickup_time, na.rm = TRUE)), .groups = "drop_last") %>%
      filter(!is.na(minimum_pickup_time)) %>%
      arrange(city, desc(minimum_pickup_time)) %>%
      group_by(city) %>%
      top_n(10, wt = minimum_pickup_time)
    
    # Create the bar plot
    minpickup_plot <- plot_ly(minpickup, x = ~name, y = ~minimum_pickup_time, color = ~city, type = "bar", colors = "yellow") %>%
      layout(title = "Minimum Pickup Time", xaxis = list(title = "Restaurant"), yaxis = list(title = "Minimum Pickup Time"), bargap = 0.2)
    
    return(minpickup_plot)
  })
  
  
  ############################################################################################# RATING ANALYSIS (BOXPLOT)
  
  # Filter data based on selected country
  filtered_data2 <- reactive({
    if (input$country2Input == "All") {
      rdata
    } else {
      rdata[rdata$country == input$country2Input, ]
    }
  })
  
  # Generate box plot
  output$boxplot_output <- renderPlot({
    ggplot(filtered_data2(), aes(x = country, y = rating, fill = country)) +
      geom_boxplot() +
      labs(title = "Box Plot by Country",
           x = "Country",
           y = "Rating") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui, server)
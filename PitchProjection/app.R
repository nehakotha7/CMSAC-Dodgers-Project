library(shiny)
library(dplyr)

# Define the UI
ui <- fluidPage(
  titlePanel("Single Pitch Prediction"),
  
  tabsetPanel(
    tabPanel("Single Pitch Prediction", 
             h2("Select Response Pitch"),
             
             # Dropdown menu for pitch types
             selectInput("pitch_type", 
                         "Choose a response pitch to predict:", 
                         choices = c("Four-Seam Fastball", "Sinker", "Cutter", 
                                     "Slider", "Curveball", "Changeup", "Splitter"),
                         selected = "Four-Seam Fastball"),  # Default selection
             
             # Display associated predictor pitch
             textOutput("predictor_pitch_text"),
             
             # Dropdown menu for pitcher names
             uiOutput("pitcher_dropdown"),  # Dynamic dropdown for pitchers
             
             # Dropdown menu for season selection
             uiOutput("season_dropdown"),  # Dynamic dropdown for season
             
             # Display predictor variables and their values
             h3("Predictor Pitch Traits"),
             verbatimTextOutput("predictor_values"),
             
             #Display response variables and their values
             h3("Response Pitch Stuff+"),
             verbatimTextOutput("response_values")
    ),
    
    tabPanel("Tab 2", 
             h2("Content for Tab 2"),
             numericInput("num1", "Enter a number for Tab 2:", value = 5),
             verbatimTextOutput("num_output2")  # Output area for Tab 2
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Always use merged_data and select additional dataset based on pitch type
  selected_data <- reactive({
    additional_data <- switch(input$pitch_type,
                              "Four-Seam Fastball" = si_only,
                              "Sinker" = ff_only,
                              "Cutter" = fs_only,
                              "Slider" = ff_only2,
                              "Curveball" = ch_only,
                              "Changeup" = cu_only,
                              "Splitter" = ff_only3)
    
    list(merged_data = merged_data, additional_data = additional_data)
  })
  
  # Define predictor pitch associated with each response pitch
  predictor_pitch_map <- list(
    "Four-Seam Fastball" = "Sinker",
    "Sinker" = "Four-Seam Fastball",
    "Cutter" = "Splitter",
    "Slider" = "Four-Seam Fastball",
    "Curveball" = "Changeup",
    "Changeup" = "Curveball",
    "Splitter" = "Four-Seam Fastball"
  )
  
  # Define predictor variables with descriptive names and units
  predictor_vars <- list(
    "Four-Seam Fastball" = list("Velocity" = "pfx_vSI", "Horizontal Movement" = "pfx_SI.X", 
                                "Vertical Movement" = "pfx_SI.Z", "Spin Rate" = "si_avg_spin", 
                                "Average Horizontal Release Point" = "avg_rp_x", 
                                "Average Vertical Release Point" = "avg_rp_z", 
                                "Average Release Extension" = "avg_release_extension"),
    "Sinker" = list("Velocity" = "pfx_vFA", "Horizontal Movement" = "pfx_FA.X", 
                    "Vertical Movement" = "pfx_FA.Z", "Spin Rate" = "ff_avg_spin", 
                    "Average Horizontal Release Point" = "avg_rp_x", 
                    "Average Vertical Release Point" = "avg_rp_z", 
                    "Average Release Extension" = "avg_release_extension"),
    "Cutter" = list("Velocity" = "pfx_vFS", "Horizontal Movement" = "pfx_FS.X", 
                    "Vertical Movement" = "pfx_FS.Z", "Spin Rate" = "fs_avg_spin", 
                    "Average Horizontal Release Point" = "avg_rp_x", 
                    "Average Vertical Release Point" = "avg_rp_z", 
                    "Average Release Extension" = "avg_release_extension"),
    "Slider" = list("Velocity" = "pfx_vFA", "Horizontal Movement" = "pfx_FA.X", 
                    "Vertical Movement" = "pfx_FA.Z", "Spin Rate" = "ff_avg_spin", 
                    "Average Horizontal Release Point" = "avg_rp_x", 
                    "Average Vertical Release Point" = "avg_rp_z", 
                    "Average Release Extension" = "avg_release_extension"),
    "Curveball" = list("Velocity" = "pfx_vCH", "Horizontal Movement" = "pfx_CH.X", 
                       "Vertical Movement" = "pfx_CH.Z", "Spin Rate" = "ch_avg_spin", 
                       "Average Horizontal Release Point" = "avg_rp_x", 
                       "Average Vertical Release Point" = "avg_rp_z", 
                       "Average Release Extension" = "avg_release_extension"),
    "Changeup" = list("Velocity" = "pfx_vCU", "Horizontal Movement" = "pfx_CU.X", 
                      "Vertical Movement" = "pfx_CU.Z", "Spin Rate" = "cu_avg_spin", 
                      "Average Horizontal Release Point" = "avg_rp_x", 
                      "Average Vertical Release Point" = "avg_rp_z", 
                      "Average Release Extension" = "avg_release_extension"),
    "Splitter" = list("Velocity" = "pfx_vFA", "Horizontal Movement" = "pfx_FA.X", 
                      "Vertical Movement" = "pfx_FA.Z", "Spin Rate" = "ff_avg_spin", 
                      "Average Horizontal Release Point" = "avg_rp_x", 
                      "Average Vertical Release Point" = "avg_rp_z", 
                      "Average Release Extension" = "avg_release_extension")
  )
  
  response_vars <- list("Four-Seam Fastball" = list("Predicted Fastball Stuff+" = "ff_preds", 
                                                    "Actual Fastball Stuff+" = "sp_s_FF"),
                        "Sinker" = list("Predicted Sinker Stuff+" = "si_preds", 
                                            "Actual Sinker Stuff+" = "sp_s_SI"),
                        "Cutter" = list("Predicted Cutter Stuff+" = "fc_preds", 
                                             "Actual Cutter Stuff+" = "sp_s_FC"),
                        "Slider" = list("Predicted Slider Stuff+" = "sl_preds", 
                                        "Actual Slider Stuff+" = "sp_s_SL"),
                        "Curveball" = list("Predicted Curveball Stuff+" = "cu_preds", 
                                           "Actual Curveball Stuff+" = "sp_s_CU"),
                        "Changeup" = list("Predicted Changeup Stuff+" = "ch_preds", 
                                          "Actual Changeup Stuff+" = "sp_s_CH"),
                        "Splitter" = list("Predicted Splitter Stuff+" = "fs_preds", 
                                       "Actual Splitter Stuff+" = "sp_s_FS")
  )
  
  # Unit mapping for each variable type
  unit_map <- list(
    "Velocity" = "mph",
    "Horizontal Movement" = "inches",
    "Vertical Movement" = "inches",
    "Spin Rate" = "rpm",
    "Average Horizontal Release Point" = "inches",
    "Average Vertical Release Point" = "inches",
    "Average Release Extension" = "inches"
  )
  
  # Output the predictor pitch text
  output$predictor_pitch_text <- renderText({
    predictor_pitch <- predictor_pitch_map[[input$pitch_type]]
    paste("Associated Predictor Pitch:", predictor_pitch)
  })
  
  # Render dynamic dropdown for pitcher names based on selected dataset
  output$pitcher_dropdown <- renderUI({
    pitcher_names <- selected_data()$additional_data %>% 
      pull(PlayerNameRoute) %>% 
      unique() %>% 
      sort()
    
    selectInput("pitcher_name", 
                "Choose a pitcher:", 
                choices = c("Choose a Pitcher" = "", pitcher_names),  # Default "Choose a Pitcher"
                selectize = TRUE)  # Enables typing/searching
  })
  
  # Render dynamic dropdown for season selection based on selected pitcher
  output$season_dropdown <- renderUI({
    req(input$pitcher_name)  # Only show after pitcher is selected
    
    # Get unique seasons for the selected pitcher
    available_seasons <- selected_data()$additional_data %>%
      filter(PlayerNameRoute == input$pitcher_name) %>%
      pull(Season) %>%
      unique() %>%
      sort()
    
    selectInput("season", 
                "Choose a season:", 
                choices = available_seasons,
                selected = max(available_seasons))  # Default to the latest season
  })
  
  # Display predictor variables and their values for the selected pitcher and season
  output$predictor_values <- renderText({
    req(input$pitcher_name, input$season)  # Ensure both pitcher and season are selected
    
    # Get the selected dataset and variables
    dataset <- selected_data()$additional_data
    selected_vars <- predictor_vars[[input$pitch_type]]
    
    # Filter dataset for the selected pitcher and season
    pitcher_data <- dataset %>% 
      filter(PlayerNameRoute == input$pitcher_name, Season == input$season)
    
    # Display predictor variable names, values, and units if data is found
    if (nrow(pitcher_data) > 0) {
      output_text <- "Predictor Values:\n"
      values <- sapply(names(selected_vars), function(var_name) {
        # Retrieve the actual value from the dataset column
        column_name <- selected_vars[[var_name]]
        value <- pitcher_data[[column_name]][1]  # Extract the first value if multiple rows
        
        # Check if value is numeric, round if so, and append the unit
        if (!is.na(value) && is.numeric(value)) {
          value <- round(value, 2)
          unit <- unit_map[[var_name]]
          paste(var_name, "=", value, unit)
        } else {
          paste(var_name, "= Not Available")
        }
      })
      paste(values, collapse = "\n")
    } else {
      "No data available for the selected pitcher and season."
    }
  })
  
  #Response Variables
  output$response_values <- renderText({
    req(input$pitcher_name, input$season)  # Ensure both pitcher and season are selected
    
    # Get the selected dataset and variables
    dataset <- selected_data()$merged_data
    selected_vars <- response_vars[[input$pitch_type]]
    
    # Filter dataset for the selected pitcher and season
    pitcher_data <- dataset %>% 
      filter(PlayerNameRoute == input$pitcher_name, Season == input$season)
    
    # Display predictor variable names, values, and units if data is found
    if (nrow(pitcher_data) > 0) {
      values <- sapply(names(selected_vars), function(var_name) {
        # Retrieve the actual value from the dataset column
        column_name <- selected_vars[[var_name]]
        # Ensure column exists in dataset and get the value
        if (column_name %in% names(pitcher_data)) {
          value <- pitcher_data[[column_name]][1]  # Extract the first value if multiple rows
          if (!is.na(value) && is.numeric(value)) {
            value <- round(value, 2)
            paste(var_name, "=", value)
          } else {
            paste(var_name, "= Not Available")
          }
        } else {
          paste(var_name, "= Not Available")
        }
      })
      
      # Combine all values into a single output text
      output_text <- paste(paste(values, collapse = "\n"), sep = "\n")
    } else {
      output_text <- "No data available for the selected pitcher and season."
    }
    
    output_text  # Return the final output text
  })
  
  # Placeholder output for Tab 2
  output$num_output2 <- renderText({
    paste("You entered the number:", input$num1)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

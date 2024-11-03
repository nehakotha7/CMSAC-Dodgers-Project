library(shiny)
library(dplyr)
#source("C:/Users/dbnol/Documents/R Files/CMU R Files/CMSAC-Dodgers-Project/dodgers_data.R")
load("si_only.RData")
load("ff_only.RData")
load("fs_only.RData")
load("ff_only2.RData")
load("ch_only.RData")
load("cu_only.RData")
load("ff_only3.RData")
load("merged_data.RData")
load("merged_hgbr.RData")

# Define the UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Basic styling for footer */
      .custom-footer {
        width: 100%;
        text-align: center;
        padding: 10px;
        background-color: #f1f1f1;
        font-size: 12px;
        position: fixed;
        bottom: 0;
        left: 0;
      }
      /* Padding-bottom for main content to prevent footer overlap */
      .content {
        padding-bottom: 50px; /* Adjust if footer height changes */
      }
    "))
  ),
  titlePanel("Weapons of Best Production: Predicting the Optimal Pitch Arsenal Adjustment for Superior Stuff+"),
  
  div(class = "content",
      tabsetPanel(
        tabPanel("About",
                 h2("About"),
                 h4("Welcome to our Shiny App! This app was constructed so that 
                    users can view the results of our model for any pitcher of their 
                    choosing. Follow the instructions below to learn how to generate
                    predicted Stuff+ values."),
                 h4("The first tab, Single Pitch, develops results using random forest 
                 modeling between pairings of pitches. Begin by selecting a 
                    response pitch, and that response pitch will be associated with
                    a specific predictor pitch. These pairings were the ones with 
                    the lowest RMSE (root mean squared error) value among all pairings of pitches. Next, 
                    select the pitcher of interest and pick a season. Note that 
                    predictions can only be made for pitchers who threw the predictor
                    pitch, so some pitchers will be missing from the dropdown menu.  
                    After selecting options from the dropdown menus, users will see
                    traits of the predictor pitch, a predicted Stuff+ value for the
                    response pitch, and an actual Stuff+ value for the response pitch,
                    if applicable."),
                 h4("The second tab, Full Arsenal, uses predictions generated from
                    histogram-based gradient boosting regression trees. This method
                    of prediction contains native support for NA values, allowing it
                    to be applied to the full dataset without restricting to pairs 
                    of pitches. On this tab, simply select a pitcher of interest
                    and a year. Predicted Stuff+ values will appear below, as will
                    actual Stuff+ values if applicable."),
                 h4("Thanks for taking the time to check out our work. We hope you 
                    enjoy it and find it useful.")
        ),
        tabPanel("Single Pitch", 
                 h2("Single Pitch Prediction System"),
                 
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
                 
                 # Display response variables and their values
                 h3("Response Pitch Stuff+"),
                 verbatimTextOutput("response_values")
        ),
        
        tabPanel("Full Arsenal", 
                 h2("Multiple Pitch Prediction System"),
                 uiOutput("pitcher_dropdown_2"),
                 uiOutput("season_dropdown_2"),  # Dropdown for season selection
                 h3("Predicted Stuff+"),
                 verbatimTextOutput("predicted_stuff"),  # Display the predicted stuff+
                 h3("Actual Stuff+"),
                 verbatimTextOutput("actual_stuff")  # Display the actual stuff+
        )
      )
  ),
  
  # Footer
  tags$footer(
    class = "custom-footer",
    HTML("App created by Gabriel Eze, Neha Kotha, and Danny Nolan. Last updated in November 2024.")
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
                              "Splitter" = ff_only)
    
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
    "Average Horizontal Release Point" = "feet",
    "Average Vertical Release Point" = "feet",
    "Average Release Extension" = "feet"
  )
  
  # Output the predictor pitch text
  output$predictor_pitch_text <- renderText({
    predictor_pitch <- predictor_pitch_map[[input$pitch_type]]
    paste("Associated Predictor Pitch:", predictor_pitch)
  })
  
  # Render dynamic dropdown for pitcher names based on selected dataset
  output$pitcher_dropdown <- renderUI({
    pitcher_names <- selected_data()$additional_data |>
      pull(PlayerNameRoute) |>
      unique() |>
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
    available_seasons <- selected_data()$additional_data |>
      filter(PlayerNameRoute == input$pitcher_name) |>
      pull(Season) |>
      unique() |>
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
    pitcher_data <- dataset |> 
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
    pitcher_data <- dataset |> 
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
  
  # Dynamic dropdown for selecting pitcher in the second tab
  output$pitcher_dropdown_2 <- renderUI({
    # Ensure merged_data is available in the environment
    pitcher_names <- merged_hgbr$PlayerNameRoute |> 
      unique() |> 
      sort()
    
    selectInput("pitcher_name_2", 
                "Select a Pitcher:", 
                choices = c("Choose a Pitcher" = "", pitcher_names),
                selectize = TRUE)
  })
  
  # Dynamic dropdown for selecting season based on selected pitcher
  output$season_dropdown_2 <- renderUI({
    req(input$pitcher_name_2)  # Ensure a pitcher is selected first
    
    # Filter seasons by selected pitcher in merged_data
    seasons <- merged_hgbr |> 
      filter(PlayerNameRoute == input$pitcher_name_2) |> 
      pull(Season) |> 
      unique() |> 
      sort()
    
    selectInput("season_2", 
                "Select a Season:", 
                choices = c("Choose a Season" = "", seasons), 
                selected = max(seasons),
                selectize = TRUE)
  })
  
  # Display predicted stuff+ values for the selected pitcher
  output$predicted_stuff <- renderText({
    req(input$pitcher_name_2, input$season_2)  # Ensure both pitcher and season are selected
    
    # Filter merged_hgbr for the selected pitcher and season
    pitcher_data <- merged_hgbr |>
      filter(PlayerNameRoute == input$pitcher_name_2, Season == input$season_2)
    
    # Check if pitcher data is available
    if (nrow(pitcher_data) > 0) {
      # Retrieve predicted stuff+ values for each pitch type
      output_text <- paste0(
        "Predicted Fastball Stuff+: ", pitcher_data$ff_preds[1], "\n",
        "Predicted Sinker Stuff+: ", pitcher_data$si_preds[1], "\n",
        "Predicted Cutter Stuff+: ", pitcher_data$fc_preds[1], "\n",
        "Predicted Slider Stuff+: ", pitcher_data$sl_preds[1], "\n",
        "Predicted Curveball Stuff+: ", pitcher_data$cu_preds[1], "\n",
        "Predicted Changeup Stuff+: ", pitcher_data$ch_preds[1], "\n",
        "Predicted Splitter Stuff+: ", pitcher_data$fs_preds[1]
      )
      output_text
    } else {
      "Not Available."
    }
  })
  
  output$actual_stuff <- renderText({
    req(input$pitcher_name_2, input$season_2)  # Ensure both pitcher and season are selected
    
    # Filter merged_hgbr for the selected pitcher and season
    pitcher_data <- merged_hgbr |>
      filter(PlayerNameRoute == input$pitcher_name_2, Season == input$season_2)
    
    # Check if pitcher data is available
    if (nrow(pitcher_data) > 0) {
      # Retrieve predicted stuff+ values for each pitch type
      output_text <- paste0(
        "Actual Fastball Stuff+: ", pitcher_data$sp_s_FF[1], "\n",
        "Actual Sinker Stuff+: ", pitcher_data$sp_s_SI[1], "\n",
        "Actual Cutter Stuff+: ", pitcher_data$sp_s_FC[1], "\n",
        "Actual Slider Stuff+: ", pitcher_data$sp_s_SL[1], "\n",
        "Actual Curveball Stuff+: ", pitcher_data$sp_s_CU[1], "\n",
        "Actual Changeup Stuff+: ", pitcher_data$sp_s_CH[1], "\n",
        "Actual Splitter Stuff+: ", pitcher_data$sp_s_FS[1]
      )
      output_text
    } else {
      "Not Available."
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

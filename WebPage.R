library(shiny)
library(rvest)
library(tidyverse)
library(gt)

# Create a lookup table for full names to short names
team_lookup <- c("Acadia Axewomen" = "Acadia",
                 "Cape Breton Capers" = "Cape%20Breton",
                 "Dalhousie Tigers" = "Dalhousie",
                 "Memorial Sea-Hawks" = "Memorial",
                 "St. Francis Xavier X-Women" = "StFX",
                 "Saint Mary's Huskies" = "Saint%20Marys",
                 "UNB Reds" = "UNB",
                 "UPEI Panthers" = "UPEI",
                 "Alberta Pandas" = "Alberta",
                 "Brandon Bobcats" = "Brandon",
                 "Calgary Dinos" = "Calgary",
                 "Lethbridge Pronghorns" = "Lethbridge",
                 "MacEwan Griffins" = "MacEwan",
                 "Manitoba Bisons" = "Manitoba",
                 "Mount Royal University Cougars" = "Mount%20Royal",
                 "Regina Cougars" = "Regina",
                 "Saskatchewan Huskies" = "Saskatchewan",
                 "Thompson Rivers Wolfpack" = "Thompson_Rivers",
                 "Trinity Western Spartans" = "Trinity_Western",
                 "UBC Thunderbirds" = "UBC",
                 "UBC Okanagan Heat" = "UBC%20Okanagan",
                 "UFV Cascades" = "UFV",
                 "UNBC Timberwolves" = "UNBC",
                 "Victoria Vikes" = "Victoria",
                 "Winnipeg Wesmen" = "Winnipeg",
                 "Algoma Thunderbirds" = "Algoma",
                 "Brock Badgers" = "Brock",
                 "Carleton Ravens" = "Carleton",
                 "Guelph Gryphons" = "Guelph",
                 "Lakehead Thunderwolves" = "Lakehead",
                 "Laurentian Voyageurs" = "Laurentian",
                 "McMaster Marauders" = "McMaster",
                 "Nipissing Lakers" = "Nipissing",
                 "Ontario Tech Ridgebacks" = "Ontario%20Tech",
                 "Ottawa Gee Gees" = "Ottawa",
                 "Queen's Gaels" = "Queens",
                 "TMU Bold" = "TMUnow",
                 "Toronto Varsity Blues" = "Toronto",
                 "Waterloo Warriors" = "Waterloo",
                 "Western Mustangs" = "Western",
                 "Wilfrid Laurier Golden Hawks" = "WLUteam",
                 "Windsor Lancers" = "Windsor",
                 "York Lions" = "York",
                 "Bishop's Gaiters" = "Bishops",
                 "Concordia Stingers" = "Concordia",
                 "Laval Rouge et Or" = "Laval",
                 "McGill Martlets" = "McGill",
                 "UQAM Citadins" = "UQAM")



# Define UI for application with multiple tabs
ui <- fluidPage(
  
  # Application title
  titlePanel("Sport Analytics"),
  
  # Create multiple tabs
  tabsetPanel(
    tabPanel("Team Stats",
             fluidRow(
               column(6, h3("Team Statistics")),
               column(6, align = "right", 
                      # Dropdown to select team for roster
                      selectInput("team", "Select Team:",
                                  choices = names(team_lookup)))
             ),
             fluidRow(
               column(12, tableOutput("merged_roster_table"))
             )
    ),
    tabPanel("Scouting Report",
             fluidRow(
               column(12, h3("WATERLOO WOMEN’S BASKETBALL SCOUTING REPORT")),
               column(12, h3(textOutput("team_name")))
             ),
             fluidRow(
               column(12, tableOutput("top_players_table"))
             )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  all_data <- read_csv("data.csv")
  
  # Reactive value to store roster data and selected team name
  roster_data <- reactiveValues(
    roster_df1 = NULL,
    roster_df2 = NULL,
    team_name = NULL
  )
  
  # Function to update roster data and team name
  observeEvent(input$team, {
    short_name <- team_lookup[input$team]
    if (!is.na(short_name)) {
      data <- all_data %>% 
        filter(team == short_name) # Assuming 'Team' column in your CSV matches the short name
      roster_data$roster_df1 <- data
      roster_data$roster_df2 <- data
      roster_data$team_name <- input$team
      
    }
  })
  
  # Render the merged roster table based on selected team
  output$merged_roster_table <- render_gt({
    roster_df1 <- roster_data$roster_df1
    roster_df2 <- roster_data$roster_df2
    if (!is.null(roster_df1) && !is.null(roster_df2)) {
      # Select all columns except the last one
      roster_df1 <- roster_df1 %>% select(-last_col())
      # You can perform any necessary data manipulation here before displaying the table
      gt_table <- roster_df1 %>% 
        gt() %>%
        # Define column widths or any other styling you want
        cols_width(
          Player ~ px(120),
          No ~ px(60),
          Position ~ px(120),
          Ht ~ px(60),
          GP ~ px(60),
          Mins ~ px(60),
          Mpg ~ px(60),
          FGM ~ px(70),
          FGA ~ px(70),
          FTM ~ px(70),
          FTA ~ px(70),
          RPG ~ px(70),
          PF ~ px(70),
          A ~ px(70),
          To ~ px(70),
          Bl ~ px(70),
          St ~ px(70),
          Pts ~ px(70),
          PPG ~ px(70),
          everything()  ~ px(70),
        )
      
      # Apply style to highlight the first row
      gt_table %>%
        tab_style(
          style = cell_text(color = "red"), # Changing the color to red
          locations = cells_body(
            rows = c(1) # Targeting the first row
          )
        )
    } else {
      return(NULL)
    }
  })
  
  # Render the top players table for the Scouting Report tab
  output$top_players_table <- renderTable({
    roster_df1 <- roster_data$roster_df1
    roster_df2 <- roster_data$roster_df2
    if (!is.null(roster_df1) && !is.null(roster_df2)) {
      # Select all columns except the last one
      roster_df1 <- roster_df1 %>% select(-last_col())
      # You can perform any necessary data manipulation here before displaying the table
      top_players <- roster_df1 %>% 
        # Assuming you're sorting by some column, modify as needed
        arrange(desc(Mpg)) %>% 
        slice(2:9) # Selecting second to ninth top players
      
      
      return(top_players)
    } else {
      return(NULL)
    }
  })
  
  # Render the selected team name for the Scouting Report tab
  output$team_name <- renderText({
    roster_data$team_name
  })
}


# Run the application
shinyApp(ui = ui, server = server)


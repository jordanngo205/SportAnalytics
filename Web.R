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

# Function to scrape roster information for a given team from URL
scrape_roster_from_url <- function(short_name) {
  url <- paste0("https://usportshoops.ca/history/teamseason.php?Gender=WBB&Season=2023-24&Team=", short_name)
  page <- read_html(url)
  
  # Extract player roster table
  roster_table1 <- page %>%
    html_nodes("table[width='100%']") %>%
    .[[4]]  # Adjust the index to select the correct table containing the roster
  
  # Extract player roster table
  roster_table2 <- page %>%
    html_nodes("table[width='100%']") %>%
    .[[5]]  # Adjust the index to select the correct table containing the roster
  
  # Convert the roster table to a data frame
  roster_df1 <- html_table(roster_table1, fill = TRUE)
  
  # Convert the roster table to a data frame
  roster_df2 <- html_table(roster_table2, fill = TRUE)
  
  # Rename columns for clarity
  colnames(roster_df1) <- c("No", "Player", "Position", "Ht", "Elig", "Hometown", "High_School")
  
  # Rename columns for clarity
  colnames(roster_df2) <- c("Player", "GP",	"Mins",	"Mpg",	"3Pt",	"3P%", "FG","FG%",	"FT", "FT%",	"Rebounds", "REB",	"RPG",	"PF",	"A"	,"To",	"Bl",	"St",	"Pts",	"PPG")
  
  return(list(roster_df1 = roster_df1, roster_df2 = roster_df2))
}

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
      data <- scrape_roster_from_url(short_name)
      roster_data$roster_df1 <- data$roster_df1
      roster_data$roster_df2 <- data$roster_df2
      roster_data$team_name <- input$team
      
    }
  })
  
  # Render the merged roster table based on selected team
  output$merged_roster_table <- render_gt({
    roster_df1 <- roster_data$roster_df1
    roster_df2 <- roster_data$roster_df2
    if (!is.null(roster_df1) && !is.null(roster_df2)) {
      # Merge the two tables using the "Player" column as primary key
      merged_roster <- merge(roster_df1, roster_df2, by = "Player", all = TRUE)
      
      # Remove columns "Elig", "Hometown", and "High_School"
      merged_roster <- merged_roster[, !names(merged_roster) %in% c("Elig", "Hometown", "High_School")]
      
      # Convert to gt table and adjust column widths
      gt_table <- merged_roster %>%
        separate_wider_delim(cols = FG, delim = "-", names = c("FGM", "FGA")) %>% 
        separate_wider_delim(cols = FT, delim = "-", names = c("FTM", "FTA")) %>% 
        separate_wider_delim(cols = `3Pt`, delim = "-", names = c("3PM", "3PA")) %>% 
        separate_wider_delim(cols = Rebounds, delim = "-", names = c("OREB", "DREB")) %>% 
        gt() %>%
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
      merged_roster <- merge(roster_df1, roster_df2, by = "Player", all = TRUE)
      merged_roster <- merged_roster[, !names(merged_roster) %in% c("Elig", "Hometown", "High_School")]
      top_players <- merged_roster[order(-merged_roster$Mpg), ][2:9, ]
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

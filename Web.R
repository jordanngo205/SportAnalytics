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

# Extract player data from dataset
read_and_process_player_data <- function(team_selected) {
  players_data <- read_csv("data.csv", show_col_types = FALSE)  # Adjust path to dataset as necessary
  team_data <- players_data %>%
    filter(team == team_selected) %>%
    filter(Player != "* Team Totals") %>%  # Exclude the 'Team Totals*' row
    arrange(desc(Mpg))
  
# Create lists for starters and reserves 
  list(
    starters = head(team_data, 5),
    key_reserves = head(tail(team_data, n = nrow(team_data) - 5), 3)  # Adjusted line
  )
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
             ),
             tags$head(
               tags$style(HTML("
      .table-header {
        background-color: #FFC000;
        color: #000000;
        text-align: center;
        font-weight: bold;
        font-size: 20px;
      }
      .section-header {
        background-color: #000000;
        color: white;
        text-align: center;
        font-weight: bold;
        font-size: 18px;
      }
      table {
        width: 100%;
        border-collapse: collapse;
      }
      th, td {
        border: 1px solid #dddddd;
        text-align: center;
        padding: 8px;
        vertical-align: middle;
      }
      .header-font-size {
        font-size: 12px;
      }
      .data-cell {
        font-size: 11px;
      }
      .stats-divider {
        position: relative;
      }
      .stats-divider:after {
        content: '';
        position: absolute;
        top: 0;
        left: 50%;
        width: 2px;
        background-color: #000000;
        height: 100%;
      }
      .nested-cell {
        display: flex;
        justify-content: space-between;
        align-items: center;
        height: 100%;
      }
      .nested-cell > div {
        width: 100%;
      }

      .key-reserves-header {
        background-color: #505050;
        color: white;
        font-weight: bold;
        text-align: center;
        padding: 8px;
        border: none;
        font-size: 18px;
      }
      .row-height {
        height:40px; /* Or any other value */
      }
    "))
             ),
             titlePanel("Keys to the Game"),
             tags$table(class = 'table',
                        tags$thead(
                          tags$tr(class = "table-header", tags$th(colspan = 4, "PERSONNEL STARTERS")),  
                        ),
                        
                        tags$tbody(
                          tags$tr( 
                            tags$th(style = "width: 5%;", "Photo"),  
                            tags$th(style = "width: 15%;", textOutput("starter1")),  
                            tags$th(style = "width: 15%;", textOutput("pos_ht1")),  
                            tags$th(style = "width: 65%;",  textOutput("stats_1"))  
                          ),
                          tags$tr(
                            tags$td(style = "width: 5%;", class="data-cell row-height", ""),
                            tags$td(style = "width: 15%;", class="data-cell row-height", "Icons"),
                            tags$td(style = "width: 15%;", class="data-cell row-height", "Shooting Chart"),
                            tags$td(style = "width: 65%;", class = "stats-divider data-cell", 
                                    tags$div(class = "nested-cell",
                                             tags$div("Tendency"),
                                             tags$div("We Must")
                                    )
                            )
                          ),
                          tags$tr( 
                            tags$th(style = "width: 5%;", "Photo"),  
                            tags$th(style = "width: 15%;", class="data-cell row-height", textOutput("starter2")),  
                            tags$th(style = "width: 15%;", class="data-cell row-height", textOutput("pos_ht2")),  
                            tags$th(style = "width: 65%;", class="data-cell row-height",  textOutput("stats_2"))  
                          ),
                          tags$tr(
                            tags$td(style = "width: 5%;", class="data-cell custom-height", ""),
                            tags$td(style = "width: 15%;", class="data-cell custom-height", "Icons"),
                            tags$td(style = "width: 15%;", class="data-cell custom-height", "Shooting Chart"),
                            tags$td(style = "width: 65%;", class = "stats-divider data-cell", 
                                    tags$div(class = "nested-cell",
                                             tags$div("Tendency"),
                                             tags$div("We Must")
                                    )
                            )
                          ),               tags$tr( 
                            tags$th(style = "width: 5%;", "Photo"),  
                            tags$th(style = "width: 15%;", class="data-cell custom-height", textOutput("starter3")),  
                            tags$th(style = "width: 15%;", class="data-cell custom-height", textOutput("pos_ht3")),  
                            tags$th(style = "width: 65%;", class="data-cell custom-height",  textOutput("stats_3"))  
                          ),
                          tags$tr(
                            tags$td(style = "width: 5%;", class="data-cell custom-height", ""),
                            tags$td(style = "width: 15%;", class="data-cell custom-height", "Icons"),
                            tags$td(style = "width: 15%;", class="data-cell custom-height", "Shooting Chart"),
                            tags$td(style = "width: 65%;", class = "stats-divider data-cell", 
                                    tags$div(class = "nested-cell",
                                             tags$div("Tendency"),
                                             tags$div("We Must")
                                    )
                            )
                          ),               tags$tr( 
                            tags$th(style = "width: 5%;", "Photo"),  
                            tags$th(style = "width: 15%;", class="data-cell custom-height", textOutput("starter4")),  
                            tags$th(style = "width: 15%;", class="data-cell custom-height", textOutput("pos_ht4")),  
                            tags$th(style = "width: 65%;", class="data-cell custom-height",  textOutput("stats_4"))  
                          ),
                          tags$tr(
                            tags$td(style = "width: 5%;", class="data-cell custom-height", ""),
                            tags$td(style = "width: 15%;", class="data-cell custom-height", "Icons"),
                            tags$td(style = "width: 15%;", class="data-cell custom-height", "Shooting Chart"),
                            tags$td(style = "width: 65%;", class = "stats-divider data-cell", 
                                    tags$div(class = "nested-cell",
                                             tags$div("Tendency"),
                                             tags$div("We Must")
                                    )
                            )
                          ),               tags$tr( 
                            tags$th(style = "width: 5%;", "Photo"),  
                            tags$th(style = "width: 15%;", class="data-cell custom-height", textOutput("starter5")),  
                            tags$th(style = "width: 15%;", class="data-cell custom-height", textOutput("pos_ht5")),  
                            tags$th(style = "width: 65%;", class="data-cell custom-height",  textOutput("stats_5"))  
                          ),
                          tags$tr(
                            tags$td(style = "width: 5%;", class="data-cell custom-height", ""),
                            tags$td(style = "width: 15%;", class="data-cell custom-height", "Icons"),
                            tags$td(style = "width: 15%;", class="data-cell custom-height", "Shooting Chart"),
                            tags$td(style = "width: 65%;", class = "stats-divider data-cell", 
                                    tags$div(class = "nested-cell",
                                             tags$div("Tendency"),
                                             tags$div("We Must")
                                    )
                            )
                          ),
                          
                          # Repeat the above tr for as many rows as you need
                          # After the last row of players, add the Key Reserves section header
                          tags$tr(
                            tags$th(class = "key-reserves-header", colspan = 4, "KEY RESERVES")
                          ),
                          tags$tr( 
                            tags$th(style = "width: 5%;", "Photo"),  
                            tags$th(style = "width: 15%;", class="data-cell custom-height", textOutput("reserve1")),  
                            tags$th(style = "width: 15%;", class="data-cell custom-height", textOutput("pos_ht_reserve1")),  
                            tags$th(style = "width: 65%;", class="data-cell custom-height", textOutput("reserve_stats_1")),  
                          ),
                          tags$tr(
                            tags$td(style = "width: 5%;", class="data-cell custom-height", ""),
                            tags$td(style = "width: 15%;", class="data-cell custom-height", "Icons"),
                            tags$td(style = "width: 15%;", class="data-cell custom-height", "Shooting Chart"),
                            tags$td(style = "width: 65%;", class = "stats-divider data-cell", 
                                    tags$div(class = "nested-cell",
                                             tags$div("Tendency"),
                                             tags$div("We Must")
                                    )
                            )
                          ),               tags$tr( 
                            tags$th(style = "width: 5%;", "Photo"),  
                            tags$th(style = "width: 15%;", class="data-cell custom-height", textOutput("reserve2")),  
                            tags$th(style = "width: 15%;", class="data-cell custom-height", textOutput("pos_ht_reserve2")),  
                            tags$th(style = "width: 65%;", class="data-cell custom-height", textOutput("reserve_stats_2")),  
                          ),
                          tags$tr(
                            tags$td(style = "width: 5%;", class="data-cell custom-height", ""),
                            tags$td(style = "width: 15%;", class="data-cell custom-height", "Icons"),
                            tags$td(style = "width: 15%;", class="data-cell custom-height", "Shooting Chart"),
                            tags$td(style = "width: 65%;", class = "stats-divider data-cell", 
                                    tags$div(class = "nested-cell",
                                             tags$div("Tendency"),
                                             tags$div("We Must")
                                    )
                            )
                          ),               tags$tr( 
                            tags$th(style = "width: 5%;", "Photo"),  
                            tags$th(style = "width: 15%;", class="data-cell custom-height", textOutput("reserve3")),  
                            tags$th(style = "width: 15%;", class="data-cell custom-height", textOutput("pos_ht_reserve3")),  
                            tags$th(style = "width: 65%;", class="data-cell custom-height", textOutput("reserve_stats_3")),  
                          ),
                          tags$tr(
                            tags$td(style = "width: 5%;", class="data-cell custom-height", ""),
                            tags$td(style = "width: 15%;", class="data-cell custom-height", "Icons"),
                            tags$td(style = "width: 15%;", class="data-cell custom-height", "Shooting Chart"),
                            tags$td(style = "width: 65%;", class = "stats-divider data-cell", 
                                    tags$div(class = "nested-cell",
                                             tags$div("Tendency"),
                                             tags$div("We Must")
                                    )
                            )
                          ),
                          # Repeat the above tr for as many reserve rows as needed
                        )
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
  
  # Reactive expression to fetch and store player data
  reactive_player_data <- reactive({
    team_selected <- input$team  # Assuming there's an input in UI to select team
    short_name <- team_lookup[team_selected]
    if (!is.na(short_name)) {
      read_and_process_player_data(short_name)
    } else {
      list(starters = NULL, key_reserves = NULL)
    }
  })
  
  # Assuming reactive_player_data() fetches and processes the data
  # Dynamically populate scouting report table
  observe({
    player_data <- reactive_player_data()
    starters <- player_data$starters
    key_reserves <- player_data$key_reserves
    
    # Populate starters
    if (!is.null(starters) && nrow(starters) >= 5) {
      output$starter1 <- renderText({ starters$Player[1] })
      output$starter2 <- renderText({ starters$Player[2] })
      output$starter3 <- renderText({ starters$Player[3] })
      output$starter4 <- renderText({ starters$Player[4] })
      output$starter5 <- renderText({ starters$Player[5] })
    }
    
    # Populate key reserves
    if (!is.null(key_reserves) && nrow(key_reserves) >= 3) {
      output$reserve1 <- renderText({ key_reserves$Player[1] })
      output$reserve2 <- renderText({ key_reserves$Player[2] })
      output$reserve3 <- renderText({ key_reserves$Player[3] })
    }
    
    # Populate starters' POS-H/T
    if (!is.null(starters) && nrow(starters) >= 5) {
      output$pos_ht1 <- renderText({ paste(starters$Position[1], ",", starters$Ht[1]) })
      output$pos_ht2 <- renderText({ paste(starters$Position[2], ",", starters$Ht[2]) })
      output$pos_ht3 <- renderText({ paste(starters$Position[3], ",", starters$Ht[3]) })
      output$pos_ht4 <- renderText({ paste(starters$Position[4], ",", starters$Ht[4]) })
      output$pos_ht5 <- renderText({ paste(starters$Position[5], ",", starters$Ht[5]) })
    }
    
    # Populate key reserves' POS-H/T in a similar manner
    if (!is.null(key_reserves) && nrow(key_reserves) >= 3) {
      output$pos_ht_reserve1 <- renderText({ paste(key_reserves$Position[1], ",", key_reserves$Ht[1]) })
      output$pos_ht_reserve2 <- renderText({ paste(key_reserves$Position[2], ",", key_reserves$Ht[2]) })
      output$pos_ht_reserve3 <- renderText({ paste(key_reserves$Position[3], ",", key_reserves$Ht[3]) })
    }
    
    # Pull key stats based on position
    format_stats <- function(position, stats) {
      if (position %in% c("Forward", "Post")) {
        # Format for Forward/Post: PPG, RPG, FT%
        paste(stats$PPG, "PPG,", stats$RPG, "RPG,", stats$`FT%`, "FT")
      } 
      else if (position %in% c("Guard", "Wing", "Guard/Forward")) {
        # Format for Guard/Wing/Guard-Forward: PPG, 3P%, FT%
        paste(stats$PPG, "PPG,", stats$`3P%`, "3P%,", stats$`FT%`, "FT")
      } 
      else {
        "Stats Not Available"
      }
    }
    
    # Populate stats boxes for starters
    if (!is.null(starters) && nrow(starters) >= 5) {
      for (i in 1:5) {
        local({
          idx <- i  # Localize iterator for reactive context
          output[[paste0("stats_", idx)]] <- renderText({
            # Assuming format_stats formats stats based on the player's position
            format_stats(starters$Position[idx], starters[idx, ])
          })
        })
      }
    }
    
    # Populate stats boxes for reserves
    if (!is.null(key_reserves) && nrow(key_reserves) >= 3) {
      for (i in 1:3) {  # Assuming you have 3 key reserves
        local({
          idx <- i  # Localize iterator for reactive context
          output[[paste0("reserve_stats_", idx)]] <- renderText({
            # Assuming format_stats formats stats based on the player's position
            format_stats(key_reserves$Position[idx], key_reserves[idx, ])
          })
        })
      }
    }
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)

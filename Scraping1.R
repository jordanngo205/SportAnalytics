library(rvest)
library(tidyverse)

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
  
   # Merge the two tables using the "Player" column as primary key
      merged_roster <- merge(roster_df1, roster_df2, by = "Player", all = TRUE)
      
      #Remove columns "Elig", "Hometown", and "High_School"
      merged_roster <- merged_roster[, !names(merged_roster) %in% c("Elig", "Hometown", "High_School")] %>% 
        separate_wider_delim(cols = FG, delim = "-", names = c("FGM", "FGA")) %>% 
        separate_wider_delim(cols = FT, delim = "-", names = c("FTM", "FTA")) %>% 
        separate_wider_delim(cols = `3Pt`, delim = "-", names = c("3PM", "3PA")) %>% 
        separate_wider_delim(cols = Rebounds, delim = "-", names = c("OREB", "DREB")) %>% 
        mutate(across(c(Player, Position, Ht), as.character),
               across(-c(Player, Position, Ht), as.numeric),
               team = short_name)
      return(merged_roster)
}


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
                 "Thompson Rivers Wolfpack" = "Thompson%20Rivers",
                 "Trinity Western Spartans" = "Trinity%20Western",
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

all_data <- data.frame()
for (team in team_lookup[1:5]) {
  new_data <- scrape_roster_from_url(team)
  all_data <- bind_rows(all_data, new_data)
  #Sys.sleep(1)
  
}

write_csv(all_data, "data.csv")


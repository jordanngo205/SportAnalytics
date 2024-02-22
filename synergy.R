if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, httr, jsonlite, sportyR, janitor, magick)

## STEP 1: GET TOKEN 

# Get the access token from Sources > login > Fetches > token after clicking 
# 'Inspect Element'. Access tokens last for 1 hour so the one you see here is expired
token <- paste("Bearer", " eyJhbGciOiJSUzI1NiIsImtpZCI6IjY3QkNEMDQ5QjBGMUQ4RTc2MEQ0NUUwOEI2NDgyMkU1IiwidHlwIjoiYXQrand0IiwiY3R5IjoiSldUIn0.eyJpc3MiOiJodHRwczovL2F1dGguc3luZXJneXNwb3J0c3RlY2guY29tIiwibmJmIjoxNzA4NjQwNDQxLCJpYXQiOjE3MDg2NDA0NDEsImV4cCI6MTcwODY0NDA0MSwiYXVkIjpbImFwaS5jb25maWciLCJhcGkuc2VjdXJpdHkiLCJhcGkuYmFza2V0YmFsbCIsImFwaS5zcG9ydCIsImFwaS5lZGl0b3IiLCJodHRwczovL2F1dGguc3luZXJneXNwb3J0c3RlY2guY29tL3Jlc291cmNlcyJdLCJzY29wZSI6WyJvcGVuaWQiLCJhcGkuY29uZmlnIiwiYXBpLnNlY3VyaXR5IiwiYXBpLmJhc2tldGJhbGwiLCJhcGkuc3BvcnQiLCJhcGkuZWRpdG9yIiwib2ZmbGluZV9hY2Nlc3MiXSwiYW1yIjpbInB3ZCJdLCJjbGllbnRfaWQiOiJjbGllbnQuYmFza2V0YmFsbC50ZWFtc2l0ZSIsInN1YiI6IjY1NGE1YmE0YzIxZDg4Y2QwMGIwZDUzMSIsImF1dGhfdGltZSI6MTcwODYyNTYzNiwiaWRwIjoibG9jYWwiLCJlbWFpbCI6ImozNG5nb0B1d2F0ZXJsb28uY2EiLCJuYW1lIjoiSm9yZGFuIE5nbyIsInNpZCI6IkMxRDkxRjBENUJFMTA4OTA2OTczRDU5NUUwODFGOEY5In0.RP9-TLWXpREjN_C_zBeEkmcGglrmNmPT-5H4W-cvxsPf87q51t_enBP5uZpU5ollfcjpql7JM_bp33L-Riu4aqsnQ67Q0o4c6bhcE9GPGKIMs0r-1NQC2C-ZBiJjR9Z7IMBdz3I2MFhFRLf6vB4ANuEwLv6DNp32q-orFX38QTwwnjrZBGvyHxG1NxhnEzpe57zMqlGnJyKUjEjFw6gU5cNzkzqTCHIxR236okYM52HzhpdEiJHrrYU8Zy2_r_iaGdtBoZ1Z4QtfHSaq2xg45UZd1ezwhjlJ9Ux5tCD3K6ISy8uXw1vy_3emRJqxjnFi2-QAzxI6Fg4rDCdXNi2OIA")

# Information that never changes

oua_sport_id <- "570aaedc46c5d11de0f8c0bc"
oua_league_id <- "54457dce300969b132fcfb38"
oua_division_id <- "54457dd2300969b132fcfe2a"

## GET SEASON OF INTEREST

url <- paste0("https://sport.synergysportstech.com/api/sports/", oua_sport_id,  "/seasons?")

# Make the request to get the event data from the backend database
request <- GET(url, add_headers("Authorization" = token))
json <- content(request, as = "text")
seasons <- as.data.frame(fromJSON(json)$result) %>% janitor::clean_names() %>% 
  unnest(league, names_sep = "_")
# This currently just filters for 2023-2024, more years can probably be added
season_id <- seasons %>% filter(league_name == "College Women", name == "2023-2024") %>% pull(id) #change this once a year

## GET COMPETITIONS OF INTEREST

url <- paste0("https://sport.synergysportstech.com/api/competitionswithstats?seasonId=", season_id)

# Make the request to get the event data from the backend database
request <- GET(url, add_headers("Authorization" = token))
json <- content(request, as = "text")
competitions <- as.data.frame(fromJSON(json)$result) %>% janitor::clean_names() 

## GET TEAM INFORMATION AND SAVE IT (ONLY ONCE PER SEASON)

headers = c(
  `Content-Type` = 'application/json; charset=utf-8',
  `Accept` = 'application/json, text/plain, */*',
  `Authorization` = token,
  `Accept-Language` = 'en-CA,en-US;q=0.9,en;q=0.8',
  `Accept-Encoding` = 'gzip, deflate', ## remember to remove ", br"
  `Host` = 'sport.synergysportstech.com',
  `Origin` = 'https://apps.synergysports.com',
  `Content-Length` = '914',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.2 Safari/605.1.15',
  `Referer` = 'https://apps.synergysports.com/',
  `Connection` = 'keep-alive',
  `newrelic` = 'eyJ2IjpbMCwxXSwiZCI6eyJ0eSI6IkJyb3dzZXIiLCJhYyI6IjE0MDM4ODIiLCJhcCI6IjExMjAwMjIxMjMiLCJpZCI6IjQ4Y2Y4NmY1NTgxOTI5MDkiLCJ0ciI6IjIxYjNkMWI2YzAxYTg3YjkzZGU5MDZjYTUxZDU1NjQwIiwidGkiOjE3MDg2MjcxOTg3MTUsInRrIjoiMTM4NDI3MyJ9fQ==',
  `X-SYNERGY-CLIENT` = 'ProductVersion=2024.02.21.827; ProductName=Basketball.TeamSite'
)

# "data" might change every season
data = '{"seasonId":"64da35a80d288f7495c0bdca","competitionIds":["560100ac8dc7a24394b95643","56d89e6d50238a164760b711","560100ac8dc7a24394b95617","560100ac8dc7a24394b95637","560100ac8dc7a24394b95684","560100ac8dc7a24394b95685","560100ac8dc7a24394b95674","56010b438dc7a25554523b23","623b3ee36b7ff0a2745e33be","560100ac8dc7a24394b95655","560100ac8dc7a24394b95653","56d89e6d50238a164760b710","56d89e6d50238a164760b70f","560100ac8dc7a24394b95662","560100ac8dc7a24394b95632","560100ac8dc7a24394b95644","5c8f491bf52909811edc8409","560100ac8dc7a24394b9562c","560100ac8dc7a24394b95665","56de080050238a16476195f2","560100ac8dc7a24394b95645","560100ac8dc7a24394b9566e","623b3ef28863b92839774246"],"conferenceIds":null,"divisionIds":["54457dd2300969b132fcfe2a"],"teamAId":null,"teamBId":null,"type":null,"offensiveRole":null,"transferStatus":null,"comparisonGroupId":null,"view":null,"take":6000,"sportId":"570aaedc46c5d11de0f8c0bc"}'

res <- httr::POST(url = 'https://sport.synergysportstech.com/api/leagues/54457dce300969b132fcfb38/teamswithstats', httr::add_headers(.headers=headers), body = data)
json <- content(res, as = "text")
team_info <- as.data.frame(fromJSON(json)$result) #%>% select(fullName, id)
write_csv(team_info, "team_info.csv")

# STEP 3: GET GAME ID FROM EACH TEAM

team_info <- read_csv("team_info.csv", show_col_types = F)

## Get all of the game information

headers = c(
  `Content-Type` = 'application/json; charset=utf-8',
  `Accept` = 'application/json, text/plain, */*',
  `Authorization` = token,
  `Accept-Language` = 'en-CA,en-US;q=0.9,en;q=0.8',
  `Accept-Encoding` = 'gzip, deflate',
  `Host` = 'basketball.synergysportstech.com',
  `Origin` = 'https://apps.synergysports.com',
  `Content-Length` = '1229',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.2 Safari/605.1.15',
  `Referer` = 'https://apps.synergysports.com/',
  `Connection` = 'keep-alive',
  `newrelic` = 'eyJ2IjpbMCwxXSwiZCI6eyJ0eSI6IkJyb3dzZXIiLCJhYyI6IjE0MDM4ODIiLCJhcCI6IjExMjAwMjIxMjMiLCJpZCI6IjUyMzRiMjYyYzYzMGFiY2IiLCJ0ciI6IjNjMTNlMjdlZDMwOWIwYWZmY2ZlYTVjNWY0NWRhOGYwIiwidGkiOjE3MDg2Mjk4NDQ2MDUsInRrIjoiMTM4NDI3MyJ9fQ==',
  `X-SYNERGY-CLIENT` = 'ProductVersion=2024.02.21.827; ProductName=Basketball.TeamSite'
)

data = '{"excludeGamesWithoutCompetition":true,"skip":0,"take":1500,"sort":"utc:desc","team1Ids":["55a494f01e31501db4413551","54457de7300969b132fd0aa7","54457de8300969b132fd0aba","54457de7300969b132fd0aa1","54457de2300969b132fd087d","5581a1681e31501db4411bf9","54457de3300969b132fd08f3","55a494f01e31501db4413553","5cc9e3de7d52cb39cab68da0","54457de6300969b132fd0a60","54457de7300969b132fd0aa5","54457de7300969b132fd0aaa","54457de7300969b132fd0aab","54457de7300969b132fd0aa0","54457de7300969b132fd0aa9"],"seasonIds":["64da35a80d288f7495c0bdca"],"endDate":"2024-02-22T19:24:04.604Z","statuses":[2],"competitionIds":["560100ac8dc7a24394b95643","56d89e6d50238a164760b711","560100ac8dc7a24394b95617","560100ac8dc7a24394b95637","560100ac8dc7a24394b95684","560100ac8dc7a24394b95685","560100ac8dc7a24394b95674","56010b438dc7a25554523b23","623b3ee36b7ff0a2745e33be","560100ac8dc7a24394b95655","560100ac8dc7a24394b95653","56d89e6d50238a164760b710","56d89e6d50238a164760b70f","560100ac8dc7a24394b95662","560100ac8dc7a24394b95632","560100ac8dc7a24394b95644","5c8f491bf52909811edc8409","560100ac8dc7a24394b9562c","560100ac8dc7a24394b95665","56de080050238a16476195f2","560100ac8dc7a24394b95645","560100ac8dc7a24394b9566e","623b3ef28863b92839774246"]}'
data1 = paste0('{"excludeGamesWithoutCompetition":true,"skip":0,"take":1500,"sort":"utc:desc","team1Ids":[', paste0('"',team_info$id, collapse = '",'), "]}")

res <- httr::POST(url = 'https://basketball.synergysportstech.com/api/games', 
                  httr::add_headers(.headers=headers)
                  , body = data)

res$status_code
json <- content(res, as = "text")
game_info <- as.data.frame(fromJSON(json)$result) # the ID column has all of the game id's

# check to see game id's that we already have 

all_event_data <- read_csv("all_event_data.csv", show_col_types = F)

existing_ids <- all_event_data %>% unnest(game, names_sep = "_") %>% select(game_id) %>% distinct()

new_game_info <- game_info %>% filter(!id %in% existing_ids)

# notice how each game has a unique id. Once you find a list of these, you can 
# scrape and combine data from multiple games. Light work
event_data <- all_event_data
for (game_id in new_game_info$id) {
url <- paste0("https://basketball.synergysportstech.com/api/games/", game_id, "/events")

# Make the request to get the event data from the backend database
request <- GET(url, add_headers("Authorization" = token))
json <- content(request, as = "text")
new_data <- as.data.frame(fromJSON(json)$result) %>% janitor::clean_names()
event_data <- bind_rows(event_data, event_data)
}

write_csv(event_data, "all_event_data.csv")

## STEP 4: PLOT SHOT DATA

shot_data <- event_data %>%
  filter(!if_any(c(shot_x, shot_y), is.na)) %>%
  unnest(offense, names_sep = "_") %>%
  mutate(name = if_else(str_detect(name, "Make"), "make", "miss"))

layer <- image_read_svg("nba-court.svg")
p <- grid::rasterGrob(layer, interpolate = F)

  #geom_basketball("nba", display_range = "full", rotation = 270) + 
    ggplot() + 
  geom_point(data = shot_data, mapping = aes(x = shot_x, y = shot_y, color = name), size = 3) +
    annotation_custom(p, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + 
    facet_wrap(~paste(offense_name, "\n")) + theme_bw()
  

# TODO: Standardize the (x,y) coordinates
# TODO: get a properly scaled shot background
  
    
    
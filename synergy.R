suppressMessages(here::i_am("DVW Downloader.R"))
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, httr, jsonlite, sportyR, janitor, magick)

# Get the access token from Sources > login > Fetches > token after clicking 
# 'Inspect Element'. Access tokens last for 1 hour so the one you see here is expired
token <- paste("Bearer", "eyJhbGciOiJSUzI1NiIsImtpZCI6IjVDNTM1N0E1OThCOTU0MzhDNjM3RTQxQTlGNjc5ODZBIiwidHlwIjoiYXQrand0IiwiY3R5IjoiSldUIn0.eyJpc3MiOiJodHRwczovL2F1dGguc3luZXJneXNwb3J0c3RlY2guY29tIiwibmJmIjoxNzA3MzIxNTA3LCJpYXQiOjE3MDczMjE1MDcsImV4cCI6MTcwNzMyNTEwNywiYXVkIjpbImFwaS5jb25maWciLCJhcGkuc2VjdXJpdHkiLCJhcGkuYmFza2V0YmFsbCIsImFwaS5zcG9ydCIsImFwaS5lZGl0b3IiLCJodHRwczovL2F1dGguc3luZXJneXNwb3J0c3RlY2guY29tL3Jlc291cmNlcyJdLCJzY29wZSI6WyJvcGVuaWQiLCJhcGkuY29uZmlnIiwiYXBpLnNlY3VyaXR5IiwiYXBpLmJhc2tldGJhbGwiLCJhcGkuc3BvcnQiLCJhcGkuZWRpdG9yIiwib2ZmbGluZV9hY2Nlc3MiXSwiYW1yIjpbInB3ZCJdLCJjbGllbnRfaWQiOiJjbGllbnQuYmFza2V0YmFsbC50ZWFtc2l0ZSIsInN1YiI6IjVkZWVhZmJkODg2MDRiYTY0MDY4NjUwMyIsImF1dGhfdGltZSI6MTcwNzMyMTUwNCwiaWRwIjoibG9jYWwiLCJlbWFpbCI6ImluZm9AdGhlc3RpbmdlcnMuY2EiLCJuYW1lIjoiQ29hY2hpbmcgU3RhZmYiLCJzaWQiOiIyOUUwMzFGODQzMDU4NDk5NjUyNjM3Nzc3MEQ2MDJFMyJ9.SSBVtClDNEQBp6t-TfkNok-ILBIHDYltU2oiYACJ7JvnmIiV7jyyI62Pz3fZsOyARiXgr6t0vwmrauSRGWhUFUIyuz9CUlfncagGElR7yfCzJxOqIjLo2yc8FSXTAK3o-GQiDB31UD4TxQOK3p2rIu5NHzGc8nJGXJWed42Aj98emNl6tp9J2WQvoG-kIPGhtrtQwVIrCE6ieNpBJNEPKdSwmec73o5alhtp596_1XGFE5ftvU7CgF4RYmP0oGAvvE2g_NcJPDkZk5Bk43MNJmFQh6wQ2WkjQJS6yP62Cq6uvzJRNV3uZz9wzAywgM03rLiNapgtpAsDZ6SC46XaEQ")

# notice how each game has a unique id. Once you find a list of these, you can 
# scrape and combine data from multiple games. Light work
url <- "https://basketball.synergysportstech.com/api/games/651af29cdc04cb22742365c8/events"

# Make the request to get the event data from the backend database
request <- GET(url, add_headers("Authorization" = token))
json <- content(request, as = "text")
event_data <- as.data.frame(fromJSON(json)$result)

glimpse(event_data)

shot_data <- event_data %>% janitor::clean_names() %>%
  filter(!if_any(c(shot_x, shot_y), is.na)) %>%
  unnest(offense, names_sep = "_") %>%
  mutate(name = if_else(str_detect(name, "Make"), "make", "miss"))

layer <- image_read_svg("nba-court.svg")
p <- grid::rasterGrob(layer, interpolate = F)

  #geom_basketball("nba", display_range = "full", rotation = 270) + 
    ggplot() + 
  geom_point(data = shot_data, mapping = aes(x = shot_x, y = shot_y, color = name), size = 3) +
    #annotation_custom(p, xmin = -Inf, xmax = Inf, ymin = -100, ymax = Inf) + 
    facet_wrap(~paste(offense_name, "\n")) + theme_bw()

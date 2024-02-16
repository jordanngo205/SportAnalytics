library(shiny)

ui <- fluidPage(
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
               # Repeat the above tr for as many reserve rows as you need
             )
  )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)

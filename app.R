## BUG IN 2003-04


library(shiny)
library(tidyverse)
#library(profvis)
library(plotly)
library(data.table)
library(rsconnect)
library(fasttime)


# Create the data
# Could be in server, but can't figure out using unique values from server df for selectInput
# This is where the data is from: http://www.football-data.co.uk/englandm.php

form_length = 5

## DEFINE URL ELEMENTS BY LEAGUE, USEFUL BELOW -----
prem = "/E0.csv"
league = prem

start_year = 2002
end_year = 2021

read_results = function(league = "/E0.csv", first_year) {
  
  second_year = first_year+1
  first_year_trimmed = str_sub(as.character(first_year), 3, 4)
  second_year_trimmed = str_sub(as.character(second_year), 3, 4)
  url = paste("https://www.football-data.co.uk/mmz4281/", 
              first_year_trimmed, second_year_trimmed, league, sep = "")
  
  
  # Create running points and goal diff totals by team
  if (first_year == 2003) {   
    performance_by_opponent_strength = read_csv(url) # data.table::fread is faster but has bug for 2003-04
  } else{
    performance_by_opponent_strength = data.table::fread(url)
  }
  
  performance_by_opponent_strength = performance_by_opponent_strength %>%
    mutate(season = first_year)
  
  return(performance_by_opponent_strength)
  
}

performance_by_opponent_strength_function = function(results_table) {
  
  #browser()
  
  performance_by_opponent_strength = results_table %>%
    mutate(Date = case_when(Date == "17/05/2016" ~ "15/05/2016",
                            TRUE ~ Date)) %>% # manual fix needed for delayed MNU-BOU game
    rename(date = Date) %>%
    filter(date != "") %>%
    select(1:10, season) %>%
    group_by(date) %>%
    mutate(fixture_id = paste(date, row_number(), sep = "_")) %>%
    pivot_longer(cols = c(HomeTeam, AwayTeam), names_to = "venue", values_to = "team") %>%
    mutate(gd_gw = case_when(venue == "HomeTeam" ~ (FTHG - FTAG),
                             venue == "AwayTeam" ~ (FTAG - FTHG)),
           pts_gw = case_when(venue == "HomeTeam" & FTHG > FTAG ~ 3,
                              venue == "HomeTeam" & FTHG < FTAG ~ 0,
                              venue == "AwayTeam" & FTAG > FTHG ~ 3,
                              venue == "AwayTeam" & FTAG < FTHG ~ 0,
                              TRUE ~ 1),
           club = team,
           team = paste(team, sprintf('%0.2d', season-2000), sprintf('%0.2d', season-1999),   
                        sep = "_"),
           gls_gw = case_when(venue == "HomeTeam" ~ FTHG,
                              venue == "AwayTeam" ~ FTAG)) %>%
    group_by(team) %>%
    mutate(gw = row_number(),
           gd = cumsum(gd_gw),
           pts = cumsum(pts_gw),
           gls = cumsum(gls_gw)) %>%
    select(date, fixture_id, gw, team, club, season, gd, pts, gls, pts_gw) %>%
    
    # Table position for every club on every date
    mutate(date = lubridate::dmy(date)) %>%
    group_by(season, date) %>%
    arrange(desc(pts), desc(gd), desc(gls)) %>%
    mutate(pos = row_number()) %>%
    
    # Table position for every club at end of season
    # assumes every team plays final game on final day of season
    ungroup() %>%
    group_by(team) %>%
    mutate(posfinal = pos[date == max(date, na.rm = TRUE)]) %>% 
    
    # Add form streaks
    group_by(team) %>%
    arrange(gw) %>%
    mutate(pts_streak = RcppRoll::roll_sum(pts_gw, n = form_length, fill = NA, align = "right")) %>%
    
    # Select the useful columns
    select(fixture_id, team, club, season, pts_gw, posfinal, pts_streak) %>%
    
    # Join opponents for each fixture back together 
    left_join(x = ., y = ., 
              by = "fixture_id",
              suffix = c("1", "2")) %>%
    
    # keep useful rows only
    filter(team1 != team2) %>%
    
    # sort opponent form and final position into buckets
    mutate(opponent_form = case_when(pts_streak2 <= 4 ~ "0-4",
                                     pts_streak2 <= 9 ~ "5-9",
                                     pts_streak2 <= 15 ~ "10-15",
                                     TRUE ~ NA_character_),
           opponent_finish = case_when(posfinal2 <= 6 ~  "1-6",
                                       posfinal2 <= 14 ~ "7-14",
                                       posfinal2 <= 20 ~ "15-20",
                                       TRUE ~ NA_character_)) %>%
    
    # summarise performance by team
    select(team1, club1, season1, posfinal1, pts_gw1, opponent_form, opponent_finish) %>%
    pivot_longer(cols = c(opponent_form, opponent_finish), names_to = "opponent_strength", values_to = "value") %>%
    mutate(value = factor(value, levels = c("0-4", "5-9", "10-15", "15-20", "7-14", "1-6"))) %>%
    group_by(team1, club1, season1, opponent_strength, value) %>%
    summarise(posfinal = first(posfinal1),
              games = n(),
              ppg = mean(pts_gw1, na.rm = TRUE)) %>%
    relocate(posfinal, .after= team1)
  
  return(performance_by_opponent_strength)
  
}

performance_by_opponent_strength_all_seasons = mapply(read_results, prem, 2002:2021) %>%
  data.table::rbindlist(fill = TRUE) %>%
  performance_by_opponent_strength_function() %>%
  # compare performance to teams who finished in same position i.e. similar quality
  group_by(posfinal, opponent_strength, value) %>%
  mutate(ppg_vs_pos_avg = ppg - mean(ppg, na.rm = TRUE)) %>%
  group_by(opponent_strength, value, club1) %>%
  complete(season1 = seq(start_year,end_year))

all_seasons = unique(performance_by_opponent_strength_all_seasons$season1)
names(all_seasons) = paste(all_seasons, "-", sprintf('%0.2d', all_seasons-1999), sep = "")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("They'll probably beat us..."),
    
    p("Does your team seem to gift results to opponents who otherwise can't buy a win? This tool let's you test that theory. 
      Examine the performance of Premier League teams while playing against opponents in ",
      # https://www.freecodecamp.org/news/bold-font-in-html-font-weight-for-letters/
      # https://www.computerhope.com/issues/ch001391.htm
      # https://www.w3schools.com/cssref/pr_text_white-space.asp
      span(" Good ", style="background-color: #619CFF;color: #ffffff;font-weight: bold;white-space: pre"), ", ",
      span(" Average ", style="background-color: #00BA38;color: #ffffff;font-weight: bold;white-space: pre"), "and ",
      span(" Bad ", style="background-color: #F8766D;color: #ffffff;font-weight: bold;white-space: pre"), "form."),
    br(),
    p("You can view a team's absolute performance, or performance vs the average for teams finishing in the same league position (both in points per game, ppg). 
      For example, the asbolute scale shows Arsenal's worsening results against teams in bad form since 2016-17. 
      But so does the vs average scale: Arsenal dropped down the table over 5 years but also started performing worse against opponents in bad form than you would expect of a mid table team. Yikes."),
    br(),
    p("Click and drag to zoom on an area in a chart, and toggle groups on/off by clicking legend entries. Toggle between recent form and final league position in the Opponent strength box. 
      Click By Season below to compare all teams from one year, and click Methods for an explanation of how form is calculated."),

    # Inputs: which metric to look at, which team to look at?
    fluidRow(
      column(2, selectInput("opponent_strength_input", "Opponent strength metric", choices = c("Recent form" = "opponent_form", "Final position" = "opponent_finish"), selected = "Recent form")),
      column(2, selectInput("pts_input", "Performance scale", choices = c("Absolute" = "ppg", "vs Average*" = "ppg_vs_pos_avg"), selected = "Absolute")),
      br()
      ),
    
    tabsetPanel(type = "tabs",
                br(),
                
                tabPanel("By Team", 
                         selectInput("club_input", "Chosen team", choices = unique(performance_by_opponent_strength_all_seasons$club1), selected = "Arsenal"),
                         plotlyOutput("team_performance_line"), plotlyOutput("team_performance_bar")),
                tabPanel("By Season", 
                         selectInput("season_input", "Chosen season", choices = all_seasons, selected = 2021),
                         plotlyOutput("season_performance_bar")),
                tabPanel("Methods",
                         h4("Source data"),
                         p("All data used in this tool comes from ", a(href = 'https://www.football-data.co.uk/englandm.php', 'football-data.co.uk'), 
                           ". I process the data and take credit for any errors you might find. If football-data.co.uk move or take down the data, then this tool will break. Maybe I'll try and fix it, maybe not ¯\\_(ツ)_/¯"),
                         br(),
                         
                         h4("Teams and Seasons included"),
                         p("I've chosen to look at the teams in the Premier league from 2002-03 to 2021-22. 
                           This is mainly because 20 is a round number that produces nice 4x5 faceted charts. 
                           It also means I'm looking at a total of 400 teams (20 teams x 20 seasons), which is another nice round number.
                           My data source does cover earlier seasons and the EFL and Conference, so I could (and might) extend the scope in future."),
                         br(),
                         
                         h4("Measuring form"),
                         p("For each team's games, the form of the opponent is categorised according to the number of points taken in the previous five games:"),
                         p(span(" Good ", style="background-color: #619CFF;color: #ffffff;font-weight: bold;white-space: pre"), " = 10-15 points"),
                         p(span(" Average ", style="background-color: #00BA38;color: #ffffff;font-weight: bold;white-space: pre"), " = 5-9 points"),
                         p(span(" Bad ", style="background-color: #F8766D;color: #ffffff;font-weight: bold;white-space: pre"), " = 0-4 points"),
                         p("Why five games to measure recent form? Seems about right. It does mean form can only be measured from the 6th game onwards. 
                           So performance by opponent form is measured over 33 games per season. There is no guarantee that every team will face an opponent in good/bad/avaerage form in a season.
                           But in practice they do: all teams in all seasons have faced at least one opponent in each of the three categories. 
                           In fact, 395/400 teams have *at least* three games against each of the three categories.")
    )

)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # https://stackoverflow.com/questions/48106504/r-shiny-how-to-display-choice-label-in-selectinput
  opponent_strength_input_vector <- c("Recent form" = "opponent_form", "Final position" = "opponent_finish")
  opponent_strength_input_label = reactive({tolower(names(opponent_strength_input_vector)[opponent_strength_input_vector == input$opponent_strength_input])})

  team_performance_line_data = reactive({
    
    performance_by_opponent_strength_all_seasons %>%
      dplyr::filter(club1 == input$club_input,
                    opponent_strength == input$opponent_strength_input,
                    !is.na(value)) %>%
      mutate(value = fct_recode(value, Bad = "0-4", Average = "5-9", Good = "10-15",
                                       `Bottom Six` = "15-20", `Midtable` = "7-14", `Top Six` = "1-6"),
             ppg = round(ppg, 3),                        # Don't like doing this but can't
             ppg_vs_pos_avg = round(ppg_vs_pos_avg, 3))  # figure out how to round in the tooltip
    
  })

  
  team_performance_bar_data = reactive({
    
    performance_by_opponent_strength_all_seasons %>%
      dplyr::filter(club1 == input$club_input,
                    opponent_strength == input$opponent_strength_input,
                    !is.na(value)) %>%
      mutate(value = fct_recode(value, Bad = "0-4", Average = "5-9", Good = "10-15",
                                `Bottom Six` = "15-20", `Midtable` = "7-14", `Top Six` = "1-6"),
             season1 = paste(season1, sprintf('%0.2d', season1-1999), sep = "-"),
             ppg = round(ppg, 3),                        # Don't like doing this but can't
             ppg_vs_pos_avg = round(ppg_vs_pos_avg, 3))  # figure out how to round in the tooltip
    
  })
  
  season_performance_bar_data = reactive({
    
    performance_by_opponent_strength_all_seasons %>%
      dplyr::filter(season1 == input$season_input,
                    opponent_strength == input$opponent_strength_input,
                    !is.na(value),
                    !is.na(team1)) %>%
      mutate(value = fct_recode(value, Bad = "0-4", Average = "5-9", Good = "10-15",
                                `Bottom Six` = "15-20", `Midtable` = "7-14", `Top Six` = "1-6"),
             season1 = paste(season1, sprintf('%0.2d', season1-1999), sep = "-"),
             ppg = round(ppg, 3),                        # Don't like doing this but can't
             ppg_vs_pos_avg = round(ppg_vs_pos_avg, 3))  # figure out how to round in the tooltip
    
  })
  
  
  output$team_performance_line = plotly::renderPlotly({
  ggplotly(
    team_performance_line_data() %>%
      # https://stackoverflow.com/a/45949453
      ggplot(aes(season1, colour = value, group=1,
                 text = paste("Season: ", paste(season1, sprintf('%0.2d', season1-1999), sep = "-"), "\n",
                              "Opponent ", opponent_strength_input_label(), ": ", value, sep = ""))) +
      geom_hline(yintercept = 0, color = "black", size = 1) +
      geom_line(aes_string(y = input$pts_input), size = 0.8) +
      geom_point(aes_string(y = input$pts_input)) +
      theme_minimal() +
      scale_x_continuous(breaks=seq(start_year,end_year,2),
                         labels = paste(seq(start_year,end_year,2),sprintf('%0.2d', seq(start_year-1999,end_year-1999, 2)), sep = "-")) + 
      labs(colour = paste("Opponent", opponent_strength_input_label()),
           title = paste(input$club_input, "performance by",
                         opponent_strength_input_label(),
                         "of opponent"),
           x = "", y = "")
  , tooltip = c("text", "y"))
  })
  
  
  output$team_performance_bar = plotly::renderPlotly({
    ggplotly(
      team_performance_bar_data() %>%
      ggplot(aes(value, fill = value,
                 text = paste("Opponent ", opponent_strength_input_label(), ": ", value, sep = ""))) +
      theme_minimal() +
      geom_hline(yintercept = 0, color = "black", size = 1) +
      geom_col(aes_string(y = input$pts_input)) +
      facet_wrap(~season1) +
      labs(fill = paste("Opponent", opponent_strength_input_label()),
           title = paste(input$club_input, "performance by",
                         opponent_strength_input_label(),
                         "of opponent"),
           x = "", y = "") +
      theme(axis.text.x = element_blank())
      , tooltip = c("text", "y"))
  })
  
  
  output$season_performance_bar = plotly::renderPlotly({
    ggplotly(
      season_performance_bar_data() %>%
        ggplot(aes(value, fill = value,
                   text = paste("Opponent ", opponent_strength_input_label(), ": ", value, sep = ""))) +
        theme_minimal() +
        geom_hline(yintercept = 0, color = "black", size = 1) +
        geom_col(aes_string(y = input$pts_input)) +
        facet_wrap(~club1) +
        labs(fill = paste("Opponent", opponent_strength_input_label()),
             title = paste("Season", input$season_input, "- all clubs performance by",
                           opponent_strength_input_label(),
                           "of opponent"),
             x = "", y = "") +
        theme(axis.text.x = element_blank())
      , tooltip = c("text", "y"))
  })
    
  }
  

# Run the application 
shinyApp(ui = ui, server = server)

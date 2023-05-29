library(shiny)
library(data.table)
library(DT)
library(plotly)
library(shinythemes)
library(shinydashboard)
library(readxl)
library(stringr)
library(tidyverse)
library(rvest)
library(dygraphs)
library(shinyWidgets)
source('functions.R')

Sys.setlocale(locale= "English")

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "NBA Standings"),
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("home", lib = "glyphicon")),
    uiOutput('my_ticker_e'),
    uiOutput('my_ticker_w'),
    uiOutput('my_slider'),
    uiOutput('my_date'),
    menuItem(paste0("Standings within season"), tabName = "plot", icon = icon("signal", lib = "glyphicon")),
    menuItem("Standings table",tabName = "data",icon = icon("list-alt", lib = "glyphicon")),
    menuItem("Standings across seasons",tabName = "dg",icon = icon("transfer", lib = "glyphicon")),
    menuItem("Best record",tabName = "infobox",icon = icon("fire", lib = "glyphicon")),
    menuItem("Team logos",tabName = "logos",icon = icon("picture", lib = "glyphicon"))
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Controls",
                    p("Description: regular season NBA data from 2005 until 2019 covering team standings by day."),
                    p("Note: a regular season usually runs from November until mid-April."),
                    p("Select a team both from the Eastern and Western Conference.\n
                      Then, select a season and a day within that season."),
                    p("Output displayed and to be browsed:"),
                    p("1. Standings snake figure by day for each team within a conference."),
                    p("2. Daily standings on the selected day in both conferences."),
                    p("3. Dygraph figure of the end-of-season standings for the selected teams."),
                    p("4. Infobox for the current best records on the selected day in both conferences."),
                    p("5. Official logos of the selected teams."),
                    tags$a(href="https://www.basketball-reference.com", "Data was obtained via scraping from here.")))
      ),
      tabItem(tabName = "plot",
              h2("Season standings after 1st of January\n"),
              h3("Eastern conference\n"),
              fluidRow(plotlyOutput('data_plot_east',width = '100%',height = '600px')),
              h3("Western conference\n"),
              fluidRow(plotlyOutput('data_plot_west',width = '100%',height = '600px'))
      ),
      tabItem(tabName = "data",
              h2("Daily standings"),
              fluidRow(column(width = 6,dataTableOutput('my_data_east',width = '100%')),
                       column(width = 6,dataTableOutput('my_data_west',width = '100%')))
      ),
      tabItem(tabName = "dg",
              h2("Standings at the end of regular seasons\nfor the selected teams\n"),
              h3("Eastern conference\n"),
              fluidRow(dygraphOutput('data_dg_east',width = '90%',height = '600px')),
              h3("Western conference\n"),
              fluidRow(dygraphOutput('data_dg_west',width = '90%',height = '600px'))
      ),
      tabItem(tabName = "infobox",
              h2("Teams with best record"),
              fluidRow(
                infoBoxOutput("pos_record_east"),
                infoBoxOutput("pos_record_west")
              )
      
      ),
      tabItem(tabName = "logos",
              h2("Logos for the selected teams\n"),
              fluidRow(
                column(6,
                       h3("Eastern conference\n")),
                column(6,
                       h3("Western conference\n"))
              ),
              fluidRow(
                #h3("Eastern conference\n"),
                column(
                  6,
                  htmlOutput(outputId = "image_east",width = "49%")
                ),
                #h3("Western conference\n"),
                column(
                  6,
                  htmlOutput(outputId = "image_west",width = "49%")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  standings_west <-data.table(get_standings_colors("west"))
  standings_east <-data.table(get_standings_colors("east"))
  
  
  output$my_ticker_e <- renderUI({
    selectInput('my_ticker_e', label = 'Select an eastern team', choices = setNames(sort(unique(standings_east$team)),
                                                                        sort(unique(standings_east$team))), multiple = FALSE)
  })
  
  output$my_ticker_w <- renderUI({
    selectInput('my_ticker_w', label = 'Select a western team', choices = setNames(sort(unique(standings_west$team)),
                                                                           sort(unique(standings_west$team))), multiple = FALSE)
  })
  
  output$my_slider <- renderUI({
    sliderTextInput('my_slider', label = 'Select a season',
                #min = min(standings_west$season),
                #max = max(standings_west$season),
                choices = sort(unique(standings_west$season))
                #value = min(standings_west$season),sep = ""
                )
  })
  
  output$my_date <- renderUI({
    dateInput('my_date',label = "Select a day within that season", 
                       value = min(standings_west$date_new))
    
  })
  
  # observeEvent(output$my_slider, {
  #   year <- output$my_slider
  #   start_date <- as.Date(paste0(year-1, "-10-01"))
  #   end_date <- as.Date(paste0(year, "-05-01"))
  #   updateDateInput(session, "my_date", minDate = start_date, maxDate = end_date, value = as.Date(start_date+30))
  # })
  
  my_reactive_df_east <- reactive({
    df<- get_data_by_slider_and_date(input$my_date, input$my_slider,"east")
    return(df)
  })
  
  my_reactive_df_west <- reactive({
    df<- get_data_by_slider_and_date(input$my_date, input$my_slider,"west")
    return(df)
  })
  
  output$my_data_east <- DT::renderDataTable(server = FALSE, {
    render_df_with_all_download_buttons(my_reactive_df_east())
    
  })
  
  output$my_data_west <- DT::renderDataTable(server = FALSE, {
    render_df_with_all_download_buttons(my_reactive_df_west())
    
  })
  
  #Plot - slightly different data is necessary
  
  my_reactive_dfplot_east <- reactive({
    df<- get_data_by_slider_and_ticker(input$my_ticker_e, input$my_slider,"east")
    return(df)
  })
  
  my_reactive_dfplot_west <- reactive({
    df<- get_data_by_slider_and_ticker(input$my_ticker_w, input$my_slider,"west")
    return(df)
  })
  
  output$data_plot_west <- renderPlotly({
    get_plot_of_data(my_reactive_dfplot_west())
  })
  
  output$data_plot_east <- renderPlotly({
    get_plot_of_data(my_reactive_dfplot_east())
  })
  
  
  #Dygraph data
  my_reactive_dg_east <- reactive({
    df<- get_data_by_ticker_dg(input$my_ticker_e,"east")
    return(df)
  })
  
  my_reactive_dg_west <- reactive({
    df<- get_data_by_ticker_dg(input$my_ticker_w,"west")
    return(df)
  })
  
  output$data_dg_west <- renderDygraph({
    get_dg_of_data(my_reactive_dg_west())
  })
  
  output$data_dg_east <- renderDygraph({
    get_dg_of_data(my_reactive_dg_east())
  })
  
  #Add logos
  
  beginning <- "https://loodibee.com/wp-content/uploads/nba-"
  
  output$image_east <- renderUI({
    url <- ifelse(input$my_ticker_e == "New Jersey Nets",
                  paste0("https://upload.wikimedia.org/wikipedia/de/a/a2/New_Jersey_Nets_logo.svg"),
                  paste0(beginning,
                  gsub(" ","-",tolower(input$my_ticker_e)),"-logo.png"))
    tags$img(src = url,width = '60%',height = '50%',align = 'center')

  })
  
  output$image_west <- renderUI({
    url <- ifelse(input$my_ticker_w=="Denver Nuggets",
                  paste0(beginning,"denver-nuggets-logo-2018.png"),
                  ifelse(input$my_ticker_w == "Los Angeles Clippers",
                         paste0(beginning,"la-clippers-logo.png"),
                         ifelse(input$my_ticker_w == "Seattle Supersonics",
                                "https://upload.wikimedia.org/wikipedia/en/thumb/a/a4/Seattle_SuperSonics_logo.svg/1200px-Seattle_SuperSonics_logo.svg.png",
                                ifelse(str_detect(input$my_ticker_w,"Hornets"),"https://cdn.freebiesupply.com/logos/large/2x/new-orleans-hornets-1-logo-png-transparent.png",
                                       paste0(beginning,
                                              gsub(" ","-",tolower(input$my_ticker_w)),"-logo.png")))))
    tags$img(src = url,width = '60%',height = '50%',align = 'center')
    
  })
  
  # output$image <- renderImage({
  #   #tags$img(src = "https://loodibee.com/wp-content/uploads/nba-san-antonio-spurs-logo.png")
  # 
  #   #team <- gsub("-", " ", input$teamInput) # Convert hyphens to spaces in team name
  #   url <- paste0("https://loodibee.com/wp-content/uploads/nba-","brooklyn-nets","-logo.png")
  #   # list(src = url,
  #   #      contentType = 'image/png',
  #   #      width = 400,
  #   #      height = 300,
  #   #      alt = "This is alternate text")
  #   list(src = url, alt = "Team Logo", width = "50%")
  # }, deleteFile = FALSE)
  
  # team_max_win <- reactive({
  #   team_max_win_val = ifelse(as.numeric(my_reactive_df_east()$`Win PCT`)>
  #                            as.numeric(my_reactive_df_west()$`Win PCT`),
  #                          my_reactive_df_east()$Team[1],
  #                          my_reactive_df_west()$Team[1])
  #   
  #   return(team_max_win_val)
  #   
  # })
  
  
  output$pos_record_east <- renderInfoBox({
    infoBox(
      "Best eastern record",
      ifelse(nrow(my_reactive_df_east())>0,paste0(my_reactive_df_east()$Team[1],":\n",
             round(max(as.numeric(my_reactive_df_east()$`Win PCT`)),
                                             digits = 2)*100,"%"),
             "No data"),
      color = "green",
      icon = icon("chevron-right", lib = "glyphicon")
    )
  })
  
  output$pos_record_west <- renderInfoBox({
    infoBox(
      "Best western record",
      ifelse(nrow(my_reactive_df_west())>0,paste0(my_reactive_df_west()$Team[1],":\n",
             round(max(as.numeric(my_reactive_df_west()$`Win PCT`)),
                   digits = 2)*100,"%"),
             "No data"),
      color = "green",
      icon = icon("chevron-left", lib = "glyphicon")
    )
  })
  
}

shinyApp(ui, server)

library(shiny)
library(jsonlite)
library(data.table)
library(httr)
library(rtsdata)
library(DT)
library(TTR)
library(plotly)
library(shinythemes)
library(shinydashboard)
library(markdown)
library(readxl)
library(stringr)
library(tidyverse)
library(rvest)
source('functions.R')

Sys.setlocale(locale= "English")

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "NBA Standings"),
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("home", lib = "glyphicon")),
    menuItem("Standing trajectories", tabName = "plot", icon = icon("signal", lib = "glyphicon")),
    menuItem("Standings table",tabName = "data",icon = icon("list-alt", lib = "glyphicon")),
    menuItem("Best record",tabName = "infobox",icon = icon("fire", lib = "glyphicon"))
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Controls",
                    uiOutput('my_ticker_e'),
                    uiOutput('my_ticker_w'),
                    uiOutput('my_slider'),
                    uiOutput('my_date'),
                    p("Description: regular season NBA data from 2005 until 2019 covering team standings by day."),
                    p("Note: a regular season usually runs from November until mid-April."),
                    tags$a(href="https://www.basketball-reference.com", "Data is obtained via scraping from here.")))
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
      tabItem(tabName = "infobox",
              h2("Teams with best record"),
              fluidRow(
                infoBoxOutput("pos_record_east"),
                infoBoxOutput("pos_record_west")
              ),
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
    sliderInput('my_slider', label = 'Select a season',
                min = min(standings_west$season),
                max = max(standings_west$season),
                value = min(standings_west$season),
                sep = "")
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

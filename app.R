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

source('functions.R')

#aims: dropdown menu, date picker, table, graph

# ui <- fluidPage(
#   uiOutput('my_ticker'),
#   textOutput('ticker_id'),
#   dateRangeInput("my_date",label = h3("Date range"),start = Sys.Date()-365,
#                  end = Sys.Date()),
#   #plot
#   plotlyOutput('data_plot',width = '80%',height = '600px'),
#   #df
#   dataTableOutput('my_data')
# )

#ctrl shift c

# ui <- fluidPage(
#   sidebarLayout(
#     sidebarPanel(
#       uiOutput('my_ticker'),
#         textOutput('ticker_id'),
#         dateRangeInput("my_date",label = h3("Date range"),start = Sys.Date()-365,
#                        end = Sys.Date()),
#       tags$a(href="https://www.tradingview.com", "Data is available here.")
#       
#     ),
#     mainPanel(
#       tabsetPanel(
#       tabPanel("Plot",plotlyOutput('data_plot',width = '80%',height = '600px')),
#       tabPanel("Table",dataTableOutput('my_data'))
#       )
#     )
#   )
#   
# )


# ui <- fluidPage(
#     mainPanel(
#       tabsetPanel(
#         tabPanel("Control",
#                  wellPanel(uiOutput('my_ticker'),
#                            textOutput('ticker_id'),
#                            dateRangeInput("my_date",label = h3("Date range"),start = Sys.Date()-365,
#                          end = Sys.Date()),
#                          tags$a(href="https://www.tradingview.com", "Data is available here."))),
#         
#         tabPanel("Plot",plotlyOutput('data_plot',width = '80%',height = '600px')),
#         tabPanel("Table",dataTableOutput('my_data')),
#         
#       )
#     )
#   )

# ui <- navbarPage(title = "Stock app", theme = shinytheme("united"),
#                  tabPanel("Control",
#                           wellPanel(uiOutput('my_ticker'),
#                                     textOutput('ticker_id'),
#                                     dateRangeInput("my_date",label = h3("Date range"),start = Sys.Date()-365,
#                                                    end = Sys.Date()),
#                                     tags$a(href="https://www.tradingview.com", "Data is available here."))),
#                  tabPanel("Plot",plotlyOutput('data_plot',width = '80%',height = '600px')),
#                  tabPanel("Table",dataTableOutput('my_data')))

# ui <- navbarPage(title = "Stock app", theme = shinytheme("united"),
#                  tabPanel("Control",
#                           wellPanel(uiOutput('my_ticker'),
#                                     textOutput('ticker_id'),
#                                     dateRangeInput("my_date",label = h3("Date range"),start = Sys.Date()-365,
#                                                    end = Sys.Date()),
#                                     tags$a(href="https://www.tradingview.com", "Data is available here."))),
#                  tabPanel("Plot and data",
#                           fluidRow(
#                             column(width = 9,plotlyOutput('data_plot',width = '75%',height = '600px')),
#                             column(width = 3,dataTableOutput('my_data',width = '25%')))))

# ui <- dashboardPage(
#   dashboardHeader(title = "Stock app"),
#   dashboardSidebar(uiOutput('my_ticker'),
#                    textOutput('ticker_id'),
#                    dateRangeInput("my_date",label = h3("Date range"),start = Sys.Date()-365,
#                                                      end = Sys.Date()),
#                    tags$a(href="https://www.tradingview.com", "Data is available here.")),
#   dashboardBody(tabPanel(
#              fluidRow(
#                column(width = 12,textOutput('my_ticker'))),
#              fluidRow(
#                column(width = 12,plotlyOutput('data_plot',width = '100%',height = '600px'))),
#              fluidRow(
#                column(width = 12,"Raw data\n")),
#              fluidRow(
#                column(width = 12,dataTableOutput('my_data',width = '100%'))))
#     
#   )
# )

ui <- dashboardPage(
  dashboardHeader(title = "Stock app"),
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("home", lib = "glyphicon")),
    menuItem("Plot", tabName = "plot", icon = icon("signal", lib = "glyphicon")),
    menuItem("Data",tabName = "data",icon = icon("list-alt", lib = "glyphicon")),
    menuItem("Info box",tabName = "infobox",icon = icon("fire", lib = "glyphicon")),
    menuItem("Value box",tabName = "valuebox",icon = icon("usd", lib = "glyphicon"))
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Controls",
                    uiOutput('my_ticker'),
                    textOutput('ticker_id'),
                    dateRangeInput("my_date",label = h3("Date range"),start = Sys.Date()-365,
                                   end = Sys.Date()),
                    tags$a(href="https://www.tradingview.com", "Data is available here.")))
      ),
      tabItem(tabName = "plot",
              h2("Stock trajectory"),
              fluidRow(plotlyOutput('data_plot',width = '100%',height = '600px'))
      ),
      tabItem(tabName = "data",
              h2("Raw data"),
              fluidRow(dataTableOutput('my_data',width = '100%'))
      ),
      tabItem(tabName = "infobox",
              h2("On fire"),
              fluidRow(
                infoBoxOutput("pos_closing_share")
              ),
      ),
      tabItem(tabName = "valuebox",
              fluidRow(
                valueBoxOutput("market_cap")
              ),
      )
      
    )
  )
)

server <- function(input, output, session) {
  sp500 <-data.table(get_sp500())
  
  setorder(sp500, description)#-market_cap_basic
  
  output$pos_closing_share <- renderInfoBox({
    infoBox(
      "Closing\npositive", paste0(round(nrow(sp500[change>0,])/nrow(sp500),
                                        digits = 2)*100,"%"),
      color = ifelse(nrow(sp500[change>0,])/nrow(sp500)>0.5,"green","red")
    )
  })
  
  output$market_cap <- renderValueBox({
    valueBox(
      paste0(round(sum(sp500$market_cap_basic)/1e9,digits = 0)," Billion USD"), 
      "Market value",
      color = "orange"
    )
  })
  
  output$my_ticker <- renderUI({
    selectInput('ticker', label = 'Select a ticker', choices = setNames(sp500$name, sp500$description), multiple = FALSE)
  })
  output$ticker_id <- renderText(paste0("Company ID: ",input$ticker))
  
  
  my_reactive_df <- reactive({
    df<- get_data_by_ticker_and_date(input$ticker, input$my_date[1], input$my_date[2])
    df <- data.frame(lapply(df, function(y) if(is.numeric(y)) round(y, 2) else y)) 
    return(df)
  })
  
  
  # go to https://rstudio.github.io/DT/shiny.html
  # output$my_data <- DT::renderDataTable({
  #   render_df_with_buttons(my_reactive_df())
  # })
  
  output$my_data <- DT::renderDataTable(server = FALSE, {
    render_df_with_all_download_buttons(my_reactive_df())
    
  })
  
  output$data_plot <- renderPlotly({
    get_plot_of_data(my_reactive_df())
  })
  
}

shinyApp(ui, server)
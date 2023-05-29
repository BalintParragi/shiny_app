
get_standings_colors <- function(conf) {
  # df <- fread(paste0("https://raw.githubusercontent.com/BalintParragi/shiny_app/main/data/standing_",conf,"_byday.csv"),
  #                    encoding = "UTF-8") %>% 
  df <- fread(paste0("data/standing_",conf,"_byday.csv"),
              encoding = "UTF-8") %>% 
    mutate(conference = conf) %>% 
    filter(season<=2019)
  df <- df %>% 
    mutate(date_new = as.Date(paste(str_sub(date,-4),
                                  match(str_sub(date,1,3),month.abb),
                                  str_sub(date,5,-7),sep="-"))) 
  
  trim_col <- names(df)[grepl("\\d", names(df))]
  df <- df %>% 
    mutate_at(trim_col, funs(gsub("\\).*", ")", .))) %>% 
    select(-date) %>% 
    data.table()
  
  df_melt <- melt(df,id.vars=c(16:18)) %>% 
    arrange(season,conference,date_new) %>% 
    mutate(seed = as.numeric(gsub("[a-z]","",variable)),
           team = str_sub(value,1,3),
           standing = gsub("\\(([^)]+)\\)", "\\1",str_sub(value,5))) %>% 
    tidyr::separate(standing,into = c("wins","losses"),sep = "-") %>% 
    select(-value,variable) %>% 
    mutate(wins = as.numeric(wins),
           losses = as.numeric(losses)) %>% 
    group_by(season) %>% 
    mutate(reg_season = last(wins)+last(losses)) %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(matches_left = reg_season-wins-losses) %>%
    data.table()
  
  colors <- read_html("https://teamcolorcodes.com/nba-team-color-codes/") %>% 
    html_nodes("table") %>% html_table()
  colors <- colors[[4]] %>% 
    rename(team = `NBA Team Name`,
           color1 = `Color 1`,
           color2 = `Color 2`) %>% 
    dplyr::select(c(team,color1,color2))
  edit_col <- names(colors)[grepl("\\d", names(colors))]
  colors_edit <- colors %>% 
    mutate_at(edit_col, funs(gsub(".*\\#", "#", .))) %>% 
    mutate(code= toupper(str_sub(team,1,3)),
           code = ifelse(team=="Los Angeles Lakers","LAL",
                         ifelse(team=="Los Angeles Clippers","LAC",
                                ifelse(team=="San Antonio Spurs","SAS",
                                       ifelse(team=="Golden State Warriors","GSW",
                                              ifelse(team=="New Jersey Nets","NJN",
                                                     ifelse(team=="New York Knicks","NYK",
                                                            ifelse(team=="Oklahoma City Thunder","OKC",
                                                                   ifelse(team=="New Orleans Pelicans","NOP",
                                                                          ifelse(team=="Brooklyn Nets","BRK",
                                                                                 ifelse(team=="New Orleans Hornets","NOH",
                                                                                        ifelse(team=="New Orleans/Oklahoma City Hornets", "NOK",
                                                                                               ifelse(team =="Charlotte Hornets","CHO",code)))))))))))))
  
  colors_edit <- colors_edit %>% 
    #add teams that have changed name/location and not present anymore
    add_row(team = "New Orleans Hornets",color1 = NA,
            color2 = NA,code = "NOH") %>% 
    add_row(team = "New Orleans/Oklahoma City Hornets",color1 = NA,
            color2 = NA,code = "NOK") %>% 
    mutate(color1 = ifelse(is.na(color1),color1[code == "NOP"],color1),
           color2 = ifelse(is.na(color2),color2[code == "NOP"],color2)) %>% 
    add_row(team = "Charlotte Hornets",color1 = NA,
            color2 = NA,code = "CHA") %>% 
    mutate(color1 = ifelse(is.na(color1),color1[code == "CHO"],color1),
           color2 = ifelse(is.na(color2),color2[code == "CHO"],color2)) %>% 
    add_row(team = "Seattle Supersonics",color1 ="#00653A",color2="#FFC200",code = "SEA") %>% 
    add_row(team = "New Jersey Nets",color1 = NA,
            color2 = NA,code = "NJN") %>% 
    mutate(color1 = ifelse(is.na(color1),color1[code == "BRK"],color1),
           color2 = ifelse(is.na(color2),color2[code == "BRK"],color2))
  
  df_melt_colors <- df_melt %>% 
    rename(code = team) %>% 
    left_join(colors_edit,by="code") %>% data.table()

  return(df_melt_colors)
}

get_data_by_slider_and_date  <- function(day, slider,conf) {
  
  tryCatch({
    my_data <- get_standings_colors(conf)
    my_data <- my_data[date_new==as.Date(day)&season==slider,] %>% 
      mutate(wins_share = wins/(wins+losses),
             wins_share_char = ifelse(str_sub(round(wins_share,digits = 3),1,1)!="1",
                                      str_sub(round(wins_share,digits = 3),2),
                                      "1.000")) %>% 
      rowwise() %>% 
      mutate(wins_share_char = ifelse(nchar(wins_share_char)<4,
                                 paste0(wins_share_char,paste0(rep("0",4-nchar(wins_share_char)),collapse="")),
                                 wins_share_char),
             wins_share_char = ifelse(wins_share_char=="0000","0.000",wins_share_char)) %>% 
      dplyr::select(c(variable,team,wins,losses,wins_share_char,matches_left)) %>% 
      rename(Wins = wins,
             Losses = losses,
             `Win PCT` = wins_share_char,
             Team = team,
             Seed = variable,
             `Matches left` = matches_left)
    
    if ( nrow(my_data[complete.cases(my_data)==F,])> 0)  {
      my_data <- my_data[complete.cases(my_data),]
      if(nrow(my_data)==0){
        text<- paste0('Error: ', my_slider, ' # problem: empty dataframe ', ' time: ', Sys.time())
        stop(text)
      }
    }
    return(my_data)
  }, error=function(x) {
    print(x)
    return(data.table())
  })
  
}

get_data_by_slider_and_ticker  <- function(ticker, slider,conf) {
  
  tryCatch({
    my_data <- get_standings_colors(conf)
    my_data <- my_data[season==slider&date_new>=as.Date(paste0(slider,"-01-01")),] %>% 
      mutate(thick = ifelse(team == ticker,1.5,0.6))
    
    if(nrow(my_data[complete.cases(my_data)==F,])> 0)  {
      my_data <- my_data[complete.cases(my_data),]
      if(nrow(my_data)==0){
        text<- paste0('Error: ', my_ticker, ' # problem: empty dataframe ', ' time: ', Sys.time())
        stop(text)
      }
    }
    return(my_data)
  }, error=function(x) {
    print(x)
    return(data.table())
  })
  
}

render_df_with_all_download_buttons <- function(my_data) {
  return(DT::datatable(my_data,extensions = c('Buttons','FixedHeader'),filter = 'top', class = 'cell-border stripe',
                       rownames= FALSE,
                       options = list(dom = 'Blfrtip',scrollX = TRUE, fixedHeader = TRUE,
                                      pageLength = 15,lengthMenu = c(5,10,15),
                                      buttons = list(list(extend = "excel", text = "Download", 
                                                          filename =  paste0('standings-data-', Sys.Date()),  exportOptions = list(modifier = list(page = "all"))))
                       )))
  
}

get_plot_of_data <- function(data){
  tryCatch({
    p <- ggplot(mapping = aes(text = paste("Date: ", date_new,
                                 "<br>Team: ", code,
                                 "<br>Seed: ", seed))) +
      geom_line(data = data,
                aes(x = date_new, y = seed, group = code,
                    color = color1, size = thick)) +
      geom_line(data = data,
                aes(x = date_new, y = seed-0.1, group = code,
                    color = color2, size = thick/2)) +
      geom_text(data = data[date_new==max(date_new),],
                mapping=aes(x=date_new+2, y=seed,
                            label=code,color = color1), hjust = 0, size = 3.25)+
      theme_minimal()+
      labs(title = "Selected team is highlighted",x = "", y = "Seed") +
      theme(axis.text = element_text(size = 13, colour = "black"),
            axis.title = element_text(size = 13, colour = "black"),
            plot.title = element_text(hjust = 0.5, size = 14),
            #panel.spacing = unit(0.8, "lines"),
            panel.grid.major.y = element_blank(),
            plot.margin = margin(0.25,0.25,0.25,0.25,unit = "cm"),
            legend.position = "none")+
      scale_color_identity()+
      scale_size_identity()+
      scale_y_reverse(breaks = 1:15)+
      scale_x_date(date_labels = "%b",expand = c(0.025,0.025))
    pl <- ggplotly(p,tooltip = 'text')
    return(pl)
    
  },error=function(x){
    return(plotly_empty())
    })
  
  
}

get_data_by_ticker_dg  <- function(ticker,conf) {
  
  tryCatch({
    my_data <- get_standings_colors(conf)
    my_data <- my_data %>% 
      filter(team == ticker) %>% 
      group_by(season) %>% 
      filter(row_number()==n()) %>% 
      select(season,seed,team,color1)
    
    if(nrow(my_data[complete.cases(my_data)==F,])> 0)  {
      my_data <- my_data[complete.cases(my_data),]
      if(nrow(my_data)==0){
        text<- paste0('Error: ', my_ticker, ' # problem: empty dataframe ', ' time: ', Sys.time())
        stop(text)
      }
    }
    return(my_data)
  }, error=function(x) {
    print(x)
    return(data.table())
  })
  
}

get_dg_of_data <- function(data){
  tryCatch({
    team <- unique(data$team)
    color1 <- unique(data$color1)
    data <- data %>% select(-team,-color1)
    dygraph_obj <- dygraph(data,main = team) %>%
      dyRangeSelector(height = 20) %>%
      dyAxis("y", valueRange = c(15,0)) %>% 
      dySeries("seed", label = "Seed",color = color1,pointSize = 7,strokeWidth = 3.5) %>%
      dyLimit(8.5, "Playoff qualification",
              strokePattern = "dotted", color = "red") %>% 
      dyHighlight(highlightCircleSize = 3,
                  highlightSeriesBackgroundAlpha = 0.5, highlightSeriesOpts = list(),
                  hideOnMouseOut = TRUE) %>% 
      dyOptions(axisLabelFontSize = 18,axisTickSize = 12)
    return(dygraph_obj)
    
  },error=function(x){
    return("Not sufficient data")
  })
  
  
}

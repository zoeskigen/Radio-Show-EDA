#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(readr)
library(DT)
library(rvest)
library(dplyr)
library(ggplot2)
library(lubridate)
library(shinydashboard)

# Assuming you've already downloaded and processed the Spinitron data
spinitron_data <- read_csv("/Users/zoeskigen/Desktop/spinitron.csv")
spinitron_data$Date <- as.Date(spinitron_data$Date)


# Convert Date-time to Date type and ensure proper formatting
spinitron_data$Date <- as.Date(spinitron_data$`Date-time`)
spinitron_data$Time <- format(as.POSIXct(spinitron_data$Time, format="%H:%M:%S"), "%H:%M")
spinitron_data$Released <- as.numeric(spinitron_data$Released)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Roboto+Slab&display=swap');
      
      body {
        background-color: #F5E6D3;
        font-family: 'Roboto Slab', serif;
      }
      .content-box {
        background-color: #FFF9E6;
        border-radius: 10px;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        border: 2px solid #8B4513;
      }
      .stat-box {
        background-color: #FAEBD7;
        border-radius: 5px;
        padding: 10px;
        margin-bottom: 10px;
        text-align: center;
        border: 1px solid #D2B48C;
      }
      .stat-value {
        font-size: 24px;
        font-weight: bold;
        color: #8B4513;
      }
      .stat-label {
        font-size: 14px;
        color: #A0522D;
      }
      h1, h2, h3 {
        color: #8B4513;
      }
      .logo-container {
        text-align: center;
        margin-bottom: 20px;
      }
      .spinning-cat {
        width: 150px;
        height: 150px;
        animation: spin 10s linear infinite;
      }
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
    "))
  ),
  
  fluidRow(
    column(width = 12,
           div(class = "logo-container",
               tags$img(src = "https://example.com/path-to-your-cat-logo.png", class = "spinning-cat")
           ),
           h1("KSPC DJ Wrapped: Vintage Edition", align = "center"),
           br()
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #FFF9E6; border: 2px solid #8B4513;",
      selectInput("dj_name", "Select DJ Name", choices = unique(spinitron_data$`DJ Name`)),
      dateRangeInput("date_range", "Select Date Range",
                     start = min(spinitron_data$Date),
                     end = max(spinitron_data$Date)),
      checkboxInput("new_only", "Show Only New Releases", value = FALSE),
      checkboxInput("local_only", "Show Only Local Artists", value = FALSE)
    ),
    
    mainPanel(
      div(class = "content-box",
          h3("Top 10 Artists"),
          plotOutput("top_artists_plot")
      ),
      div(class = "content-box",
          h3("Top 10 Songs"),
          plotOutput("top_songs_plot")
      ),
      div(class = "content-box",
          h3("Top 3 Genres"),
          plotOutput("top_genres_plot")
      ),
      div(class = "content-box",
          h3("Genre Frequency Over Time"),
          plotOutput("genre_time_plot")
      ),
      div(class = "content-box",
          h3("Release Year Distribution"),
          plotOutput("release_year_hist")
      ),
      div(class = "content-box",
          h3("KSPC Total Stats"),
          verbatimTextOutput("kspc_stats")
      )
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    data <- spinitron_data %>% 
      filter(`DJ Name` == input$dj_name,
             Date >= input$date_range[1],
             Date <= input$date_range[2])
    
    if(input$new_only) {
      data <- data %>% filter(New == "Y")
    }
    
    if(input$local_only) {
      data <- data %>% filter(Local == "Y")
    }
    
    data
  })
  
  vintage_theme <- theme_minimal() +
    theme(
      text = element_text(family = "Roboto Slab"),
      plot.background = element_rect(fill = "#FFF9E6"),
      panel.background = element_rect(fill = "#FFF9E6"),
      axis.text = element_text(color = "#8B4513"),
      axis.title = element_text(color = "#8B4513"),
      legend.text = element_text(color = "#8B4513"),
      legend.title = element_text(color = "#8B4513")
    )
  
  output$top_artists_plot <- renderPlot({
    filtered_data() %>%
      count(Artist, sort = TRUE) %>%
      top_n(10) %>%
      ggplot(aes(x = reorder(Artist, n), y = n)) +
      geom_col(fill = "#D2691E") +
      coord_flip() +
      labs(x = "Artist", y = "Number of Plays") +
      vintage_theme
  })
  
  output$top_songs_plot <- renderPlot({
    filtered_data() %>%
      count(Song, sort = TRUE) %>%
      top_n(10) %>%
      ggplot(aes(x = reorder(Song, n), y = n)) +
      geom_col(fill = "#D2691E") +
      coord_flip() +
      labs(x = "Song", y = "Number of Plays") +
      vintage_theme
  })
  
  output$top_genres_plot <- renderPlot({
    filtered_data() %>%
      count(Genre, sort = TRUE) %>%
      top_n(3) %>%
      ggplot(aes(x = reorder(Genre, n), y = n, fill = Genre)) +
      geom_col() +
      coord_flip() +
      labs(x = "Genre", y = "Number of Plays") +
      scale_fill_manual(values = c("#D2691E", "#CD853F", "#DEB887")) +
      vintage_theme +
      theme(legend.position = "none")
  })
  
  output$genre_time_plot <- renderPlot({
    filtered_data() %>%
      count(Date, Genre) %>%
      ggplot(aes(x = Date, y = n, color = Genre)) +
      geom_line() +
      labs(x = "Date", y = "Number of Plays") +
      scale_color_brewer(palette = "Set3") +
      vintage_theme
  })
  
  output$release_year_hist <- renderPlot({
    filtered_data() %>%
      ggplot(aes(x = Released)) +
      geom_histogram(binwidth = 1, fill = "#D2691E", color = "#8B4513") +
      labs(x = "Release Year", y = "Count") +
      vintage_theme
  })
  
  output$kspc_stats <- renderText({
    total_data <- spinitron_data
    paste(
      "Total Songs Played:", nrow(total_data), "\n",
      "Unique Artists:", length(unique(total_data$Artist)), "\n",
      "Unique Genres:", length(unique(total_data$Genre)), "\n",
      "Date Range:", min(total_data$Date), "to", max(total_data$Date), "\n",
      "Number of DJs:", length(unique(total_data$`DJ Name`)), "\n",
      "New Releases Played:", sum(total_data$New == "Y", na.rm = TRUE), "\n",
      "Local Artists Played:", sum(total_data$Local == "Y", na.rm = TRUE), "\n",
      "Most Played Artist:", total_data %>% count(Artist, sort = TRUE) %>% slice(1) %>% pull(Artist), "\n",
      "Most Played Song:", total_data %>% count(Song, sort = TRUE) %>% slice(1) %>% pull(Song), "\n",
      "Most Common Genre:", total_data %>% count(Genre, sort = TRUE) %>% slice(1) %>% pull(Genre)
    )
  })
}

shinyApp(ui = ui, server = server)

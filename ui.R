library(shiny)
library(plotly)
options(scipen = 999)
library(shinyWidgets)
library(tidyverse)
library(lubridate)

### read data
getwd()
#setwd("C:/Users/Avinash/Documents/Python Scripts/project/Bookings.com")

room_tbl <- read_csv("room_rates_types.csv")
booking_tbl <- read_csv("booking_merged_file_v2.csv")
review_tbl <- read.csv("miami_review_only.csv")


### Data cleaning

##======= room data===========

room_tbl_data <- as.data.frame(str_split_fixed(room_tbl$dates, "_", n = 7))

room_tbl_data$date <- paste0(room_tbl_data$V4, "-", room_tbl_data$V3, "-", room_tbl_data$V2)

date <- room_tbl_data %>% 
    select(date) %>% 
    mutate(date = ymd(date))   

clean_room_tbl <- cbind(room_tbl,date) %>% 
    select(-X1, -dates) 

##========= booking data=========

booking_tbl_data <- as.data.frame(str_split_fixed(booking_tbl$date, "_", n = 7))

booking_tbl_data$dates <- paste0(booking_tbl_data$V4, "-", booking_tbl_data$V3, "-", booking_tbl_data$V2)

dates <- booking_tbl_data %>% 
    select(dates) %>% 
    mutate(dates = ymd(dates))   

clean_booking_tbl <- cbind(booking_tbl,dates) %>% 
    select(-X1, -date) %>% 
    rename(date = dates)

## Data aggregation

##===========room data=======

aggregate_room_tbl <- clean_room_tbl %>% 
    mutate(month = month(date, label = TRUE),
           price = gsub("\\$", "", price),
           price = gsub("US", "", price),
           price = gsub(",", "", price),
           price = as.numeric(price)) %>% 
    group_by(hotel, month) %>% 
    summarize(price = median(price, na.rm = TRUE)) 


##===========booking data=======

aggregate_booking_tbl <- clean_booking_tbl %>% 
    mutate(month = month(date, label = TRUE),
           total_reviews = as.numeric(total_reviews)) %>% 
    group_by(location, hotel_name, month) %>% 
    summarize(overall_rating = median(overall_rating, na.rm = TRUE),
              total_reviews = median(total_reviews, na.rm = TRUE)) %>% 
    rename(hotel = hotel_name) 

## Data join

merged_data <- aggregate_booking_tbl %>% 
    left_join(aggregate_room_tbl)



ui <- fluidPage(theme = "bootstrap.min.css",
                
    setBackgroundImage(src = "https://upload.wikimedia.org/wikipedia/commons/6/64/Hualien_Chihsingtan_Beach.jpg"),
    
    
    tabsetPanel(
        
        tabPanel("Seasons",fluid = T,
            titlePanel('Bookings.com'),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("location", label = h4("Select a location:"),
                                     choices = unique(merged_data$location),
                                     selected = list("Bahamas","Miami","Honolulu"),
                                     multiple = TRUE),
                         
                         selectInput("sentiment", label = h4("Sentiment Type"), 
                                     choices = list("positive", "negative"),
                                     selected = list("positive"),
                                     multiple = TRUE),
                         sliderInput('plotHeight', 'Height of plot (in pixels)', 
                                     min = 100, max = 500, value = 300)
                     ),
                     mainPanel(fluidRow(
                         h4("Price Dynamics across Four Seasons "),
                         plotlyOutput("plot_price"),
                         
                         h4("Sentiment Analysis of Guest Reviews "),
                         plotOutput("plot_sentiment")
                     ))
                 )
        ),
        tabPanel("Guest Review",fluid = T,
            titlePanel('Bookings.com'),
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("words_sentiment", 
                                     "Number of Words for Sentiments:", min = 1, max = 50, value = 10),
                         
                         sliderInput("freq",
                                     "Top 'n' Words for Wordcloud:",
                                     min = 1,  max = 100, value = 25)
                     ),
                     mainPanel(fluidRow(
                         h4("Does price affect rating or vice-vesa ? "),
                         plotlyOutput("plot_rating"),
                        
                         h4("A Word Cloud of Customer Reactions"),
                         plotOutput("plot_wordcloud")
                    ))
                )
        )
    )
)
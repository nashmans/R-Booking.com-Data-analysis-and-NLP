library(shiny)
library(jsonlite)
library(rjson)
library(tidyverse)
library(ggvis)
library(chorddiag)
library(plotly)
library(scales)
library(lubridate)
library(tidytext)
library(wordcloud)
library(ggwordcloud)
library(igraph)
library(ggthemes)
library("ggpubr")
#install.packages("ggpubr")
options(scipen = 999)


### read data
#getwd()
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



### Review data set


# restructure it in the one-token-per-row format, making  vector
review <- review_tbl %>% unnest_tokens(word,reviews.text, token = "sentences")

reviews_t = as.vector(review$word)

text_df <- tibble(text = reviews_t)
# We can remove stop words (kept in the tidytext dataset stop_words) with an anti_join().
tidy_reviews = text_df %>%
    unnest_tokens(word, text)
data("stop_words")
tidy_reviews <- tidy_reviews %>%
    anti_join(stop_words)

# plotting for frequency
# tidy_reviews %>%
#     count(word, sort = TRUE) %>%
#     filter(n > 2000) %>%
#     mutate(word = reorder(word, n)) %>%
#     ggplot(aes(word, n)) +
#     geom_col() +
#     xlab(NULL) +
#     coord_flip()

#most common words
tidy_reviews %>%
    count(word, sort = TRUE)

#get sentiments  # afinn or bing or nrc
bing_word_counts <- tidy_reviews %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()



shinyServer(function(input, output) {
    ###
    ### price SEASONAL
    ###
    output$plot_price <- renderPlotly({
        
        price_data <- merged_data %>% 
            filter(location %in% input$location) %>% 
            group_by(hotel, month, location) %>% 
            summarize(price = median(price, na.rm = TRUE))
        
        p <- ggplot(price_data, aes(x  = month, y = price,  
                                         fill = location)) + 
            geom_bar(stat = "identity", position = "dodge",alpha=0.5) +
            scale_y_continuous(labels = dollar)+
            theme_transparent()+ scale_fill_tableau()+
            theme(panel.border = element_rect(colour = "black",fill = NA))
            xlab("Seasons 2019")
        
        a <- list(tickangle = 45)
        
        ggplotly(p) %>% 
            layout(height = input$plotHeight, xaxis = a)
        
    })

    ###
    ### seniment analysis
    ###
    
     output$plot_sentiment <- renderPlot({
    
         sentiment_data <- bing_word_counts %>%
             group_by(sentiment) %>%
             top_n(input$words_sentiment) %>%
             ungroup() %>%
             mutate(word = reorder(word, n)) %>%
             filter(sentiment %in% input$sentiment)
    
         p3 <- ggplot(data = sentiment_data, aes(word, n, fill = sentiment)) +
             geom_col(show.legend = FALSE,alpha = 0.5) +
             facet_wrap(~sentiment, scales = "free_y") +
             labs(y = "Contribution to sentiment",
                  x = NULL) +
             coord_flip() +
             theme_transparent() + scale_fill_tableau()
    
         p3
         
    
     })
     
     ###
     ### price vs. rating
     ###
     
     output$plot_rating <- renderPlotly({
         
         price_data_rating <- merged_data %>% 
             filter(location %in% input$location) %>% 
             group_by(location, hotel, month) %>% 
             summarise(price = median(price, na.rm = TRUE),
                       rating = median(overall_rating, na.rm = TRUE))
         
         p2 <- ggplot(price_data_rating, aes(x  = rating, y = price,  
                                             color = location, group = month)) + 
             geom_point(alpha = 0.2) +
             
             scale_y_continuous(labels = dollar)+
             scale_x_continuous(limits = c(5, 10))+
             theme_tufte() + scale_fill_tableau() +
             labs(x = "rating")+
             facet_wrap(~location, scales = "free_y")
         
         a <- list(tickangle = 45)
         
         ggplotly(p2) %>% 
             layout(height = input$plotHeight, xaxis = a)
         
     })
     
     ###
     ### WORD cloud
     ###
     
     output$plot_wordcloud <- renderPlot({
         
         word <- tidy_reviews %>%
             anti_join(stop_words) %>%
             count(word) %>% 
             top_n(input$freq)
         
         ggplot(word, aes(label = word, size = n)) +
             geom_text_wordcloud_area(eccentricity = 1,shape = "rectangle") +
             scale_size_area(max_size = 40) +
             theme(panel.background =  element_rect(fill = "#006994"))
         
     })
})


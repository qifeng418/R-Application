#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#-------------------------Group 16----------------------------------
server<- function(session,input, output) {
  
  set.seed(122)
  histdata <- rnorm(500)
  

  
#----------------first tap data table overview-------------------------
  output$table <- DT::renderDataTable(DT::datatable({
    
    if(input$select_airline_page1 != "All"){
      AirlineData <- AirlineData%>%
        filter(airline_name == as.character(input$select_airline_page1))
    }
    if(input$select_star_page1 != "All"){
      AirlineData <- AirlineData%>%
        filter(star == as.character(input$select_star_page1))
    }
    if(input$select_country_page1 != "All"){
      AirlineData <- AirlineData%>%
        filter(airline_country == as.character(input$select_country_page1))
    }
    

    AirlineData <- AirlineData[rowSums(is.na(AirlineData)) != ncol(AirlineData),]
    
    #select columns that we are caring only and remove those we dont care
    newAirlineData <- AirlineData[,12]
    newAirlineData[1] <- AirlineData$airline_name
    newAirlineData[2] <- AirlineData$airline_country
    newAirlineData[3] <- AirlineData$cabin_flown
    newAirlineData[4] <- AirlineData$overall_rating
    newAirlineData[5] <- AirlineData$star
    newAirlineData[6] <- AirlineData$seat_comfort_rating
    newAirlineData[7] <- AirlineData$cabin_staff_rating
    newAirlineData[8] <- AirlineData$food_beverages_rating
    newAirlineData[9] <- AirlineData$inflight_entertainment_rating
    newAirlineData[10] <- AirlineData$value_money_rating
    newAirlineData[11] <- AirlineData$recommended
    newAirlineData[12] <- AirlineData$content
    
    names(newAirlineData) <- c("Airline","Airline Country","Cabin Flown","Overall Rating","Star","Seat Comfort Rating","Cabin Staff Rating","Food and Beverages Rating","Inflight Entertainment Rating","Value of Money Rating","Recommended", "Content")
    newAirlineData
    
  },
  
  options = list(dom ='Bftsp', columnDefs = list(list(targets = c(1, 3)))),
  
  caption = htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: center;',
    '', htmltools::em('Copyright 2019 Group 16. All rights reserved.')
  ), extensions = 'Responsive')) 
  
  
#-----------------second tap on customer rating comparison----------------------------
  
  output$likert <- renderPlot({
    
#    observe({
#      x <- AirlineData %>% filter(cabin_flown==input$select_cabin_page2) %>% select(star)
#      updateSelectInput(session,"select_star2_page2","Star Level", choices = unique(x))
#    })
    
#    observe({
#      countrydata <- AirlineData$airline_country[AirlineData$star == input$select_star2_page2]
#      updateSelectInput(session,"select_country_page2","Country",choices = countrydata)
#    })
    
    if(input$select_cabin_page2 != "All"){
      AirlineData <- AirlineData%>%
        filter(cabin_flown == as.character(input$select_cabin_page2))
    }
    
    if(input$select_star2_page2 != "All"){
      AirlineData <- AirlineData%>%
        filter(star == as.character(input$select_star2_page2))
    }
    if(dim(AirlineData)>0){
   
    
    # create "measure" and "response" containg all measures(1-8) and responses(1-8)
    Ratings <- gather(AirlineData, measure, response,c(13:16,19))
    #Ratings <- gather(AirlineData, measure, response,c(AirlineData$overall_rating,AirlineData$seat_comfort_rating, AirlineData$cabin_staff_rating,AirlineData$food_beverages_rating, AirlineData$inflight_entertainment_rating, AirlineData$value_money_rating))
    
    # The measure and response columns are recoded as factors.
    Ratings$measure <- as.factor(Ratings$measure)
    Ratings$response <- as.factor(Ratings$response)
    
    # The data is filtered by cabin_flown using the filter function from dplyr.
    Economy <- Ratings
    
    # The data is tabulated into a contingency table by measure and response (satisfaction rating)
    Economy_df <- table(Economy$measure,Economy$response) %>% 
      as.data.frame.matrix()
    if(dim(Economy_df)>2){
    # The column names are changed to reflect the satisfaction rating (Very Poor, Poor, Satisfactory, Good, Excellent) instead of the number scale( 1,2,3,4,5). The number "0" is recoded as "Missing". The row names are changed to the actual 8 questions instead of using the names Measure 1 to Measure 8
    colnames(Economy_df) <- c("No_Resp","Very Poor","Poor","Satisfactory","Good","Excellent")
    print(Economy_df)
    if(dim(Economy_df)>2){
    rownames(Economy_df)<-c("seat_comfort_rating","cabin_staff_rating","food_beverages_rating","inflight_entertainment_rating","value_money_rating")
    if(dim(Economy_df)>2){
    Economy_df <- tibble::rownames_to_column(Economy_df, var="Measure") 
    
    
      likert(Measure ~ ., data=Economy_df, ylab=NULL, xlab = 10,
             ReferenceZero=3, as.percent=TRUE,
             positive.order=TRUE, 
             main = list(c(input$select_cabin, input$select_star) ,x=unit(.55, "npc")), 
             sub= list("Satisfaction Rating",x=unit(.57, "npc")), 
             xlim=c(-100,-80, -60,-40,-20,0,20,40,60,80,100),
             strip=FALSE, 
             par.strip.text=list(cex=.7))
    
    }}}
    }
    
  })
  
 
  output$likert2 <- renderPlot({
    
    if(input$select_cabin_page2 != "All"){
      AirlineData <- AirlineData%>%
        filter(cabin_flown == as.character(input$select_cabin_page2))
    }
    
    if(input$select_star2_page2 != "All"){
      AirlineData <- AirlineData%>%
        filter(star == as.character(input$select_star2_page2))
    }
    
    if(input$select_airline_page2 != "All"){
      AirlineData <- AirlineData%>%
        filter(airline_name == as.character(input$select_airline_page2))
    }
    if(count(AirlineData[,1])>5){
    # create "measure" and "response" containg all measures(1-8) and responses(1-8)
    Ratings <- gather(AirlineData, measure, response,c(13:16,19))
      #Ratings <- gather(AirlineData, measure, response,c(AirlineData$overall_rating,AirlineData$seat_comfort_rating, AirlineData$cabin_staff_rating,AirlineData$food_beverages_rating, AirlineData$inflight_entertainment_rating, AirlineData$value_money_rating))
    
    # The measure and response columns are recoded as factors.
    Ratings$measure <- as.factor(Ratings$measure)
    Ratings$response <- as.factor(Ratings$response)
    
    # The data is filtered by cabin_flown using the filter function from dplyr.
    Economy <- Ratings
    
    # The data is tabulated into a contingency table by measure and response (satisfaction rating)
    Economy_df <- table(Economy$measure,Economy$response) %>% 
      as.data.frame.matrix()
    if(dim(Economy_df)>2){
    # The column names are changed to reflect the satisfaction rating (Very Poor, Poor, Satisfactory, Good, Excellent) instead of the number scale( 1,2,3,4,5). The number "0" is recoded as "Missing". The row names are changed to the actual 8 questions instead of using the names Measure 1 to Measure 8
    colnames(Economy_df) <- c("No_Resp","Very Poor","Poor","Satisfactory","Good","Excellent")
    if(dim(Economy_df)>2){
    rownames(Economy_df)<-c("seat_comfort_rating","cabin_staff_rating","food_beverages_rating","inflight_entertainment_rating","value_money_rating")
    if(dim(Economy_df)>2){
    Economy_df <- tibble::rownames_to_column(Economy_df, var="Measure") 
    if(dim(Economy_df)>2){
    likert(Measure ~ ., data=Economy_df, ylab=NULL, xlab = 10,
           ReferenceZero=3, as.percent=TRUE,
           positive.order=TRUE, 
           main = list(input$select_country_page2,x=unit(.55, "npc")), 
           sub= list("Satisfaction Rating",x=unit(.57, "npc")), 
           xlim=c(-100,-80, -60,-40,-20,0,20,40,60,80,100),
           strip=FALSE, 
           par.strip.text=list(cex=.7))
    
    }}}}}
  })
 
#-------------------------text analysis-----------------------------
  output$Recommend_or_not <- renderPlot({
    
    if(input$select_cabin_page3 != "All"){
      tidy_review <- tidy_review %>%
        filter(cabin_flown == as.character(input$select_cabin_page3))
    }
    if(input$select_star_page3 != "All"){
      tidy_review <- tidy_review %>%
        filter(star == as.character(input$select_star_page3))
    }
    if(input$select_seat_score_page3 != "All"){
      tidy_review <- tidy_review%>%
        filter(seat_comfort_rating == as.numeric(input$select_seat_score_page3))
    }
    if(input$select_staff_score_page3 != "All"){
      tidy_review <- tidy_review%>%
        filter(cabin_staff_rating == as.numeric(input$select_staff_score_page3))
    }
    if(input$select_food_score_page3 != "All"){
      tidy_review <- tidy_review%>%
        filter(food_beverages_rating == as.numeric(input$select_food_score_page3))
    }
    if(input$select_entertainment_score_page3 != "All"){
      tidy_review <- tidy_review%>%
        filter(inflight_entertainment_rating == as.numeric(input$select_entertainment_score_page3))
    }
    if(input$select_money_value_score_page3 != "All"){
      tidy_review <- tidy_review%>%
        filter(value_money_rating == as.numeric(input$select_money_value_score_page3))
    }
    if(dim(tidy_review)>0){
  
    word_counts <- tidy_review %>%
      # Count words by whether or not its a complaint
      count(word, recommended) %>%
      # Group by whether or not its a complaint
      group_by(recommended) %>%
      # Keep the top 20 words
      top_n(30, n) %>%
      ungroup()%>%
      mutate(word2 = fct_reorder(word, n))
    
    
    # Include a color aesthetic tied to whether or not its a complaint
    ggplot(word_counts, aes(x = word2, y = n, fill = recommended)) +
      # Don't include the lengend for the column plot
      geom_col(show.legend = FALSE) +
      # Facet by whether or not its a complaint and make the y-axis free
      facet_wrap(~recommended, scales = 'free_y') +
      # Flip the coordinates and add a title: "Twitter Word Counts"
      coord_flip() +
      ggtitle("Recommended or not")
    
    }
    
  })
  
  output$wordcloudPositive3 <- renderPlot({
    if(input$select_cabin_page3 != "All"){
      tidy_review <- tidy_review %>%
        filter(cabin_flown == as.character(input$select_cabin_page3))
    }
    if(input$select_star_page3 != "All"){
      tidy_review <- tidy_review %>%
        filter(star == as.character(input$select_star_page3))
    }
    if(input$select_seat_score_page3 != "All"){
      tidy_review <- tidy_review%>%
        filter(seat_comfort_rating == as.numeric(input$select_seat_score_page3))
    }
    if(input$select_staff_score_page3 != "All"){
      tidy_review <- tidy_review%>%
        filter(cabin_staff_rating == as.numeric(input$select_staff_score_page3))
    }
    if(input$select_food_score_page3 != "All"){
      tidy_review <- tidy_review%>%
        filter(food_beverages_rating == as.numeric(input$select_food_score_page3))
    }
    if(input$select_entertainment_score_page3 != "All"){
      tidy_review <- tidy_review%>%
        filter(inflight_entertainment_rating == as.numeric(input$select_entertainment_score_page3))
    }
    if(input$select_money_value_score_page3 != "All"){
      tidy_review <- tidy_review%>%
        filter(value_money_rating == as.numeric(input$select_money_value_score_page3))
    }
    
    if(dim(tidy_review)>0){

    word_counts <- tidy_review %>%
      # Count words by whether or not its a complaint
      count(word) %>%
      # Keep the top 20 words
      top_n(30, n) %>%
      mutate(word2 = fct_reorder(word, n))
    
#    wordcloud(words = word_counts$word,freq = word_counts$n,max.words = 30)
    wordcloud(words = word_counts$word,freq = word_counts$n,max.words = 30, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    }
    
  })
  
  
  output$correlation <-renderPlot(
    {
      if(input$select_cabin_page3 != "All"){
        tidy_review <- tidy_review %>%
          filter(cabin_flown == as.character(input$select_cabin_page3))
      }
      if(input$select_star_page3 != "All"){
        tidy_review <- tidy_review %>%
          filter(star == as.character(input$select_star_page3))
      }
      if(input$select_seat_score_page3 != "All"){
        tidy_review <- tidy_review%>%
          filter(seat_comfort_rating == as.numeric(input$select_seat_score_page3))
      }
      if(input$select_staff_score_page3 != "All"){
        tidy_review <- tidy_review%>%
          filter(cabin_staff_rating == as.numeric(input$select_staff_score_page3))
      }
      if(input$select_food_score_page3 != "All"){
        tidy_review <- tidy_review%>%
          filter(food_beverages_rating == as.numeric(input$select_food_score_page3))
      }
      if(input$select_entertainment_score_page3 != "All"){
        tidy_review <- tidy_review%>%
          filter(inflight_entertainment_rating == as.numeric(input$select_entertainment_score_page3))
      }
      if(input$select_money_value_score_page3 != "All"){
        tidy_review <- tidy_review%>%
          filter(value_money_rating == as.numeric(input$select_money_value_score_page3))
      }
      
      bigrams_separated <- tidy_review %>%
        separate(word, c("word1", "word2"), sep = " ")
      
      bigrams_filtered <- bigrams_separated %>%
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word)
      
      # new bigram counts:
      bigram_counts <- bigrams_filtered %>% 
        count(word1, word2, sort = TRUE)
      
      bigram_counts
      
      library(igraph)
      word_counts <- tidy_review %>% 
        #  filter(complaint_label == "Complaint") %>% 
        count(word) %>% 
        # Keep words with count greater than 100
        top_n(30, n)
      
      # original counts
      #word_counts
      
      bigram_graph <- bigram_counts %>%
        filter(n > 500) %>%
        graph_from_data_frame()
      
      bigram_graph
      
      library(ggraph)
      set.seed(2017)
      
      a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
      
      ggraph(bigram_graph, layout = "fr" ) +
        geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                       arrow = a, end_cap = circle(.07, 'inches')) +
        geom_node_point(color = "lightblue", size = 5) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1) + 
    #   labs(title ="Bigram Relation Graph") +
        ggtitle("Bigram Relation Graph") +
        theme(plot.title = element_text(hjust = 1))
      
    }
  )
  
  output$sentiment1 <- renderPlotly({
    if(input$select_cabin_page3 != "All"){
      tidy_review_single <- tidy_review_single %>%
        filter(cabin_flown == as.character(input$select_cabin_page3))
    }
    if(input$select_star_page3 != "All"){
      tidy_review_single <- tidy_review_single %>%
        filter(star == as.character(input$select_star_page3))
    }
    if(input$select_seat_score_page3 != "All"){
      tidy_review_single <- tidy_review_single%>%
        filter(seat_comfort_rating == as.numeric(input$select_seat_score_page3))
    }
    if(input$select_staff_score_page3 != "All"){
      tidy_review_single <- tidy_review_single%>%
        filter(cabin_staff_rating == as.numeric(input$select_staff_score_page3))
    }
    if(input$select_food_score_page3 != "All"){
      tidy_review_single <- tidy_review_single%>%
        filter(food_beverages_rating == as.numeric(input$select_food_score_page3))
    }
    if(input$select_entertainment_score_page3 != "All"){
      tidy_review_single <- tidy_review_single%>%
        filter(inflight_entertainment_rating == as.numeric(input$select_entertainment_score_page3))
    }
    if(input$select_money_value_score_page3 != "All"){
      tidy_review_single <- tidy_review_single%>%
        filter(value_money_rating == as.numeric(input$select_money_value_score_page3))
    }

    if(dim(tidy_review_single)>0){
      
      word_counts_sentiment <- tidy_review_single %>% 
        # Append the NRC dictionary and filter for positive, fear, and trust
        inner_join(get_sentiments("bing")) %>% 
        filter(sentiment %in% c("positive", "negative")) %>%
        # Count by word and sentiment and take the top 10 of each
        count(word, sentiment) %>% 
        group_by(sentiment) %>% 
        top_n(10, n) %>% 
        ungroup() %>% 
        # Create a factor called word2 that has each word ordered by the count
        mutate(word2 = fct_reorder(word, n))
      
      print(
        ggplotly(
      # Create a bar plot out of the word counts colored by sentiment
      ggplot(word_counts_sentiment, aes(x = word2, y = n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        # Create a separate facet for each sentiment with free axes
        facet_wrap(~sentiment, scales = 'free') +
        coord_flip() +
        # Title the plot "Sentiment Word Counts" with "Words" for the x-axis
        labs(
          title = "Sentiment Word Counts",
          x = "Words"
        )))
}
  })
  
  output$sentiment2 <- renderPlotly({
    if(input$select_cabin_page3 != "All"){
      tidy_review_single <- tidy_review_single %>%
        filter(cabin_flown == as.character(input$select_cabin_page3))
    }
    if(input$select_star_page3 != "All"){
      tidy_review_single <- tidy_review_single %>%
        filter(star == as.character(input$select_star_page3))
    }
    if(input$select_seat_score_page3 != "All"){
      tidy_review_single <- tidy_review_single%>%
        filter(seat_comfort_rating == as.numeric(input$select_seat_score_page3))
    }
    if(input$select_staff_score_page3 != "All"){
      tidy_review_single <- tidy_review_single%>%
        filter(cabin_staff_rating == as.numeric(input$select_staff_score_page3))
    }
    if(input$select_food_score_page3 != "All"){
      tidy_review_single <- tidy_review_single%>%
        filter(food_beverages_rating == as.numeric(input$select_food_score_page3))
    }
    if(input$select_entertainment_score_page3 != "All"){
      tidy_review_single <- tidy_review_single%>%
        filter(inflight_entertainment_rating == as.numeric(input$select_entertainment_score_page3))
    }
    if(input$select_money_value_score_page3 != "All"){
      tidy_review_single <- tidy_review_single%>%
        filter(value_money_rating == as.numeric(input$select_money_value_score_page3))
    }
    
    if(dim(tidy_review_single)>0){
    
    sentiment_review <- tidy_review_single %>% 
      # Append the bing sentiment dictionary
      inner_join(get_sentiments("bing")) %>% 
      # Count by complaint label and sentiment
      count(overall_rating,sentiment) %>% 
      # Spread the sentiment and count columns
      spread(sentiment,n) %>% 
      # Compute overall_sentiment = positive - negative
      mutate(overall_sentiment = positive - negative)
    
    print(
      ggplotly(  
    # Create a bar plot out of overall sentiment by complaint level, colored by a complaint label factor
    ggplot(
      sentiment_review, 
      aes(x = overall_rating, y = overall_sentiment, fill = as.factor(overall_rating))
    ) +
      geom_col(show.legend = FALSE) +
      coord_flip() + 
      # Title the plot "Overall Sentiment by Complaint Type," with an "Airline Twitter Data" subtitle
       labs(
        title = "Overall Sentiment by overall_rating",
        subtitle = "Airline Data"
      )))
    
    }
  })
  
  output$TextOutput <- renderPlot({
    
    
    custom_stop_words_inter <- tribble(
      # Column names should match stop_words
      ~word, ~lexicon,
      # Add http, win, and t.co as custom stop words
      input$TextInput, "CUSTOM"
    )
    
    print(custom_stop_words_inter)
    
    stop_words_iter <- stop_words_iter %>% 
      bind_rows(custom_stop_words_inter)
    
    print(stop_words_iter)
    
    tidy_review_iter <- AirlineData %>% 
      # Tokenize the twitter data
      unnest_tokens(word, content)  %>% 
      # Remove stop words
      anti_join(stop_words_iter)
    
    word_counts_iter <- tidy_review_iter %>% 
      #  filter(complaint_label == "Complaint") %>% 
      count(word) %>% 
      mutate(word = reorder(word,n)) %>%
      # Keep words with count greater than 100
      top_n(30, n)
    
    # Create a bar plot using word_counts with x = word
    ggplot(word_counts_iter, aes(x = word,y = n)) +
      geom_col() +
      # Flip the plot coordinates
      coord_flip()
    
    
  })
  
  
}
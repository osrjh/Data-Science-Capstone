
shinyServer(function(input, output) {
  
  #installing packages
  packages <- c("knitr", "dplyr", "ggplot2", "data.table", "quanteda", "readtext", "quanteda.textstats", "stringr", "caret")
  
  #loading packages
  invisible(lapply(packages,
                   library,
                   character.only = TRUE))
  
  ## Reading the Data
  unigrams <- readRDS("../data/unigram.rds")
  bigrams <- readRDS("../data/bigram.rds")
  trigrams <- readRDS("../data/trigram.rds")
  quadgrams <- readRDS("../data/quadgram.rds")
  
  ## Prediction function
  predict.word <- function(phrase){
    phrase <- tolower(str_trim(phrase))
    n <- length(str_split(phrase, " ")[[1]])
    
    if (n >= 3) {
      
      word1 <- str_split(phrase, " ")[[1]][n-2]
      word2 <- str_split(phrase, " ")[[1]][n-1]
      word3 <- str_split(phrase, " ")[[1]][n]
      
      if(word1 %in% c("shit", "piss", "fuck", "cunt", "cocksucker", "motherfucker", "tits") | 
         word2 %in% c("shit", "piss", "fuck", "cunt", "cocksucker", "motherfucker", "tits") | 
         word3 %in% c("shit", "piss", "fuck", "cunt", "cocksucker", "motherfucker", "tits")) {
return(rep("Please do not use profanities!",3))
      } else if (!is.na(quadgrams[ngram %like% paste0("^", word1, " ", word2, " ", word3, " "),1][1])){
        quadgrams[ngram %like% paste0("^", word1, " ", word2, " ", word3, " "),1][1] -> ng
        return(c(str_split(ng[1], " ")[[1]][4],str_split(ng[2], " ")[[1]][4],str_split(ng[3], " ")[[1]][4]))
      } else if (!is.na(trigrams[ngram %like% paste0("^", word2, " ", word3, " "),1][1])){
        trigrams[ngram %like% paste0("^", word2, " ", word3, " "),1] -> ng
        return(c(str_split(ng[1], " ")[[1]][3],str_split(ng[2], " ")[[1]][3],str_split(ng[3], " ")[[1]][3]))
      } else if (!is.na(bigrams[ngram %like% paste0("^", word3, " "),1][1])){
        bigrams[ngram %like% paste0("^", word3, " "),1] -> ng
        return(c(str_split(ng[1], " ")[[1]][2],str_split(ng[2], " ")[[1]][2],str_split(ng[3], " ")[[1]][2]))
      } else {
        return(rep("Could not predict",3))
      }
    }
    
    else if (n == 2) {
      
      word2 <- str_split(phrase, " ")[[1]][n-1]
      word3 <- str_split(phrase, " ")[[1]][n]
      
      if(word1 %in% c("shit", "piss", "fuck", "cunt", "cocksucker", "motherfucker", "tits") | 
         word2 %in% c("shit", "piss", "fuck", "cunt", "cocksucker", "motherfucker", "tits") | 
         word3 %in% c("shit", "piss", "fuck", "cunt", "cocksucker", "motherfucker", "tits")) {
return(rep("Please do not use profanities!",3))
      } else if (!is.na(trigrams[ngram %like% paste0("^", word2, " ", word3, " "),1][1])){
        trigrams[ngram %like% paste0("^", word2, " ", word3, " "),1] -> ng
        return(c(str_split(ng[1], " ")[[1]][3],str_split(ng[2], " ")[[1]][3],str_split(ng[3], " ")[[1]][3]))
      } else if (!is.na(bigrams[ngram %like% paste0("^", word3, " "),1][1])){
        bigrams[ngram %like% paste0("^", word3, " "),1] -> ng
        return(c(str_split(ng[1], " ")[[1]][2],str_split(ng[2], " ")[[1]][2],str_split(ng[3], " ")[[1]][2]))
      } else {
        return(rep("Could not predict",3))
      }
    }
    
    else if (n == 1) {
      
      word3 <- str_split(phrase, " ")[[1]][n]
      
      if(word1 %in% c("shit", "piss", "fuck", "cunt", "cocksucker", "motherfucker", "tits") | 
         word2 %in% c("shit", "piss", "fuck", "cunt", "cocksucker", "motherfucker", "tits") | 
         word3 %in% c("shit", "piss", "fuck", "cunt", "cocksucker", "motherfucker", "tits")) {
        return(rep("Please do not use profanities!",3))
      } else if (!is.na(bigrams[ngram %like% paste0("^", word3, " "),1][1])){
        bigrams[ngram %like% paste0("^", word3, " "),1] -> ng
        return(c(str_split(ng[1], " ")[[1]][2],str_split(ng[2], " ")[[1]][2],str_split(ng[3], " ")[[1]][2]))
      } else {
        return(rep("Could not predict",3))
      }
    }
  }
  
  ## Plot function
  predict.plot <- function(phrase){
    phrase <- tolower(str_trim(phrase))
    n <- length(str_split(phrase, " ")[[1]])
    
    if (n >= 3) {
      
      word1 <- str_split(phrase, " ")[[1]][n-2]
      word2 <- str_split(phrase, " ")[[1]][n-1]
      word3 <- str_split(phrase, " ")[[1]][n]
      
      if(word1 %in% c("shit", "piss", "fuck", "cunt", "cocksucker", "motherfucker", "tits") | 
         word2 %in% c("shit", "piss", "fuck", "cunt", "cocksucker", "motherfucker", "tits") | 
         word3 %in% c("shit", "piss", "fuck", "cunt", "cocksucker", "motherfucker", "tits")) {
return(rep("Please do not use profanities!",3))
      } else if (!is.na(quadgrams[ngram %like% paste0("^", word1, " ", word2, " ", word3, " "),1][1])){
        head(quadgrams[ngram %like% paste0("^", word1, " ", word2, " ", word3, " "),c(1,3)][1], 20) -> plot_data
        plot_data %>%
          ggplot() +
          geom_bar(aes(x = reorder(ngram, -prob), y = prob), stat = "identity", col = "#3b3b3b", fill = "#00C19A") +
          ylab("Probability") +
          xlab("Next Word") +
          theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
          scale_x_discrete(labels = sapply(str_split(plot_data$ngram, " "),"[[", 4)) -> ngplot
        return(ngplot)
      } else if (!is.na(trigrams[ngram %like% paste0("^", word2, " ", word3, " "),1][1])){
        head(trigrams[ngram %like% paste0("^", word2, " ", word3, " "),c(1,3)], 20) -> plot_data
        plot_data %>%
          ggplot() +
          geom_bar(aes(x = reorder(ngram, -prob), y = prob), stat = "identity", col = "#3b3b3b", fill = "#00C19A") +
          ylab("Probability") +
          xlab("Next Word") +
          theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
          scale_x_discrete(labels = sapply(str_split(plot_data$ngram, " "),"[[", 3)) -> ngplot
        return(ngplot)
      } else if (!is.na(bigrams[ngram %like% paste0("^", word3, " "),1][1])){
        head(bigrams[ngram %like% paste0("^", word3, " "),c(1,3)], 20) -> plot_data
        plot_data %>%
          ggplot() +
          geom_bar(aes(x = reorder(ngram, -prob), y = prob), stat = "identity", col = "#3b3b3b", fill = "#00C19A") +
          ylab("Probability") +
          xlab("Next Word") +
          theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
          scale_x_discrete(labels = sapply(str_split(plot_data$ngram, " "),"[[", 2)) -> ngplot
        return(ngplot)
      } else {
        return(rep("Could not predict",3))
      }
    }
    
    else if (n == 2) {
      
      word2 <- str_split(phrase, " ")[[1]][n-1]
      word3 <- str_split(phrase, " ")[[1]][n]
      
      if(word1 %in% c("shit", "piss", "fuck", "cunt", "cocksucker", "motherfucker", "tits") | 
         word2 %in% c("shit", "piss", "fuck", "cunt", "cocksucker", "motherfucker", "tits") | 
         word3 %in% c("shit", "piss", "fuck", "cunt", "cocksucker", "motherfucker", "tits")) {
return(rep("Please do not use profanities!",3))
      } else if (!is.na(trigrams[ngram %like% paste0("^", word2, " ", word3, " "),1][1])){
        head(trigrams[ngram %like% paste0("^", word2, " ", word3, " "),c(1,3)], 20) -> plot_data
        plot_data %>%
          ggplot() +
          geom_bar(aes(x = reorder(ngram, -prob), y = prob), stat = "identity", col = "#3b3b3b", fill = "#00C19A") +
          ylab("Probability") +
          xlab("Next Word") +
          theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
          scale_x_discrete(labels = sapply(str_split(plot_data$ngram, " "),"[[", 3)) -> ngplot
        return(ngplot)
      } else if (!is.na(bigrams[ngram %like% paste0("^", word3, " "),1][1])){
        head(bigrams[ngram %like% paste0("^", word3, " "),c(1,3)],20) -> plot_data
        plot_data %>%
          ggplot() +
          geom_bar(aes(x = reorder(ngram, -prob), y = prob), stat = "identity", col = "#3b3b3b", fill = "#00C19A") +
          ylab("Probability") +
          xlab("Next Word") +
          theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
          scale_x_discrete(labels = sapply(str_split(plot_data$ngram, " "),"[[", 2)) -> ngplot
        return(ngplot)
      } else {
        return(rep("Could not predict",3))
      }
    }
    
    else if (n == 1) {
      
      word3 <- str_split(phrase, " ")[[1]][n]
      
      if(word1 %in% c("shit", "piss", "fuck", "cunt", "cocksucker", "motherfucker", "tits") | 
         word2 %in% c("shit", "piss", "fuck", "cunt", "cocksucker", "motherfucker", "tits") | 
         word3 %in% c("shit", "piss", "fuck", "cunt", "cocksucker", "motherfucker", "tits")) {
return(rep("Please do not use profanities!",3))
      } else if (!is.na(bigrams[ngram %like% paste0("^", word3, " "),1][1])){
        head(bigrams[ngram %like% paste0("^", word3, " "),c(1,3)],20) -> plot_data
        plot_data %>%
          ggplot() +
          geom_bar(aes(x = reorder(ngram, -prob), y = prob), stat = "identity", col = "#3b3b3b", fill = "#00C19A") +
          ylab("Probability") +
          xlab("Next Word") +
          theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
          scale_x_discrete(labels = sapply(str_split(plot_data$ngram, " "),"[[", 2)) -> ngplot
        return(ngplot)
      } else {
        return(rep("Could not predict",3))
      }
    }
  }
  
  observeEvent(input$submit_button, {
    prediction <- predict.word(input$user_input)
    if (is.na(prediction[[1]]) == TRUE) {
      output$next_word1 <- renderText(" ")
    } else {
      output$next_word1 <- renderText(prediction[[1]])
      plot_print <- predict.plot(input$user_input)
    }
    
    if (is.na(prediction[[2]]) == TRUE) {
      output$next_word2 <- renderText(" ")
    } else {
      output$next_word2 <- renderText(prediction[[2]])
      plot_print <- predict.plot(input$user_input)
    }
    
    if (is.na(prediction[[3]]) == TRUE) {
      output$next_word3 <- renderText(" ")
    } else {
      output$next_word3 <- renderText(prediction[[3]])
      plot_print <- predict.plot(input$user_input)
    }
    
    output$plot1 <- renderPlot({
      plot_print
    })
    
  })
})






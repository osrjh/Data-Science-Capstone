#installing packages
library(shiny)
library(knitr)
library(dplyr)
library(ggplot2)
library(data.table)
library(readtext)
library(stringr)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("JHU Data Science Specialisation Capstone Project"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("user_input", "Enter your sentence"),
      actionButton("submit_button", "Predict next word"), 
      br(),
      h5("Prediction 1: "),
      verbatimTextOutput("next_word1"),
      h5("Prediction 2: "),
      verbatimTextOutput("next_word2"),
      h5("Prediction 3: "),
      verbatimTextOutput("next_word3")
    ),
    mainPanel(h1("NLP Model"),
              h5("This is a basic app which will showcase a simple natural language processing prediction algorithm - the stupid back-off algorithm. "),
              h3("Instructions"),
              h5("Type a sentence in the input box and click on the 'Predict next word' button."),
              h3("Bar Graph of the Top 20 Most Likely Next Words"),
              plotOutput("plot1",brush = brushOpts(id="brush1")),
  ))))


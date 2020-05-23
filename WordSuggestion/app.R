#
# This is a Shiny word suggestion web application. 
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#   https://www.youtube.com/watch?v=DG_JDOQgF5E

require(shiny)
require(shinythemes)
require(RSQLite)
require(dplyr)
require(ggplot2)

setwd("C:/Users/testsubject941/Documents/GitHub/Capstone/WordSuggestion")
dir()

# data
freq_3grams <- readRDS("freq_3grams.rds")
freq_2grams <- readRDS("freq_2grams.rds")

# user
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Word Suggestion Generator"),
  p("The next wprd suggestion generator was built on news, blog and twitter reference data to identify words that appear together in either bigrams or trigrams." ),
  p("The reference data was cleaned as follows:"),
  tags$li("Profane, obscene and vulgar words were replaced with the literal 'profanity'."),
  tags$li("Emojis/emoticons were replaced with English equivalents."),
  tags$li("Common English stopwords such as a, be, can, do, for, I, the, etc were removed."),
  p(""),
  p("The word generator expects a single word or two words separated by an '_'. "),
  
  hr(),
    textInput("inWord", "Enter your word"),
    tags$head(tags$script(src = "message-handler.js")),
    actionButton("do", "Go"),
    tableOutput("topSuggestion"),
    tableOutput("allSuggestions")
)


# server
# Define server logic to lookup frequently occuring word
server <- function(input, output) {

  observeEvent(input$do, {
    
    #word1 match
    word1Filter <- freq_3grams %>% 
      filter(word1 == input$inWord) %>%
      arrange(-cnt) %>%
      filter(cnt == max(cnt)) %>%
      select (cnt, word2) %>%
      rename(Frequency = cnt, Word = word2) 

    #word2 match
    word2Filter <- freq_3grams %>% 
      filter(word2 == input$inWord) %>%
      arrange(-cnt) %>%
      filter(cnt == max(cnt)) %>%
      select (cnt, word3) %>%
      rename(Frequency = cnt, Word = word3 )
    
    allSuggestions <- bind_rows(word1Filter, word2Filter) %>%
      add_row(Frequency = 1, Word = 'profanity') %>%
      arrange(desc(Frequency))

    topSuggestion <- allSuggestions %>%
      filter(Frequency == max(Frequency)) %>%
      rename("Top Suggestion" = Word) %>%
      select ("Top Suggestion")
    

  
    output$topSuggestion <- renderTable ( { topSuggestion} )
    
    output$allSuggestions <- renderTable ( { allSuggestions} )
    
  })    
}
 


# Run the application 
shinyApp(ui = ui, server = server)


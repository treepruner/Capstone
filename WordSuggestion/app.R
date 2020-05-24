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
require(quanteda)

setwd("C:/Users/testsubject941/Documents/GitHub/Capstone/WordSuggestion")
dir()

# data
freq_3grams <- readRDS("freq_3grams.rds")
freq_2grams <- readRDS("freq_2grams.rds")

# user
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Word Suggestion Generator"),
  
  p("The word generator accepts a single word or phrase. "), 
  p(" "),
  "Documentation:",
  p("News, blog and twitter files were used to create reference data to predict words that appear together. The reference data was cleaned as follows:"),
  tags$li("Profane, obscene and vulgar words were replaced with the literal 'profanity'."),
  tags$li("Emojis/emoticons were replaced with English equivalents."),
  tags$li("Numbers, symbols and extra spaces removed."),
  tags$li("Common English stopwords such as a, be, can, do, for, I, the, etc were removed."),
  p(" "),
  p("Note: input data has been subjected to the same cleaning."),
  

  
  hr(),
  
  fluidRow(
    column(width = 4,
        textInput("inWord", "Enter your word or phrase."),
        tags$head(tags$script(src = "message-handler.js")),
        actionButton("do", "Go") ),
    column(width = 4, tableOutput("topSuggestion")),
    #column(width = 4, tableOutput("allSuggestions")),
    column(width = 4, plotOutput("allSuggestionsBarPlot") )
    
    )
)
# server
# Define server logic to lookup frequently occuring word
server <- function(input, output) {

  observeEvent(input$do, {
    # format input  
    tok <- tokens(corpus(input$inWord)
                  , remove_punct = TRUE
                  , remove_numbers = TRUE
                  , remove_url = TRUE
                  , remove_symbols = TRUE) %>% 
      tokens_select( stopwords('english'), selection = 'remove') %>%
      tokens_tolower() %>%
      unlist %>%
      noquote %>%
      tail(n=1)
    

    #word1 match
    word1Filter <- freq_3grams %>% 
      filter(word1 == tok) %>%
      #filter(word1 == input$inWord) %>%
      arrange(-cnt) %>%
      filter(cnt == max(cnt)) %>%
      select (cnt, word2) %>%
      rename(Frequency = cnt, Word = word2) 

    #word2 match
    word2Filter <- freq_3grams %>% 
      filter(word2 == tok) %>%
      #filter(word2 == input$inWord) %>%
      arrange(-cnt) %>%
      filter(cnt == max(cnt)) %>%
      select (cnt, word3) %>%
      rename(Frequency = cnt, Word = word3 )
    
    allSuggestions <- bind_rows(word1Filter, word2Filter) %>%
      add_row(Frequency = 1, Word = 'profanity') %>%
      arrange(desc(Frequency))

    topSuggestion <- allSuggestions %>%
      filter(Frequency == max(Frequency)) %>%
      rename("Top Next Word Suggestion" = Word) %>%
      select ("Top Next Word Suggestion")
    

  
    output$topSuggestion <- renderTable ( { topSuggestion} )
    
    output$allSuggestions <- renderTable ( { allSuggestions} )
    
    output$allSuggestionsBarPlot <-renderPlot({
      ggplot(data = allSuggestions, aes(x = Frequency, y = reorder(Word, Frequency) ))  +
        geom_bar(stat = "identity", fill =  "steelblue4" ) +
        labs(title = " Words Frequently Associated with Your Word") +
       scale_y_discrete( name = "Suggested Words") +
        theme( axis.text.y = element_text(face = "bold", size = 14),
               plot.title = element_text(face = "bold", hjust = 0.5 ))
    })
    
  })    
}
 


# Run the application 
shinyApp(ui = ui, server = server)


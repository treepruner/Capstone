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

setwd("C:/Users/testsubject941/Documents/GitHub/Capstone/WordSuggestion")

# data
freq_3grams <- readRDS("freq_3grams.rds")

# user
ui <- fluidPage(
    theme = shinytheme("cerulean"),
    titlePanel("Word Suggestion Generator"),
    p("This application was built on news, blog and twitter reference data to identify words that appear together in either bigrams or trigrams." ),
    p("The reference data was cleaned."),
    p("    Profane words were replaced with the literal 'profanity'."),
      p("   Emojis/emoticons with English equivalents."),
    p("    Common English stopwords such as a, be, can, do, for, I, the, etc have been removed."),
    p("The word generator expects a single word or two words separated by an '_'. "),
    
    hr(),
    sidebarLayout(
        sidebarPanel(
            
            textInput("inWord1", "Enter your word")
                    ),
        mainPanel = (
            tableOutput("word1Suggestions")
                    )
                )
)

# server
# Define server logic to lookup frequently occuring word
server <- function(input, output) {
    

    output$word1Suggestions <- renderTable ( {

    word1Filter <- freq_3grams %>% 
        filter(word1 == input$inWord1) %>%
        arrange(-cnt) %>%
        filter(cnt == max(cnt)) %>%
        select (cnt, word1, word2, word3)
    
   })
    
    # load data from table
    #    loadData <- function( fields, table, SortCol = ' ', whereCls = ' ') {
    #        # Construct select query
    #if(WhereCls =='')
    #            query <- sprintf("select %s from %s", fields, table)
    #        else
    #            query <- sprintf("select %s from %s where %s", fields, table) 
        # submit select query and disconnect
    #        dataDB <- dbGetQuery(db, query)
    #if (SortCol !='') dataDB[order(dataDB[SortCol]),]
    #        else dataDB
    #}
    

}
   
# Run the application 
shinyApp(ui = ui, server = server)

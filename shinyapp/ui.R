
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

inputTextarea <- function(inputId, value="", nrows, ncols) {
    tagList(
        singleton(tags$head(tags$script(src = "textarea.js"))),
        tags$textarea(id = inputId,
                      class = "inputtextarea",
                      rows = nrows,
                      cols = ncols,
                      as.character(value))
    )
}

shinyUI(pageWithSidebar(
    # Application title
    headerPanel("Text positivity metric"),

    # Sidebar with a slider input for number of observations
    sidebarPanel(
        h5("Calculates how positive given text is. 0 means most negative, 10 - most positive."),
        inputTextarea('inputText', 'Please paste your text here (at least few sentences, english only)...', 15, 300),
        submitButton('Submit')
    ),
  
    # Show a plot of the generated distribution
    mainPanel(
        h4('Positivity metric of the text (scale 0:10):'),
        verbatimTextOutput("positivity"),
        h4('Top5 positive words:'),
        verbatimTextOutput("topPositive"),
        h4('Top5 negative words:'),
        verbatimTextOutput("topNegative")
    )
))

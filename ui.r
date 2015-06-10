if (!require("shiny") && !require("sqldf") && !require("car")) {
  install.packages(c("shiny", "sqldf", "car"), dependencies = TRUE)
}

library(shiny)
library(sqldf)
library(car)

data(Moore)
ContextElements = colnames(Moore)
ContextElements[1] = 'partner_status'
colnames(Moore) = ContextElements
library(shiny)
 ui = shinyUI(fluidPage(
    
    titlePanel(h3("Data Select", style="background-color:gray")),
    hr(),
    div(class="row-fluid", style="height=350px; background-color:lightgray",
        column(width=3,
               uiOutput("firstAxe"),
               uiOutput("firstAxeOutput")
        ),
        column(width=3,
               uiOutput("secondAxe"),
               uiOutput('type1'),
               uiOutput('type1v'),
               uiOutput("secondAxeOutput")
        ),
        column(width=3,
               uiOutput("thirdAxe"),
               uiOutput('type2'),
               uiOutput("thirdAxeOutput")
        ),
        column(width=3,
               uiOutput("fourthAxe"),
               uiOutput('type3'),
               uiOutput("fourthAxeOutput")
        )
    ),
    hr(),
    hr(),
    div(class="row-fluid",
        column(12,
               verbatimTextOutput("mainQuery"))),
    hr(),
    div(class="row-fluid",
        column(12,
               verbatimTextOutput("query1"))),
    div(class="row-fluid",
        column(12,
               verbatimTextOutput("query2"))),
    div(class="row-fluid",
        column(12,
               verbatimTextOutput("query3"))),
    div(class="row-fluid",
        column(6,h4("Selected Lines"),
               verbatimTextOutput("lines")),
        column(6,h4("conformity"),
               verbatimTextOutput("conformity"))),
    div(class="row-fluid",
        column(12,
               dataTableOutput("moore")))
    
  )
)

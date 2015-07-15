rm(list = ls())
if (!require("shiny") && !require("sqldf")) {
  install.packages(c("shiny", "sqldf"), dependencies = TRUE)
}

library(shiny)
library(sqldf)

  ui = shinyUI(fluidPage(
    title = 'test Menu',
    div(
      class = "row-fluid", style = "height=350px; background-color:lightgray",
      column(width = 3,
             selectInput("firstCol", "First Axis", choices = NULL),
             selectInput("firstColValue", NULL, choices = NULL, multiple = TRUE, selectize = FALSE)),
      column(width = 3,
             selectInput("secondCol", "Second Axis", choices = NULL),
             selectInput("secondColValue", NULL, choices = NULL, multiple = TRUE, selectize = FALSE)),
      column(width = 3,
             selectInput("thirdCol", "Third Axis", choices = NULL),
             selectInput("thirdColValue", NULL, choices = NULL, multiple = TRUE, selectize = FALSE))
    ),
    div(class="row-fluid",
        column(width=12,
               tableOutput("da")
               )
        )
  ))
